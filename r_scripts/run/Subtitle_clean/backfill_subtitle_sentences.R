library(DBI)
library(dplyr)
library(here)
library(purrr)
library(stringr)
library(tibble)

source(here::here("r_scripts", "lib", "utils", "repo_env.R"))
source(here::here("r_scripts", "lib", "utils", "datalake_root.r"))
source(here::here("r_scripts", "lib", "duckdb", "db_connect.R"))
source(here::here(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "clean_subtitles.R"
))
source(here::here(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "punctuation_client.R"
))
source(here::here(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "subtitle_units.R"
))
source(here::here("r_scripts", "lib", "duckdb", "subtitle_sentence_schema.R"))
source(here::here("r_scripts", "lib", "duckdb", "subtitle_sentence_publish.R"))
source(here::here("r_scripts", "lib", "duckdb", "subtitle_sentence_backfill.R"))

backfill_exit_status <- with_inference_machine({
load_repo_env(repo_root = here::here())

backfill_env_text <- function(name, default = NULL) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) default else value
}

backfill_env_bool <- function(name, default = FALSE) {
  value <- backfill_env_text(name)
  if (is.null(value)) return(default)
  tolower(value) %in% c("1", "true", "yes", "on")
}

backfill_env_integer <- function(name, default, minimum = 0L) {
  value <- suppressWarnings(as.integer(backfill_env_text(name, as.character(default))))
  if (is.na(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".", call. = FALSE)
  }
  value
}

backfill_env_number <- function(name, default, minimum = 0) {
  value <- suppressWarnings(as.numeric(backfill_env_text(name, as.character(default))))
  if (is.na(value) || value < minimum) {
    stop(name, " must be numeric and >= ", minimum, ".", call. = FALSE)
  }
  value
}

dry_run <- backfill_env_bool("SUBTITLE_BACKFILL_DRY_RUN", default = TRUE)
talent_code <- backfill_env_text("SUBTITLE_BACKFILL_TALENT_CODE")
video_id <- backfill_env_text("SUBTITLE_BACKFILL_VIDEO_ID")
max_videos <- backfill_env_integer("SUBTITLE_BACKFILL_MAX_VIDEOS", 0L, 0L)
max_attempts <- backfill_env_integer("SUBTITLE_BACKFILL_MAX_ATTEMPTS", 3L, 1L)
timeout_sec <- backfill_env_number("SUBTITLE_PUNCTUATION_TIMEOUT_SEC", 120, 1)
request_pause_sec <- backfill_env_number("SUBTITLE_BACKFILL_REQUEST_PAUSE_SEC", 0.2, 0)
target_words <- backfill_env_integer("SUBTITLE_BLOCK_TARGET_WORDS", 175L, 1L)
max_words <- backfill_env_integer("SUBTITLE_BLOCK_MAX_WORDS", 200L, target_words)
pipeline_version <- backfill_env_text(
  "SUBTITLE_BACKFILL_PIPELINE_VERSION",
  "subtitle_sentence_v1"
)
punctuation_url <- backfill_env_text(
  "SUBTITLE_PUNCTUATION_URL",
  inference_punctuation_url()
)
allow_unknown_language <- backfill_env_bool(
  "SUBTITLE_PUNCTUATION_ALLOW_UNKNOWN_LANGUAGE",
  default = TRUE
)
retry_failed <- backfill_env_bool("SUBTITLE_BACKFILL_RETRY_FAILED", default = FALSE)
force <- backfill_env_bool("SUBTITLE_BACKFILL_FORCE", default = FALSE)
source_scope <- "full_track"
db_path <- talent_lakehouse_db_path()

with_backfill_reader <- function(operation) {
  con <- duckdb_connect(db_path = db_path, read_only = TRUE)
  tryCatch(operation(con), finally = DBI::dbDisconnect(con, shutdown = TRUE))
}

tracks <- with_backfill_reader(function(con) {
  list_subtitle_backfill_tracks(
    con,
    talent_code = talent_code,
    video_id = video_id
  )
})
if (nrow(tracks) == 0L) stop("No subtitle tracks matched the selection.", call. = FALSE)

language_supported <- subtitle_language_is_english(
  tracks$subtitle_language,
  allow_unknown = allow_unknown_language
)
language_supported[is.na(language_supported)] <- FALSE
unsupported_tracks <- tracks[!language_supported, , drop = FALSE]
tracks <- tracks[language_supported, , drop = FALSE]
if (max_videos > 0L) tracks <- utils::head(tracks, max_videos)

message("Subtitle sentence backfill configuration")
message("  database: ", db_path)
message("  mode: ", if (dry_run) "DRY RUN" else "EXECUTE")
message("  pipeline version: ", pipeline_version)
message("  selected tracks: ", nrow(tracks))
message("  selected raw rows: ", format(sum(tracks$raw_rows), big.mark = ","))
message("  unsupported-language tracks: ", nrow(unsupported_tracks))
message("  FullStop URL: ", punctuation_url)
message("  block target/max words: ", target_words, "/", max_words)
message("  per-block attempts: ", max_attempts)
message("  request pause seconds: ", request_pause_sec)
message("  retry failed checkpoints: ", retry_failed)
message("  force reconstruction: ", force)

if (dry_run) {
  print(utils::head(
    tracks[, c(
      "video_id", "talent_code", "content_type", "raw_rows", "title"
    )],
    25L
  ))
  message("Dry run complete. No model calls or database writes were made.")
  quit(save = "no", status = 0L)
}

pipeline_run_id <- start_subtitle_backfill_run(db_path)
run_finished <- FALSE
on.exit({
  if (!run_finished) {
    try(
      finish_subtitle_backfill_run(
        db_path,
        pipeline_run_id,
        status = "failed",
        error_summary = "Backfill process ended before normal completion."
      ),
      silent = TRUE
    )
  }
}, add = TRUE)

successful_tracks <- 0L
current_tracks <- 0L
failed_tracks <- 0L
requested_blocks <- 0L
reused_blocks <- 0L
failure_messages <- character()
started_at <- Sys.time()

for (track_index in seq_len(nrow(tracks))) {
  track <- tracks[track_index, , drop = FALSE]
  label <- paste0(
    "[",
    track_index,
    "/",
    nrow(tracks),
    "] ",
    track$video_id[[1]],
    " ",
    track$talent_code[[1]],
    " rows=",
    track$raw_rows[[1]]
  )
  message("")
  message(label, " - loading")

  track_result <- tryCatch(
    {
      video_input <- with_backfill_reader(function(con) {
        raw_units <- load_subtitle_track_for_backfill(
          con,
          video_id = track$video_id[[1]],
          subtitle_language = track$subtitle_language[[1]],
          subtitle_track_type = track$subtitle_track_type[[1]]
        )
        list(
          raw_units = raw_units,
          is_current = !force && subtitle_backfill_track_is_current(
            con, raw_units, pipeline_version, source_scope
          ),
          checkpoints = load_subtitle_backfill_checkpoints(
            con, track$video_id[[1]], track$subtitle_language[[1]],
            track$subtitle_track_type[[1]], subtitle_sentence_source_checksum(raw_units),
            pipeline_version, source_scope
          )
        )
      })
      raw_units <- video_input$raw_units
      is_current <- video_input$is_current
      if (is_current) {
        list(status = "current")
      } else {
        normalized_units <- normalize_subtitle_units_for_reconstruction(raw_units)
        blocks <- build_punctuation_blocks(
          normalized_units,
          target_words = target_words,
          max_words = max_words,
          talent_name = track$talent_code[[1]]
        )
        message(label, " - blocks=", nrow(blocks))
        result <- reconstruct_subtitle_track_with_checkpoints(
          db_path = db_path,
          raw_units = raw_units,
          blocks = blocks,
          pipeline_run_id = pipeline_run_id,
          pipeline_version = pipeline_version,
          source_scope = source_scope,
          punctuation_url = punctuation_url,
          timeout_sec = timeout_sec,
          max_attempts = max_attempts,
          request_pause_sec = request_pause_sec,
          retry_failed = retry_failed,
          force = force,
          checkpoints = video_input$checkpoints
        )
        c(list(status = "published"), result)
      }
    },
    error = function(error) list(status = "failed", error = error)
  )

  if (identical(track_result$status, "current")) {
    current_tracks <- current_tracks + 1L
    message(label, " - already current; skipped")
  } else if (identical(track_result$status, "published")) {
    successful_tracks <- successful_tracks + 1L
    requested_blocks <- requested_blocks + track_result$requested_blocks
    reused_blocks <- reused_blocks + track_result$reused_blocks
    message(
      label,
      " - published sentences=",
      track_result$sentences,
      " blocks=",
      track_result$blocks,
      " requested=",
      track_result$requested_blocks,
      " reused=",
      track_result$reused_blocks
    )
  } else {
    failed_tracks <- failed_tracks + 1L
    failure_text <- paste0(label, " - FAILED: ", conditionMessage(track_result$error))
    failure_messages <- c(failure_messages, failure_text)
    message(failure_text)
  }

  elapsed_minutes <- as.numeric(difftime(Sys.time(), started_at, units = "mins"))
  message(
    "Progress: published=",
    successful_tracks,
    " current=",
    current_tracks,
    " failed=",
    failed_tracks,
    " elapsed_min=",
    sprintf("%.1f", elapsed_minutes)
  )
}

summary_text <- paste0(
  "selected=",
  nrow(tracks),
  "; published=",
  successful_tracks,
  "; current=",
  current_tracks,
  "; failed=",
  failed_tracks,
  "; requested_blocks=",
  requested_blocks,
  "; reused_blocks=",
  reused_blocks
)
if (length(failure_messages) > 0L) {
  summary_text <- paste(
    summary_text,
    paste(utils::head(failure_messages, 10L), collapse = " | "),
    sep = "; "
  )
}
summary_text <- substr(summary_text, 1L, 1000L)
finish_subtitle_backfill_run(
  db_path,
  pipeline_run_id,
  status = if (failed_tracks == 0L) "completed" else "failed",
  error_summary = summary_text
)
run_finished <- TRUE

message("")
message("Subtitle sentence backfill finished: ", summary_text)
# Return through the lifecycle scope before exiting, so its on.exit cleanup runs.
if (failed_tracks > 0L) 1L else 0L

})
quit(save = "no", status = backfill_exit_status)
