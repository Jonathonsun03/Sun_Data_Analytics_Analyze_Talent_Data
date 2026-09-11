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
source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_tracks.R"))
source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_database.R"))
source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_checkpoints.R"))
source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_inference.R"))
source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_reconstruction.R"))
source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_runtime.R"))
source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_batch.R"))

backfill_exit_status <- with_inference_machine({
load_repo_env(repo_root = here::here())

config <- subtitle_backfill_config()

tracks <- subtitle_backfill_with_reader(
  config$db_path,
  config$collection_marker,
  function(con) {
    list_subtitle_backfill_tracks(
      con,
      talent_code = config$talent_code,
      video_id = config$video_id
    )
  }
)
if (nrow(tracks) == 0L) stop("No subtitle tracks matched the selection.", call. = FALSE)

language_supported <- subtitle_language_is_english(
  tracks$subtitle_language,
  allow_unknown = config$allow_unknown_language
)
language_supported[is.na(language_supported)] <- FALSE
unsupported_tracks <- tracks[!language_supported, , drop = FALSE]
tracks <- tracks[language_supported, , drop = FALSE]
if (config$max_videos > 0L) tracks <- utils::head(tracks, config$max_videos)

message("Subtitle sentence backfill configuration")
message("  database: ", config$db_path)
message("  mode: ", if (config$dry_run) "DRY RUN" else "EXECUTE")
message("  pipeline version: ", config$pipeline_version)
message("  selected tracks: ", nrow(tracks))
message(
  "  max new videos: ",
  if (config$max_new_videos == 0L) "all" else config$max_new_videos
)
message("  collection marker: ", config$collection_marker)
message("  selected raw rows: ", format(sum(tracks$raw_rows), big.mark = ","))
message("  unsupported-language tracks: ", nrow(unsupported_tracks))
message("  FullStop URL: ", config$punctuation_url)
message("  block target/max words: ", config$target_words, "/", config$max_words)
message("  per-block attempts: ", config$max_attempts)
message("  request pause seconds: ", config$request_pause_sec)
message("  retry failed checkpoints: ", config$retry_failed)
message("  force reconstruction: ", config$force)

if (config$dry_run) {
  print(utils::head(
    tracks[, c(
      "video_id", "talent_code", "content_type", "raw_rows", "title"
    )],
    25L
  ))
  message("Dry run complete. No model calls or database writes were made.")
  quit(save = "no", status = 0L)
}

subtitle_backfill_wait_for_collection(
  config$collection_marker,
  "starting the backfill run"
)
pipeline_run_id <- start_subtitle_backfill_run(config$db_path)
run_finished <- FALSE
on.exit({
  if (!run_finished) {
    try(
      finish_subtitle_backfill_run(
        config$db_path,
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
examined_tracks <- 0L
new_videos_started <- 0L

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
    subtitle_backfill_process_track(track, config, pipeline_run_id, label),
    error = function(error) {
      list(status = "failed", error = error, examined = FALSE, is_new = FALSE)
    }
  )
  if (isTRUE(track_result$examined)) examined_tracks <- examined_tracks + 1L
  if (isTRUE(track_result$is_new)) {
    new_videos_started <- new_videos_started + 1L
  }

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
  if (config$max_new_videos > 0L &&
      new_videos_started >= config$max_new_videos) {
    break
  }
}

summary_text <- paste0(
  "selected=",
  nrow(tracks),
  "; examined=",
  examined_tracks,
  "; new_started=",
  new_videos_started,
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
subtitle_backfill_wait_for_collection(
  config$collection_marker,
  "finishing the backfill run"
)
finish_subtitle_backfill_run(
  config$db_path,
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
