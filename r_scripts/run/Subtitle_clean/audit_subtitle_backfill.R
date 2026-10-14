# Run from the repository root in the same R environment as the scheduled job.
# Reads existing checkpoints only; never publishes, saves checkpoints, or wakes hosts.
# Usage: Rscript --vanilla r_scripts/run/Subtitle_clean/audit_subtitle_backfill.R
#        [VIDEO_ID] [mock|live] [MAX_LIVE_COMPARISONS]
# Live mode makes bounded speaker API requests; remaining comparisons are mocked.

library(DBI)
library(dplyr)
library(here)
library(purrr)
library(stringr)
library(tibble)

args <- commandArgs(trailingOnly = TRUE)
video_id <- if (length(args) >= 1L) args[[1]] else "LcqXuzMFT8g"
speaker_mode <- if (length(args) >= 2L) args[[2]] else "mock"
live_limit <- if (length(args) >= 3L) as.integer(args[[3]]) else 1L
if (!speaker_mode %in% c("mock", "live") || is.na(live_limit) || live_limit < 1L) {
  stop("Expected VIDEO_ID [mock|live] [positive MAX_LIVE_COMPARISONS].")
}

helper_paths <- c(
  "utils/repo_env.R", "utils/datalake_root.r", "duckdb/db_connect.R",
  "clean_data/clean_subtitles/clean_subtitles.R",
  "clean_data/clean_subtitles/punctuation_client.R",
  "clean_data/clean_subtitles/subtitle_units.R",
  "duckdb/subtitle_sentence_schema.R", "duckdb/subtitle_sentence_publish.R",
  "subtitle_backfill/subtitle_backfill_tracks.R",
  "subtitle_backfill/subtitle_backfill_checkpoints.R",
  "subtitle_backfill/subtitle_backfill_inference.R",
  "subtitle_backfill/subtitle_backfill_speakers.R",
  "subtitle_backfill/subtitle_backfill_reconstruction.R",
  "subtitle_backfill/subtitle_backfill_runtime.R"
)
for (path in helper_paths) source(here::here("r_scripts", "lib", path))
load_repo_env()
config <- subtitle_backfill_config()

cat("Repository:", here::here(), "\n")
cat("Git revision:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
cat("Git changes:\n", paste(system2("git", c("status", "--short"), stdout = TRUE), collapse = "\n"), "\n")
cat("Video:", video_id, " speaker mode:", speaker_mode, "\n")
cat("Database:", config$db_path, "\n")
cat("Pipeline:", config$pipeline_version, " block limits:", config$target_words, config$max_words, "\n")
cat("Speaker endpoint:", config$speaker_change_url, " model:", config$speaker_change_model, "\n")
cat("R library paths:\n", paste(.libPaths(), collapse = "\n"), "\n")
cat("JIT:", Sys.getenv("R_ENABLE_JIT", unset = "default"), " expressions:", getOption("expressions"), "\n")
print(Cstack_info())
print(sessionInfo())
cat("Helper MD5 fingerprints:\n")
print(tools::md5sum(file.path(here::here(), "r_scripts", "lib", helper_paths)))

audit_stage <- function(label, operation) {
  cat("ENTER", label, "\n")
  started <- Sys.time()
  result <- withCallingHandlers(operation(), error = function(error) {
    cat("ERROR STAGE:", label, "\n")
    cat("ERROR:", conditionMessage(error), "\n")
    # Function names avoid dumping transcript text embedded in call arguments.
    cat("ACTIVE CALLS (outermost first):\n")
    for (call in sys.calls()) cat(paste(deparse(call[[1]]), collapse = " "), "\n")
  })
  cat("EXIT", label, " seconds:", as.numeric(difftime(Sys.time(), started, units = "secs")), "\n")
  result
}

# Guard every write path reachable by reconstruction. The publication callback
# is deliberately never evaluated, and punctuation cache misses fail immediately.
save_subtitle_backfill_checkpoints <- function(...) invisible(NULL)
upsert_subtitle_backfill_checkpoint <- function(...) stop("Audit forbids checkpoint writes.")
publish_subtitle_reconstruction <- function(...) stop("Audit forbids publication.")
subtitle_backfill_with_writer <- function(...) {
  cat("PUBLICATION REACHED AND SUPPRESSED\n")
  list(audit_only = TRUE)
}
punctuate_text <- function(...) stop("Audit requires valid cached punctuation; model requests are disabled.")

# Trace the existing helpers so the original reconstruction code, including its
# sentence assembly and validation, executes without a copied implementation.
for (name in c(
  "subtitle_backfill_load_checkpoint_state", "subtitle_backfill_infer_blocks",
  "validate_subtitle_reconstruction", "infer_subtitle_speaker_turns"
)) {
  trace(
    name,
    tracer = substitute(cat("ENTER HELPER", LABEL, "\n"), list(LABEL = name)),
    exit = substitute(cat("EXIT HELPER", LABEL, "\n"), list(LABEL = name)),
    print = FALSE
  )
}

comparison_count <- 0L
live_count <- 0L
classify <- function(previous, sentence_a, sentence_b, following) {
  comparison_count <<- comparison_count + 1L
  if (speaker_mode == "live" && live_count < live_limit) {
    live_count <<- live_count + 1L
    audit_stage(paste("live speaker comparison", live_count), function() {
      request_subtitle_speaker_change(
        previous, sentence_a, sentence_b, following,
        url = config$speaker_change_url,
        model = config$speaker_change_model,
        timeout_sec = min(config$speaker_change_timeout_sec, 30),
        max_attempts = 1L
      )
    })
  } else {
    "uncertain"
  }
}

exit_status <- tryCatch(with_inference_machine({
  input <- audit_stage("read source and database checkpoints", function() {
    con <- duckdb_connect(db_path = config$db_path, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    raw <- load_subtitle_units_for_reconstruction(con, video_id)
    if (nrow(raw) == 0L) stop("No source rows for this video.")
    language <- subtitle_sentence_single_value(raw$subtitle_language, "language")
    track_type <- subtitle_sentence_single_value(raw$subtitle_track_type, "track type")
    checkpoints <- load_subtitle_backfill_checkpoints(
      con, video_id, language, track_type,
      subtitle_sentence_source_checksum(raw), config$pipeline_version, config$source_scope
    )
    list(raw = raw, checkpoints = checkpoints)
  })
  normalized <- audit_stage("normalize source", function() {
    normalize_subtitle_units_for_reconstruction(input$raw)
  })
  blocks <- audit_stage("build punctuation blocks", function() {
    build_punctuation_blocks(
      normalized, target_words = config$target_words, max_words = config$max_words,
      talent_name = subtitle_sentence_single_value(input$raw$talent_code, "talent")
    )
  })
  result <- audit_stage("reconstruct with cached punctuation and isolated speakers", function() {
    reconstruct_subtitle_track_with_checkpoints(
      db_path = config$db_path, raw_units = input$raw, blocks = blocks,
      pipeline_run_id = "read_only_audit", pipeline_version = config$pipeline_version,
      source_scope = config$source_scope, checkpoints = input$checkpoints,
      punctuate_fn = punctuate_text, max_attempts = 1L, request_pause_sec = 0,
      speaker_change_fn = classify,
      speaker_cache_identity = list(
        model = config$speaker_change_model,
        url = config$speaker_change_url,
        prompt = subtitle_speaker_change_prompt()
      )
    )
  })
  cat("AUDIT PASSED: sentences=", result$sentences, " blocks=", result$blocks,
      " reused=", result$reused_blocks, " requested=", result$requested_blocks,
      " speaker comparisons=", comparison_count, " live comparisons=", live_count, "\n")
  0L
}), error = function(error) 1L)
quit(save = "no", status = exit_status)
