source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_controls.R"), local = TRUE)

# Environment parsing and collection-aware DuckDB reads for the backfill runner.

subtitle_backfill_env_text <- function(name, default = NULL) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) default else value
}

subtitle_backfill_env_bool <- function(name, default = FALSE) {
  value <- subtitle_backfill_env_text(name)
  if (is.null(value)) return(default)
  tolower(value) %in% c("1", "true", "yes", "on")
}

subtitle_backfill_env_integer <- function(name, default, minimum = 0L) {
  value <- suppressWarnings(as.integer(
    subtitle_backfill_env_text(name, as.character(default))
  ))
  if (is.na(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".", call. = FALSE)
  }
  value
}

subtitle_backfill_env_number <- function(name, default, minimum = 0) {
  value <- suppressWarnings(as.numeric(
    subtitle_backfill_env_text(name, as.character(default))
  ))
  if (!is.finite(value) || value < minimum) {
    stop(name, " must be numeric and >= ", minimum, ".", call. = FALSE)
  }
  value
}

subtitle_backfill_collection_marker <- function() {
  subtitle_backfill_env_text(
    "COLLECTION_ACTIVE_MARKER",
    file.path(get_datalake_root(), "Logs", "collection-active")
  )
}

subtitle_backfill_config <- function() {
  target_words <- subtitle_backfill_env_integer(
    "SUBTITLE_BLOCK_TARGET_WORDS",
    175L,
    1L
  )
  list(
    max_runtime_sec = subtitle_backfill_env_number("SUBTITLE_BACKFILL_MAX_RUNTIME_SEC", 14400),
    dry_run = subtitle_backfill_env_bool(
      "SUBTITLE_BACKFILL_DRY_RUN",
      default = TRUE
    ),
    talent_code = subtitle_backfill_env_text("SUBTITLE_BACKFILL_TALENT_CODE"),
    video_id = subtitle_backfill_env_text("SUBTITLE_BACKFILL_VIDEO_ID"),
    max_videos = subtitle_backfill_env_integer(
      "SUBTITLE_BACKFILL_MAX_VIDEOS",
      0L,
      0L
    ),
    max_new_videos = subtitle_backfill_env_integer(
      "SUBTITLE_BACKFILL_MAX_NEW_VIDEOS",
      0L,
      0L
    ),
    max_attempts = subtitle_backfill_env_integer(
      "SUBTITLE_BACKFILL_MAX_ATTEMPTS",
      3L,
      1L
    ),
    timeout_sec = subtitle_backfill_env_number(
      "SUBTITLE_PUNCTUATION_TIMEOUT_SEC",
      120,
      1
    ),
    request_pause_sec = subtitle_backfill_env_number(
      "SUBTITLE_BACKFILL_REQUEST_PAUSE_SEC",
      0.2,
      0
    ),
    target_words = target_words,
    max_words = subtitle_backfill_env_integer(
      "SUBTITLE_BLOCK_MAX_WORDS",
      200L,
      target_words
    ),
    pipeline_version = subtitle_backfill_env_text(
      "SUBTITLE_BACKFILL_PIPELINE_VERSION",
      "subtitle_sentence_v2"
    ),
    punctuation_url = subtitle_backfill_env_text(
      "SUBTITLE_PUNCTUATION_URL",
      inference_punctuation_url()
    ),
    speaker_change_url = paste0(
      sub("/+$", "", subtitle_backfill_env_text(
        "NLP_INFERENCE_URL",
        "http://192.168.1.173:8000"
      )),
      "/v1/chat"
    ),
    speaker_change_model = "Qwen/Qwen2.5-3B-Instruct",
    speaker_turns_enabled = subtitle_backfill_env_bool(
      "SUBTITLE_SPEAKER_TURNS_ENABLED",
      default = TRUE
    ),
    speaker_change_timeout_sec = subtitle_backfill_env_number(
      "SUBTITLE_SPEAKER_CHANGE_TIMEOUT_SEC",
      120,
      1
    ),
    allow_unknown_language = subtitle_backfill_env_bool(
      "SUBTITLE_PUNCTUATION_ALLOW_UNKNOWN_LANGUAGE",
      default = TRUE
    ),
    retry_failed = subtitle_backfill_env_bool(
      "SUBTITLE_BACKFILL_RETRY_FAILED",
      default = FALSE
    ),
    force = subtitle_backfill_env_bool("SUBTITLE_BACKFILL_FORCE", default = FALSE),
    source_scope = "full_track",
    db_path = talent_lakehouse_db_path(),
    collection_marker = subtitle_backfill_collection_marker(),
    stop_request_path = subtitle_backfill_stop_request_path(),
    active_marker_path = subtitle_backfill_active_marker_path()
  )
}

subtitle_backfill_wait_for_collection <- function(
    marker_path,
    context,
    poll_interval_sec = 15) {
  waiting <- FALSE
  while (file.exists(marker_path)) {
    subtitle_backfill_assert_running(subtitle_backfill_stop_request_path())
    if (!waiting) {
      message(
        "Collection is active; waiting before ",
        context,
        ": ",
        marker_path
      )
      waiting <- TRUE
    }
    Sys.sleep(min(poll_interval_sec, 3))
  }
  if (waiting) message("Collection finished; continuing ", context, ".")
  invisible(NULL)
}

subtitle_backfill_with_reader <- function(db_path, marker_path, operation) {
  subtitle_backfill_with_connection(
    db_path, operation, read_only = TRUE, marker_path = marker_path
  )
}
