# Load and process one candidate track for the batch runner.

subtitle_backfill_process_track <- function(track, config, pipeline_run_id, label) {
  video_input <- subtitle_backfill_with_reader(
    config$db_path,
    config$collection_marker,
    function(con) {
      raw_units <- load_subtitle_track_for_backfill(
        con,
        video_id = track$video_id[[1]],
        subtitle_language = track$subtitle_language[[1]],
        subtitle_track_type = track$subtitle_track_type[[1]]
      )
      list(
        raw_units = raw_units,
        is_current = !config$force && subtitle_backfill_track_is_current(
          con,
          raw_units,
          config$pipeline_version,
          config$source_scope
        ),
        checkpoints = load_subtitle_backfill_checkpoints(
          con,
          track$video_id[[1]],
          track$subtitle_language[[1]],
          track$subtitle_track_type[[1]],
          subtitle_sentence_source_checksum(raw_units),
          config$pipeline_version,
          config$source_scope
        )
      )
    }
  )
  if (video_input$is_current) {
    return(list(status = "current", examined = TRUE, is_new = FALSE))
  }

  tryCatch({
    blocks <- build_punctuation_blocks(
      normalize_subtitle_units_for_reconstruction(video_input$raw_units),
      target_words = config$target_words,
      max_words = config$max_words,
      talent_name = track$talent_code[[1]]
    )
    message(label, " - blocks=", nrow(blocks))

    result <- reconstruct_subtitle_track_with_checkpoints(
      db_path = config$db_path,
      raw_units = video_input$raw_units,
      blocks = blocks,
      pipeline_run_id = pipeline_run_id,
      pipeline_version = config$pipeline_version,
      source_scope = config$source_scope,
      punctuation_url = config$punctuation_url,
      timeout_sec = config$timeout_sec,
      max_attempts = config$max_attempts,
      request_pause_sec = config$request_pause_sec,
      retry_failed = config$retry_failed,
      force = config$force,
      checkpoints = video_input$checkpoints,
      speaker_change_fn = if (config$speaker_turns_enabled) {
        function(previous, sentence_a, sentence_b, following) {
          request_subtitle_speaker_change(
            previous,
            sentence_a,
            sentence_b,
            following,
            url = config$speaker_change_url,
            model = config$speaker_change_model,
            timeout_sec = config$speaker_change_timeout_sec,
            stop_requested_fn = function() subtitle_backfill_stop_requested(config$stop_request_path)
          )
        }
      } else {
        NULL
      },
      speaker_cache_identity = list(
        model = config$speaker_change_model,
        url = config$speaker_change_url,
        prompt = subtitle_speaker_change_prompt()
      ),
      stop_requested_fn = function() {
        subtitle_backfill_stop_requested(config$stop_request_path)
      },
      before_publish = function() {
        subtitle_backfill_wait_for_collection(
          config$collection_marker,
          "publishing completed video"
        )
        subtitle_backfill_assert_running(config$stop_request_path)
      }
    )
    c(list(status = "published", examined = TRUE, is_new = TRUE), result)
  },
  subtitle_backfill_stop_requested = function(error) {
    list(status = "cancelled", error = error, examined = TRUE, is_new = TRUE)
  },
  error = function(error) {
    list(status = "failed", error = error, examined = TRUE, is_new = TRUE)
  })
}
