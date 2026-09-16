source(file.path(
  "r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_speakers.R"
))

stopifnot(
  subtitle_speaker_http_status_is_transient(408L),
  subtitle_speaker_http_status_is_transient(429L),
  subtitle_speaker_http_status_is_transient(503L),
  !subtitle_speaker_http_status_is_transient(400L),
  !subtitle_speaker_http_status_is_transient(404L)
)

retry_attempts <- 0L
retry_delays <- numeric()
retry_result <- subtitle_speaker_with_retries(
  operation = function() {
    retry_attempts <<- retry_attempts + 1L
    if (retry_attempts < 3L) {
      stop(subtitle_speaker_transient_error("temporary outage"))
    }
    "recovered"
  },
  max_attempts = 5L,
  retry_delays = c(2, 5, 10, 20),
  sleep_fn = function(delay) retry_delays <<- c(retry_delays, delay)
)
stopifnot(
  identical(retry_result, "recovered"),
  retry_attempts == 3L,
  identical(retry_delays, c(2, 5))
)

permanent_attempts <- 0L
permanent_failed <- tryCatch(
  {
    subtitle_speaker_with_retries(function() {
      permanent_attempts <<- permanent_attempts + 1L
      stop("permanent failure")
    })
    FALSE
  },
  error = function(error) TRUE
)
stopifnot(permanent_failed, permanent_attempts == 1L)

exhausted_attempts <- 0L
exhausted_failed <- tryCatch(
  {
    subtitle_speaker_with_retries(
      operation = function() {
        exhausted_attempts <<- exhausted_attempts + 1L
        stop(subtitle_speaker_transient_error("still unavailable"))
      },
      max_attempts = 3L,
      retry_delays = 0,
      sleep_fn = function(delay) invisible(delay)
    )
    FALSE
  },
  subtitle_speaker_transient_error = function(error) TRUE
)
stopifnot(exhausted_failed, exhausted_attempts == 3L)

single_call_count <- 0L
single_sentence <- data.frame(
  text = "Only sentence",
  speaker_turn_id = 1L,
  speaker_change = FALSE
)
single_result <- infer_subtitle_speaker_turns(single_sentence, function(...) {
  single_call_count <<- single_call_count + 1L
  "same_speaker"
})
stopifnot(
  identical(single_result$inferred_speaker_turn_id, 1L),
  single_call_count == 0L
)

sentences <- data.frame(
  text = c("A1", "A2", "B1", "C1", "C2"),
  speaker_turn_id = c(1L, 1L, 1L, 2L, 2L),
  speaker_change = c(FALSE, FALSE, FALSE, TRUE, FALSE)
)
responses <- c("same_speaker", "speaker_change", "uncertain")
call_index <- 0L
result <- infer_subtitle_speaker_turns(sentences, function(...) {
  call_index <<- call_index + 1L
  responses[[call_index]]
})

stopifnot(identical(result$inferred_speaker_turn_id, c(1L, 1L, 2L, 3L, 3L)))
stopifnot(call_index == 3L)
message("Subtitle inferred speaker-turn tests passed.")
