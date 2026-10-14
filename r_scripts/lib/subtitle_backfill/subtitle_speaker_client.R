# Infer missing speaker boundaries after sentence reconstruction.

subtitle_speaker_change_prompt <- function() {
  paste(
    "Determine whether Sentence B is spoken by the same person as Sentence A.",
    "same_speaker: Sentence B continues the same person's thought, explanation, story, or statement.",
    "speaker_change: Sentence B is a response, interruption, question/answer reversal, direct reply, or otherwise appears to come from another participant.",
    "uncertain: There is not enough textual evidence to decide.",
    "Example:",
    "A: I'm sure you're wondering where I've been.",
    "B: Don't worry, I'm safe.",
    "Answer: same_speaker",
    "Example:",
    "A: I thought that was your job, Hawthorne.",
    "B: Yes, that is my job.",
    "Answer: speaker_change",
    "Return exactly one label and nothing else.",
    sep = "\n"
  )
}

subtitle_speaker_transient_error <- function(message) {
  errorCondition(
    message = message,
    class = "subtitle_speaker_transient_error"
  )
}

subtitle_speaker_http_status_is_transient <- function(status) {
  status %in% c(408L, 429L) || (status >= 500L && status <= 599L)
}

subtitle_speaker_error_is_transient <- function(message) {
  grepl("(?:node|C) stack overflow", message, ignore.case = TRUE)
}

subtitle_speaker_response_label <- function(response) {
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  payload <- tryCatch(
    jsonlite::fromJSON(response_text, simplifyVector = TRUE),
    error = function(error) error
  )
  if (inherits(payload, "error")) stop(payload)
  result <- trimws(tolower(payload$message$content))
  if (!result %in% c("same_speaker", "speaker_change", "uncertain")) {
    "uncertain"
  } else {
    result
  }
}

subtitle_speaker_with_retries <- function(
    operation,
    max_attempts = 5L,
    retry_delays = c(2, 5, 10, 20),
    sleep_fn = Sys.sleep,
    stop_requested_fn = function() FALSE) {
  max_attempts <- as.integer(max_attempts)
  if (is.na(max_attempts) || max_attempts < 1L) {
    stop("`max_attempts` must be an integer >= 1.", call. = FALSE)
  }
  if (length(retry_delays) == 0L || any(!is.finite(retry_delays)) ||
      any(retry_delays < 0)) {
    stop("`retry_delays` must contain non-negative numbers.", call. = FALSE)
  }

  for (attempt in seq_len(max_attempts)) {
    if (isTRUE(stop_requested_fn())) stop(subtitle_backfill_stop_condition())
    result <- tryCatch(
      operation(),
      subtitle_speaker_transient_error = function(error) error
    )
    if (!inherits(result, "subtitle_speaker_transient_error")) return(result)
    if (attempt == max_attempts) stop(result)

    delay <- retry_delays[[min(attempt, length(retry_delays))]]
    message(
      "Speaker-change request temporarily unavailable; retrying in ",
      delay,
      " seconds (attempt ",
      attempt + 1L,
      "/",
      max_attempts,
      ")."
    )
    if (isTRUE(stop_requested_fn())) stop(subtitle_backfill_stop_condition())
    sleep_fn(delay)
  }
}

request_subtitle_speaker_change <- function(
    previous_context,
    sentence_a,
    sentence_b,
    following_context,
    url,
    model = "Qwen/Qwen2.5-3B-Instruct",
    timeout_sec = 120,
    max_attempts = 5L,
    stop_requested_fn = function() FALSE) {
  dialogue <- paste(
    "Previous context:", ifelse(nzchar(previous_context), previous_context, "[none]"),
    "Sentence A:", sentence_a,
    "Sentence B:", sentence_b,
    "Following context:", ifelse(nzchar(following_context), following_context, "[none]"),
    sep = "\n"
  )
  response <- subtitle_speaker_with_retries(
    function() {
      response <- tryCatch(
        httr::POST(
          url,
          httr::accept_json(),
          httr::content_type_json(),
          httr::timeout(timeout_sec),
          body = list(
            messages = list(
              list(
                role = "system",
                content = subtitle_speaker_change_prompt()
              ),
              list(role = "user", content = dialogue)
            ),
            model = model,
            max_new_tokens = 8L,
            temperature = 0
          ),
          encode = "json"
        ),
        error = function(error) {
          stop(subtitle_speaker_transient_error(conditionMessage(error)))
        }
      )
      status <- httr::status_code(response)
      if (subtitle_speaker_http_status_is_transient(status)) {
        stop(subtitle_speaker_transient_error(paste(
          "Speaker-change request failed with HTTP",
          status
        )))
      }
      if (status < 200L || status >= 300L) {
        detail <- trimws(substr(
          httr::content(response, as = "text", encoding = "UTF-8"),
          1L,
          500L
        ))
        stop(
          "Speaker-change request failed with HTTP ", status, ": ",
          if (nzchar(detail)) detail else "no response body",
          call. = FALSE
        )
      }
      tryCatch(
        subtitle_speaker_response_label(response),
        error = function(error) {
          message <- conditionMessage(error)
          if (subtitle_speaker_error_is_transient(message)) {
            stop(subtitle_speaker_transient_error(paste(
              "Speaker-change response could not be parsed:", message
            )))
          }
          stop(error)
        }
      )
    },
    max_attempts = max_attempts,
    stop_requested_fn = stop_requested_fn
  )
  response
}
