source(here::here("r_scripts", "lib", "utils", "inference_machine.R"))

parse_punctuation_response <- function(body) {
  payload <- if (is.list(body)) {
    body
  } else {
    body_text <- paste(as.character(body), collapse = "")
    tryCatch(
      jsonlite::fromJSON(body_text, simplifyVector = TRUE),
      error = function(e) {
        stop("Punctuation API returned invalid JSON: ", conditionMessage(e), call. = FALSE)
      }
    )
  }

  if (is.null(payload$text) || length(payload$text) != 1) {
    stop("Punctuation API response is missing a scalar `text` field.", call. = FALSE)
  }

  punctuated_text <- trimws(as.character(payload$text[[1]]))
  if (is.na(punctuated_text) || !nzchar(punctuated_text)) {
    stop("Punctuation API response contains an empty `text` field.", call. = FALSE)
  }

  model <- NA_character_
  if (!is.null(payload$model) && length(payload$model) >= 1) {
    model <- trimws(as.character(payload$model[[1]]))
    if (!nzchar(model)) model <- NA_character_
  }

  list(text = punctuated_text, model = model)
}

punctuate_text <- function(
    text,
    url = inference_punctuation_url(),
    timeout_sec = 120,
    include_model = FALSE) {
  input_text <- trimws(as.character(text))
  if (length(input_text) != 1 || is.na(input_text) || !nzchar(input_text)) {
    stop("`text` must be one non-empty string.", call. = FALSE)
  }
  if (length(url) != 1 || is.na(url) || !nzchar(trimws(url))) {
    stop("`url` must be one non-empty string.", call. = FALSE)
  }
  if (length(timeout_sec) != 1 || is.na(timeout_sec) || timeout_sec <= 0) {
    stop("`timeout_sec` must be a positive number.", call. = FALSE)
  }

  with_inference_machine({
  ensure_inference_machine(url)
  response <- httr::POST(
    url = url,
    httr::accept_json(),
    httr::content_type_json(),
    httr::timeout(timeout_sec),
    body = list(text = input_text),
    encode = "json"
  )
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  status <- httr::status_code(response)
  if (status < 200 || status >= 300) {
    detail <- trimws(substr(response_text, 1, 500))
    if (!nzchar(detail)) detail <- "no response body"
    stop(
      "Punctuation API request failed with HTTP ", status, ": ", detail,
      call. = FALSE
    )
  }

  parsed <- parse_punctuation_response(response_text)
  if (isTRUE(include_model)) parsed else parsed$text
  })
}
