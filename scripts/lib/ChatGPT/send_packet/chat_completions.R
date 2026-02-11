chatgpt_send_chat <- function(
    messages,
    model = NULL,
    temperature = 0.2,
    api_key = NULL,
    base_url = NULL,
    timeout_seconds = 120
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package `httr` is required. Install it or replace with httr2.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required.")
  }

  model <- chatgpt_get_model(model = model)
  if (is.null(api_key)) {
    api_key <- chatgpt_get_api_key()
  }
  if (is.null(base_url)) {
    base_url <- chatgpt_get_base_url()
  }

  url <- paste0(base_url, "/chat/completions")
  if (!is.null(temperature) && grepl("^gpt-5", model) && temperature != 1) {
    temperature <- NULL
  }

  body <- list(
    model = model,
    messages = messages
  )
  if (!is.null(temperature)) {
    body$temperature <- temperature
  }

  resp <- httr::POST(
    url,
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = body,
    encode = "json",
    httr::timeout(timeout_seconds)
  )

  if (httr::http_error(resp)) {
    err_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    status <- httr::status_code(resp)
    stop(sprintf("HTTP %s error: %s", status, err_text), call. = FALSE)
  }
  httr::content(resp, as = "parsed", type = "application/json")
}

chatgpt_extract_text <- function(response) {
  if (is.null(response$choices) || length(response$choices) == 0) {
    return(NA_character_)
  }
  response$choices[[1]]$message$content
}
