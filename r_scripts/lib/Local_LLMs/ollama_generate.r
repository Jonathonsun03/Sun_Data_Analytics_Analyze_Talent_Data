library(httr)
library(jsonlite)

ollama_generate <- function(prompt,
                            model = "mistral:latest",
                            host  = "http://100.126.162.100:11434",
                            temperature = 0.2,
                            timeout_s = 600) {

  url <- paste0(sub("/+$", "", host), "/api/generate")
  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    options = list(temperature = temperature)
  )

  resp <- httr::POST(url, body = body, encode = "json", httr::timeout(timeout_s))

  if (httr::http_error(resp)) {
    msg <- httr::content(resp, as = "text", encoding = "UTF-8")
    stop(sprintf("Ollama error %s: %s", httr::status_code(resp), msg), call. = FALSE)
  }

  out <- httr::content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
  out$response
}

chunk_text <- function(x, chunk_chars = 5000) {
  n <- nchar(x, type = "chars")
  starts <- seq(1, n, by = chunk_chars)
  ends <- pmin(starts + chunk_chars - 1, n)
  mapply(function(a, b) substr(x, a, b), starts, ends, SIMPLIFY = TRUE)
}

summarize_chat <- function(snippet,
                           host = "http://100.126.162.100:11434",
                           chunk_chars = 5000,
                           chunk_model = "phi:latest",
                           final_model = "mistral:latest") {

  chunks <- chunk_text(snippet, chunk_chars = chunk_chars)

  chunk_summaries <- vapply(chunks, function(ch) {
    ollama_generate(
      prompt = paste0(
        "Summarize this chunk in 3 bullets (<=16 words each). Bullets only.\n\nCHUNK:\n",
        ch
      ),
      model = chunk_model,
      host = host,
      temperature = 0.1,
      timeout_s = 600
    )
  }, character(1))

  ollama_generate(
    prompt = paste0(
      "Combine these into EXACTLY 5 bullets (<=18 words each). Bullets only.\n\n",
      paste(chunk_summaries, collapse = "\n\n")
    ),
    model = final_model,
    host = host,
    temperature = 0.1,
    timeout_s = 900
  )
}
