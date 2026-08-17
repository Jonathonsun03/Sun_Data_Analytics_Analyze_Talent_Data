library(jsonlite)
library(stringr)

read_text_file <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

normalize_talent_key <- function(x) {
  x <- enc2utf8(as.character(x))

  # Strip exported unicode marker artifacts before normalization.
  x <- gsub("(?i)<\\s*u\\+[0-9a-f]{4,6}\\s*>", " ", x, perl = TRUE)
  x <- gsub("(?i)<\\s*[0-9a-f]{2}\\s*>", " ", x, perl = TRUE)

  # Transliterate to ASCII for locale-safe matching (e.g., LC_ALL=C).
  x_ascii <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  fallback_ascii <- iconv(x, to = "ASCII//TRANSLIT", sub = " ")
  use_ascii <- ifelse(is.na(x_ascii), fallback_ascii, x_ascii)
  x <- ifelse(is.na(use_ascii), x, use_ascii)

  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_+|_+$", "")
  x
}
