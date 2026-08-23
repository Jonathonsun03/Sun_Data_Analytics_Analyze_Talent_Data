library(here)
library(readr)

source(here("r_scripts", "lib", "clean_data", "clean_subtitles", "punctuation_client.R"))
source(here("r_scripts", "lib", "clean_data", "clean_subtitles", "subtitle_units.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || args[[1]] %in% c("-h", "--help")) {
  stop(
    paste(
      "Usage:",
      "Rscript --vanilla r_scripts/run/Subtitle_clean/run_sentence_reconstruction.R",
      "<cleaned-subtitle.csv> [output.parquet] [language] [talent-name]"
    ),
    call. = FALSE
  )
}

input_path <- normalizePath(args[[1]], mustWork = TRUE)
subtitle_root <- dirname(dirname(input_path))
default_output <- file.path(
  subtitle_root,
  "Sentence_Units",
  paste0(tools::file_path_sans_ext(basename(input_path)), ".parquet")
)
output_path <- if (length(args) >= 2L) args[[2]] else default_output
subtitle_language <- if (length(args) >= 3L) args[[3]] else "en"
talent_name <- if (length(args) >= 4L) args[[4]] else basename(dirname(subtitle_root))

url <- Sys.getenv(
  "SUBTITLE_PUNCTUATION_URL",
  unset = "http://192.168.1.165:8000/v1/punctuate"
)
timeout_sec <- as.numeric(Sys.getenv("SUBTITLE_PUNCTUATION_TIMEOUT_SEC", unset = "120"))
target_words <- as.integer(Sys.getenv("SUBTITLE_BLOCK_TARGET_WORDS", unset = "175"))
max_words <- as.integer(Sys.getenv("SUBTITLE_BLOCK_MAX_WORDS", unset = "200"))

result <- reconstruct_sentence_file(
  input_path = input_path,
  output_path = output_path,
  subtitle_language = subtitle_language,
  talent_name = talent_name,
  target_words = target_words,
  max_words = max_words,
  url = url,
  timeout_sec = timeout_sec,
  allow_unknown_language = FALSE
)

message("Cleaned caption rows: ", result$cleaned_rows)
message("Punctuation blocks: ", nrow(result$blocks))
message("Sentence units: ", nrow(result$sentences))
message("Wrote sentence Parquet: ", result$output_path)
print(utils::head(result$sentences, 10L))
