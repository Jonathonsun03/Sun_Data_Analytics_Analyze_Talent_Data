get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

repo_root <- normalizePath(
  file.path(get_script_dir(), "..", "..", "..", ".."),
  winslash = "/",
  mustWork = FALSE
)
repo_path <- function(...) normalizePath(file.path(repo_root, ...), winslash = "/", mustWork = FALSE)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) {
    return(default)
  }
  pos <- idx[[1]] + 1L
  if (pos > length(args)) {
    return(default)
  }
  args[[pos]]
}

has_flag <- function(flag) {
  any(args == flag)
}

csv_path <- arg_value("--csv", "notes/titles.csv")
talent_col <- arg_value("--talent-col", "talent")
video_id_col <- arg_value("--video-id-col", "Video ID")
title_col <- arg_value("--title-col", "Title")
content_type_col <- arg_value("--content-type-col", "Content Type")
published_at_col <- arg_value("--published-at-col", "")
talent_filter <- arg_value("--talent", "")

limit_per_talent <- suppressWarnings(as.integer(arg_value("--limit-per-talent", "0")))
if (is.na(limit_per_talent) || limit_per_talent < 0) {
  limit_per_talent <- 0L
}

model <- arg_value("--model", Sys.getenv("OPENAI_MODEL", unset = "gpt-5-mini"))
batch_size <- arg_value("--batch-size", Sys.getenv("CLASSIFICATION_BATCH_SIZE", unset = "25"))
max_retries <- arg_value("--max-retries", Sys.getenv("CLASSIFICATION_MAX_RETRIES", unset = "2"))
force_reclassify <- has_flag("--force-reclassify")

Sys.setenv(OPENAI_MODEL = model)
Sys.setenv(CLASSIFICATION_BATCH_SIZE = as.character(batch_size))
Sys.setenv(CLASSIFICATION_MAX_RETRIES = as.character(max_retries))
Sys.setenv(CLASSIFICATION_FORCE_RECLASSIFY = if (force_reclassify) "1" else "0")

full_csv <- if (grepl("^/", csv_path)) csv_path else repo_path(csv_path)
if (!file.exists(full_csv)) {
  stop("CSV not found: ", full_csv)
}

ingest_script <- repo_path("scripts", "run", "Title_classification", "title_classification", "01_ingest_titles.R")
classify_script <- repo_path("scripts", "run", "Title_classification", "title_classification", "02_classify_pending_titles.R")
if (!file.exists(ingest_script) || !file.exists(classify_script)) {
  stop("Missing ingest/classify scripts under scripts/run/Title_classification/title_classification/")
}

x <- read.csv(
  full_csv,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

required_cols <- c(talent_col, video_id_col, title_col, content_type_col)
missing_cols <- setdiff(required_cols, names(x))
if (length(missing_cols) > 0) {
  stop("Missing required CSV columns: ", paste(missing_cols, collapse = ", "))
}

if (nzchar(talent_filter)) {
  x <- x[tolower(x[[talent_col]]) == tolower(talent_filter), , drop = FALSE]
}
if (nrow(x) == 0) {
  cat("No rows found after filters.\n")
  quit(status = 0)
}

talents <- unique(x[[talent_col]])
total_rows <- 0L

for (t in talents) {
  rows <- x[x[[talent_col]] == t, c(video_id_col, title_col, content_type_col), drop = FALSE]
  if (nrow(rows) == 0) {
    next
  }
  if (limit_per_talent > 0 && nrow(rows) > limit_per_talent) {
    rows <- head(rows, limit_per_talent)
  }

  names(rows) <- c("Video ID", "Title", "Content Type")
  if (nzchar(published_at_col) && published_at_col %in% names(x)) {
    published_values <- x[x[[talent_col]] == t, published_at_col, drop = TRUE]
    if (limit_per_talent > 0 && length(published_values) > limit_per_talent) {
      published_values <- head(published_values, limit_per_talent)
    }
    rows[["Published At"]] <- as.POSIXct(published_values, tz = "UTC")
  } else {
    rows[["Published At"]] <- as.POSIXct(Sys.time(), tz = "UTC")
  }

  Classification <- rows
  Talent <- as.character(t)

  cat("Running talent:", Talent, "rows:", nrow(Classification), "\n")
  source(ingest_script, local = TRUE)
  source(classify_script, local = TRUE)
  total_rows <- total_rows + nrow(Classification)
}

cat("Weekly classification run complete. Input rows processed:", total_rows, "\n")
