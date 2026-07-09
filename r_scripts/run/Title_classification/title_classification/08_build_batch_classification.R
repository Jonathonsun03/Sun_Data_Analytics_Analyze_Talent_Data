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

source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))

default_batch_run_root <- function() {
  datalake_root <- normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)
  file.path(
    dirname(datalake_root),
    "Processed",
    "Logs",
    "classification",
    "title_classification",
    "batch_runs"
  )
}

csv_path <- arg_value("--csv", "notes/titles.csv")
talent_col <- arg_value("--talent-col", "talent")
video_id_col <- arg_value("--video-id-col", "Video ID")
title_col <- arg_value("--title-col", "Title")
content_type_col <- arg_value("--content-type-col", "Content Type")
published_at_col <- arg_value("--published-at-col", "Published At")
talent_filter <- arg_value("--talent", "")
model <- arg_value("--model", Sys.getenv("OPENAI_MODEL", unset = "gpt-5-mini"))
batch_size <- suppressWarnings(as.integer(arg_value("--batch-size", "25")))
if (is.na(batch_size) || batch_size < 1) {
  batch_size <- 25L
}
run_id <- arg_value("--run-id", paste0("title_classification_batch_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S_%z")))
run_root <- arg_value(
  "--run-root",
  Sys.getenv("TITLE_CLASSIFICATION_BATCH_RUN_ROOT", unset = default_batch_run_root())
)
force_reclassify <- has_flag("--force-reclassify")
classification_key <- "video_id"

full_csv <- if (grepl("^/", csv_path)) csv_path else repo_path(csv_path)
if (!file.exists(full_csv)) {
  stop("CSV not found: ", full_csv)
}

source(repo_path("r_scripts", "lib", "utils", "staging_root.R"))
source(repo_path("r_scripts", "lib", "utils", "talent_select.R"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "duckdb", "db_schema.R"))
source(repo_path("r_scripts", "lib", "duckdb", "ingest_videos.R"))
source(repo_path("r_scripts", "lib", "duckdb", "pending.R"))
source(repo_path("r_scripts", "lib", "ChatGPT", "chatgpt_load_all.R"))
source(repo_path("r_scripts", "lib", "stream_classification", "talent_rules.R"))
source(repo_path("r_scripts", "lib", "stream_classification", "prompt_builder.R"))
source(repo_path("r_scripts", "lib", "title_classification", "batch_helpers.R"))

chatgpt_load_all(exclude_dirs = c("examples"))

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
  stop("No rows found after filters.")
}

run_dir <- file.path(run_root, run_id)
if (dir.exists(run_dir)) {
  stop("Run directory already exists: ", run_dir)
}
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "logs"), recursive = TRUE, showWarnings = FALSE)

con <- duckdb_connect()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
init_duckdb_schema(con)

batch_input_path <- file.path(run_dir, "batch_input.jsonl")
candidate_rows_path <- file.path(run_dir, "candidate_rows.csv")
manifest_path <- file.path(run_dir, "manifest.json")

talents <- unique(x[[talent_col]])
request_count <- 0L
pending_total <- 0L
input_rows_total <- 0L
candidate_rows <- list()
custom_id_map <- list()
prompt_profiles <- list()
manifest_versions <- NULL

write_request <- function(request) {
  cat(
    jsonlite::toJSON(request, auto_unbox = TRUE, null = "null"),
    "\n",
    file = batch_input_path,
    append = TRUE,
    sep = ""
  )
}

custom_id_part <- function(x, max_len = 48L) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", as.character(x))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) {
    x <- "unknown"
  }
  substr(x, 1L, max_len)
}

make_request_custom_id <- function(talent_name, batch_index, batch_df) {
  talent_part <- custom_id_part(talent_name, max_len = 32L)
  if (nrow(batch_df) == 1L) {
    video_part <- custom_id_part(batch_df$video_id[[1]], max_len = 40L)
    hash_part <- substr(as.character(batch_df$title_hash[[1]]), 1L, 10L)
    return(paste("tc", talent_part, video_part, hash_part, sep = "__"))
  }

  batch_hash <- substr(
    digest::digest(
      paste(as.character(batch_df$video_id), collapse = "||"),
      algo = "xxhash64"
    ),
    1L,
    10L
  )
  paste("tc_batch", talent_part, sprintf("%04d", batch_index), batch_hash, sep = "__")
}

if (file.exists(batch_input_path)) {
  file.remove(batch_input_path)
}

for (talent_value in talents) {
  rows <- x[x[[talent_col]] == talent_value, c(video_id_col, title_col, content_type_col), drop = FALSE]
  if (nrow(rows) == 0) {
    next
  }
  names(rows) <- c("Video ID", "Title", "Content Type")
  if (nzchar(published_at_col) && published_at_col %in% names(x)) {
    rows[["Published At"]] <- as.POSIXct(
      x[x[[talent_col]] == talent_value, published_at_col, drop = TRUE],
      tz = "UTC"
    )
  } else {
    rows[["Published At"]] <- as.POSIXct(Sys.time(), tz = "UTC")
  }

  talent_root <- tryCatch(
    select_talent(talent_value),
    error = function(e) NA_character_
  )
  if (is.na(talent_root)) {
    warning("Talent not found in staging folders; using provided Talent value directly.")
    talent_name <- clean_talent_name(talent_value, underscores = TRUE)
  } else {
    talent_name <- safe_basename(talent_root)
  }
  talent_meta <- ensure_talent_id(con, talent_name)
  talent_id <- talent_meta$talent_id[[1]]
  to_ingest <- build_to_ingest(rows, talent_id, talent_name)
  upsert_videos(con, to_ingest)

  prompt_bundle <- load_prompt_bundle(talent_name = talent_name)
  ensure_classification_boolean_columns(con, prompt_bundle$definition_fields)
  if (is.null(manifest_versions)) {
    manifest_versions <- list(
      taxonomy_version = prompt_bundle$taxonomy_version,
      prompt_version = prompt_bundle$prompt_version
    )
  }

  pending <- get_pending_titles(
    con,
    to_ingest,
    prompt_bundle$taxonomy_version,
    prompt_bundle$prompt_version,
    model,
    key = classification_key,
    force_reclassify = force_reclassify
  )

  input_rows_total <- input_rows_total + nrow(to_ingest)
  pending_total <- pending_total + nrow(pending)
  prompt_profiles[[talent_name]] <- prompt_bundle$profile_name
  if (nrow(pending) == 0) {
    message("No pending titles for ", talent_name)
    next
  }

  pending$talent_profile <- prompt_bundle$profile_name
  pending$taxonomy_version <- prompt_bundle$taxonomy_version
  pending$prompt_version <- prompt_bundle$prompt_version
  candidate_rows[[length(candidate_rows) + 1L]] <- pending

  batch_starts <- seq.int(1L, nrow(pending), by = batch_size)
  for (batch_index in seq_along(batch_starts)) {
    start_i <- batch_starts[[batch_index]]
    end_i <- min(start_i + batch_size - 1L, nrow(pending))
    batch_df <- pending[start_i:end_i, , drop = FALSE]
    request_count <- request_count + 1L
    custom_id <- make_request_custom_id(talent_name, batch_index, batch_df)
    messages <- build_title_classification_messages(
      batch_df = batch_df,
      user_prompt_template = prompt_bundle$user_prompt_template,
      schema_text = prompt_bundle$schema_text,
      system_prompt = prompt_bundle$system_prompt,
      talent_name = talent_name,
      talent_profile = prompt_bundle$profile_name
    )
    request <- list(
      custom_id = custom_id,
      method = "POST",
      url = "/v1/chat/completions",
      body = list(
        model = model,
        messages = messages,
        temperature = 1
      )
    )
    write_request(request)
    custom_id_map[[custom_id]] <- list(
      talent_name = talent_name,
      talent_id = talent_id,
      talent_profile = prompt_bundle$profile_name,
      taxonomy_version = prompt_bundle$taxonomy_version,
      prompt_version = prompt_bundle$prompt_version,
      definition_fields = prompt_bundle$definition_fields,
      video_ids = as.character(batch_df$video_id),
      title_hashes = as.character(batch_df$title_hash)
    )
  }

  message("Prepared ", nrow(pending), " pending titles for ", talent_name)
}

candidate_out <- if (length(candidate_rows) > 0) {
  do.call(rbind, candidate_rows)
} else {
  data.frame()
}
write.csv(candidate_out, candidate_rows_path, row.names = FALSE, na = "")

manifest <- list(
  run_id = run_id,
  created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  mode = "title_classification_batch",
  endpoint = "/v1/chat/completions",
  model = model,
  taxonomy_version = manifest_versions$taxonomy_version,
  prompt_version = manifest_versions$prompt_version,
  batch_size = batch_size,
  input_rows = input_rows_total,
  pending_rows = pending_total,
  request_count = request_count,
  force_reclassify = force_reclassify,
  prompt_profiles = prompt_profiles,
  custom_id_map = custom_id_map,
  batch_id = NULL,
  batch_status = NULL,
  input_file_id = NULL,
  output_file_id = NULL,
  error_file_id = NULL,
  artifacts = list(
    batch_input_jsonl = batch_input_path,
    candidate_rows_csv = candidate_rows_path
  )
)
jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE, null = "null")

message("Run directory: ", run_dir)
message("Input rows: ", input_rows_total)
message("Pending rows: ", pending_total)
message("Batch requests: ", request_count)
message("Batch input: ", batch_input_path)
message("Manifest: ", manifest_path)
