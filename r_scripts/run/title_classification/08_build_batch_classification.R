get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/")))
  }
  normalizePath(getwd(), winslash = "/")
}

repo_root <- rprojroot::find_root(rprojroot::is_git_root, path = get_script_dir())
repo_path <- function(...) file.path(repo_root, ...)
args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  index <- which(args == flag)
  if (length(index) == 0L || index[[1]] == length(args)) return(default)
  args[[index[[1]] + 1L]]
}

has_flag <- function(flag) flag %in% args

source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "ChatGPT", "chatgpt_load_all.R"))
source(repo_path("r_scripts", "lib", "title_classification", "prompt_builder.R"))
source(repo_path("r_scripts", "lib", "title_classification", "schema.R"))
source(repo_path("r_scripts", "lib", "title_classification", "store.R"))
source(repo_path("r_scripts", "lib", "title_classification", "batch_helpers.R"))
chatgpt_load_all(exclude_dirs = c("examples"))

default_batch_run_root <- function() {
  file.path(
    dirname(normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)),
    "Processed", "Logs", "classification", "title_classification", "batch_runs"
  )
}

model <- arg_value("--model", Sys.getenv("OPENAI_MODEL", unset = "gpt-5-mini"))
talent_filter <- arg_value("--talent", "")
title_version_id <- arg_value("--title-version-id", "")
db_path_arg <- arg_value("--db-path", "")
batch_size <- suppressWarnings(as.integer(arg_value("--batch-size", "25")))
limit_per_talent <- suppressWarnings(as.integer(arg_value("--limit-per-talent", "0")))
if (is.na(batch_size) || batch_size < 1L) batch_size <- 25L
if (is.na(limit_per_talent) || limit_per_talent < 0L) limit_per_talent <- 0L
run_id <- arg_value(
  "--run-id",
  paste0("title_classification_batch_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S_%z"))
)
run_root <- arg_value(
  "--run-root",
  Sys.getenv("TITLE_CLASSIFICATION_BATCH_RUN_ROOT", unset = default_batch_run_root())
)
force_reclassify <- has_flag("--force-reclassify")

db_path <- if (nzchar(db_path_arg)) db_path_arg else talent_lakehouse_db_path()
con <- title_classification_connect(db_path = db_path, read_only = TRUE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
version <- load_active_title_version(con, title_version_id)
pending <- list_pending_title_classifications(
  con = con,
  title_version_id = version$title_version_id[[1]],
  talent_code = talent_filter,
  force_reclassify = force_reclassify
)

if (limit_per_talent > 0L && nrow(pending) > 0L) {
  pending <- do.call(
    rbind,
    lapply(split(pending, pending$talent_code), utils::head, n = limit_per_talent)
  )
  rownames(pending) <- NULL
}

run_dir <- file.path(run_root, run_id)
if (dir.exists(run_dir)) stop("Run directory already exists: ", run_dir)
dir.create(file.path(run_dir, "logs"), recursive = TRUE, showWarnings = FALSE)
batch_input_path <- file.path(run_dir, "batch_input.jsonl")
candidate_rows_path <- file.path(run_dir, "candidate_rows.csv")
manifest_path <- file.path(run_dir, "manifest.json")
file.create(batch_input_path)

custom_id_part <- function(x, max_len = 48L) {
  value <- gsub("[^A-Za-z0-9_-]+", "_", as.character(x))
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) value <- "unknown"
  substr(value, 1L, max_len)
}

make_request_custom_id <- function(talent_code, batch_index, batch_df) {
  if (nrow(batch_df) == 1L) {
    return(paste(
      "tc", custom_id_part(talent_code, 16L),
      custom_id_part(batch_df$video_id[[1]], 40L),
      substr(batch_df$title_hash[[1]], 1L, 10L),
      sep = "__"
    ))
  }
  batch_hash <- substr(
    digest::digest(paste(batch_df$video_id, collapse = "||"), algo = "xxhash64"),
    1L, 10L
  )
  paste(
    "tc_batch", custom_id_part(talent_code, 16L),
    sprintf("%04d", batch_index), batch_hash,
    sep = "__"
  )
}

write_request <- function(request) {
  cat(
    jsonlite::toJSON(request, auto_unbox = TRUE, null = "null"),
    "\n", file = batch_input_path, append = TRUE, sep = ""
  )
}

request_count <- 0L
custom_id_map <- list()
prompt_profiles <- list()
if (nrow(pending) > 0L) {
  for (talent_code in unique(pending$talent_code)) {
    talent_rows <- pending[pending$talent_code == talent_code, , drop = FALSE]
    prompt_bundle <- load_title_prompt_bundle(
      con,
      talent_code = talent_code,
      title_version_id = version$title_version_id[[1]]
    )
    prompt_profiles[[talent_code]] <- prompt_bundle$profile_id
    starts <- seq.int(1L, nrow(talent_rows), by = batch_size)
    for (batch_index in seq_along(starts)) {
      end_index <- min(starts[[batch_index]] + batch_size - 1L, nrow(talent_rows))
      batch_df <- talent_rows[starts[[batch_index]]:end_index, , drop = FALSE]
      custom_id <- make_request_custom_id(talent_code, batch_index, batch_df)
      messages <- build_title_classification_messages(
        batch_df = batch_df,
        user_prompt_template = prompt_bundle$user_prompt_template,
        schema_text = prompt_bundle$schema_text,
        system_prompt = prompt_bundle$system_prompt,
        talent_name = batch_df$talent_name[[1]],
        talent_profile = prompt_bundle$profile_name
      )
      write_request(list(
        custom_id = custom_id,
        method = "POST",
        url = "/v1/chat/completions",
        body = list(model = model, messages = messages, temperature = 1)
      ))
      custom_id_map[[custom_id]] <- list(
        talent_code = talent_code,
        talent_name = batch_df$talent_name[[1]],
        profile_id = prompt_bundle$profile_id,
        title_version_id = prompt_bundle$title_version_id,
        definition_fields = prompt_bundle$definition_fields,
        video_ids = as.character(batch_df$video_id),
        title_hashes = as.character(batch_df$title_hash)
      )
      request_count <- request_count + 1L
    }
    message("Prepared ", nrow(talent_rows), " pending titles for ", talent_code)
  }
}

write.csv(pending, candidate_rows_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
manifest <- list(
  run_id = run_id,
  pipeline_run_id = title_classification_pipeline_run_id(),
  created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  mode = "title_classification_batch",
  endpoint = "/v1/chat/completions",
  model = model,
  title_version_id = version$title_version_id[[1]],
  taxonomy_version = version$taxonomy_version[[1]],
  prompt_version = version$prompt_version[[1]],
  version_checksum_sha256 = version$checksum_sha256[[1]],
  batch_size = batch_size,
  pending_rows = nrow(pending),
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
message("Active title version: ", version$title_version_id[[1]])
message("Pending rows: ", nrow(pending))
message("Batch requests: ", request_count)
message("Manifest: ", manifest_path)
