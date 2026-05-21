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

run_dir <- arg_value("--run-dir", "")
if (!nzchar(run_dir)) {
  stop("--run-dir is required.")
}
manifest_path <- file.path(run_dir, "manifest.json")
if (!file.exists(manifest_path)) {
  stop("Missing manifest: ", manifest_path)
}

source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "duckdb", "db_schema.R"))
source(repo_path("r_scripts", "lib", "stream_classification", "talent_rules.R"))
source(repo_path("r_scripts", "lib", "stream_classification", "prompt_builder.R"))
source(repo_path("r_scripts", "lib", "title_classification", "batch_helpers.R"))

manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
output_path <- manifest$artifacts$batch_output_jsonl
if (is.null(output_path) || !nzchar(output_path)) {
  output_path <- file.path(run_dir, "batch_output.jsonl")
}
if (!file.exists(output_path)) {
  stop("Missing batch output JSONL: ", output_path)
}
candidate_rows_path <- manifest$artifacts$candidate_rows_csv
if (is.null(candidate_rows_path) || !file.exists(candidate_rows_path)) {
  stop("Missing candidate rows CSV recorded in manifest.")
}
candidate_rows <- read.csv(candidate_rows_path, stringsAsFactors = FALSE, check.names = FALSE)

con <- duckdb_connect()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
init_duckdb_schema(con)

custom_id_map <- manifest$custom_id_map
if (!is.list(custom_id_map) || length(custom_id_map) == 0) {
  stop("Manifest has no custom_id_map.")
}

lines <- readLines(output_path, warn = FALSE, encoding = "UTF-8")
inserted_total <- 0L
validated_total <- 0L
failed <- list()

for (line in lines[nzchar(lines)]) {
  wrapper <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  custom_id <- wrapper$custom_id
  mapping <- custom_id_map[[custom_id]]
  if (is.null(mapping)) {
    failed[[length(failed) + 1L]] <- list(custom_id = custom_id, error = "custom_id not found in manifest")
    next
  }
  status_code <- wrapper$response$status_code
  if (is.null(status_code) || as.integer(status_code) < 200L || as.integer(status_code) >= 300L) {
    failed[[length(failed) + 1L]] <- list(custom_id = custom_id, error = "non-2xx response")
    next
  }
  response_body <- wrapper$response$body
  text <- tryCatch(
    response_body$choices[[1]]$message$content,
    error = function(e) NA_character_
  )
  text <- strip_title_classification_code_fences(text)
  if (!is.character(text) || !nzchar(text)) {
    failed[[length(failed) + 1L]] <- list(custom_id = custom_id, error = "empty response text")
    next
  }

  prompt_bundle <- load_prompt_bundle(talent_name = mapping$talent_name)
  ensure_classification_boolean_columns(con, prompt_bundle$definition_fields)
  batch_result <- tryCatch(
    validate_title_classification_batch_response(
      response_text = text,
      expected_video_ids = unlist(mapping$video_ids, use.names = FALSE),
      schema = prompt_bundle$schema,
      definition_fields = prompt_bundle$definition_fields
    ),
    error = function(e) {
      failed[[length(failed) + 1L]] <<- list(custom_id = custom_id, error = conditionMessage(e))
      NULL
    }
  )
  if (is.null(batch_result)) {
    next
  }

  rows_for_request <- candidate_rows[candidate_rows$video_id %in% batch_result$video_id, , drop = FALSE]
  to_insert <- merge(
    rows_for_request[, c("title_hash", "video_id", "talent_id"), drop = FALSE],
    batch_result,
    by = "video_id",
    all = FALSE
  )
  if (isTRUE(manifest$force_reclassify)) {
    delete_title_classifications_for_videos(
      con = con,
      video_ids = to_insert$video_id,
      taxonomy_version = mapping$taxonomy_version,
      prompt_version = mapping$prompt_version,
      model = manifest$model
    )
  }
  inserted <- insert_title_classification_batch(
    con = con,
    rows = to_insert,
    taxonomy_version = mapping$taxonomy_version,
    prompt_version = mapping$prompt_version,
    model = manifest$model,
    talent_profile = mapping$talent_profile,
    definition_fields = prompt_bundle$definition_fields
  )
  inserted_total <- inserted_total + max(0L, as.integer(inserted))
  validated_total <- validated_total + nrow(to_insert)
}

apply_summary <- list(
  run_id = manifest$run_id,
  applied_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  output_path = output_path,
  validated_rows = validated_total,
  inserted_rows = inserted_total,
  failed_count = length(failed),
  failed = failed
)
summary_path <- file.path(run_dir, "apply_summary.json")
jsonlite::write_json(apply_summary, summary_path, auto_unbox = TRUE, pretty = TRUE, null = "null")

message("Validated rows: ", validated_total)
message("Inserted rows: ", inserted_total)
message("Failed responses: ", length(failed))
message("Apply summary: ", summary_path)

if (length(failed) > 0) {
  stop("Some batch responses failed validation. See: ", summary_path)
}
