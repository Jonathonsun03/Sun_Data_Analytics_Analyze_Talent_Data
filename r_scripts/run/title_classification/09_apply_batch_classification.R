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

run_dir <- arg_value("--run-dir", "")
db_path_arg <- arg_value("--db-path", "")
allow_failures <- "--allow-failures" %in% args
if (!nzchar(run_dir)) stop("--run-dir is required.")
manifest_path <- file.path(run_dir, "manifest.json")
if (!file.exists(manifest_path)) stop("Missing manifest: ", manifest_path)

source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "title_classification", "prompt_builder.R"))
source(repo_path("r_scripts", "lib", "title_classification", "schema.R"))
source(repo_path("r_scripts", "lib", "title_classification", "store.R"))
source(repo_path("r_scripts", "lib", "title_classification", "batch_helpers.R"))

manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
output_path <- manifest$artifacts$batch_output_jsonl
if (is.null(output_path) || !nzchar(output_path)) {
  output_path <- file.path(run_dir, "batch_output.jsonl")
}
if (!file.exists(output_path)) stop("Missing batch output JSONL: ", output_path)
candidate_rows_path <- manifest$artifacts$candidate_rows_csv
if (is.null(candidate_rows_path) || !file.exists(candidate_rows_path)) {
  stop("Missing candidate rows CSV recorded in manifest.")
}
candidate_rows <- read.csv(
  candidate_rows_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

db_path <- if (nzchar(db_path_arg)) db_path_arg else talent_lakehouse_db_path()
con <- title_classification_connect(db_path = db_path)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
ensure_title_classification_schema(con)
current_version <- load_active_title_version(con, manifest$title_version_id)
if (!identical(current_version$checksum_sha256[[1]], manifest$version_checksum_sha256)) {
  stop("The stored title version checksum no longer matches the batch manifest.")
}

custom_id_map <- manifest$custom_id_map
if (!is.list(custom_id_map) || length(custom_id_map) == 0L) {
  stop("Manifest has no custom_id_map.")
}

validated <- list()
failed <- list()
lines <- readLines(output_path, warn = FALSE, encoding = "UTF-8")
for (line in lines[nzchar(lines)]) {
  wrapper <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  custom_id <- wrapper$custom_id
  mapping <- custom_id_map[[custom_id]]
  if (is.null(mapping)) {
    failed[[length(failed) + 1L]] <- list(
      custom_id = custom_id,
      error = "custom_id not found in manifest"
    )
    next
  }
  status_code <- wrapper$response$status_code
  if (is.null(status_code) || status_code < 200L || status_code >= 300L) {
    failed[[length(failed) + 1L]] <- list(custom_id = custom_id, error = "non-2xx response")
    next
  }
  response_text <- tryCatch(
    wrapper$response$body$choices[[1]]$message$content,
    error = function(e) NA_character_
  )
  response_text <- strip_title_classification_code_fences(response_text)
  if (is.na(response_text) || !nzchar(response_text)) {
    failed[[length(failed) + 1L]] <- list(custom_id = custom_id, error = "empty response text")
    next
  }

  prompt_bundle <- load_title_prompt_bundle(
    con,
    talent_code = mapping$talent_code,
    title_version_id = mapping$title_version_id
  )
  batch_result <- tryCatch(
    validate_title_classification_batch_response(
      response_text = response_text,
      expected_video_ids = unlist(mapping$video_ids, use.names = FALSE),
      schema = prompt_bundle$schema,
      definition_fields = prompt_bundle$definition_fields
    ),
    error = function(e) {
      failed[[length(failed) + 1L]] <<- list(
        custom_id = custom_id,
        error = conditionMessage(e)
      )
      NULL
    }
  )
  if (is.null(batch_result)) next

  candidate <- candidate_rows[
    candidate_rows$video_id %in% batch_result$video_id,
    c(
      "video_id", "talent_code", "channel_id", "legacy_talent_id",
      "title_raw", "title_hash"
    ),
    drop = FALSE
  ]
  rows <- merge(candidate, batch_result, by = "video_id", all = FALSE)
  if (nrow(rows) == 0L) {
    failed[[length(failed) + 1L]] <- list(
      custom_id = custom_id,
      error = "validated response did not match candidate rows"
    )
    next
  }
  current_hashes <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT video.video_id, video.title, ",
      title_classification_title_hash_sql("video"),
      " AS title_hash FROM catalog.videos AS video WHERE video.video_id IN (",
      paste(rep("?", nrow(rows)), collapse = ", "),
      ")"
    ),
    params = as.list(rows$video_id)
  )
  hash_check <- merge(
    rows[, c("video_id", "title_raw", "title_hash"), drop = FALSE],
    current_hashes,
    by = "video_id",
    suffixes = c("_batch", "_current"),
    all.x = TRUE
  )
  missing_batch_hash <- is.na(hash_check$title_hash_batch) |
    !nzchar(hash_check$title_hash_batch)
  valid_current_hash <- !is.na(hash_check$title_hash_current) &
    nzchar(hash_check$title_hash_current)
  same_title <- !is.na(hash_check$title_raw) &
    !is.na(hash_check$title) &
    hash_check$title_raw == hash_check$title
  matching_hash <- !missing_batch_hash &
    hash_check$title_hash_batch == hash_check$title_hash_current
  current_title_matches_batch <- ifelse(
    missing_batch_hash,
    same_title,
    matching_hash
  )
  current_title_matches_batch[is.na(current_title_matches_batch)] <- FALSE
  stale_ids <- hash_check$video_id[
    !valid_current_hash | !current_title_matches_batch
  ]
  if (length(stale_ids) > 0L) {
    failed[[length(failed) + 1L]] <- list(
      custom_id = custom_id,
      error = paste("title changed after batch build:", paste(stale_ids, collapse = ", "))
    )
    rows <- rows[!rows$video_id %in% stale_ids, , drop = FALSE]
  }
  if (nrow(rows) > 0L) {
    current_hash_by_id <- stats::setNames(
      current_hashes$title_hash,
      current_hashes$video_id
    )
    missing_row_hash <- is.na(rows$title_hash) | !nzchar(rows$title_hash)
    rows$title_hash[missing_row_hash] <- unname(
      current_hash_by_id[rows$video_id[missing_row_hash]]
    )
  }
  if (nrow(rows) > 0L) {
    validated[[length(validated) + 1L]] <- list(rows = rows, prompt_bundle = prompt_bundle)
  }
}

if (length(failed) > 0L && !allow_failures) {
  summary_path <- file.path(run_dir, "apply_summary.json")
  jsonlite::write_json(
    list(
      run_id = manifest$run_id,
      applied_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      validated_rows = sum(vapply(validated, function(x) nrow(x$rows), integer(1))),
      inserted_rows = 0L,
      failed_count = length(failed),
      failed = failed
    ),
    summary_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  stop("Batch validation failed; no results were written. See: ", summary_path)
}

pipeline_run_id <- manifest$pipeline_run_id
register_title_classification_run(con, pipeline_run_id)
inserted_total <- 0L
tryCatch(
  DBI::dbWithTransaction(con, {
    for (item in validated) {
      if (isTRUE(manifest$force_reclassify)) {
        delete_title_classification_results(
          con,
          item$rows$video_id,
          item$prompt_bundle$title_version_id
        )
      }
      inserted_total <- inserted_total + insert_title_classification_results(
        con = con,
        rows = item$rows,
        prompt_bundle = item$prompt_bundle,
        model = manifest$model,
        pipeline_run_id = pipeline_run_id
      )
    }
  }),
  error = function(e) {
    complete_title_classification_run(
      con,
      pipeline_run_id,
      status = "failed",
      error_summary = conditionMessage(e)
    )
    stop(e)
  }
)
run_status <- if (length(failed) > 0L) "completed_with_errors" else "completed"
complete_title_classification_run(con, pipeline_run_id, status = run_status)

validated_total <- sum(vapply(validated, function(x) nrow(x$rows), integer(1)))
summary_path <- file.path(run_dir, "apply_summary.json")
jsonlite::write_json(
  list(
    run_id = manifest$run_id,
    pipeline_run_id = pipeline_run_id,
    applied_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    output_path = output_path,
    validated_rows = validated_total,
    inserted_rows = inserted_total,
    failed_count = length(failed),
    failed = failed
  ),
  summary_path,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
message("Validated rows: ", validated_total)
message("Inserted rows: ", inserted_total)
message("Failed responses: ", length(failed))
message("Apply summary: ", summary_path)
