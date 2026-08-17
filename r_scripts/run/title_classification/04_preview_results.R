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

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

bind_rows_fill <- function(rows) {
  if (length(rows) == 0L) return(data.frame())
  column_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  normalized <- lapply(rows, function(row) {
    missing_names <- setdiff(column_names, names(row))
    for (column_name in missing_names) row[[column_name]] <- NA
    row[, column_names, drop = FALSE]
  })
  do.call(rbind, normalized)
}

collapse_json_values <- function(value) {
  if (is.null(value) || length(value) == 0L) return(NA_character_)
  values <- trimws(as.character(unlist(value, use.names = FALSE)))
  values <- values[nzchar(values)]
  if (length(values) == 0L) return(NA_character_)
  paste(values, collapse = ", ")
}

source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "title_classification", "prompt_builder.R"))
source(repo_path("r_scripts", "lib", "title_classification", "schema.R"))
source(repo_path("r_scripts", "lib", "title_classification", "store.R"))
source(repo_path("r_scripts", "lib", "title_classification", "batch_helpers.R"))

run_dir <- arg_value("--run-dir", "")
limit <- suppressWarnings(as.integer(arg_value("--limit", "20")))
if (is.na(limit) || limit < 1L) limit <- 20L

preview_batch_response <- function(run_dir) {
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)
  manifest_path <- file.path(run_dir, "manifest.json")
  if (!file.exists(manifest_path)) stop("Missing manifest: ", manifest_path)
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)

  output_path <- manifest$artifacts$batch_output_jsonl
  if (is.null(output_path) || !nzchar(output_path)) {
    output_path <- file.path(run_dir, "batch_output.jsonl")
  }
  if (!file.exists(output_path)) {
    stop(
      "Missing retrieved batch output: ",
      output_path,
      ". Run check mode with --retrieve-output first."
    )
  }

  candidate_path <- manifest$artifacts$candidate_rows_csv
  if (is.null(candidate_path) || !file.exists(candidate_path)) {
    candidate_path <- file.path(run_dir, "candidate_rows.csv")
  }
  if (!file.exists(candidate_path)) stop("Missing candidate rows: ", candidate_path)
  candidates <- read.csv(
    candidate_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  db_path_arg <- arg_value("--db-path", "")
  db_path <- if (nzchar(db_path_arg)) db_path_arg else talent_lakehouse_db_path()
  con <- title_classification_connect(db_path = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  custom_id_map <- manifest$custom_id_map
  if (!is.list(custom_id_map) || length(custom_id_map) == 0L) {
    stop("Manifest has no custom_id_map.")
  }

  preview_rows <- list()
  lines <- readLines(output_path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(lines)]
  for (line in lines) {
    wrapper <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    custom_id <- as.character(wrapper$custom_id %||% "")
    mapping <- custom_id_map[[custom_id]]
    api_model <- as.character(
      wrapper$response$body$model %||% manifest$model %||% NA_character_
    )
    finish_reason <- as.character(
      wrapper$response$body$choices[[1]]$finish_reason %||% NA_character_
    )
    status_code <- suppressWarnings(
      as.integer(wrapper$response$status_code %||% NA_integer_)
    )

    make_invalid_rows <- function(error_message) {
      mapped_ids <- if (is.null(mapping)) {
        NA_character_
      } else {
        as.character(unlist(mapping$video_ids, use.names = FALSE))
      }
      candidate_match <- match(mapped_ids, candidates$video_id)
      mapped_candidates <- candidates[candidate_match, , drop = FALSE]
      data.frame(
        custom_id = custom_id,
        api_status_code = status_code,
        api_model = api_model,
        finish_reason = finish_reason,
        validation_status = "invalid",
        validation_error = error_message,
        video_id = mapped_ids,
        talent_code = mapped_candidates$talent_code,
        title_raw = mapped_candidates$title_raw,
        content_type = mapped_candidates$content_type,
        published_at = mapped_candidates$published_at,
        stringsAsFactors = FALSE
      )
    }

    if (is.null(mapping)) {
      preview_rows[[length(preview_rows) + 1L]] <- make_invalid_rows(
        "custom_id not found in manifest"
      )
      next
    }
    if (is.na(status_code) || status_code < 200L || status_code >= 300L) {
      error_message <- wrapper$error$message %||% "non-2xx API response"
      preview_rows[[length(preview_rows) + 1L]] <- make_invalid_rows(error_message)
      next
    }

    response_text <- tryCatch(
      wrapper$response$body$choices[[1]]$message$content,
      error = function(e) NA_character_
    )
    response_text <- strip_title_classification_code_fences(response_text)
    if (is.na(response_text) || !nzchar(response_text)) {
      preview_rows[[length(preview_rows) + 1L]] <- make_invalid_rows(
        "empty response text"
      )
      next
    }

    prompt_bundle <- load_title_prompt_bundle(
      con,
      talent_code = mapping$talent_code,
      title_version_id = mapping$title_version_id
    )
    validation_error <- NULL
    validated <- tryCatch(
      validate_title_classification_batch_response(
        response_text = response_text,
        expected_video_ids = as.character(
          unlist(mapping$video_ids, use.names = FALSE)
        ),
        schema = prompt_bundle$schema,
        definition_fields = prompt_bundle$definition_fields
      ),
      error = function(error) {
        validation_error <<- conditionMessage(error)
        NULL
      }
    )
    if (is.null(validated)) {
      preview_rows[[length(preview_rows) + 1L]] <- make_invalid_rows(
        validation_error
      )
      next
    }

    candidate_match <- match(validated$video_id, candidates$video_id)
    matched_candidates <- candidates[candidate_match, , drop = FALSE]
    core_fields <- lapply(
      validated$classification_json,
      function(classification_json) {
        classification <- jsonlite::fromJSON(
          classification_json,
          simplifyVector = FALSE
        )
        data.frame(
          topic = as.character(classification$topic %||% NA_character_),
          language = as.character(classification$language %||% NA_character_),
          tags = collapse_json_values(classification$tags),
          primary_reference = as.character(
            classification$primary_reference %||% NA_character_
          ),
          referenced_entities = collapse_json_values(
            classification$referenced_entities
          ),
          stringsAsFactors = FALSE
        )
      }
    )
    core_fields <- do.call(rbind, core_fields)
    definition_columns <- intersect(
      prompt_bundle$definition_fields,
      names(validated)
    )
    preview_rows[[length(preview_rows) + 1L]] <- cbind(
      data.frame(
        custom_id = custom_id,
        api_status_code = status_code,
        api_model = api_model,
        finish_reason = finish_reason,
        validation_status = "valid",
        validation_error = NA_character_,
        video_id = validated$video_id,
        talent_code = matched_candidates$talent_code,
        title_raw = matched_candidates$title_raw,
        content_type = matched_candidates$content_type,
        published_at = matched_candidates$published_at,
        confidence = validated$confidence,
        stringsAsFactors = FALSE
      ),
      core_fields,
      validated[, definition_columns, drop = FALSE]
    )
  }

  preview <- bind_rows_fill(preview_rows)
  if (nrow(preview) == 0L) stop("Retrieved batch output contained no responses.")
  out_path <- arg_value(
    "--out",
    file.path(run_dir, "batch_response_preview.csv")
  )
  write.csv(
    preview,
    out_path,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )

  valid_count <- sum(preview$validation_status == "valid", na.rm = TRUE)
  invalid_count <- sum(preview$validation_status != "valid", na.rm = TRUE)
  display_columns <- intersect(
    c(
      "validation_status", "video_id", "title_raw", "topic", "language",
      "tags", "primary_reference", "confidence", "validation_error"
    ),
    names(preview)
  )
  old_width <- getOption("width")
  options(width = max(160L, old_width))
  on.exit(options(width = old_width), add = TRUE)

  cat("Run directory: ", run_dir, "\n", sep = "")
  cat("Model: ", manifest$model, "\n", sep = "")
  cat("Validated title rows: ", valid_count, "\n", sep = "")
  cat("Invalid title rows: ", invalid_count, "\n\n", sep = "")
  print(preview[, display_columns, drop = FALSE], row.names = FALSE)
  cat("\nReadable CSV: ", out_path, "\n", sep = "")
  invisible(preview)
}

if (nzchar(run_dir)) {
  preview_batch_response(run_dir)
  quit(status = 0L)
}

db_path <- arg_value("--db-path", talent_lakehouse_db_path())
con <- title_classification_connect(db_path = db_path, read_only = TRUE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
version <- load_active_title_version(con)
status <- DBI::dbGetQuery(
  con,
  paste(
    "SELECT is_classified, COUNT(*) AS videos",
    "FROM classification.title_classification_status",
    "GROUP BY is_classified ORDER BY is_classified DESC"
  )
)
latest <- DBI::dbGetQuery(
  con,
  sprintf(
    paste(
      "SELECT result.created_at, result.video_id, video.title,",
      "result.talent_code, result.model, result.confidence,",
      "result.classification_json",
      "FROM classification.title_classification_results AS result",
      "JOIN catalog.videos AS video USING (video_id)",
      "JOIN classification.title_classification_status AS status",
      "ON status.video_id = result.video_id",
      "AND status.title_version_id = result.title_version_id",
      "WHERE result.title_version_id = ?",
      "AND result.title_hash = status.title_hash",
      "ORDER BY result.created_at DESC LIMIT %d"
    ),
    limit
  ),
  params = list(version$title_version_id[[1]])
)
cat("DuckDB: ", db_path, "\n", sep = "")
cat("Active title version: ", version$title_version_id[[1]], "\n\n", sep = "")
cat("Current title status:\n")
print(status, row.names = FALSE)
cat("\nLatest results:\n")
print(latest, row.names = FALSE)
