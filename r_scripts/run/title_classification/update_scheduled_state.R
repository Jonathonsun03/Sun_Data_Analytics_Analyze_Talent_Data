get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

repo_root <- rprojroot::find_root(rprojroot::is_git_root, path = get_script_dir())
repo_path <- function(...) normalizePath(file.path(repo_root, ...), winslash = "/", mustWork = FALSE)

source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "duckdb", "db_schema.R"))

args <- commandArgs(trailingOnly = TRUE)
command <- if (length(args) > 0) args[[1]] else ""
args <- if (length(args) > 1) args[-1] else character()

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

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  update_scheduled_state.R write --run-dir PATH [--status STATUS] [--applied-at ISO] [--db-path PATH]",
      "  update_scheduled_state.R get --field NAME [--db-path PATH]",
      "  update_scheduled_state.R exists [--db-path PATH]",
      "  update_scheduled_state.R archive --archive-path PATH [--db-path PATH]",
      "  update_scheduled_state.R clear [--archive-path PATH] [--db-path PATH]",
      "  update_scheduled_state.R find-active --run-root PATH",
      sep = "\n"
    ),
    "\n"
  )
}

fail <- function(...) {
  message(...)
  quit(status = 1, save = "no")
}

read_json <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

write_json <- function(path, data) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(data, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

now_iso <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
}

state_db_path <- function() {
  db_path <- arg_value("--db-path", Sys.getenv("TITLE_CLASSIFICATION_STATE_DB_PATH", unset = ""))
  if (nzchar(db_path)) {
    return(normalizePath(db_path, winslash = "/", mustWork = FALSE))
  }
  duckdb_get_db_path()
}

connect_state_db <- function() {
  con <- duckdb_connect(db_path = state_db_path())
  init_duckdb_schema(con)
  con
}

state_from_manifest <- function(run_dir, status_override = NULL, applied_at = NULL) {
  manifest_path <- file.path(run_dir, "manifest.json")
  if (!file.exists(manifest_path)) {
    fail("Missing manifest: ", manifest_path)
  }
  manifest <- read_json(manifest_path)
  artifacts <- manifest$artifacts
  if (is.null(artifacts) || !is.list(artifacts)) {
    artifacts <- list()
  }
  status <- status_override
  if (is.null(status) || !nzchar(status)) {
    status <- manifest$batch_status
  }
  if (is.null(status) || !nzchar(status)) {
    status <- manifest$status
  }
  if (is.null(status) || !nzchar(status)) {
    status <- "created"
  }

  list(
    run_dir = normalizePath(run_dir, winslash = "/", mustWork = FALSE),
    manifest_path = normalizePath(manifest_path, winslash = "/", mustWork = FALSE),
    batch_id = manifest$batch_id,
    input_file_id = manifest$input_file_id,
    output_file_id = manifest$output_file_id,
    error_file_id = manifest$error_file_id,
    status = status,
    created_at = manifest$created_at,
    submitted_at = manifest$submitted_at,
    last_checked_at = manifest$last_checked_at,
    applied_at = applied_at,
    request_count = manifest$request_count,
    pending_rows = manifest$pending_rows,
    artifacts = artifacts,
    updated_at = now_iso()
  )
}

scalar_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

integer_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(x[[1]]))
}

read_state <- function(con, state_key = "pending") {
  rows <- DBI::dbGetQuery(
    con,
    "SELECT state_json FROM title_classification_scheduled_state WHERE state_key = ?",
    params = list(state_key)
  )
  if (nrow(rows) == 0) {
    return(NULL)
  }
  jsonlite::fromJSON(rows$state_json[[1]], simplifyVector = FALSE)
}

write_state <- function(con, state, state_key = "pending") {
  state_json <- jsonlite::toJSON(state, auto_unbox = TRUE, null = "null")
  artifacts_json <- jsonlite::toJSON(state$artifacts %||% list(), auto_unbox = TRUE, null = "null")

  DBI::dbExecute(
    con,
    "DELETE FROM title_classification_scheduled_state WHERE state_key = ?",
    params = list(state_key)
  )
  DBI::dbExecute(
    con,
    paste(
      "INSERT INTO title_classification_scheduled_state (",
      "state_key, run_dir, manifest_path, batch_id, input_file_id, output_file_id,",
      "error_file_id, status, created_at, submitted_at, last_checked_at, applied_at,",
      "request_count, pending_rows, artifacts_json, state_json, updated_at",
      ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      state_key,
      scalar_or_na(state$run_dir),
      scalar_or_na(state$manifest_path),
      scalar_or_na(state$batch_id),
      scalar_or_na(state$input_file_id),
      scalar_or_na(state$output_file_id),
      scalar_or_na(state$error_file_id),
      scalar_or_na(state$status),
      scalar_or_na(state$created_at),
      scalar_or_na(state$submitted_at),
      scalar_or_na(state$last_checked_at),
      scalar_or_na(state$applied_at),
      integer_or_na(state$request_count),
      integer_or_na(state$pending_rows),
      as.character(artifacts_json),
      as.character(state_json),
      scalar_or_na(state$updated_at)
    )
  )
}

archive_state <- function(con, archive_path) {
  state <- read_state(con)
  if (is.null(state)) {
    return(invisible(FALSE))
  }
  write_json(archive_path, state)
  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

if (!nzchar(command) || command %in% c("-h", "--help")) {
  usage()
  quit(status = if (nzchar(command)) 0 else 1, save = "no")
}

if (command == "find-active") {
  run_root <- arg_value("--run-root", "")
  if (!nzchar(run_root)) {
    fail("--run-root is required.")
  }
  manifest_paths <- list.files(run_root, pattern = "^manifest[.]json$", recursive = TRUE, full.names = TRUE)
  terminal_statuses <- c("failed", "expired", "cancelled", "applied")
  candidates <- data.frame(mtime = numeric(), run_dir = character())
  for (manifest_path in manifest_paths) {
    manifest <- tryCatch(read_json(manifest_path), error = function(e) NULL)
    if (is.null(manifest) || is.null(manifest$batch_id) || !nzchar(manifest$batch_id)) {
      next
    }
    run_dir <- dirname(manifest_path)
    status <- manifest$batch_status %||% ""
    if (file.exists(file.path(run_dir, "apply_summary.json")) ||
        file.exists(file.path(run_dir, "completed_state.json")) ||
        status %in% terminal_statuses) {
      next
    }
    candidates <- rbind(
      candidates,
      data.frame(mtime = file.info(manifest_path)$mtime, run_dir = run_dir)
    )
  }
  if (nrow(candidates) == 0) {
    quit(status = 1, save = "no")
  }
  candidates <- candidates[order(candidates$mtime, decreasing = TRUE), , drop = FALSE]
  cat(candidates$run_dir[[1]], "\n", sep = "")
  quit(status = 0, save = "no")
}

con <- connect_state_db()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

if (command == "write") {
  run_dir <- arg_value("--run-dir", "")
  if (!nzchar(run_dir)) {
    fail("--run-dir is required.")
  }
  state <- state_from_manifest(
    run_dir,
    status_override = arg_value("--status", NULL),
    applied_at = arg_value("--applied-at", NULL)
  )
  write_state(con, state)
  cat(state_db_path(), "\n", sep = "")
  quit(status = 0, save = "no")
}

if (command == "get") {
  field <- arg_value("--field", "")
  if (!nzchar(field)) {
    fail("--field is required.")
  }
  state <- read_state(con)
  if (is.null(state) || is.null(state[[field]])) {
    quit(status = 1, save = "no")
  }
  value <- state[[field]]
  if (is.list(value)) {
    cat(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null"), "\n", sep = "")
  } else {
    cat(as.character(value[[1]]), "\n", sep = "")
  }
  quit(status = 0, save = "no")
}

if (command == "exists") {
  state <- read_state(con)
  quit(status = if (is.null(state)) 1 else 0, save = "no")
}

if (command == "archive") {
  archive_path <- arg_value("--archive-path", "")
  if (!nzchar(archive_path)) {
    fail("--archive-path is required.")
  }
  ok <- archive_state(con, archive_path)
  quit(status = if (isTRUE(ok)) 0 else 1, save = "no")
}

if (command == "clear") {
  archive_path <- arg_value("--archive-path", "")
  if (nzchar(archive_path)) {
    archive_state(con, archive_path)
  }
  DBI::dbExecute(
    con,
    "DELETE FROM title_classification_scheduled_state WHERE state_key = ?",
    params = list("pending")
  )
  quit(status = 0, save = "no")
}

usage()
quit(status = 1, save = "no")
