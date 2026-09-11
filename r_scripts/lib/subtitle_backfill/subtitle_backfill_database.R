# Short-lived DuckDB writes and pipeline-run bookkeeping for subtitle backfills.

subtitle_backfill_with_writer <- function(
    db_path,
    operation,
    connection_attempts = 12L,
    retry_delay_sec = 5) {
  last_error <- NULL
  for (attempt in seq_len(connection_attempts)) {
    con <- NULL
    result <- tryCatch(
      {
        con <- duckdb_connect(db_path = db_path, read_only = FALSE)
        value <- operation(con)
        DBI::dbDisconnect(con, shutdown = TRUE)
        con <- NULL
        list(ok = TRUE, value = value)
      },
      error = function(error) list(ok = FALSE, error = error)
    )
    if (!is.null(con)) {
      suppressWarnings(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE))
    }
    if (isTRUE(result$ok)) return(result$value)

    last_error <- result$error
    if (!grepl("Could not set lock|Conflicting lock|database is locked",
               conditionMessage(last_error), ignore.case = TRUE)) {
      stop(conditionMessage(last_error), call. = FALSE)
    }
    if (attempt < connection_attempts) {
      message(
        "DuckDB write attempt ",
        attempt,
        "/",
        connection_attempts,
        " failed: ",
        conditionMessage(last_error),
        "; retrying in ",
        retry_delay_sec,
        " seconds."
      )
      Sys.sleep(retry_delay_sec)
    }
  }
  stop(conditionMessage(last_error), call. = FALSE)
}

start_subtitle_backfill_run <- function(db_path) {
  pipeline_run_id <- paste0(
    "subtitle_backfill_",
    format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
    "_",
    substr(subtitle_sentence_hash(Sys.getpid(), runif(1)), 1L, 12L)
  )
  subtitle_backfill_with_writer(db_path, function(con) {
    init_subtitle_sentence_schema(con)
    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO ops.pipeline_runs",
        "(pipeline_run_id, pipeline_name, started_at, status)",
        "VALUES (?, 'subtitle_sentence_backfill', ?, 'running')"
      ),
      params = list(pipeline_run_id, as.POSIXct(Sys.time(), tz = "UTC"))
    )
  })
  pipeline_run_id
}

finish_subtitle_backfill_run <- function(
    db_path,
    pipeline_run_id,
    status,
    error_summary = NA_character_) {
  subtitle_backfill_with_writer(db_path, function(con) {
    DBI::dbExecute(
      con,
      paste(
        "UPDATE ops.pipeline_runs SET completed_at = ?, status = ?,",
        "error_summary = ? WHERE pipeline_run_id = ?"
      ),
      params = list(
        as.POSIXct(Sys.time(), tz = "UTC"),
        status,
        error_summary,
        pipeline_run_id
      )
    )
  })
  invisible(TRUE)
}
