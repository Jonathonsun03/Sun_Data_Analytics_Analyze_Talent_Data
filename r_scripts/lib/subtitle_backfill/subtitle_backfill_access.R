# Short database access windows; no inference or checkpoint work holds a connection.
source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_controls.R"), local = TRUE)

subtitle_backfill_access_wait <- function(marker_path, stop_path, honor_stop, deadline) {
  repeat {
    if (honor_stop && subtitle_backfill_stop_requested(stop_path)) {
      stop(subtitle_backfill_stop_condition())
    }
    if (!file.exists(marker_path)) return(invisible(NULL))
    if (proc.time()[["elapsed"]] >= deadline) {
      stop("Collection is active; terminal bookkeeping could not acquire a database window.", call. = FALSE)
    }
    Sys.sleep(1)
  }
}

subtitle_backfill_with_connection <- function(
    db_path, operation, read_only,
    connection_attempts = 12L, retry_delay_sec = 5,
    honor_stop = TRUE, marker_path = NULL) {
  logs <- file.path(dirname(dirname(db_path)), "Logs")
  if (is.null(marker_path)) {
    marker_path <- Sys.getenv("COLLECTION_ACTIVE_MARKER", file.path(logs, "collection-active"))
  }
  stop_path <- Sys.getenv("SUBTITLE_BACKFILL_STOP_REQUEST", file.path(
    logs, "subtitle_sentence_backfill", "control", "stop.request"
  ))
  # Final status writes get a bounded grace period, not a fresh processing budget.
  deadline <- if (honor_stop) Inf else proc.time()[["elapsed"]] + 60
  for (attempt in seq_len(connection_attempts)) {
    subtitle_backfill_access_wait(marker_path, stop_path, honor_stop, deadline)
    con <- NULL
    result <- tryCatch({
      con <- duckdb_connect(db_path = db_path, read_only = read_only)
      list(ok = TRUE, value = operation(con))
    }, error = function(error) list(ok = FALSE, error = error), finally = {
      if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    })
    if (result$ok) return(result$value)
    if (!grepl("Could not set lock|Conflicting lock|database is locked",
               conditionMessage(result$error), ignore.case = TRUE) ||
        attempt == connection_attempts || proc.time()[["elapsed"]] >= deadline) {
      stop(result$error)
    }
    message("DuckDB busy; retrying access (", attempt, "/", connection_attempts, ").")
    if (honor_stop && subtitle_backfill_stop_requested(stop_path)) {
      stop(subtitle_backfill_stop_condition())
    }
    Sys.sleep(retry_delay_sec)
  }
}
