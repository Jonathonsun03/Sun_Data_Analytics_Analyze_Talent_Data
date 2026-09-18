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

start_subtitle_backfill_attempt <- function(
    db_path,
    batch_pipeline_run_id,
    track,
    candidate_position,
    pipeline_version,
    source_scope) {
  attempt_id <- subtitle_sentence_hash(
    batch_pipeline_run_id,
    track$video_id[[1]],
    track$subtitle_language[[1]],
    track$subtitle_track_type[[1]],
    source_scope,
    pipeline_version
  )
  subtitle_backfill_with_writer(db_path, function(con) {
    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO ops.subtitle_backfill_attempts (",
        "attempt_id, batch_pipeline_run_id, candidate_position, video_id,",
        "talent_code, subtitle_language, subtitle_track_type, source_scope,",
        "pipeline_version, raw_rows, started_at, status",
        ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'running')"
      ),
      params = list(
        attempt_id,
        batch_pipeline_run_id,
        as.integer(candidate_position),
        as.character(track$video_id[[1]]),
        as.character(track$talent_code[[1]]),
        subtitle_backfill_scalar_text(track$subtitle_language),
        subtitle_backfill_scalar_text(track$subtitle_track_type),
        as.character(source_scope),
        as.character(pipeline_version),
        as.integer(track$raw_rows[[1]]),
        as.POSIXct(Sys.time(), tz = "UTC")
      )
    )
  })
  attempt_id
}

finish_subtitle_backfill_attempt <- function(
    db_path,
    attempt_id,
    track_result) {
  publication_run_id <- if (
    identical(track_result$status, "published") &&
      !is.null(track_result$publication$pipeline_run_id)
  ) {
    track_result$publication$pipeline_run_id
  } else {
    NA_character_
  }
  error_summary <- if (identical(track_result$status, "failed")) {
    substr(conditionMessage(track_result$error), 1L, 1000L)
  } else {
    NA_character_
  }
  metric <- function(name) {
    value <- track_result[[name]]
    if (is.null(value) || length(value) == 0L || is.na(value[[1]])) {
      return(NA_integer_)
    }
    as.integer(value[[1]])
  }

  subtitle_backfill_with_writer(db_path, function(con) {
    DBI::dbExecute(
      con,
      paste(
        "UPDATE ops.subtitle_backfill_attempts SET completed_at = ?,",
        "status = ?, sentences = ?, blocks = ?, requested_blocks = ?,",
        "reused_blocks = ?, publication_pipeline_run_id = ?, error_summary = ?",
        "WHERE attempt_id = ?"
      ),
      params = list(
        as.POSIXct(Sys.time(), tz = "UTC"),
        as.character(track_result$status),
        metric("sentences"),
        metric("blocks"),
        metric("requested_blocks"),
        metric("reused_blocks"),
        publication_run_id,
        error_summary,
        attempt_id
      )
    )
  })
  invisible(TRUE)
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
