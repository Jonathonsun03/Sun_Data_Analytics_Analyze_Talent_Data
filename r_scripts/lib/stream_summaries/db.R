ss_init_db <- function(con) {
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS stream_summaries (
       talent_project TEXT,
       source_file TEXT,
       source_title TEXT,
       source_mtime TIMESTAMP,
       source_size_bytes BIGINT,
       prompt_path TEXT,
       prompt_md5 TEXT,
       model TEXT,
       output_path TEXT,
       prompt_tokens BIGINT,
       completion_tokens BIGINT,
       total_tokens BIGINT,
       status TEXT,
       error_message TEXT,
       processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
       UNIQUE(source_file, prompt_md5, model)
     )"
  )
  invisible(TRUE)
}

ss_stage_sources <- function(con, input_dir) {
  paths <- list.files(input_dir, full.names = TRUE, pattern = "\\.csv$")
  if (length(paths) == 0) {
    stop("No CSV files found in: ", input_dir)
  }

  file_info <- file.info(paths)
  to_stage <- tibble::tibble(
    source_file = paths,
    source_title = tools::file_path_sans_ext(basename(paths)),
    source_mtime = as.character(file_info$mtime),
    source_size_bytes = as.numeric(file_info$size)
  )

  DBI::dbWriteTable(
    con,
    "staging_stream_logs",
    to_stage,
    temporary = TRUE,
    overwrite = TRUE
  )

  to_stage
}

ss_get_pending_sources <- function(
    con,
    prompt_md5,
    model_name,
    force_resummarize = FALSE
) {
  if (isTRUE(force_resummarize)) {
    return(DBI::dbGetQuery(
      con,
      "SELECT source_file, source_title, source_mtime, source_size_bytes
       FROM staging_stream_logs
       ORDER BY source_file"
    ))
  }

  DBI::dbGetQuery(
    con,
    "SELECT s.source_file, s.source_title, s.source_mtime, s.source_size_bytes
     FROM staging_stream_logs s
     LEFT JOIN stream_summaries r
       ON s.source_file = r.source_file
      AND s.source_mtime = r.source_mtime
      AND s.source_size_bytes = r.source_size_bytes
      AND r.prompt_md5 = ?
      AND r.model = ?
      AND r.status = 'success'
     WHERE r.source_file IS NULL
     ORDER BY s.source_file",
    params = list(prompt_md5, model_name)
  )
}

ss_record_result <- function(
    con,
    talent_project,
    row,
    prompt_path,
    prompt_md5,
    model_name,
    output_path,
    result
) {
  DBI::dbExecute(
    con,
    "INSERT INTO stream_summaries (
       talent_project, source_file, source_title, source_mtime, source_size_bytes,
       prompt_path, prompt_md5, model, output_path,
       prompt_tokens, completion_tokens, total_tokens,
       status, error_message, processed_at
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
     ON CONFLICT (source_file, prompt_md5, model) DO UPDATE SET
       talent_project = excluded.talent_project,
       source_title = excluded.source_title,
       source_mtime = excluded.source_mtime,
       source_size_bytes = excluded.source_size_bytes,
       prompt_path = excluded.prompt_path,
       output_path = excluded.output_path,
       prompt_tokens = excluded.prompt_tokens,
       completion_tokens = excluded.completion_tokens,
       total_tokens = excluded.total_tokens,
       status = excluded.status,
       error_message = excluded.error_message,
       processed_at = excluded.processed_at",
    params = list(
      talent_project,
      row$source_file[[1]],
      row$source_title[[1]],
      as.character(row$source_mtime[[1]]),
      as.numeric(row$source_size_bytes[[1]]),
      prompt_path,
      prompt_md5,
      model_name,
      output_path,
      result$prompt_tokens,
      result$completion_tokens,
      result$total_tokens,
      result$status,
      result$error_message,
      as.character(Sys.time())
    )
  )
}
