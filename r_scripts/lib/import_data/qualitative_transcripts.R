qualitative_loader_connection <- function(con = NULL, db_path = NULL) {
  if (!is.null(con)) {
    return(list(con = con, owned = FALSE))
  }
  if (!exists("duckdb_connect", mode = "function") ||
      !exists("talent_lakehouse_db_path", mode = "function")) {
    stop(
      "Source r_scripts/lib/duckdb/db_connect.R before using the ",
      "qualitative transcript loader.",
      call. = FALSE
    )
  }
  if (is.null(db_path)) {
    db_path <- talent_lakehouse_db_path()
  }
  list(
    con = duckdb_connect(db_path = db_path, read_only = TRUE),
    owned = TRUE
  )
}

qualitative_close_loader_connection <- function(connection) {
  if (isTRUE(connection$owned)) {
    suppressWarnings(
      tryCatch(
        DBI::dbDisconnect(connection$con, shutdown = TRUE),
        error = function(e) NULL
      )
    )
  }
  invisible(NULL)
}

qualitative_codebook_view_metadata <- function(con, codebook_id) {
  metadata <- DBI::dbGetQuery(
    con,
    "SELECT
       code_id,
       code_column_name,
       wide_view_name,
       display_order
     FROM qualitative.codebooks
     WHERE codebook_id = ?
     ORDER BY display_order, code_id",
    params = list(codebook_id)
  )
  if (nrow(metadata) == 0L) {
    stop("Unknown qualitative codebook: ", codebook_id, call. = FALSE)
  }
  view_names <- unique(metadata$wide_view_name)
  if (length(view_names) != 1L) {
    stop(
      "Codebook `", codebook_id, "` does not resolve to one wide view.",
      call. = FALSE
    )
  }

  view_exists <- DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n
     FROM information_schema.views
     WHERE table_schema = 'qualitative'
       AND table_name = ?",
    params = list(view_names[[1]])
  )$n[[1]]
  if (view_exists != 1L) {
    stop(
      "Wide qualitative view is missing: qualitative.",
      view_names[[1]],
      call. = FALSE
    )
  }

  metadata
}

qualitative_filter_clause <- function(
    column,
    values,
    params,
    allow_null = TRUE
) {
  if (is.null(values) && isTRUE(allow_null)) {
    return(list(sql = NULL, params = params))
  }
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0L) {
    return(list(sql = "FALSE", params = params))
  }
  placeholders <- paste(rep("?", length(values)), collapse = ", ")
  list(
    sql = paste0(column, " IN (", placeholders, ")"),
    params = c(params, as.list(values))
  )
}

load_qualitative_transcripts_wide <- function(
    codebook_id,
    dataset_id = NULL,
    pipeline_run_id = "latest",
    video_ids = NULL,
    talent_codes = NULL,
    response_status = "coded",
    con = NULL,
    db_path = NULL
) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.", call. = FALSE)
  }

  connection <- qualitative_loader_connection(con = con, db_path = db_path)
  on.exit(qualitative_close_loader_connection(connection), add = TRUE)
  con <- connection$con

  metadata <- qualitative_codebook_view_metadata(con, codebook_id)
  view_name <- metadata$wide_view_name[[1]]
  code_columns <- metadata$code_column_name
  code_select <- paste0(
    "c.",
    as.character(DBI::dbQuoteIdentifier(con, code_columns))
  )
  view_relation <- paste0(
    as.character(DBI::dbQuoteIdentifier(con, "qualitative")),
    ".",
    as.character(DBI::dbQuoteIdentifier(con, view_name))
  )

  coding_where <- character()
  params <- list()
  if (!is.null(pipeline_run_id) &&
      !identical(as.character(pipeline_run_id), "latest")) {
    filter <- qualitative_filter_clause(
      "c.pipeline_run_id",
      pipeline_run_id,
      params
    )
    coding_where <- c(coding_where, filter$sql)
    params <- filter$params
  }

  outer_where <- character()
  filters <- list(
    list(column = "t.dataset_id", values = dataset_id),
    list(column = "t.video_id", values = video_ids),
    list(column = "t.talent_code", values = talent_codes),
    list(column = "c.response_status", values = response_status)
  )
  for (filter_spec in filters) {
    filter <- qualitative_filter_clause(
      filter_spec$column,
      filter_spec$values,
      params
    )
    if (!is.null(filter$sql)) {
      outer_where <- c(outer_where, filter$sql)
    }
    params <- filter$params
  }

  coding_where_sql <- if (length(coding_where) > 0L) {
    paste0("WHERE ", paste(coding_where, collapse = "\n  AND "))
  } else {
    ""
  }
  outer_where_sql <- if (length(outer_where) > 0L) {
    paste0("WHERE ", paste(outer_where, collapse = "\n  AND "))
  } else {
    ""
  }
  latest_filter <- if (identical(as.character(pipeline_run_id), "latest")) {
    "WHERE coding_rank = 1"
  } else {
    ""
  }

  query <- paste0(
    "WITH coding_ranked AS (\n",
    "  SELECT\n",
    "    c.*,\n",
    "    row_number() OVER (\n",
    "      PARTITION BY c.transcript_line_id\n",
    "      ORDER BY p.completed_at DESC NULLS LAST, c.pipeline_run_id DESC\n",
    "    ) AS coding_rank\n",
    "  FROM ", view_relation, " c\n",
    "  LEFT JOIN ops.pipeline_runs p\n",
    "    ON c.pipeline_run_id = p.pipeline_run_id\n",
    "  ", coding_where_sql, "\n",
    "),\n",
    "coding_selected AS (\n",
    "  SELECT * EXCLUDE (coding_rank)\n",
    "  FROM coding_ranked\n",
    "  ", latest_filter, "\n",
    ")\n",
    "SELECT\n",
    "  t.dataset_id,\n",
    "  t.transcript_line_id,\n",
    "  t.video_id,\n",
    "  t.talent_code,\n",
    "  talent.talent_name,\n",
    "  video.title AS video_title,\n",
    "  video.published_at,\n",
    "  video.content_type,\n",
    "  t.line_number,\n",
    "  t.seconds,\n",
    "  t.timecode,\n",
    "  t.source,\n",
    "  t.speaker,\n",
    "  t.text,\n",
    "  list_extract(t.source_record_keys, 1) AS source_record_key,\n",
    "  t.alignment_status,\n",
    "  t.source_file,\n",
    "  t.legacy_row_id AS row_id,\n",
    "  c.pipeline_run_id,\n",
    "  c.codebook_id,\n",
    "  c.request_custom_id,\n",
    "  c.response_status,\n",
    "  c.confidence,\n",
    "  c.needs_review,\n",
    "  c.review_reason,\n",
    "  c.response_decision_count,\n",
    "  c.response_duplicate_status,\n",
    "  c.validation_error,\n  ",
    paste(code_select, collapse = ",\n  "),
    "\nFROM qualitative.transcripts t\n",
    "JOIN coding_selected c\n",
    "  ON t.transcript_line_id = c.transcript_line_id\n",
    "JOIN catalog.videos video\n",
    "  ON t.video_id = video.video_id\n",
    "JOIN catalog.talents talent\n",
    "  ON t.talent_code = talent.talent_code\n",
    outer_where_sql,
    "\nORDER BY t.talent_code, t.video_id, t.line_number"
  )

  out <- DBI::dbGetQuery(con, query, params = params)

  chat_keys <- unique(out[
    out$source == "chat" &
      !is.na(out$source_record_key) &
      nzchar(out$source_record_key),
    c("transcript_line_id", "source_record_key"),
    drop = FALSE
  ])
  chat_columns <- c(
    "message_type",
    "paid_amount_text",
    "paid_amount_value",
    "paid_currency"
  )
  for (column in chat_columns) {
    out[[column]] <- NA
  }
  out$paid_amount_value <- as.numeric(out$paid_amount_value)

  if (nrow(chat_keys) > 0L) {
    DBI::dbWriteTable(
      con,
      "qualitative_loader_chat_keys",
      chat_keys,
      temporary = TRUE,
      overwrite = TRUE
    )
    chat_metadata <- DBI::dbGetQuery(
      con,
      "SELECT
         keys.transcript_line_id,
         chat.message_type,
         chat.paid_amount_text,
         chat.paid_amount_value,
         chat.paid_currency
       FROM qualitative_loader_chat_keys keys
       LEFT JOIN text.chat_messages chat
         ON keys.source_record_key = chat.message_key"
    )
    row_match <- match(
      out$transcript_line_id,
      chat_metadata$transcript_line_id
    )
    matched <- !is.na(row_match)
    for (column in chat_columns) {
      out[[column]][matched] <- chat_metadata[[column]][row_match[matched]]
    }
  }
  missing_code_columns <- setdiff(code_columns, names(out))
  if (length(missing_code_columns) > 0L) {
    stop(
      "Wide qualitative import is missing code columns: ",
      paste(missing_code_columns, collapse = ", "),
      call. = FALSE
    )
  }
  non_logical <- code_columns[
    !vapply(out[code_columns], is.logical, logical(1))
  ]
  if (length(non_logical) > 0L) {
    stop(
      "Wide qualitative code columns are not logical: ",
      paste(non_logical, collapse = ", "),
      call. = FALSE
    )
  }

  tibble::as_tibble(out)
}

load_qualitative_codebook <- function(
    codebook_id,
    con = NULL,
    db_path = NULL
) {
  connection <- qualitative_loader_connection(con = con, db_path = db_path)
  on.exit(qualitative_close_loader_connection(connection), add = TRUE)

  out <- DBI::dbGetQuery(
    connection$con,
    "SELECT
       primary_code_id AS \"Primary Code ID\",
       primary_code_name AS \"Primary Code\",
       secondary_code_id AS \"Secondary Code ID\",
       secondary_code_name AS \"Secondary Code\",
       definition AS \"Definition\",
       examples AS \"Examples from text\",
       code_id,
       code_column_name
     FROM qualitative.codebooks
     WHERE codebook_id = ?
     ORDER BY display_order, code_id",
    params = list(codebook_id)
  )
  if (nrow(out) == 0L) {
    stop("Unknown qualitative codebook: ", codebook_id, call. = FALSE)
  }
  tibble::as_tibble(out)
}

load_qualitative_video_performance <- function(
    video_ids,
    con = NULL,
    db_path = NULL
) {
  video_ids <- unique(as.character(video_ids))
  video_ids <- video_ids[!is.na(video_ids) & nzchar(video_ids)]
  if (length(video_ids) == 0L) {
    return(tibble::tibble())
  }

  connection <- qualitative_loader_connection(con = con, db_path = db_path)
  on.exit(qualitative_close_loader_connection(connection), add = TRUE)

  DBI::dbWriteTable(
    connection$con,
    "qualitative_loader_video_ids",
    data.frame(video_id = video_ids),
    temporary = TRUE,
    overwrite = TRUE
  )
  out <- DBI::dbGetQuery(
    connection$con,
    "SELECT
       performance.video_id,
       CAST(performance.views AS DOUBLE) AS analytics_views,
       performance.estimated_minutes_watched
         AS analytics_estimated_minutes_watched,
       performance.average_view_duration
         AS analytics_average_view_duration,
       performance.average_view_percentage
         AS analytics_average_view_percentage,
       CAST(performance.subscribers_gained AS DOUBLE)
         AS analytics_subscribers_gained,
       CAST(performance.subscribers_lost AS DOUBLE)
         AS analytics_subscribers_lost,
       performance.duration_seconds / 60.0
         AS analytics_duration_minutes,
       sin(
         2 * pi() * extract(hour FROM performance.published_at) / 24.0
       ) AS analytics_publish_hour_sin,
       cos(
         2 * pi() * extract(hour FROM performance.published_at) / 24.0
       ) AS analytics_publish_hour_cos,
       CAST(
         dayofweek(performance.published_at) IN (0, 6)
         AS INTEGER
       ) AS analytics_is_weekend
     FROM analytics.video_latest_performance performance
     JOIN qualitative_loader_video_ids selected
       ON performance.video_id = selected.video_id
     ORDER BY performance.video_id"
  )
  tibble::as_tibble(out)
}
