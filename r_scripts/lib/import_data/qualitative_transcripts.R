qualitative_load_sql_query <- function(filename) {
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package `here` is required to load SQL queries.", call. = FALSE)
  }
  if (!grepl("^[a-z0-9_]+[.]sql$", filename)) {
    stop("Invalid qualitative SQL query filename: ", filename, call. = FALSE)
  }

  query_path <- here::here(
    "sql_queries",
    "unified_db",
    "qualitative",
    filename
  )
  if (!file.exists(query_path)) {
    stop("Qualitative SQL query not found: ", query_path, call. = FALSE)
  }

  paste(
    readLines(query_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
}

qualitative_render_sql_query <- function(filename, replacements) {
  query <- qualitative_load_sql_query(filename)
  for (name in names(replacements)) {
    token <- paste0("{{", name, "}}")
    if (!grepl(token, query, fixed = TRUE)) {
      stop("SQL template token not found: ", token, call. = FALSE)
    }
    query <- gsub(
      token,
      as.character(replacements[[name]]),
      query,
      fixed = TRUE
    )
  }
  if (grepl("{{", query, fixed = TRUE)) {
    stop("SQL template contains unresolved tokens.", call. = FALSE)
  }
  query
}

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
    qualitative_load_sql_query("codebook_view_metadata.sql"),
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
    qualitative_load_sql_query("wide_view_exists.sql"),
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

  query <- qualitative_render_sql_query(
    "load_transcripts_wide.sql",
    list(
      view_relation = view_relation,
      coding_where = coding_where_sql,
      latest_filter = latest_filter,
      code_columns = paste(code_select, collapse = ",\n  "),
      outer_where = outer_where_sql
    )
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
      qualitative_load_sql_query("chat_metadata.sql")
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
    qualitative_load_sql_query("load_codebook.sql"),
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
    qualitative_load_sql_query("video_performance.sql")
  )
  tibble::as_tibble(out)
}
