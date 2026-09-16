raw_data_admin_scope <- function() {
  data.frame(
    table_schema = c(
      rep("catalog", 5L),
      rep("clean", 6L),
      "analytics",
      rep("text", 2L),
      rep("ops", 6L)
    ),
    table_name = c(
      "talents", "channels", "videos", "talent_aliases", "talent_profiles",
      "video_analytics_snapshots", "video_monetary_snapshots",
      "video_demographics", "video_geography", "subscriber_daily",
      "public_subscriber_snapshots", "video_latest_performance",
      "subtitle_units", "chat_messages", "pipeline_runs",
      "ingestion_events", "source_files", "data_quality_results",
      "raw_ingestion_quality", "collection_failures"
    ),
    domain = c(
      rep("Talent catalog", 5L),
      rep("Analytics", 7L),
      rep("Text", 2L),
      rep("Operations", 6L)
    ),
    description = c(
      "Canonical talent records",
      "Talent-to-channel relationships",
      "Canonical video records",
      "Normalized talent aliases",
      "Versioned talent profiles",
      "Longitudinal video analytics snapshots",
      "Longitudinal video revenue snapshots",
      "Video demographic breakdowns",
      "Video geography breakdowns",
      "Daily subscriber analytics",
      "Public subscriber observations",
      "Latest performance record per video",
      "Canonical subtitle units",
      "Canonical chat messages",
      "Pipeline execution history",
      "Published-row lineage by ingestion event",
      "Registered source files",
      "Recorded data-quality checks",
      "Raw-to-published reconciliation checks",
      "Unresolved and resolved collection failures"
    ),
    stringsAsFactors = FALSE
  )
}

raw_data_admin_relation_key <- function(table_schema, table_name) {
  paste(table_schema, table_name, sep = ".")
}

raw_data_admin_connect <- function(database_path) {
  duckdb_connect(db_path = database_path, read_only = TRUE)
}

raw_data_admin_disconnect <- function(con) {
  tryCatch(
    DBI::dbDisconnect(con, shutdown = TRUE),
    error = function(error) invisible(NULL)
  )
}

raw_data_admin_relation_sql <- function(con, table_schema, table_name) {
  as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = table_schema, table = table_name)
  ))
}

raw_data_admin_pipeline_condition <- function(column = "pipeline_name") {
  paste0(
    "REGEXP_MATCHES(LOWER(COALESCE(", column, ", '')), '",
    "analytics|subscriber|subtitle|chat|youtube|video|talent|catalog|",
    "ingest|text|geograph|demograph|monetary|performance') ",
    "AND NOT REGEXP_MATCHES(LOWER(COALESCE(", column, ", '')), ",
    "'classif|qualitative|summary')"
  )
}

raw_data_admin_source_condition <- function(column = "source_type") {
  paste0(
    "REGEXP_MATCHES(LOWER(COALESCE(", column, ", '')), '",
    "subtitle|chat|video_|subscriber|subs_daily|talent_profile|catalog') ",
    "AND NOT REGEXP_MATCHES(LOWER(COALESCE(", column, ", '')), ",
    "'classif|qualitative|summary')"
  )
}

raw_data_admin_scoped_relation_sql <- function(con, table_schema, table_name) {
  relation_sql <- raw_data_admin_relation_sql(con, table_schema, table_name)
  relation <- raw_data_admin_relation_key(table_schema, table_name)
  if (relation == "ops.pipeline_runs") {
    return(paste0(
      "(SELECT run.* FROM ", relation_sql, " AS run WHERE ",
      raw_data_admin_pipeline_condition("run.pipeline_name"),
      ") AS scoped_relation"
    ))
  }
  if (relation == "ops.ingestion_events") {
    return(paste0(
      "(SELECT event.* FROM ", relation_sql, " AS event ",
      "LEFT JOIN ops.pipeline_runs AS run USING (pipeline_run_id) WHERE ",
      "REGEXP_MATCHES(LOWER(COALESCE(event.destination_table, '')), ",
      "'^(catalog|clean|analytics|text)[.]') OR (",
      raw_data_admin_pipeline_condition("run.pipeline_name"),
      ")) AS scoped_relation"
    ))
  }
  if (relation == "ops.source_files") {
    return(paste0(
      "(SELECT source.* FROM ", relation_sql, " AS source WHERE ",
      raw_data_admin_source_condition("source.source_type"),
      ") AS scoped_relation"
    ))
  }
  if (relation == "ops.data_quality_results") {
    return(paste0(
      "(SELECT result.* FROM ", relation_sql, " AS result ",
      "LEFT JOIN ops.pipeline_runs AS run USING (pipeline_run_id) WHERE ",
      raw_data_admin_pipeline_condition("run.pipeline_name"),
      ") AS scoped_relation"
    ))
  }
  if (relation == "ops.collection_failures") {
    return(paste0(
      "(SELECT failure.* FROM ", relation_sql, " AS failure WHERE ",
      raw_data_admin_source_condition("failure.source_type"),
      ") AS scoped_relation"
    ))
  }
  relation_sql
}

raw_data_admin_column_sql <- function(con, column_name) {
  as.character(DBI::dbQuoteIdentifier(con, column_name))
}

raw_data_admin_columns <- function(con, table_schema, table_name) {
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT column_name, data_type, ordinal_position, is_nullable",
      "FROM information_schema.columns",
      "WHERE table_schema = ? AND table_name = ?",
      "ORDER BY ordinal_position"
    ),
    params = list(table_schema, table_name)
  )
}

raw_data_admin_relation_catalog <- function(con) {
  scope <- raw_data_admin_scope()
  available <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT table_schema, table_name, table_type",
      "FROM information_schema.tables",
      "WHERE table_schema NOT IN ('information_schema', 'pg_catalog')"
    )
  )
  relations <- merge(
    scope,
    available,
    by = c("table_schema", "table_name"),
    all = FALSE,
    sort = FALSE
  )
  relations$relation <- raw_data_admin_relation_key(
    relations$table_schema,
    relations$table_name
  )
  if (nrow(relations) == 0L) {
    relations$row_count <- numeric()
    relations$column_count <- integer()
    relations$recency_column <- character()
    relations$earliest_value <- as.POSIXct(character(), tz = "UTC")
    relations$latest_value <- as.POSIXct(character(), tz = "UTC")
    return(relations)
  }

  recency_priority <- c(
    "snapshot_date", "collected_at", "ingested_at", "event_at", "checked_at",
    "started_at", "modified_at", "discovered_at", "updated_at", "last_seen_at",
    "created_at", "published_at", "first_seen_at"
  )
  relation_details <- lapply(seq_len(nrow(relations)), function(index) {
    table_schema <- relations$table_schema[[index]]
    table_name <- relations$table_name[[index]]
    relation_sql <- raw_data_admin_scoped_relation_sql(
      con,
      table_schema,
      table_name
    )
    columns <- raw_data_admin_columns(con, table_schema, table_name)
    recency_column <- recency_priority[recency_priority %in% columns$column_name]
    recency_column <- if (length(recency_column) == 0L) {
      NA_character_
    } else {
      recency_column[[1]]
    }
    summary_sql <- if (is.na(recency_column)) {
      paste("SELECT COUNT(*) AS row_count FROM", relation_sql)
    } else {
      recency_sql <- raw_data_admin_column_sql(con, recency_column)
      paste(
        "SELECT COUNT(*) AS row_count,",
        paste0("MIN(", recency_sql, ") AS earliest_value,"),
        paste0("MAX(", recency_sql, ") AS latest_value"),
        "FROM", relation_sql
      )
    }
    summary <- tryCatch(
      DBI::dbGetQuery(con, summary_sql),
      error = function(error) data.frame(row_count = NA_real_)
    )
    data.frame(
      relation = raw_data_admin_relation_key(table_schema, table_name),
      row_count = as.numeric(summary$row_count[[1]]),
      column_count = nrow(columns),
      recency_column = recency_column,
      earliest_value = if ("earliest_value" %in% names(summary)) {
        as.character(summary$earliest_value[[1]])
      } else {
        NA_character_
      },
      latest_value = if ("latest_value" %in% names(summary)) {
        as.character(summary$latest_value[[1]])
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )
  })
  details <- do.call(rbind, relation_details)
  relations <- merge(relations, details, by = "relation", all.x = TRUE, sort = FALSE)
  relations[order(relations$domain, relations$table_schema, relations$table_name), ]
}

raw_data_admin_resolve_relation <- function(con, relation) {
  scope <- raw_data_admin_scope()
  scope$relation <- raw_data_admin_relation_key(
    scope$table_schema,
    scope$table_name
  )
  selected <- scope[scope$relation == relation, , drop = FALSE]
  if (nrow(selected) != 1L) {
    stop("Relation is outside the raw-data dashboard scope: ", relation, call. = FALSE)
  }
  columns <- raw_data_admin_columns(
    con,
    selected$table_schema[[1]],
    selected$table_name[[1]]
  )
  if (nrow(columns) == 0L) {
    stop("Relation is not available in this database: ", relation, call. = FALSE)
  }
  list(
    table_schema = selected$table_schema[[1]],
    table_name = selected$table_name[[1]],
    columns = columns,
    relation_sql = raw_data_admin_scoped_relation_sql(
      con,
      selected$table_schema[[1]],
      selected$table_name[[1]]
    )
  )
}

raw_data_admin_is_relevant_pipeline <- function(pipeline_name) {
  pipeline_name <- tolower(ifelse(is.na(pipeline_name), "", pipeline_name))
  included <- grepl(
    paste(
      "analytics|subscriber|subtitle|chat|youtube|video|talent|catalog|",
      "ingest|text|geograph|demograph|monetary|performance",
      sep = ""
    ),
    pipeline_name
  )
  excluded <- grepl("classif|qualitative|summary", pipeline_name)
  included & !excluded
}

raw_data_admin_recent_runs <- function(con, limit = 100L) {
  limit <- max(1L, min(as.integer(limit), 500L))
  runs <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT pipeline_run_id, pipeline_name, started_at, completed_at,",
      "status, error_summary",
      "FROM ops.pipeline_runs",
      "ORDER BY started_at DESC"
    )
  )
  runs <- runs[raw_data_admin_is_relevant_pipeline(runs$pipeline_name), , drop = FALSE]
  pipeline_rank <- ave(
    seq_len(nrow(runs)),
    runs$pipeline_name,
    FUN = seq_along
  )
  runs <- runs[pipeline_rank <= 10L, , drop = FALSE]
  runs <- runs[order(runs$started_at, decreasing = TRUE), , drop = FALSE]
  utils::head(runs, limit)
}

raw_data_admin_recent_events <- function(con, limit = 200L) {
  limit <- max(1L, min(as.integer(limit), 1000L))
  events <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT event.ingestion_event_id, event.pipeline_run_id,",
      "run.pipeline_name, event.destination_table, event.rows_read,",
      "event.rows_published, event.event_at, event.source_file_id",
      "FROM ops.ingestion_events AS event",
      "LEFT JOIN ops.pipeline_runs AS run USING (pipeline_run_id)",
      "ORDER BY event.event_at DESC"
    )
  )
  destination_relevant <- grepl(
    "^(catalog|clean|analytics|text)[.]",
    ifelse(is.na(events$destination_table), "", events$destination_table)
  )
  pipeline_relevant <- raw_data_admin_is_relevant_pipeline(events$pipeline_name)
  events <- events[destination_relevant | pipeline_relevant, , drop = FALSE]
  event_group <- ifelse(
    is.na(events$destination_table) | !nzchar(events$destination_table),
    events$pipeline_name,
    events$destination_table
  )
  destination_rank <- ave(seq_len(nrow(events)), event_group, FUN = seq_along)
  events <- events[destination_rank <= 10L, , drop = FALSE]
  events <- events[order(events$event_at, decreasing = TRUE), , drop = FALSE]
  utils::head(events, limit)
}

raw_data_admin_health_check <- function(
    con,
    available_relations,
    check_name,
    relation,
    severity,
    details,
    sql
) {
  if (!all(relation %in% available_relations)) return(NULL)
  result <- tryCatch(
    DBI::dbGetQuery(con, sql),
    error = function(error) structure(
      data.frame(affected_rows = NA_real_),
      query_error = conditionMessage(error)
    )
  )
  affected_rows <- as.numeric(result$affected_rows[[1]])
  query_error <- attr(result, "query_error")
  status <- if (!is.null(query_error) || is.na(affected_rows)) {
    "Unavailable"
  } else if (affected_rows == 0) {
    "Pass"
  } else if (severity == "Critical") {
    "Critical"
  } else {
    "Review"
  }
  if (!is.null(query_error)) details <- paste(details, query_error, sep = " Error: ")
  data.frame(
    status = status,
    severity = severity,
    check_name = check_name,
    affected_rows = affected_rows,
    relation = paste(relation, collapse = ", "),
    details = details,
    stringsAsFactors = FALSE
  )
}

raw_data_admin_health_checks <- function(con, relation_catalog) {
  available <- relation_catalog$relation
  analytics_cutoff <- format(Sys.Date() - 2L, "%Y-%m-%d")
  collection_cutoff <- format(
    as.POSIXct(Sys.time() - 7 * 24 * 60 * 60, tz = "UTC"),
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  specifications <- list(
    list(
      "Videos without a talent", "catalog.videos", "Critical",
      "Every video should resolve to catalog.talents.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM catalog.videos AS video",
        "LEFT JOIN catalog.talents AS talent USING (talent_code)",
        "WHERE talent.talent_code IS NULL"
      )
    ),
    list(
      "Analytics rows without a video", c("clean.video_analytics_snapshots", "catalog.videos"),
      "Critical", "Every analytics snapshot should resolve to catalog.videos.",
      paste(
        "SELECT COUNT(*) AS affected_rows",
        "FROM clean.video_analytics_snapshots AS snapshot",
        "LEFT JOIN catalog.videos AS video USING (video_id)",
        "WHERE video.video_id IS NULL"
      )
    ),
    list(
      "Subtitle rows without a video", c("text.subtitle_units", "catalog.videos"),
      "Critical", "Every subtitle unit should resolve to catalog.videos.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM text.subtitle_units AS subtitle",
        "LEFT JOIN catalog.videos AS video USING (video_id)",
        "WHERE video.video_id IS NULL"
      )
    ),
    list(
      "Chat rows without a video", c("text.chat_messages", "catalog.videos"),
      "Critical", "Every chat message should resolve to catalog.videos.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM text.chat_messages AS chat",
        "LEFT JOIN catalog.videos AS video USING (video_id)",
        "WHERE video.video_id IS NULL"
      )
    ),
    list(
      "Analytics rows missing lineage", "clean.video_analytics_snapshots",
      "Warning", "Analytics rows should retain source-file and pipeline-run lineage.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM clean.video_analytics_snapshots",
        "WHERE source_file_id IS NULL OR pipeline_run_id IS NULL"
      )
    ),
    list(
      "Subtitle rows missing lineage", "text.subtitle_units", "Warning",
      "Subtitle rows should retain source-file and pipeline-run lineage.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM text.subtitle_units",
        "WHERE source_file_id IS NULL OR pipeline_run_id IS NULL"
      )
    ),
    list(
      "Chat rows missing lineage", "text.chat_messages", "Warning",
      "Chat rows should retain source-file and pipeline-run lineage.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM text.chat_messages",
        "WHERE source_file_id IS NULL OR pipeline_run_id IS NULL"
      )
    ),
    list(
      "Blank subtitle text", "text.subtitle_units", "Warning",
      "Subtitle text should not be null or blank.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM text.subtitle_units",
        "WHERE subtitle_text IS NULL OR TRIM(subtitle_text) = ''"
      )
    ),
    list(
      "Blank chat messages", "text.chat_messages", "Warning",
      "Chat message text should not be null or blank.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM text.chat_messages",
        "WHERE message IS NULL OR TRIM(message) = ''"
      )
    ),
    list(
      "Negative video metrics", "clean.video_analytics_snapshots", "Critical",
      "Count-like analytics measures should not be negative.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM clean.video_analytics_snapshots",
        "WHERE views < 0 OR subscribers_gained < 0 OR subscribers_lost < 0"
      )
    ),
    list(
      "Stale analytics collection", "clean.video_analytics_snapshots", "Warning",
      "The newest analytics snapshot should normally be no more than two days old.",
      paste(
        "SELECT CASE WHEN MAX(snapshot_date) IS NULL",
        paste0("OR MAX(snapshot_date) < DATE '", analytics_cutoff, "'"),
        "THEN 1 ELSE 0 END AS affected_rows",
        "FROM clean.video_analytics_snapshots"
      )
    ),
    list(
      "Stale subtitle collection", "text.subtitle_units", "Warning",
      "The newest subtitle collection should normally be no more than seven days old.",
      paste(
        "SELECT CASE WHEN MAX(collected_at) IS NULL",
        paste0("OR MAX(collected_at) < TIMESTAMP '", collection_cutoff, "'"),
        "THEN 1 ELSE 0 END AS affected_rows FROM text.subtitle_units"
      )
    ),
    list(
      "Stale chat collection", "text.chat_messages", "Warning",
      "The newest chat collection should normally be no more than seven days old.",
      paste(
        "SELECT CASE WHEN MAX(collected_at) IS NULL",
        paste0("OR MAX(collected_at) < TIMESTAMP '", collection_cutoff, "'"),
        "THEN 1 ELSE 0 END AS affected_rows FROM text.chat_messages"
      )
    ),
    list(
      "Raw ingestion reconciliation mismatch", "ops.raw_ingestion_quality", "Critical",
      "For the newest check per dataset, source rows should equal published plus excluded rows.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM (",
        "SELECT *, ROW_NUMBER() OVER (PARTITION BY dataset ORDER BY checked_at DESC) AS recency",
        "FROM ops.raw_ingestion_quality",
        ") AS quality WHERE recency = 1",
        "AND source_rows <> published_rows + excluded_rows"
      )
    ),
    list(
      "Stale raw ingestion reconciliation", "ops.raw_ingestion_quality", "Warning",
      "The newest raw-ingestion reconciliation per dataset should be no more than seven days old.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM (",
        "SELECT dataset, MAX(checked_at) AS checked_at",
        "FROM ops.raw_ingestion_quality GROUP BY dataset",
        ") AS quality",
        paste0("WHERE checked_at < TIMESTAMP '", collection_cutoff, "'")
      )
    ),
    list(
      "Recorded data-quality violations", "ops.data_quality_results", "Warning",
      "Recorded checks with violations or a non-passing status need review.",
      paste(
        "SELECT COUNT(*) AS affected_rows",
        "FROM ops.data_quality_results AS result",
        "LEFT JOIN ops.pipeline_runs AS run USING (pipeline_run_id)",
        "WHERE (COALESCE(violation_count, 0) > 0",
        "OR LOWER(COALESCE(result.status, ''))",
        "NOT IN ('pass', 'passed', 'ok', 'success'))",
        "AND", raw_data_admin_pipeline_condition("run.pipeline_name")
      )
    ),
    list(
      "Unresolved collection failures", "ops.collection_failures", "Warning",
      "Unresolved source collection failures need review.",
      paste(
        "SELECT COUNT(*) AS affected_rows FROM ops.collection_failures AS failure",
        "WHERE resolved_at IS NULL AND",
        raw_data_admin_source_condition("failure.source_type")
      )
    )
  )
  checks <- lapply(specifications, function(specification) {
    raw_data_admin_health_check(
      con = con,
      available_relations = available,
      check_name = specification[[1]],
      relation = specification[[2]],
      severity = specification[[3]],
      details = specification[[4]],
      sql = specification[[5]]
    )
  })

  if ("ops.pipeline_runs" %in% available) {
    recent_failed_runs <- raw_data_admin_recent_runs(con, limit = 500L)
    recent_failed_runs <- recent_failed_runs[
      !is.na(recent_failed_runs$status) &
        tolower(recent_failed_runs$status) %in% c("failed", "error"),
      ,
      drop = FALSE
    ]
    checks[[length(checks) + 1L]] <- data.frame(
      status = if (nrow(recent_failed_runs) == 0L) "Pass" else "Review",
      severity = "Warning",
      check_name = "Failed relevant pipeline runs",
      affected_rows = nrow(recent_failed_runs),
      relation = "ops.pipeline_runs",
      details = "Relevant pipeline runs with a failed or error status in retained history.",
      stringsAsFactors = FALSE
    )
  }
  checks <- checks[!vapply(checks, is.null, logical(1))]
  if (length(checks) == 0L) return(data.frame())
  result <- do.call(rbind, checks)
  status_order <- match(result$status, c("Critical", "Review", "Unavailable", "Pass"))
  result[order(status_order, result$check_name), , drop = FALSE]
}

raw_data_admin_query_if_available <- function(con, relation_catalog, relation, sql) {
  if (!relation %in% relation_catalog$relation) return(data.frame())
  DBI::dbGetQuery(con, sql)
}

raw_data_admin_talent_coverage <- function(con, relation_catalog) {
  talents <- raw_data_admin_query_if_available(
    con,
    relation_catalog,
    "catalog.talents",
    paste(
      "SELECT talent_code, talent_name, active FROM catalog.talents",
      "ORDER BY talent_code"
    )
  )
  if (nrow(talents) == 0L) return(data.frame())
  summaries <- list(
    raw_data_admin_query_if_available(
      con, relation_catalog, "catalog.videos",
      paste(
        "SELECT talent_code, COUNT(*) AS catalog_videos,",
        "MAX(published_at) AS latest_published_at",
        "FROM catalog.videos GROUP BY talent_code"
      )
    ),
    raw_data_admin_query_if_available(
      con, relation_catalog, "clean.video_analytics_snapshots",
      paste(
        "SELECT talent_code, COUNT(*) AS analytics_rows,",
        "COUNT(DISTINCT video_id) AS videos_with_analytics,",
        "MAX(snapshot_date) AS latest_analytics_date",
        "FROM clean.video_analytics_snapshots GROUP BY talent_code"
      )
    ),
    raw_data_admin_query_if_available(
      con, relation_catalog, "text.subtitle_units",
      paste(
        "SELECT talent_code, COUNT(*) AS subtitle_units,",
        "COUNT(DISTINCT video_id) AS videos_with_subtitles,",
        "MAX(collected_at) AS latest_subtitle_collection",
        "FROM text.subtitle_units GROUP BY talent_code"
      )
    ),
    raw_data_admin_query_if_available(
      con, relation_catalog, "text.chat_messages",
      paste(
        "SELECT talent_code, COUNT(*) AS chat_messages,",
        "COUNT(DISTINCT video_id) AS videos_with_chat,",
        "MAX(collected_at) AS latest_chat_collection",
        "FROM text.chat_messages GROUP BY talent_code"
      )
    )
  )
  summaries <- summaries[vapply(summaries, nrow, integer(1)) > 0L]
  coverage <- talents
  for (summary in summaries) {
    coverage <- merge(coverage, summary, by = "talent_code", all.x = TRUE, sort = FALSE)
  }
  count_columns <- intersect(
    c(
      "catalog_videos", "analytics_rows", "videos_with_analytics",
      "subtitle_units", "videos_with_subtitles", "chat_messages",
      "videos_with_chat"
    ),
    names(coverage)
  )
  coverage[count_columns] <- lapply(coverage[count_columns], function(value) {
    value[is.na(value)] <- 0
    as.numeric(value)
  })
  coverage[order(coverage$talent_code), , drop = FALSE]
}

raw_data_admin_snapshot <- function(database_path = NULL) {
  if (is.null(database_path) || !nzchar(database_path)) {
    database_path <- talent_lakehouse_db_path()
  }
  database_path <- normalizePath(database_path, winslash = "/", mustWork = TRUE)
  con <- raw_data_admin_connect(database_path)
  on.exit(raw_data_admin_disconnect(con), add = TRUE)
  relation_catalog <- raw_data_admin_relation_catalog(con)
  list(
    database_path = database_path,
    database_bytes = as.numeric(file.info(database_path)$size),
    database_modified_at = as.POSIXct(file.info(database_path)$mtime, tz = "UTC"),
    refreshed_at = Sys.time(),
    relations = relation_catalog,
    health_checks = raw_data_admin_health_checks(con, relation_catalog),
    recent_runs = raw_data_admin_recent_runs(con),
    recent_events = raw_data_admin_recent_events(con),
    talent_coverage = raw_data_admin_talent_coverage(con, relation_catalog)
  )
}

raw_data_admin_preview <- function(
    database_path,
    relation,
    talent_code = "",
    video_id = "",
    limit = 500L
) {
  limit <- max(1L, min(as.integer(limit), 5000L))
  con <- raw_data_admin_connect(database_path)
  on.exit(raw_data_admin_disconnect(con), add = TRUE)
  resolved <- raw_data_admin_resolve_relation(con, relation)
  available_columns <- resolved$columns$column_name
  where <- character()
  params <- list()
  if (nzchar(trimws(talent_code)) && "talent_code" %in% available_columns) {
    where <- c(where, paste(raw_data_admin_column_sql(con, "talent_code"), "= ?"))
    params <- c(params, trimws(talent_code))
  }
  if (nzchar(trimws(video_id)) && "video_id" %in% available_columns) {
    where <- c(where, paste(raw_data_admin_column_sql(con, "video_id"), "= ?"))
    params <- c(params, trimws(video_id))
  }
  recency_priority <- c(
    "snapshot_date", "collected_at", "ingested_at", "event_at", "checked_at",
    "started_at", "modified_at", "updated_at", "created_at", "published_at"
  )
  order_column <- recency_priority[recency_priority %in% available_columns]
  sql <- paste("SELECT * FROM", resolved$relation_sql)
  if (length(where) > 0L) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  if (length(order_column) > 0L) {
    sql <- paste(
      sql,
      "ORDER BY",
      raw_data_admin_column_sql(con, order_column[[1]]),
      "DESC NULLS LAST"
    )
  }
  sql <- paste(sql, "LIMIT", limit)
  DBI::dbGetQuery(con, sql, params = params)
}

raw_data_admin_profile <- function(database_path, relation) {
  con <- raw_data_admin_connect(database_path)
  on.exit(raw_data_admin_disconnect(con), add = TRUE)
  resolved <- raw_data_admin_resolve_relation(con, relation)
  columns <- resolved$columns
  if (nrow(columns) == 0L) {
    return(list(columns = data.frame(), numeric = data.frame()))
  }
  metric_sql <- c("COUNT(*) AS total_rows")
  for (index in seq_len(nrow(columns))) {
    column_sql <- raw_data_admin_column_sql(con, columns$column_name[[index]])
    metric_sql <- c(
      metric_sql,
      paste0(
        "COUNT(*) - COUNT(", column_sql, ") AS null_", index
      ),
      paste0(
        "approx_count_distinct(", column_sql, ") AS distinct_", index
      )
    )
  }
  metrics <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT", paste(metric_sql, collapse = ", "),
      "FROM", resolved$relation_sql
    )
  )
  total_rows <- as.numeric(metrics$total_rows[[1]])
  column_profile <- data.frame(
    column_name = columns$column_name,
    data_type = columns$data_type,
    nullable = columns$is_nullable,
    null_rows = vapply(seq_len(nrow(columns)), function(index) {
      as.numeric(metrics[[paste0("null_", index)]][[1]])
    }, numeric(1)),
    approx_distinct = vapply(seq_len(nrow(columns)), function(index) {
      as.numeric(metrics[[paste0("distinct_", index)]][[1]])
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
  column_profile$null_percent <- if (total_rows == 0) {
    0
  } else {
    round(100 * column_profile$null_rows / total_rows, 2)
  }
  column_profile <- column_profile[
    , c(
      "column_name", "data_type", "nullable", "null_rows", "null_percent",
      "approx_distinct"
    )
  ]

  numeric_types <- paste(
    "TINYINT|SMALLINT|INTEGER|BIGINT|HUGEINT|UTINYINT|USMALLINT|",
    "UINTEGER|UBIGINT|FLOAT|DOUBLE|DECIMAL|NUMERIC|REAL",
    sep = ""
  )
  numeric_columns <- columns[grepl(numeric_types, columns$data_type), , drop = FALSE]
  numeric_profile <- lapply(seq_len(nrow(numeric_columns)), function(index) {
    column_name <- numeric_columns$column_name[[index]]
    column_sql <- raw_data_admin_column_sql(con, column_name)
    summary <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT",
        paste0("MIN(", column_sql, ") AS minimum,"),
        paste0("AVG(", column_sql, ") AS mean,"),
        paste0("MEDIAN(", column_sql, ") AS median,"),
        paste0("MAX(", column_sql, ") AS maximum"),
        "FROM", resolved$relation_sql
      )
    )
    data.frame(column_name = column_name, summary, stringsAsFactors = FALSE)
  })
  numeric_profile <- if (length(numeric_profile) == 0L) {
    data.frame()
  } else {
    do.call(rbind, numeric_profile)
  }
  list(columns = column_profile, numeric = numeric_profile)
}

# Transcript operations use exact run lineage; never substitute a newer transcript.
raw_data_admin_transcript_run_details <- function(error_summary) {
  if (length(error_summary) == 0L || is.na(error_summary[[1]]) ||
      !nzchar(trimws(as.character(error_summary[[1]])))) {
    return(list(counts = "No summary recorded.", issues = NA_character_))
  }

  parts <- strsplit(as.character(error_summary[[1]]), "; ", fixed = TRUE)[[1]]
  metric_labels <- c(
    backlog_at_start = "Videos awaiting processing at batch start",
    selected = "Videos awaiting processing at batch start",
    batch_limit = "Maximum videos this batch",
    attempted = "Videos attempted",
    new_started = "Videos attempted",
    completed = "Videos completed",
    published = "Videos completed",
    skipped_current = "Videos already current and skipped",
    current = "Videos already current and skipped",
    failed = "Videos failed",
    requested_blocks = "Model blocks requested",
    reused_blocks = "Model blocks reused"
  )
  count_parts <- character()
  issue_parts <- character()
  seen_labels <- character()

  for (part in parts) {
    key <- sub("=.*$", "", part)
    if (key %in% names(metric_labels)) {
      label <- unname(metric_labels[[key]])
      if (label %in% seen_labels) next
      value <- sub("^[^=]*=", "", part)
      numeric_value <- suppressWarnings(as.numeric(value))
      if (!is.na(numeric_value)) {
        value <- format(numeric_value, big.mark = ",", scientific = FALSE, trim = TRUE)
      }
      count_parts <- c(count_parts, paste0(label, ": ", value))
      seen_labels <- c(seen_labels, label)
    } else if (!identical(key, "examined")) {
      issue_parts <- c(issue_parts, part)
    }
  }

  list(
    counts = if (length(count_parts) > 0L) {
      paste(count_parts, collapse = " • ")
    } else {
      "No batch counts recorded."
    },
    issues = if (length(issue_parts) > 0L) {
      paste(issue_parts, collapse = "; ")
    } else {
      NA_character_
    }
  )
}

raw_data_admin_transcript_run_metric <- function(error_summary, keys) {
  if (length(error_summary) == 0L || is.na(error_summary[[1]]) ||
      !nzchar(trimws(as.character(error_summary[[1]])))) {
    return(NA_real_)
  }
  parts <- strsplit(as.character(error_summary[[1]]), "; ", fixed = TRUE)[[1]]
  for (key in keys) {
    match <- parts[startsWith(parts, paste0(key, "="))]
    if (length(match) > 0L) {
      value <- suppressWarnings(as.numeric(sub("^[^=]*=", "", match[[1]])))
      if (!is.na(value)) return(value)
    }
  }
  NA_real_
}

raw_data_admin_transcript_runs <- function(database_path) {
  con <- raw_data_admin_connect(database_path)
  on.exit(raw_data_admin_disconnect(con), add = TRUE)
  if (nrow(raw_data_admin_columns(con, "ops", "pipeline_runs")) == 0L) {
    return(data.frame())
  }
  DBI::dbGetQuery(con, paste(
    "SELECT pipeline_run_id, pipeline_name, started_at, completed_at, status,",
    "epoch(completed_at - started_at) AS duration_seconds, error_summary",
    "FROM ops.pipeline_runs WHERE pipeline_name = 'subtitle_sentence_backfill'",
    "ORDER BY started_at DESC, pipeline_run_id DESC LIMIT 100"
  ))
}

raw_data_admin_transcript_related_run_ids <- function(con, pipeline_run_id) {
  if (nrow(raw_data_admin_columns(con, "ops", "pipeline_runs")) == 0L) {
    return(as.character(pipeline_run_id))
  }
  related <- DBI::dbGetQuery(
    con,
    paste(
      "WITH batch AS (SELECT started_at, completed_at FROM ops.pipeline_runs",
      "WHERE pipeline_run_id = ? AND pipeline_name = 'subtitle_sentence_backfill')",
      "SELECT pipeline_run_id FROM ops.pipeline_runs WHERE pipeline_run_id = ?",
      "OR (pipeline_name = 'subtitle_sentence_reconstruction'",
      "AND started_at >= (SELECT started_at FROM batch)",
      "AND started_at <= COALESCE((SELECT completed_at FROM batch), CURRENT_TIMESTAMP))",
      "ORDER BY started_at, pipeline_run_id"
    ),
    params = list(pipeline_run_id, pipeline_run_id)
  )$pipeline_run_id
  attempt_runs <- character()
  if (nrow(raw_data_admin_columns(
    con,
    "ops",
    "subtitle_backfill_attempts"
  )) > 0L) {
    attempt_runs <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT publication_pipeline_run_id FROM ops.subtitle_backfill_attempts",
        "WHERE batch_pipeline_run_id = ?",
        "AND publication_pipeline_run_id IS NOT NULL"
      ),
      params = list(pipeline_run_id)
    )$publication_pipeline_run_id
  }
  unique(c(
    as.character(pipeline_run_id),
    as.character(related),
    as.character(attempt_runs)
  ))
}

raw_data_admin_transcript_tracks <- function(database_path, pipeline_run_id) {
  con <- raw_data_admin_connect(database_path)
  on.exit(raw_data_admin_disconnect(con), add = TRUE)
  if (nrow(raw_data_admin_columns(
    con,
    "ops",
    "subtitle_backfill_attempts"
  )) > 0L) {
    attempts <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT attempt.candidate_position AS batch_position,",
        "attempt.video_id, video.title, attempt.talent_code, talent.talent_name,",
        "attempt.subtitle_language, attempt.subtitle_track_type,",
        "attempt.source_scope, attempt.pipeline_version, attempt.raw_rows,",
        "attempt.started_at, attempt.completed_at,",
        "attempt.sentences, attempt.blocks, attempt.requested_blocks,",
        "attempt.reused_blocks,",
        "attempt.error_summary AS block_errors,",
        "CASE attempt.status WHEN 'published' THEN 'Published'",
        "WHEN 'current' THEN 'Already current'",
        "WHEN 'failed' THEN 'Failed' ELSE 'In progress' END AS result",
        "FROM ops.subtitle_backfill_attempts AS attempt",
        "LEFT JOIN catalog.videos AS video",
        "ON attempt.video_id = video.video_id",
        "AND attempt.talent_code = video.talent_code",
        "LEFT JOIN catalog.talents AS talent",
        "ON attempt.talent_code = talent.talent_code",
        "WHERE attempt.batch_pipeline_run_id = ?",
        "ORDER BY attempt.candidate_position"
      ),
      params = list(pipeline_run_id)
    )
    if (nrow(attempts) > 0L) return(attempts)
  }

  related_run_ids <- raw_data_admin_transcript_related_run_ids(con, pipeline_run_id)
  run_placeholders <- paste(rep("?", length(related_run_ids)), collapse = ", ")
  sources <- character()
  params <- list()
  if (nrow(raw_data_admin_columns(con, "text", "subtitle_sentence_units")) > 0L) {
    sources <- c(sources, paste(
      "SELECT video_id, talent_code, subtitle_language, subtitle_track_type,",
      "source_scope, pipeline_version, COUNT(*) AS sentences,",
      "0 AS blocks, 0 AS failed_blocks, 0 AS incomplete_blocks,",
      "SUM(CASE WHEN source_alignment_status = 'block_approximate' THEN 1 ELSE 0 END)",
      "AS approximate_alignment, NULL::VARCHAR AS block_errors",
      "FROM text.subtitle_sentence_units WHERE pipeline_run_id IN (",
      run_placeholders, ") GROUP BY ALL"
    ))
    params <- c(params, as.list(related_run_ids))
  }
  if (nrow(raw_data_admin_columns(con, "ops", "subtitle_reconstruction_blocks")) > 0L) {
    sources <- c(sources, paste(
      "SELECT video_id, talent_code, subtitle_language, subtitle_track_type,",
      "source_scope, pipeline_version, 0 AS sentences, COUNT(*) AS blocks,",
      "SUM(CASE WHEN status = 'failed' THEN 1 ELSE 0 END) AS failed_blocks,",
      "SUM(CASE WHEN status <> 'complete' THEN 1 ELSE 0 END) AS incomplete_blocks,",
      "0 AS approximate_alignment, string_agg(DISTINCT error_summary, ' | ') AS block_errors",
      "FROM ops.subtitle_reconstruction_blocks WHERE pipeline_run_id IN (",
      run_placeholders, ") GROUP BY ALL"
    ))
    params <- c(params, as.list(related_run_ids))
  }
  if (length(sources) == 0L) return(data.frame())
  DBI::dbGetQuery(con, paste(
    "WITH records AS (", paste(sources, collapse = " UNION ALL "), "),",
    "tracks AS (SELECT video_id, talent_code, subtitle_language, subtitle_track_type,",
    "source_scope, pipeline_version, SUM(sentences) AS sentences, SUM(blocks) AS blocks,",
    "SUM(failed_blocks) AS failed_blocks, SUM(incomplete_blocks) AS incomplete_blocks,",
    "SUM(approximate_alignment) AS approximate_alignment,",
    "string_agg(block_errors, ' | ') AS block_errors FROM records GROUP BY ALL)",
    "SELECT tracks.video_id, video.title, tracks.talent_code, talent.talent_name,",
    "tracks.subtitle_language, tracks.subtitle_track_type, tracks.source_scope,",
    "tracks.pipeline_version, tracks.sentences, tracks.blocks, tracks.failed_blocks,",
    "tracks.incomplete_blocks, tracks.approximate_alignment, tracks.block_errors,",
    "CASE WHEN failed_blocks > 0 THEN 'Failed blocks'",
    "WHEN incomplete_blocks > 0 THEN 'Incomplete blocks'",
    "WHEN sentences > 0 THEN 'Published' ELSE 'No published sentences' END AS result",
    "FROM tracks LEFT JOIN catalog.videos AS video",
    "ON tracks.video_id = video.video_id AND tracks.talent_code = video.talent_code",
    "LEFT JOIN catalog.talents AS talent ON tracks.talent_code = talent.talent_code",
    "ORDER BY tracks.talent_code, tracks.video_id, tracks.subtitle_language,",
    "tracks.subtitle_track_type, tracks.source_scope, tracks.pipeline_version"
  ), params = params)
}

raw_data_admin_filter_transcript_tracks <- function(tracks, search = "") {
  search <- if (is.null(search) || length(search) == 0L || is.na(search[[1]])) {
    ""
  } else {
    trimws(as.character(search[[1]]))
  }
  if (nrow(tracks) == 0L || !nzchar(search)) return(tracks)

  searchable_columns <- intersect(
    c("video_id", "title", "talent_code", "talent_name", "result", "block_errors"),
    names(tracks)
  )
  matches <- Reduce(`|`, lapply(searchable_columns, function(column) {
    grepl(
      tolower(search),
      tolower(ifelse(is.na(tracks[[column]]), "", tracks[[column]])),
      fixed = TRUE
    )
  }))
  tracks[matches, , drop = FALSE]
}

raw_data_admin_transcript_text <- function(database_path, pipeline_run_id, track) {
  con <- raw_data_admin_connect(database_path)
  on.exit(raw_data_admin_disconnect(con), add = TRUE)
  if (nrow(raw_data_admin_columns(con, "text", "subtitle_sentence_units")) == 0L) {
    return(data.frame())
  }
  related_run_ids <- raw_data_admin_transcript_related_run_ids(con, pipeline_run_id)
  run_placeholders <- paste(rep("?", length(related_run_ids)), collapse = ", ")
  keys <- c("video_id", "talent_code", "subtitle_language", "subtitle_track_type",
            "source_scope", "pipeline_version")
  where <- paste(paste(keys, "IS NOT DISTINCT FROM ?"), collapse = " AND ")
  DBI::dbGetQuery(con, paste(
    "SELECT block_number, sentence_number, start_sec, end_sec, sentence_text,",
    "source_alignment_status, timestamps_approximate",
    "FROM text.subtitle_sentence_units WHERE pipeline_run_id IN (",
    run_placeholders, ") AND", where,
    "ORDER BY block_number, sentence_number, sentence_unit_key"
  ), params = c(
    as.list(related_run_ids),
    lapply(keys, function(key) track[[key]][[1]])
  ))
}
