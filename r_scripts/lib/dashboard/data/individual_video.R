# Permission-scoped individual-video catalog and history loaders.

dashboard_individual_video_catalog <- function(database_path, talent_code) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("duckdb", quietly = TRUE)) {
    stop("Packages `DBI` and `duckdb` are required for the unified database.", call. = FALSE)
  }
  if (length(talent_code) != 1 || is.na(talent_code) || !nzchar(talent_code)) {
    stop("A single talent code is required.", call. = FALSE)
  }

  database_path <- dashboard_resolve_database_path(database_path)
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = database_path,
    read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  catalog <- DBI::dbGetQuery(
    con,
    paste(
      "WITH coverage AS (",
      "  SELECT",
      "    talent_code, channel_id, video_id,",
      "    MIN(snapshot_date) AS first_snapshot_date,",
      "    MAX(snapshot_date) AS latest_snapshot_date,",
      "    COUNT(DISTINCT snapshot_date) AS snapshot_count",
      "  FROM clean.video_analytics_snapshots",
      "  WHERE talent_code = ?",
      "  GROUP BY talent_code, channel_id, video_id",
      "), latest AS (",
      "  SELECT",
      "    talent_code, channel_id, video_id, views AS latest_views,",
      "    ROW_NUMBER() OVER (",
      "      PARTITION BY talent_code, channel_id, video_id",
      "      ORDER BY snapshot_date DESC",
      "    ) AS row_number",
      "  FROM clean.video_analytics_snapshots",
      "  WHERE talent_code = ?",
      ")",
      "SELECT",
      "  v.video_id,",
      "  v.title,",
      "  v.published_at,",
      "  v.content_type,",
      "  v.duration_seconds,",
      "  c.channel_name,",
      "  coverage.first_snapshot_date,",
      "  coverage.latest_snapshot_date,",
      "  coverage.snapshot_count,",
      "  latest.latest_views",
      "FROM catalog.videos AS v",
      "JOIN catalog.channels AS c",
      "  USING (talent_code, channel_id)",
      "LEFT JOIN coverage",
      "  USING (talent_code, channel_id, video_id)",
      "LEFT JOIN latest",
      "  ON latest.talent_code = v.talent_code",
      " AND latest.channel_id = v.channel_id",
      " AND latest.video_id = v.video_id",
      " AND latest.row_number = 1",
      "WHERE v.talent_code = ?",
      "ORDER BY CASE WHEN coverage.snapshot_count IS NULL THEN 1 ELSE 0 END,",
      "  v.published_at DESC NULLS LAST, v.video_id"
    ),
    params = list(talent_code, talent_code, talent_code)
  )

  catalog %>%
    dplyr::mutate(
      published_at = as.POSIXct(.data$published_at, tz = "UTC"),
      first_snapshot_date = as.Date(.data$first_snapshot_date),
      latest_snapshot_date = as.Date(.data$latest_snapshot_date),
      snapshot_count = dplyr::coalesce(as.integer(.data$snapshot_count), 0L),
      latest_views = as.numeric(.data$latest_views)
    )
}

dashboard_individual_video_choices <- function(video_catalog) {
  if (is.null(video_catalog) || nrow(video_catalog) == 0) {
    return(character())
  }

  title <- trimws(as.character(video_catalog$title))
  title[is.na(title) | !nzchar(title)] <- "Untitled video"
  stats::setNames(as.character(video_catalog$video_id), title)
}

dashboard_load_individual_video_history <- function(
  database_path,
  talent_code,
  video_id
) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("duckdb", quietly = TRUE)) {
    stop("Packages `DBI` and `duckdb` are required for the unified database.", call. = FALSE)
  }
  if (length(talent_code) != 1 || is.na(talent_code) || !nzchar(talent_code) ||
      length(video_id) != 1 || is.na(video_id) || !nzchar(video_id)) {
    stop("A single talent code and video ID are required.", call. = FALSE)
  }

  database_path <- dashboard_resolve_database_path(database_path)
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = database_path,
    read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  history <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  a.talent_code,",
      "  a.video_id,",
      "  a.channel_id,",
      "  c.channel_name,",
      "  v.title,",
      "  v.published_at,",
      "  v.content_type,",
      "  v.duration_seconds,",
      "  a.snapshot_date,",
      "  a.views,",
      "  a.estimated_minutes_watched,",
      "  a.average_view_duration,",
      "  a.average_view_percentage,",
      "  a.subscribers_gained,",
      "  a.subscribers_lost,",
      "  m.estimated_revenue,",
      "  m.cpm",
      "FROM clean.video_analytics_snapshots AS a",
      "JOIN catalog.videos AS v",
      "  USING (talent_code, channel_id, video_id)",
      "JOIN catalog.channels AS c",
      "  USING (talent_code, channel_id)",
      "LEFT JOIN clean.video_monetary_snapshots AS m",
      "  USING (talent_code, channel_id, video_id, snapshot_date)",
      "WHERE a.talent_code = ? AND a.video_id = ?",
      "ORDER BY a.snapshot_date"
    ),
    params = list(talent_code, video_id)
  )

  history %>%
    dplyr::mutate(
      published_at = as.POSIXct(.data$published_at, tz = "UTC"),
      snapshot_date = as.Date(.data$snapshot_date)
    )
}
