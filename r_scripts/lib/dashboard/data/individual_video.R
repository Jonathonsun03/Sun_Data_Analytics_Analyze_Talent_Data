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

dashboard_individual_video_timecode_seconds <- function(x) {
  x <- trimws(as.character(x))
  vapply(x, function(value) {
    if (is.na(value) || !nzchar(value)) {
      return(NA_real_)
    }
    parts <- suppressWarnings(as.numeric(strsplit(value, ":", fixed = TRUE)[[1]]))
    if (any(!is.finite(parts))) {
      return(NA_real_)
    }
    if (length(parts) == 3) {
      return(parts[[1]] * 3600 + parts[[2]] * 60 + parts[[3]])
    }
    if (length(parts) == 2) {
      return(parts[[1]] * 60 + parts[[2]])
    }
    if (length(parts) == 1) {
      return(parts[[1]])
    }
    NA_real_
  }, numeric(1), USE.NAMES = FALSE)
}

dashboard_load_individual_video_transcript <- function(
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

  subtitles <- data.frame()
  subtitle_status <- "missing"
  if (DBI::dbExistsTable(con, DBI::Id(schema = "text", table = "subtitle_sentence_units"))) {
    sentence_columns <- DBI::dbListFields(
      con,
      DBI::Id(schema = "text", table = "subtitle_sentence_units")
    )
    sentence_end_select <- if ("end_sec" %in% sentence_columns) {
      "subtitle.end_sec AS end_seconds,"
    } else {
      "CAST(NULL AS DOUBLE) AS end_seconds,"
    }
    subtitles <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT channel.channel_name AS speaker,",
        "  subtitle.start_sec AS timestamp_raw,",
        sentence_end_select,
        "  subtitle.sentence_text AS dialogue,",
        "  ROW_NUMBER() OVER (ORDER BY subtitle.block_number, subtitle.sentence_number) AS source_order",
        "FROM text.subtitle_sentence_units AS subtitle",
        "JOIN catalog.videos AS video",
        "  ON video.talent_code = subtitle.talent_code",
        " AND video.channel_id = subtitle.channel_id",
        " AND video.video_id = subtitle.video_id",
        "JOIN catalog.channels AS channel",
        "  ON channel.talent_code = subtitle.talent_code",
        " AND channel.channel_id = subtitle.channel_id",
        "WHERE subtitle.talent_code = ? AND subtitle.video_id = ?",
        "  AND subtitle.source_scope = 'full_track'",
        "  AND TRIM(subtitle.sentence_text) <> ''",
        "ORDER BY source_order"
      ),
      params = list(talent_code, video_id)
    )
    if (nrow(subtitles) > 0) subtitle_status <- "cleaned"
  }

  if (nrow(subtitles) == 0) {
    subtitle_columns <- DBI::dbListFields(
      con,
      DBI::Id(schema = "text", table = "subtitle_units")
    )
    subtitle_end_select <- if ("subtitle_end" %in% subtitle_columns) {
      "subtitle.subtitle_end AS timestamp_end_raw,"
    } else {
      "CAST(NULL AS VARCHAR) AS timestamp_end_raw,"
    }
    subtitles <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT",
        "  channel.channel_name AS speaker,",
        "  subtitle.subtitle_start AS timestamp_raw,",
        subtitle_end_select,
        "  subtitle.subtitle_text AS dialogue,",
        "  subtitle.sequence_number AS source_order",
        "FROM text.subtitle_units AS subtitle",
        "JOIN catalog.videos AS video",
        "  ON video.talent_code = subtitle.talent_code",
        " AND video.channel_id = subtitle.channel_id",
        " AND video.video_id = subtitle.video_id",
        "JOIN catalog.channels AS channel",
        "  ON channel.talent_code = subtitle.talent_code",
        " AND channel.channel_id = subtitle.channel_id",
        "WHERE subtitle.talent_code = ? AND subtitle.video_id = ?",
        "ORDER BY subtitle.sequence_number"
      ),
      params = list(talent_code, video_id)
    )

    if (any(!is.na(subtitles$dialogue) & nzchar(trimws(subtitles$dialogue)))) {
      subtitle_status <- "raw"
    }
  }

  chat <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  chat.username AS speaker,",
      "  chat.time_in_seconds AS seconds,",
      "  chat.message AS dialogue,",
      "  ROW_NUMBER() OVER (",
      "    ORDER BY chat.time_in_seconds, chat.message_timestamp, chat.message_key",
      "  ) AS source_order",
      "FROM text.chat_messages AS chat",
      "JOIN catalog.videos AS video",
      "  ON video.talent_code = chat.talent_code",
      " AND video.channel_id = chat.channel_id",
      " AND video.video_id = chat.video_id",
      "WHERE chat.talent_code = ? AND chat.video_id = ?",
      "ORDER BY source_order"
    ),
    params = list(talent_code, video_id)
  )

  subtitles <- subtitles %>%
    dplyr::transmute(
      source = "subtitle",
      speaker = as.character(.data$speaker),
      seconds = if (subtitle_status == "cleaned") {
        as.numeric(.data$timestamp_raw)
      } else {
        dashboard_individual_video_timecode_seconds(.data$timestamp_raw)
      },
      end_seconds = if (subtitle_status == "cleaned") {
        suppressWarnings(as.numeric(.data$end_seconds))
      } else {
        dashboard_individual_video_timecode_seconds(.data$timestamp_end_raw)
      },
      dialogue = as.character(.data$dialogue),
      source_order = as.numeric(.data$source_order)
    )
  chat <- chat %>%
    dplyr::transmute(
      source = "chat",
      speaker = as.character(.data$speaker),
      seconds = suppressWarnings(as.numeric(.data$seconds)),
      end_seconds = NA_real_,
      dialogue = as.character(.data$dialogue),
      source_order = as.numeric(.data$source_order)
    )

  transcript <- dplyr::bind_rows(subtitles, chat) %>%
    dplyr::mutate(
      speaker = dplyr::case_when(
        !is.na(.data$speaker) & nzchar(trimws(.data$speaker)) ~ .data$speaker,
        .data$source == "subtitle" ~ "Streamer",
        TRUE ~ "Chat viewer"
      ),
      source_rank = dplyr::if_else(.data$source == "subtitle", 1L, 2L)
    ) %>%
    dplyr::filter(!is.na(.data$dialogue), nzchar(trimws(.data$dialogue))) %>%
    dplyr::arrange(
      is.na(.data$seconds),
      .data$seconds,
      .data$source_rank,
      .data$source_order
    ) %>%
    dplyr::select(dplyr::all_of(c(
      "speaker", "seconds", "end_seconds", "dialogue", "source", "source_order"
    )))
  attr(transcript, "subtitle_status") <- subtitle_status
  transcript
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
