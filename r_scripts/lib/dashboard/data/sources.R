# Data-source resolution and unified-database loading helpers.

dashboard_parse_optional_date <- function(x, label = "date") {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]]) || !nzchar(trimws(as.character(x[[1]])))) {
    return(as.Date(NA))
  }
  out <- suppressWarnings(as.Date(as.character(x[[1]])))
  if (is.na(out)) {
    stop("`", label, "` must be in YYYY-MM-DD format.", call. = FALSE)
  }
  out
}

dashboard_resolve_data_root <- function(data_source = c("datalake", "staging"), data_root = NULL) {
  data_source <- match.arg(tolower(data_source), c("datalake", "staging"))
  if (!is.null(data_root) && length(data_root) > 0 && !is.na(data_root[[1]]) &&
      nzchar(trimws(as.character(data_root[[1]])))) {
    return(normalizePath(trimws(as.character(data_root[[1]])), winslash = "/", mustWork = FALSE))
  }
  if (identical(data_source, "datalake")) {
    return(normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE))
  }
  normalizePath(get_staging_root(), winslash = "/", mustWork = FALSE)
}

dashboard_resolve_database_path <- function(database_path = NULL) {
  if (!is.null(database_path) && length(database_path) > 0 &&
      !is.na(database_path[[1]]) && nzchar(trimws(as.character(database_path[[1]])))) {
    path <- trimws(as.character(database_path[[1]]))
  } else {
    load_repo_env()
    path <- trimws(Sys.getenv("UNIFIED_CATALOG_DB_PATH", unset = ""))
    if (!nzchar(path)) {
      path <- file.path(
        get_datalake_root(),
        "Data_lakehouse",
        "talent_lakehouse.duckdb"
      )
    }
  }

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(path)) {
    stop("Unified DuckDB database not found: ", path, call. = FALSE)
  }
  path
}

dashboard_normalize_talent_key <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))
  gsub("[^a-z0-9]+", "", x)
}

dashboard_select_unified_talent <- function(con, talent) {
  talents <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT talent_code, talent_name, legacy_talent_id",
      "FROM catalog.talents",
      "WHERE active",
      "ORDER BY talent_code"
    )
  )
  if (nrow(talents) == 0) {
    stop("The unified database has no active talents.", call. = FALSE)
  }

  query_key <- dashboard_normalize_talent_key(talent[[1]])
  talent_keys <- apply(
    talents[c("talent_code", "talent_name", "legacy_talent_id")],
    1,
    dashboard_normalize_talent_key
  )
  exact <- apply(talent_keys, 2, function(keys) query_key %in% keys)
  matches <- talents[exact, , drop = FALSE]

  if (nrow(matches) == 0 && nzchar(query_key)) {
    fuzzy <- apply(
      talents[c("talent_code", "talent_name", "legacy_talent_id")],
      1,
      function(fields) {
        tokens <- unlist(strsplit(tolower(as.character(fields)), "[^a-z0-9]+"))
        tokens <- tokens[nzchar(tokens) & nchar(tokens) >= 3]
        any(startsWith(tokens, query_key) | startsWith(query_key, tokens))
      }
    )
    matches <- talents[fuzzy, , drop = FALSE]
  }

  if (nrow(matches) == 0) {
    stop("No active unified-database talent matched `", talent[[1]], "`.", call. = FALSE)
  }
  if (nrow(matches) > 1) {
    stop(
      "Talent query `", talent[[1]], "` matched multiple active talents: ",
      paste(matches$talent_name, collapse = ", "),
      call. = FALSE
    )
  }
  matches[1, , drop = FALSE]
}

dashboard_unified_talent_catalog <- function(database_path = NULL) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("duckdb", quietly = TRUE)) {
    stop("Packages `DBI` and `duckdb` are required for the unified database.", call. = FALSE)
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
      "SELECT",
      "  t.talent_code,",
      "  t.talent_name,",
      "  MIN(CAST(v.published_at AS DATE)) AS earliest_publish_date,",
      "  MAX(CAST(v.published_at AS DATE)) AS latest_publish_date",
      "FROM catalog.talents AS t",
      "LEFT JOIN catalog.videos AS v USING (talent_code)",
      "WHERE t.active",
      "GROUP BY t.talent_code, t.talent_name",
      "ORDER BY t.talent_name"
    )
  )
  if (nrow(catalog) == 0) {
    stop("The unified database has no active talents.", call. = FALSE)
  }

  catalog %>%
    dplyr::mutate(
      earliest_publish_date = as.Date(.data$earliest_publish_date),
      latest_publish_date = as.Date(.data$latest_publish_date)
    )
}

dashboard_load_unified_database <- function(database_path, talent) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Packages `DBI`, `duckdb`, and `jsonlite` are required for the unified database.",
      call. = FALSE
    )
  }

  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = database_path,
    read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  talent_row <- dashboard_select_unified_talent(con, talent)
  talent_code <- talent_row$talent_code[[1]]
  params <- list(talent_code)

  analytics <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  a.video_id AS \"Video ID\",",
      "  a.channel_id AS \"Channel ID\",",
      "  c.channel_name AS \"Channel Name\",",
      "  v.title AS \"Title\",",
      "  v.published_at AS \"Published At\",",
      "  a.snapshot_date AS date,",
      "  v.content_type AS \"Content Type\",",
      "  a.views,",
      "  a.estimated_minutes_watched AS estimatedMinutesWatched,",
      "  a.average_view_duration AS averageViewDuration,",
      "  a.average_view_percentage AS averageViewPercentage,",
      "  a.subscribers_gained AS subscribersGained,",
      "  a.subscribers_lost AS subscribersLost,",
      "  v.duration_seconds AS DurationSeconds,",
      "  v.duration_iso AS DurationISO",
      "FROM clean.video_analytics_snapshots AS a",
      "JOIN catalog.videos AS v USING (video_id, channel_id, talent_code)",
      "JOIN catalog.channels AS c USING (channel_id, talent_code)",
      "WHERE a.talent_code = ?",
      "ORDER BY a.snapshot_date, a.video_id"
    ),
    params = params
  )

  monetary <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  m.video_id AS \"Video ID\",",
      "  m.channel_id AS \"Channel ID\",",
      "  c.channel_name AS \"Channel Name\",",
      "  v.title AS \"Title\",",
      "  v.published_at AS \"Published At\",",
      "  m.snapshot_date AS date,",
      "  v.content_type AS \"Content Type\",",
      "  m.views_monetary_check,",
      "  m.estimated_revenue AS \"Estimated Revenue\",",
      "  m.cpm AS CPM,",
      "  v.duration_seconds AS DurationSeconds,",
      "  v.duration_iso AS DurationISO",
      "FROM clean.video_monetary_snapshots AS m",
      "JOIN catalog.videos AS v USING (video_id, channel_id, talent_code)",
      "JOIN catalog.channels AS c USING (channel_id, talent_code)",
      "WHERE m.talent_code = ?",
      "ORDER BY m.snapshot_date, m.video_id"
    ),
    params = params
  )

  demographics <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  d.video_id AS \"Video ID\",",
      "  d.channel_id AS \"Channel ID\",",
      "  c.channel_name AS \"Channel Name\",",
      "  d.snapshot_date AS date,",
      "  d.viewer_age AS \"Viewer Age\",",
      "  d.viewer_gender AS \"Viewer Gender\",",
      "  d.viewer_percentage AS \"Viewer Percentage\"",
      "FROM clean.video_demographics AS d",
      "JOIN catalog.channels AS c USING (channel_id, talent_code)",
      "WHERE d.talent_code = ?",
      "ORDER BY d.snapshot_date, d.video_id, d.viewer_age, d.viewer_gender"
    ),
    params = params
  )

  geography <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  g.video_id AS \"Video ID\",",
      "  g.channel_id AS \"Channel ID\",",
      "  c.channel_name AS \"Channel Name\",",
      "  g.snapshot_date AS date,",
      "  g.country AS Country,",
      "  g.viewer_percentage AS \"Viewer Percentage\",",
      "  g.estimated_views AS \"Est Views (calc)\",",
      "  g.estimated_minutes_watched AS estimatedMinutesWatched",
      "FROM clean.video_geography AS g",
      "JOIN catalog.channels AS c USING (channel_id, talent_code)",
      "WHERE g.talent_code = ?",
      "ORDER BY g.snapshot_date, g.video_id, g.country"
    ),
    params = params
  )

  titles <- DBI::dbGetQuery(
    con,
    paste(
      "WITH ranked AS (",
      "  SELECT *,",
      "    ROW_NUMBER() OVER (",
      "      PARTITION BY video_id",
      "      ORDER BY created_at DESC NULLS LAST, confidence DESC NULLS LAST",
      "    ) AS row_number",
      "  FROM classification.title_classification_results",
      "  WHERE talent_code = ?",
      ")",
      "SELECT",
      "  r.video_id AS \"Video ID\",",
      "  t.talent_name,",
      "  r.confidence,",
      "  v.title AS title_raw,",
      "  v.content_type,",
      "  v.published_at,",
      "  r.classification_json,",
      "  r.collaborative_energy,",
      "  r.community_milestones,",
      "  r.interactive_entertainment,",
      "  r.meme_viral,",
      "  r.monetization,",
      "  r.narrative_serialization,",
      "  r.performance_artistry,",
      "  r.personality_conversation",
      "FROM ranked AS r",
      "JOIN catalog.videos AS v USING (video_id, talent_code, channel_id)",
      "JOIN catalog.talents AS t USING (talent_code)",
      "WHERE r.row_number = 1",
      "ORDER BY r.video_id"
    ),
    params = params
  )

  classification_details <- lapply(titles$classification_json, function(value) {
    if (is.na(value) || !nzchar(trimws(value))) {
      return(list())
    }
    tryCatch(
      jsonlite::fromJSON(value, simplifyVector = TRUE),
      error = function(e) list()
    )
  })
  classification_value <- function(details, field) {
    value <- details[[field]]
    if (is.null(value) || length(value) == 0) NA_character_ else as.character(value[[1]])
  }
  titles$topic <- vapply(classification_details, classification_value, character(1), field = "topic")
  titles$tags <- vapply(
    classification_details,
    function(details) {
      tags <- details$tags
      if (is.null(tags) || length(tags) == 0) NA_character_ else paste(tags, collapse = ", ")
    },
    character(1)
  )
  titles$primary_reference <- vapply(
    classification_details,
    classification_value,
    character(1),
    field = "primary_reference"
  )
  titles$is_music <- vapply(
    classification_details,
    function(details) {
      value <- details$is_music
      if (is.null(value) || length(value) == 0) NA else as.logical(value[[1]])
    },
    logical(1)
  )
  titles <- titles %>%
    dplyr::select(-"classification_json")

  if (nrow(analytics) == 0) {
    stop("No analytics rows found for talent `", talent_row$talent_name[[1]], "`.", call. = FALSE)
  }

  latest_snapshot_date <- max(as.Date(analytics$date), na.rm = TRUE)
  latest_analytics <- analytics[as.Date(analytics$date) == latest_snapshot_date, , drop = FALSE]

  list(
    talent_code = talent_code,
    talent_name = talent_row$talent_name[[1]],
    talent_files = list(
      video_analytics = analytics,
      video_monetary = monetary,
      video_demographics = demographics,
      video_geography = geography
    ),
    latest_analytics = latest_analytics,
    title_classifications = titles
  )
}
