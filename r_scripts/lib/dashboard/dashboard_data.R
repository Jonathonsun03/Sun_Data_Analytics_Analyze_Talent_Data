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

dashboard_apply_publish_window <- function(df, start_date = NULL, end_date = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  start_date <- dashboard_parse_optional_date(start_date, "start_date")
  end_date <- dashboard_parse_optional_date(end_date, "end_date")
  if (!is.na(start_date) && !is.na(end_date) && start_date > end_date) {
    stop("`start_date` cannot be after `end_date`.", call. = FALSE)
  }
  if (is.na(start_date) && is.na(end_date)) {
    return(df)
  }

  date_col <- bundle_a_optional_col(
    df,
    candidates = c("publish_date", "Published At", "date"),
    label = "dashboard publish date column"
  )
  if (is.null(date_col)) {
    return(df)
  }

  out <- df %>%
    dplyr::mutate(.dashboard_publish_date = bundle_a_as_date(.data[[date_col]])) %>%
    dplyr::filter(!is.na(.data$.dashboard_publish_date))
  if (!is.na(start_date)) {
    out <- out %>% dplyr::filter(.data$.dashboard_publish_date >= start_date)
  }
  if (!is.na(end_date)) {
    out <- out %>% dplyr::filter(.data$.dashboard_publish_date <= end_date)
  }
  out %>% dplyr::select(-.data$.dashboard_publish_date)
}

dashboard_canonical_content_types <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(c("live", "video", "short"))
  }
  x <- tolower(trimws(as.character(unlist(x))))
  x <- x[nzchar(x)]
  x <- dplyr::recode(x, videos = "video", shorts = "short", all = "all", .default = x)
  if ("all" %in% x) {
    return(c("live", "video", "short"))
  }
  unique(x)
}

dashboard_try <- function(expr, todo = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      if (!is.null(todo)) {
        attr(todo, "dashboard_error") <- conditionMessage(e)
      }
      todo
    }
  )
}

dashboard_build_overview <- function(analytics, monetary, demo, talent, data_source, data_root) {
  video_id_col <- bundle_a_optional_col(analytics, candidates = c("Video ID", "video_id"))
  date_col <- bundle_a_optional_col(analytics, candidates = c("publish_date", "Published At", "date"))
  publish_dates <- if (is.null(date_col)) as.Date(character()) else bundle_a_as_date(analytics[[date_col]])
  source_location_label <- if (identical(data_source, "unified_db")) {
    "Database path"
  } else {
    "Data root"
  }

  tibble::tibble(
    metric = c(
      "Talent",
      "Data source",
      source_location_label,
      "Analytics rows",
      "Monetary rows",
      "Audience rows",
      "Unique videos",
      "Publish date range"
    ),
    value = c(
      talent,
      data_source,
      data_root,
      scales::comma(nrow(analytics)),
      scales::comma(nrow(monetary)),
      scales::comma(nrow(demo)),
      if (is.null(video_id_col)) "N/A" else scales::comma(dplyr::n_distinct(analytics[[video_id_col]])),
      if (length(stats::na.omit(publish_dates)) == 0) {
        "N/A"
      } else {
        paste0(format(min(publish_dates, na.rm = TRUE), "%Y-%m-%d"), " to ", format(max(publish_dates, na.rm = TRUE), "%Y-%m-%d"))
      }
    )
  )
}

dashboard_build_top_videos <- function(analytics, monetary = NULL, top_n = 20) {
  if (is.null(analytics) || nrow(analytics) == 0 || !("views" %in% names(analytics))) {
    return(NULL)
  }

  revenue_by_video <- if (!is.null(monetary) && nrow(monetary) > 0 &&
      all(c("Video ID", "Estimated Revenue") %in% names(monetary))) {
    monetary %>%
      dplyr::group_by(.data$`Video ID`) %>%
      dplyr::summarise(`Estimated Revenue` = sum(.data$`Estimated Revenue`, na.rm = TRUE), .groups = "drop")
  } else {
    NULL
  }

  out <- analytics %>%
    dplyr::mutate(views = suppressWarnings(as.numeric(.data$views))) %>%
    dplyr::arrange(dplyr::desc(.data$views)) %>%
    dplyr::select(dplyr::any_of(c("Video ID", "Title", "publish_date", "Content Type", "topic", "views"))) %>%
    dplyr::slice_head(n = top_n)

  if (!is.null(revenue_by_video) && "Video ID" %in% names(out)) {
    out <- out %>% dplyr::left_join(revenue_by_video, by = "Video ID")
  }
  out
}

dashboard_build_topic_weekday <- function(content_df, reference_day = "Monday", min_topic_n = 3) {
  topic_df <- content_df %>%
    dow_prep(reference_day = reference_day) %>%
    topic_clean(min_topic_n = min_topic_n)

  topic_weekday <- topic_weekday_summary(topic_df)
  models <- fit_topic_day_models(topic_df)
  recommendations <- model_recommend_weekday_by_topic(topic_df, models)

  list(
    topic_df = topic_df,
    topic_weekday = topic_weekday,
    observed_best = observed_best_weekday_by_topic(topic_weekday, min_streams = 2),
    model_check = model_comparison(models),
    recommendations = recommendations
  )
}

dashboard_filter_bundle_e_content_types <- function(panel_df, content_types) {
  if (is.null(panel_df) || nrow(panel_df) == 0 || !("Content Type" %in% names(panel_df))) {
    return(panel_df)
  }

  content_types <- dashboard_canonical_content_types(content_types)
  panel_df %>%
    dplyr::filter(tolower(trimws(as.character(.data$`Content Type`))) %in% content_types)
}

dashboard_build_lifecycle_data <- function(
  talent_files,
  title_classifications,
  talent_folder,
  start_date = NULL,
  end_date = NULL,
  content_types = c("live", "video", "short")
) {
  panel <- build_bundle_e_long_panel(
    files = talent_files,
    titles = title_classifications,
    talent = talent_folder
  )
  panel <- dashboard_apply_publish_window(panel, start_date, end_date)
  panel <- dashboard_filter_bundle_e_content_types(panel, content_types)

  video_summary <- build_bundle_e_video_summary(panel) %>%
    add_bundle_e_sleeper_flag()

  detail_tables <- build_bundle_e_video_type_detail_tables(video_summary)
  type_curves <- purrr::map(
    c(short = "short", video = "video", live = "live"),
    ~ build_bundle_e_type_age_curve_comparison(
      panel_df = panel,
      comparison_df = detail_tables$video_type_top_performing_videos,
      newest_df = detail_tables$video_type_newest_five_videos,
      content_type = .x
    )
  )

  list(
    panel = panel,
    video_summary = video_summary,
    library_growth_snapshot = build_bundle_e_library_growth_snapshot(panel),
    back_catalog_contribution = build_bundle_e_back_catalog_contribution(panel),
    publish_cohort_performance = build_bundle_e_publish_cohort_performance(video_summary),
    publish_cohort_performance_by_type = build_bundle_e_publish_cohort_performance_by_type(video_summary),
    video_type_longevity = build_bundle_e_attribute_summary(video_summary, "Content Type"),
    detail_tables = detail_tables,
    type_curves = type_curves,
    leaders = build_bundle_e_leaders(video_summary)
  )
}

dashboard_iso3166_lookup <- function() {
  iso_path <- "/usr/share/iso-codes/json/iso_3166-1.json"
  if (file.exists(iso_path) && requireNamespace("jsonlite", quietly = TRUE)) {
    iso <- jsonlite::fromJSON(iso_path)[["3166-1"]]
    return(iso %>%
      dplyr::transmute(
        country_code = toupper(.data$alpha_2),
        country_iso3 = toupper(.data$alpha_3),
        country_name = .data$name
      ))
  }

  # Fallback for common YouTube geography codes if the system ISO table is unavailable.
  tibble::tribble(
    ~country_code, ~country_iso3, ~country_name,
    "AR", "ARG", "Argentina",
    "AU", "AUS", "Australia",
    "BR", "BRA", "Brazil",
    "CA", "CAN", "Canada",
    "DE", "DEU", "Germany",
    "ES", "ESP", "Spain",
    "FR", "FRA", "France",
    "GB", "GBR", "United Kingdom",
    "ID", "IDN", "Indonesia",
    "IN", "IND", "India",
    "IT", "ITA", "Italy",
    "JP", "JPN", "Japan",
    "KR", "KOR", "Korea, Republic of",
    "MX", "MEX", "Mexico",
    "MY", "MYS", "Malaysia",
    "NL", "NLD", "Netherlands",
    "PH", "PHL", "Philippines",
    "SE", "SWE", "Sweden",
    "SG", "SGP", "Singapore",
    "TH", "THA", "Thailand",
    "TW", "TWN", "Taiwan, Province of China",
    "UA", "UKR", "Ukraine",
    "US", "USA", "United States",
    "VN", "VNM", "Viet Nam",
    "ZA", "ZAF", "South Africa"
  )
}

dashboard_select_geography_metric <- function(geo_df) {
  metric_candidates <- c(
    "views",
    "Views",
    "Est Views (calc)",
    "Estimated Views",
    "estimated_views",
    "Viewer Percentage",
    "viewer_percentage",
    "Audience Share",
    "audience_share"
  )
  metric_col <- metric_candidates[metric_candidates %in% names(geo_df)][1]
  if (is.na(metric_col)) {
    return(NULL)
  }

  metric_label <- dplyr::case_when(
    metric_col %in% c("views", "Views") ~ "Views",
    metric_col %in% c("Est Views (calc)", "Estimated Views", "estimated_views") ~ "Estimated Cumulative Views",
    metric_col %in% c("Viewer Percentage", "viewer_percentage") ~ "Viewer Percentage",
    metric_col %in% c("Audience Share", "audience_share") ~ "Audience Share",
    TRUE ~ metric_col
  )
  metric_agg <- if (metric_label %in% c("Viewer Percentage", "Audience Share")) "mean" else "sum"

  list(column = metric_col, label = metric_label, aggregation = metric_agg)
}

dashboard_build_audience_geography <- function(geo_df) {
  if (is.null(geo_df) || nrow(geo_df) == 0 || !("Country" %in% names(geo_df))) {
    return(NULL)
  }

  date_col <- c("snapshot_date", "date", "Date", "Report Date", "report_date")
  date_col <- date_col[date_col %in% names(geo_df)][1]
  if (is.na(date_col)) {
    return(NULL)
  }

  metric <- dashboard_select_geography_metric(geo_df)
  if (is.null(metric)) {
    return(NULL)
  }

  cleaned <- geo_df %>%
    dplyr::transmute(
      snapshot_date = suppressWarnings(as.Date(.data[[date_col]])),
      country_code = toupper(trimws(as.character(.data$Country))),
      metric_value = suppressWarnings(as.numeric(.data[[metric$column]]))
    ) %>%
    dplyr::mutate(
      country_code = dplyr::na_if(.data$country_code, ""),
      country_code = dplyr::na_if(.data$country_code, "(NO DATA)")
    ) %>%
    dplyr::filter(!is.na(.data$snapshot_date), !is.na(.data$country_code), !is.na(.data$metric_value))

  if (nrow(cleaned) == 0) {
    return(NULL)
  }

  grouped <- cleaned %>%
    dplyr::group_by(.data$snapshot_date, .data$country_code)

  summary_df <- if (identical(metric$aggregation, "mean")) {
    grouped %>% dplyr::summarise(metric_value = mean(.data$metric_value, na.rm = TRUE), .groups = "drop")
  } else {
    grouped %>% dplyr::summarise(metric_value = sum(.data$metric_value, na.rm = TRUE), .groups = "drop")
  }

  summary_df <- summary_df %>%
    dplyr::left_join(dashboard_iso3166_lookup(), by = "country_code") %>%
    dplyr::mutate(
      snapshot_date_label = format(.data$snapshot_date, "%Y-%m-%d"),
      metric_label = metric$label,
      metric_display = if (metric$label %in% c("Viewer Percentage", "Audience Share")) {
        scales::percent(.data$metric_value, accuracy = 0.1)
      } else {
        scales::comma(round(.data$metric_value))
      }
    ) %>%
    dplyr::arrange(.data$snapshot_date, dplyr::desc(.data$metric_value))

  unmatched_codes <- summary_df %>%
    dplyr::filter(is.na(.data$country_iso3)) %>%
    dplyr::distinct(.data$country_code) %>%
    dplyr::pull("country_code")

  attr(summary_df, "metric_label") <- metric$label
  attr(summary_df, "metric_source_column") <- metric$column
  attr(summary_df, "metric_aggregation") <- metric$aggregation
  attr(summary_df, "unmatched_country_count") <- length(unmatched_codes)
  attr(summary_df, "unmatched_country_codes") <- unmatched_codes
  summary_df
}

build_creator_dashboard_data <- function(
  talent,
  data_source = c("unified_db", "datalake", "staging"),
  data_root = NULL,
  database_path = NULL,
  start_date = NULL,
  end_date = NULL,
  content_types = c("live", "video", "short"),
  reference_day = "Monday",
  min_topic_n = 3,
  top_n_videos = 20
) {
  data_source <- match.arg(tolower(data_source), c("unified_db", "datalake", "staging"))
  if (identical(data_source, "unified_db")) {
    database_path <- dashboard_resolve_database_path(database_path)
    unified_data <- dashboard_load_unified_database(database_path, talent)
    data_root <- database_path
    talent_path <- NA_character_
    talent_folder <- unified_data$talent_name
    talent_files <- unified_data$talent_files
    latest_analytics <- unified_data$latest_analytics
    title_classifications <- unified_data$title_classifications
  } else {
    data_root <- dashboard_resolve_data_root(data_source = data_source, data_root = data_root)
    talent_path <- select_talent(talent, root = data_root)
    talent_folder <- basename(talent_path)
    talent_files <- TalentFiles(talent_path)
    latest_analytics_path <- latest_talent_snapshot_path(
      talent_path,
      snapshot_type = "video_analytics"
    )
    latest_analytics <- .get_type_data(
      TalentFiles(list(latest_analytics_path)),
      type = "video_analytics"
    )
    title_classifications <- load_title_classifications(talent = talent_folder)
  }

  dashboard_files <- .normalize_talent_files(talent_files)
  dashboard_files$video_analytics <- latest_analytics

  prepared <- video_preps_with_titles(
    files = dashboard_files,
    titles = title_classifications,
    talent = talent_folder,
    dedupe = TRUE,
    key_cols = "Video ID",
    sort_cols = c("confidence", "published_at", "Published At"),
    # Match Bundle A/B dashboard-facing imports: dedupe video-level analytics
    # and monetary rows, but preserve demographic segment rows for audience plots.
    dedupe_sets = c("analytics", "monetary"),
    prep_sets = c("analytics", "monetary", "demo"),
    title_sets = c("analytics", "monetary"),
    content_type_sets = c("analytics", "monetary")
  )

  analytics <- dashboard_apply_publish_window(prepared$analytics, start_date, end_date)
  monetary <- dashboard_apply_publish_window(prepared$monetary, start_date, end_date)
  demo <- dashboard_apply_publish_window(prepared$demo, start_date, end_date)
  geo <- dashboard_try(
    dashboard_apply_publish_window(.get_type_data(talent_files, "video_geography"), start_date, end_date),
    NULL
  )

  content_types <- dashboard_canonical_content_types(content_types)
  stream_video_level <- analytics
  if (!is.null(monetary) && nrow(monetary) > 0 && "Video ID" %in% names(stream_video_level)) {
    stream_video_level <- stream_video_level %>%
      dplyr::select(-dplyr::any_of(c("Estimated Revenue", "CPM"))) %>%
      dplyr::left_join(
        monetary %>% dplyr::select(dplyr::all_of("Video ID"), dplyr::any_of(c("Estimated Revenue", "CPM"))),
        by = "Video ID"
      )
  }
  stream_video_level <- stream_video_level %>% dplyr::mutate(talent_name = talent_folder)

  content <- if ("Content Type" %in% names(stream_video_level)) {
    available <- sort(unique(tolower(trimws(as.character(stream_video_level$`Content Type`)))))
    keep_types <- intersect(content_types, available)
    purrr::map(
      keep_types,
      ~ stream_video_level %>%
        dplyr::filter(tolower(trimws(as.character(.data$`Content Type`))) == .x)
    ) %>%
      rlang::set_names(keep_types)
  } else {
    list()
  }

  weekday_summary <- dashboard_try(day_of_week_performance_prep(analytics, monetary), NULL)
  topic_weekday_summary <- purrr::map(
    content,
    ~ dashboard_try(
      dashboard_build_topic_weekday(.x, reference_day = reference_day, min_topic_n = min_topic_n),
      NULL
    )
  )
  lifecycle <- dashboard_try(
    dashboard_build_lifecycle_data(
      talent_files = talent_files,
      title_classifications = title_classifications,
      talent_folder = talent_folder,
      start_date = start_date,
      end_date = end_date,
      content_types = content_types
    ),
    NULL
  )

  out <- list(
    overview = dashboard_build_overview(analytics, monetary, demo, talent_folder, data_source, data_root),
    monthly_performance = dashboard_try(
      performance_trends_over_time_prep(analytics, monetary, freq = "month"),
      NULL
    ),
    content_summary = list(
      total_views = dashboard_try(total_views_content_type_prep(analytics), NULL),
      average_views = dashboard_try(average_views_content_type_prep(analytics), NULL),
      engagement = dashboard_try(
        engagement_summary_content_type_prep(
          analytics %>% dplyr::mutate(avg_view_prop = .data$averageViewPercentage / 100),
          metric_col = "avg_view_prop"
        ),
        NULL
      )
    ),
    content_strategy = list(
      collab_summary = dashboard_try(
        collaboration_effectiveness_prep(analytics, monetary),
        NULL
      ),
      topic_summary = dashboard_try(
        topic_performance_prep(analytics, monetary, top_n = 8),
        NULL
      ),
      tag_summary = dashboard_try(
        tag_performance_prep(
          analytics,
          monetary,
          top_n = 15,
          min_videos = 2
        ),
        NULL
      )
    ),
    weekday_summary = weekday_summary,
    weekend_weekday_distribution = dashboard_try(
      weekend_vs_weekday_distribution_prep(analytics, monetary),
      NULL
    ),
    topic_weekday_summary = topic_weekday_summary,
    lifecycle = lifecycle,
    audience_summary = dashboard_try(audience_age_gender_trends_prep(demo_df = demo, freq = "month"), NULL),
    audience_geography = dashboard_try(dashboard_build_audience_geography(geo), NULL),
    top_videos = dashboard_build_top_videos(analytics, monetary, top_n = top_n_videos),
    recommendations = list(
      weekday = if (is.null(weekday_summary) || nrow(weekday_summary) == 0) {
        NULL
      } else {
        rank_col <- if ("MedianViewsPerVideo" %in% names(weekday_summary)) {
          "MedianViewsPerVideo"
        } else if ("AverageViewsPerVideo" %in% names(weekday_summary)) {
          "AverageViewsPerVideo"
        } else {
          NULL
        }
        if (is.null(rank_col)) {
          weekday_summary
        } else {
        weekday_summary %>%
          dplyr::arrange(dplyr::desc(.data[[rank_col]]))
        }
      },
      topic_weekday = purrr::map(topic_weekday_summary, "recommendations"),
      lifecycle = if (is.null(lifecycle)) NULL else lifecycle$leaders
    ),
    source_data = list(
      talent_path = talent_path,
      analytics = analytics,
      monetary = monetary,
      demo = demo,
      geo = geo,
      content = content,
      title_classifications = title_classifications
    )
  )
  out$recommendations$story <- dashboard_build_recommendations(out)
  out
}
