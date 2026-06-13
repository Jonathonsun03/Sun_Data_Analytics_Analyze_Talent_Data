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

  tibble::tibble(
    metric = c(
      "Talent",
      "Data source",
      "Data root",
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

build_creator_dashboard_data <- function(
  talent,
  data_source = c("datalake", "staging"),
  data_root = NULL,
  start_date = NULL,
  end_date = NULL,
  content_types = c("live", "video", "short"),
  reference_day = "Monday",
  min_topic_n = 3,
  top_n_videos = 20
) {
  data_source <- match.arg(tolower(data_source), c("datalake", "staging"))
  data_root <- dashboard_resolve_data_root(data_source = data_source, data_root = data_root)
  talent_path <- select_talent(talent, root = data_root)
  talent_folder <- basename(talent_path)

  talent_files <- TalentFiles(talent_path)
  title_classifications <- load_title_classifications(talent = talent_folder)
  prepared <- video_preps_with_titles(
    files = talent_files,
    titles = title_classifications,
    talent = talent_folder,
    dedupe = TRUE,
    key_cols = "Video ID",
    sort_cols = c("confidence", "published_at", "Published At")
  )

  analytics <- dashboard_apply_publish_window(prepared$analytics, start_date, end_date)
  monetary <- dashboard_apply_publish_window(prepared$monetary, start_date, end_date)
  demo <- dashboard_apply_publish_window(prepared$demo, start_date, end_date)

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

  list(
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
    weekday_summary = weekday_summary,
    topic_weekday_summary = topic_weekday_summary,
    audience_summary = dashboard_try(audience_age_gender_trends_prep(demo_df = demo, freq = "month"), NULL),
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
      lifecycle = NULL # TODO: load Bundle E artifact CSVs through a reusable artifact loader.
    ),
    source_data = list(
      talent_path = talent_path,
      analytics = analytics,
      monetary = monetary,
      demo = demo,
      content = content,
      title_classifications = title_classifications
    )
  )
}
