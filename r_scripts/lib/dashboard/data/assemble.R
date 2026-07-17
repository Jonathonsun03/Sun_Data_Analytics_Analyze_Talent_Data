# Creator-dashboard data assembly.

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
