# Lifecycle metric preparation helpers.

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
