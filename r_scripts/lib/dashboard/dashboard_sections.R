dashboard_datatable <- function(df, caption = NULL, page_length = 10, scroll_x = TRUE) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  DTSettings(
    df,
    class = "display stripe hover",
    scroll_x = scroll_x,
    options = list(pageLength = page_length),
    filter = "none"
  )
}

dashboard_overview_table <- function(dashboard_data) {
  dashboard_datatable(dashboard_data$overview, page_length = nrow(dashboard_data$overview), scroll_x = TRUE)
}

dashboard_overview_value_boxes <- function(dashboard_data) {
  # index.qmd can map this compact table into Quarto value boxes.
  dashboard_data$overview
}

dashboard_monthly_performance_plot <- function(dashboard_data, talent, value_mode = c("raw", "index")) {
  value_mode <- match.arg(value_mode)
  if (is.null(dashboard_data$monthly_performance)) {
    return(NULL)
  }
  performance_trends_over_time_plot(
    dashboard_data$monthly_performance,
    talent = talent,
    value_mode = value_mode
  )
}

dashboard_content_views_plot <- function(dashboard_data, talent) {
  analytics <- dashboard_data$source_data$analytics
  if (is.null(analytics) || nrow(analytics) == 0) {
    return(NULL)
  }
  views_content_type_comparison(analytics, talent)
}

dashboard_content_engagement_plot <- function(dashboard_data, talent) {
  analytics <- dashboard_data$source_data$analytics
  if (is.null(analytics) || nrow(analytics) == 0 || !("averageViewPercentage" %in% names(analytics))) {
    return(NULL)
  }
  engagement_distribution_content_type(
    analytics %>% dplyr::mutate(avg_view_prop = .data$averageViewPercentage / 100),
    talent,
    metric_col = "avg_view_prop",
    metric_label = "Average View %",
    y_as_percent = TRUE
  )
}

dashboard_weekday_table <- function(dashboard_data) {
  dashboard_datatable(dashboard_data$weekday_summary, page_length = 7, scroll_x = TRUE)
}

dashboard_weekday_plot <- function(dashboard_data, talent, as_share = FALSE) {
  if (is.null(dashboard_data$weekday_summary)) {
    return(NULL)
  }
  day_of_week_performance_plot(
    dashboard_data$weekday_summary,
    talent = talent,
    as_share = as_share
  )
}

dashboard_audience_plot <- function(dashboard_data, talent) {
  if (is.null(dashboard_data$audience_summary) || nrow(dashboard_data$audience_summary) == 0) {
    return(NULL)
  }
  audience_age_gender_trends_plot(
    plot_df = dashboard_data$audience_summary,
    talent = talent
  )
}

dashboard_top_videos_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(dashboard_data$top_videos, page_length = page_length, scroll_x = TRUE)
}

dashboard_topic_weekday_recommendation_table <- function(dashboard_data, content_type, page_length = 15) {
  recs <- dashboard_data$recommendations$topic_weekday[[content_type]]
  if (is.null(recs) || nrow(recs) == 0) {
    return(NULL)
  }
  dashboard_datatable(recs, page_length = page_length, scroll_x = TRUE)
}

dashboard_topic_weekday_heatmap <- function(dashboard_data, content_type, content_label = content_type) {
  topic_result <- dashboard_data$topic_weekday_summary[[content_type]]
  if (is.null(topic_result) || is.null(topic_result$topic_weekday) || nrow(topic_result$topic_weekday) == 0) {
    return(NULL)
  }
  topic_day_heatmap(topic_result$topic_weekday) +
    ggplot2::labs(title = paste("Topic-weekday performance:", content_label))
}

dashboard_lifecycle_recommendations_table <- function(dashboard_data) {
  # TODO: wire this after Bundle E artifact loading is extracted from bundle_e.Rmd.
  dashboard_datatable(dashboard_data$recommendations$lifecycle, page_length = 10, scroll_x = TRUE)
}
