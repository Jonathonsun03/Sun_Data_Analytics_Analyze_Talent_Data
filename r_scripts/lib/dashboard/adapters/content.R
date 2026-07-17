# Content-strategy plot and table adapters.

dashboard_revenue_content_type_plot <- function(dashboard_data, talent) {
  monetary <- dashboard_data$source_data$monetary
  if (is.null(monetary) || nrow(monetary) == 0 || !("Estimated Revenue" %in% names(monetary))) {
    return(NULL)
  }
  plot_df <- total_metric_content_type_prep(
    monetary,
    metric_col = "Estimated Revenue",
    window_months = 1
  )
  dashboard_ggplotly(total_metric_content_type_plot(
    plot_df,
    talent = talent,
    metric_label = "Revenue",
    subtitle_text = NULL,
    x_axis_label = "Month",
    bar_position = "stack",
    show_counts = TRUE,
    unique_bar_colors = TRUE
  ))
}

dashboard_content_engagement_plot <- function(dashboard_data, talent) {
  analytics <- dashboard_data$source_data$analytics
  if (is.null(analytics) || nrow(analytics) == 0 || !("averageViewPercentage" %in% names(analytics))) {
    return(NULL)
  }
  dashboard_ggplotly(engagement_distribution_content_type(
    analytics %>% dplyr::mutate(avg_view_prop = .data$averageViewPercentage / 100),
    talent,
    metric_col = "avg_view_prop",
    metric_label = "Average View %",
    y_as_percent = TRUE
  ))
}

dashboard_topic_performance_plot <- function(dashboard_data, talent, as_share = FALSE) {
  topic_summary <- dashboard_data$content_strategy$topic_summary
  if (is.null(topic_summary) || nrow(topic_summary) == 0) {
    return(NULL)
  }
  dashboard_ggplotly(topic_performance_plot(
    topic_summary,
    talent = talent,
    as_share = as_share
  ))
}

dashboard_topic_performance_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(
    dashboard_data$content_strategy$topic_summary,
    page_length = page_length,
    scroll_x = TRUE
  )
}

dashboard_tag_performance_plot <- function(dashboard_data, talent, as_share = FALSE) {
  tag_summary <- dashboard_data$content_strategy$tag_summary
  if (is.null(tag_summary) || nrow(tag_summary) == 0) {
    return(NULL)
  }
  dashboard_ggplotly(tag_performance_plot(
    tag_summary,
    talent = talent,
    as_share = as_share
  ))
}

dashboard_tag_performance_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(
    dashboard_data$content_strategy$tag_summary,
    page_length = page_length,
    scroll_x = TRUE
  )
}


dashboard_collaboration_performance_compact_plot <- function(dashboard_data) {
  collaboration_performance_plotly(dashboard_data$content_strategy$collab_summary)
}
