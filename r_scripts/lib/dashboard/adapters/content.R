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

dashboard_collaboration_performance_plot <- function(dashboard_data, talent, as_share = FALSE) {
  collab_summary <- dashboard_data$content_strategy$collab_summary
  if (is.null(collab_summary) || nrow(collab_summary) == 0) {
    return(NULL)
  }
  dashboard_ggplotly(collaboration_effectiveness_plot(
    collab_summary,
    talent = talent,
    as_share = as_share
  ))
}

dashboard_collaboration_performance_compact_plot <- function(dashboard_data) {
  collab_summary <- dashboard_data$content_strategy$collab_summary
  if (is.null(collab_summary) || nrow(collab_summary) == 0 || !requireNamespace("plotly", quietly = TRUE)) {
    return(NULL)
  }

  plot_df <- collab_summary %>%
    dplyr::mutate(collab_group = as.character(.data$collab_group)) %>%
    dplyr::select(
      collab_group,
      AverageViewsPerVideo,
      AverageRevenuePerVideo
    ) %>%
    tidyr::pivot_longer(
      cols = c("AverageViewsPerVideo", "AverageRevenuePerVideo"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric = dplyr::recode(
        .data$metric,
        AverageViewsPerVideo = "Avg views/video",
        AverageRevenuePerVideo = "Avg revenue/video"
      ),
      hover_text = paste0(
        .data$collab_group,
        "<br>", .data$metric, ": ",
        dplyr::if_else(
          .data$metric == "Avg revenue/video",
          scales::dollar(.data$value, accuracy = 0.01),
          scales::comma(.data$value, accuracy = 1)
        )
      )
    )

  brand_colors <- sun_data_brand_colors()
  collab_colors <- c(
    "Non-collaborative" = brand_colors[["steel"]],
    "Collaborative" = brand_colors[["blue"]]
  )
  present_groups <- unique(plot_df$collab_group)
  missing_groups <- setdiff(present_groups, names(collab_colors))
  if (length(missing_groups) > 0) {
    fallback_colors <- sun_data_palette(length(missing_groups), variant = "brand")
    names(fallback_colors) <- missing_groups
    collab_colors <- c(collab_colors, fallback_colors)
  }
  collab_colors <- collab_colors[intersect(names(collab_colors), present_groups)]

  views_df <- plot_df %>% dplyr::filter(.data$metric == "Avg views/video")
  revenue_df <- plot_df %>% dplyr::filter(.data$metric == "Avg revenue/video")

  views_plot <- plotly::plot_ly(
    views_df,
    x = ~collab_group,
    y = ~value,
    color = ~collab_group,
    colors = collab_colors,
    text = ~hover_text,
    hoverinfo = "text",
    type = "bar",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      title = list(text = "Avg views/video", font = list(size = 12)),
      xaxis = list(title = "", automargin = TRUE, tickangle = -35),
      yaxis = list(title = "", automargin = TRUE, tickformat = ",")
    )

  revenue_plot <- plotly::plot_ly(
    revenue_df,
    x = ~collab_group,
    y = ~value,
    color = ~collab_group,
    colors = collab_colors,
    text = ~hover_text,
    hoverinfo = "text",
    type = "bar",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      title = list(text = "Avg revenue/video", font = list(size = 12)),
      xaxis = list(title = "", automargin = TRUE, tickangle = -35),
      yaxis = list(title = "", automargin = TRUE, tickprefix = "$", tickformat = ",.0f")
    )

  plotly::subplot(
    views_plot,
    revenue_plot,
    nrows = 1,
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) %>%
    plotly::layout(
      margin = list(l = 55, r = 18, b = 80, t = 30),
      autosize = TRUE
    ) %>%
    plotly::config(responsive = TRUE, displaylogo = FALSE)
}

dashboard_collaboration_performance_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(
    dashboard_data$content_strategy$collab_summary,
    page_length = page_length,
    scroll_x = TRUE
  )
}
