# Publishing-schedule plot, table, and heatmap adapters.

dashboard_weekday_table <- function(dashboard_data) {
  if (is.null(dashboard_data$weekday_summary) || nrow(dashboard_data$weekday_summary) == 0) {
    return(dashboard_empty_state("Weekday summary is unavailable for the active filters."))
  }
  dashboard_datatable(dashboard_data$weekday_summary, page_length = 7, scroll_x = TRUE)
}

dashboard_weekday_plot <- function(dashboard_data, talent, as_share = FALSE) {
  if (is.null(dashboard_data$weekday_summary) || nrow(dashboard_data$weekday_summary) == 0) {
    return(dashboard_empty_state("Day-of-week performance is unavailable for the active filters."))
  }
  dashboard_ggplotly(day_of_week_performance_plot(
    dashboard_data$weekday_summary,
    talent = talent,
    as_share = as_share
  ), compact = TRUE)
}

dashboard_day_of_week_deviation_plot <- function(dashboard_data, talent) {
  if (is.null(dashboard_data$weekday_summary) || nrow(dashboard_data$weekday_summary) == 0) {
    return(dashboard_empty_state("Day-of-week deviation is unavailable for the active filters."))
  }
  dashboard_ggplotly(
    dashboard_card_ggplot(bundle_b_day_of_week_lift_plot(
      dashboard_data$weekday_summary,
      talent = talent
    )),
    tooltip = "text",
    compact = TRUE,
    tickangle = -35
  )
}

dashboard_weekend_weekday_distribution_plot <- function(dashboard_data, talent) {
  wk_dist <- dashboard_data$weekend_weekday_distribution
  if (is.null(wk_dist)) {
    return(dashboard_empty_state("Weekend versus weekday distribution is unavailable for the active filters."))
  }
  dashboard_ggplotly(
    dashboard_card_ggplot(bundle_b_weekend_weekday_distribution_plot(
      wk_dist,
      talent = talent,
      use_repel = FALSE
    )),
    tooltip = "text",
    compact = TRUE,
    tickangle = 0
  )
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
    return(dashboard_empty_state("Topic-weekday heatmap is unavailable for the active filters."))
  }

  heatmap_df <- topic_result$topic_weekday %>%
    dplyr::mutate(
      .hover_text = paste0(
        "Topic: ", .data$topic_lumped,
        "<br>Weekday: ", .data$publish_wday,
        "<br>Median views: ", scales::comma(round(.data$median_views)),
        "<br>Streams: ", scales::comma(.data$streams)
      )
    )

  dashboard_ggplotly(
    ggplot2::ggplot(
      heatmap_df,
      ggplot2::aes(
        x = .data$publish_wday,
        y = .data$topic_lumped,
        fill = .data$median_views,
        text = .data$.hover_text
      )
    ) +
      ggplot2::geom_tile(color = "white", linewidth = 0.35) +
      ggplot2::scale_fill_viridis_c(option = "C", labels = scales::comma) +
      ggplot2::labs(x = "", y = "", fill = "Median views") +
      theme_nyt() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.margin = ggplot2::margin(t = 4, r = 8, b = 4, l = 4),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
      ),
    tooltip = "text",
    compact = TRUE,
    tickangle = -45
  )
}
