# Reusable topic-by-weekday heatmap.

topic_weekday_heatmap_ggplot <- function(
  summary_df,
  compact = FALSE,
  show_counts = !compact,
  include_hover = compact
) {
  required_cols <- c("topic_lumped", "publish_wday", "median_views", "streams")
  missing_cols <- setdiff(required_cols, names(summary_df))
  if (length(missing_cols) > 0) {
    stop(
      "Topic-weekday heatmap is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  plot_df <- summary_df %>%
    dplyr::mutate(
      .hover_text = paste0(
        "Topic: ", .data$topic_lumped,
        "<br>Weekday: ", .data$publish_wday,
        "<br>Median views: ", scales::comma(round(.data$median_views)),
        "<br>Streams: ", scales::comma(.data$streams)
      )
    )

  plot_mapping <- if (isTRUE(include_hover)) {
    ggplot2::aes(
      x = .data$publish_wday,
      y = .data$topic_lumped,
      fill = .data$median_views,
      text = .data$.hover_text
    )
  } else {
    ggplot2::aes(
      x = .data$publish_wday,
      y = .data$topic_lumped,
      fill = .data$median_views
    )
  }

  plot_theme <- if (exists("theme_nyt", mode = "function")) {
    theme_nyt()
  } else {
    ggplot2::theme_minimal()
  }

  p <- ggplot2::ggplot(plot_df, plot_mapping) +
    ggplot2::geom_tile(color = "white", linewidth = if (isTRUE(compact)) 0.35 else 0.5) +
    ggplot2::scale_fill_viridis_c(option = "C", labels = scales::comma) +
    ggplot2::labs(
      x = if (isTRUE(compact)) "" else "Publish weekday",
      y = if (isTRUE(compact)) "" else "Topic",
      fill = "Median views",
      title = if (isTRUE(compact)) NULL else "Observed median views by live stream topic and weekday",
      subtitle = if (isTRUE(compact)) NULL else "Tile color is observed median views; labels are stream counts."
    ) +
    plot_theme

  if (isTRUE(show_counts)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$streams), size = 3)
  }

  if (isTRUE(compact)) {
    p <- p +
      ggplot2::theme(
        legend.position = "bottom",
        plot.margin = ggplot2::margin(t = 4, r = 8, b = 4, l = 4),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
      )
  }

  p
}
