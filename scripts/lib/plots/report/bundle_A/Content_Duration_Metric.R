content_duration_metric_prep <- function(df, metric_col) {
  df %>%
    group_by(`Content Type`, publish_date) %>%
    summarize(Total = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop")
}

content_duration_metric <- function(df, talent, metric_col, metric_label = metric_col, span = 0.35, show_ci = TRUE) {
  plot_df <- content_duration_metric_prep(df, metric_col)
  content_duration_metric_plot(
    plot_df,
    talent,
    metric_label = metric_label,
    span = span,
    show_ci = show_ci
  )
}

content_duration_metric_plot <- function(plot_df, talent, metric_label, span = 0.35, show_ci = TRUE) {
  plot_df %>%
    ggplot(aes(
      x = publish_date,
      y = Total
    )) +
    geom_line(
      aes(color = `Content Type`),
      linewidth = 0.7,
      alpha = 0.6
    ) +
    geom_smooth(
      method = "loess",
      span = span,
      se = show_ci,
      color = "black",
      linewidth = 1.4
    ) +
    facet_wrap(~`Content Type`, ncol = 1, scales = "free_y") +
    guides(color = "none") +
    theme_nyt() +
    labs(
      title = paste0(talent, " - ", metric_label, " Over Time by Content Type"),
      x = "Publish date",
      y = paste0("Total ", metric_label)
    )
}

content_duration_metric_with_data <- function(
  df,
  talent,
  metric_col,
  metric_label = metric_col,
  span = 0.35,
  show_ci = TRUE
) {
  plot_df <- content_duration_metric_prep(df, metric_col)
  list(
    data = plot_df,
    plot = content_duration_metric_plot(
      plot_df,
      talent,
      metric_label = metric_label,
      span = span,
      show_ci = show_ci
    )
  )
}
