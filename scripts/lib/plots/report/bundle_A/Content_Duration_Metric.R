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
  start_date <- min(plot_df$publish_date, na.rm = TRUE)
  end_date <- max(plot_df$publish_date, na.rm = TRUE)
  subtitle_text <- paste0(
    "Date range: ",
    format(start_date, "%b %Y"),
    " to ",
    format(end_date, "%b %Y")
  )

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
      color = sun_data_brand_colors()[["midnight"]],
      linewidth = 1.4
    ) +
    facet_wrap(~`Content Type`, ncol = 1, scales = "free_y") +
    scale_color_sun_data(variant = "brand") +
    guides(color = "none") +
    theme_nyt() +
    labs(
      title = paste0(talent, " - ", metric_label, " Over Time by Content Type"),
      subtitle = subtitle_text,
      x = "Publish date",
      y = paste0("Total ", metric_label)
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
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
