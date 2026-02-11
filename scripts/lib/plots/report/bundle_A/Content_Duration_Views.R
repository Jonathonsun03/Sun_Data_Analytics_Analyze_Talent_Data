content_duration_views_prep <- function(df) {
  df %>%
    group_by(`Content Type`, publish_date) %>%
    summarize(Total_Views = sum(views), .groups = "drop")
}

Content_duration_views <- function(
  df,
  talent,
  span = 0.35,
  show_ci = TRUE,
  y_label = "Total views",
  title_suffix = "- Views Over Time by Content Type"
) {
  plot_df <- content_duration_views_prep(df)
  content_duration_views_plot(
    plot_df,
    talent,
    span = span,
    show_ci = show_ci,
    y_label = y_label,
    title_suffix = title_suffix
  )
}

content_duration_views_plot <- function(
  plot_df,
  talent,
  span = 0.35,
  show_ci = TRUE,
  y_label = "Total views",
  title_suffix = "- Views Over Time by Content Type"
) {
  plot_df %>%
    ggplot(aes(
      x = publish_date,
      y = Total_Views
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
      title = paste0(talent, " ", title_suffix),
      x = "Publish date",
      y = y_label
    )
}

Content_duration_views_with_data <- function(
  df,
  talent,
  span = 0.35,
  show_ci = TRUE,
  y_label = "Total views",
  title_suffix = "- Views Over Time by Content Type"
) {
  plot_df <- content_duration_views_prep(df)
  list(
    data = plot_df,
    plot = content_duration_views_plot(
      plot_df,
      talent,
      span = span,
      show_ci = show_ci,
      y_label = y_label,
      title_suffix = title_suffix
    )
  )
}
