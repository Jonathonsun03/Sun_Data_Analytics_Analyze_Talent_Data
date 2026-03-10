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
  raw_linewidth = 0.55,
  smooth_linewidth = 0.9,
  smooth_alpha = 0.18,
  y_label = "Total views",
  title_suffix = "- Views Over Time by Content Type"
) {
  plot_df <- content_duration_views_prep(df)
  content_duration_views_plot(
    plot_df,
    talent,
    span = span,
    show_ci = show_ci,
    raw_linewidth = raw_linewidth,
    smooth_linewidth = smooth_linewidth,
    smooth_alpha = smooth_alpha,
    y_label = y_label,
    title_suffix = title_suffix
  )
}

content_duration_views_plot <- function(
  plot_df,
  talent,
  span = 0.35,
  show_ci = TRUE,
  raw_linewidth = 0.55,
  smooth_linewidth = 0.9,
  smooth_alpha = 0.18,
  y_label = "Total views",
  title_suffix = "- Views Over Time by Content Type"
) {
  start_date <- min(plot_df$publish_date, na.rm = TRUE)
  end_date <- max(plot_df$publish_date, na.rm = TRUE)
  subtitle_text <- paste0(
    "Date range: ",
    format(start_date, "%b %Y"),
    " to ",
    format(end_date, "%b %Y")
  )
  clean_title <- trimws(gsub("^-\\s*", "", as.character(title_suffix)))
  if (!nzchar(clean_title)) {
    clean_title <- "Views Over Time by Content Type"
  }

  plot_df %>%
    ggplot(aes(
      x = publish_date,
      y = Total_Views
    )) +
    geom_line(
      aes(color = `Content Type`),
      linewidth = raw_linewidth,
      alpha = 0.6
    ) +
    geom_smooth(
      method = "loess",
      span = span,
      se = show_ci,
      color = sun_data_brand_colors()[["midnight"]],
      fill = sun_data_brand_colors()[["cloud"]],
      alpha = smooth_alpha,
      linewidth = smooth_linewidth
    ) +
    facet_wrap(~`Content Type`, ncol = 1, scales = "free_y") +
    scale_color_sun_data(variant = "brand") +
    guides(color = "none") +
    theme_nyt() +
    labs(
      title = bundle_a_wrap_text(clean_title, width = 58),
      subtitle = bundle_a_talent_subtitle(talent, subtitle_text),
      x = "Publish date",
      y = y_label
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    )
}

Content_duration_views_with_data <- function(
  df,
  talent,
  span = 0.35,
  show_ci = TRUE,
  raw_linewidth = 0.55,
  smooth_linewidth = 0.9,
  smooth_alpha = 0.18,
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
      raw_linewidth = raw_linewidth,
      smooth_linewidth = smooth_linewidth,
      smooth_alpha = smooth_alpha,
      y_label = y_label,
      title_suffix = title_suffix
    )
  )
}
