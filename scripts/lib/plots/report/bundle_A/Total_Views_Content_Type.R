total_views_content_type_prep <- function(df) {
  df %>%
    group_by(`Content Type`) %>%
    summarize(Total_Views = sum(views), .groups = "drop")
}

total_views_content_type_plot <- function(plot_df, talent) {
  plot_df %>%
    ggplot(aes(
      x = `Content Type`,
      y = Total_Views,
      fill = `Content Type`
    )) +
    geom_col() +
    geom_text(
      aes(label = scales::comma(Total_Views)),
      vjust = -0.4,
      size = 3.5,
      color = "grey20"
    ) +
    scale_fill_grey(start = 0.35, end = 0.65) +
    guides(fill = "none") +
    theme_nyt() +
    labs(
      title = paste0(talent, " — Total Views by Content Type"),
      x = "Content type",
      y = "Total views"
    ) +
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.12))
    )
}

total_views_content_type <- function(df, talent) {
  plot_df <- total_views_content_type_prep(df)
  total_views_content_type_plot(plot_df, talent)
}

total_views_content_type_with_data <- function(df, talent) {
  plot_df <- total_views_content_type_prep(df)
  list(
    data = plot_df,
    plot = total_views_content_type_plot(plot_df, talent)
  )
}

average_views_content_type_prep <- function(df) {
  df %>%
    mutate(.views = suppressWarnings(as.numeric(.data$views))) %>%
    group_by(`Content Type`) %>%
    summarize(
      Average_Views = mean(.data$.views, na.rm = TRUE),
      VideoCount = dplyr::n(),
      .groups = "drop"
    ) %>%
    filter(
      !is.na(`Content Type`),
      !is.na(Average_Views),
      is.finite(Average_Views)
    )
}

average_views_content_type_plot <- function(plot_df, talent) {
  plot_df %>%
    ggplot(aes(
      x = `Content Type`,
      y = Average_Views,
      fill = `Content Type`
    )) +
    geom_col() +
    geom_text(
      aes(label = scales::comma(round(Average_Views))),
      vjust = -0.4,
      size = 3.5,
      color = "grey20"
    ) +
    scale_fill_grey(start = 0.35, end = 0.65) +
    guides(fill = "none") +
    theme_nyt() +
    labs(
      title = paste0(talent, " — Average Views per Video by Content Type"),
      x = "Content type",
      y = "Average views per video"
    ) +
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.12))
    )
}

average_views_content_type <- function(df, talent) {
  plot_df <- average_views_content_type_prep(df)
  average_views_content_type_plot(plot_df, talent)
}

average_views_content_type_with_data <- function(df, talent) {
  plot_df <- average_views_content_type_prep(df)
  list(
    data = plot_df,
    plot = average_views_content_type_plot(plot_df, talent)
  )
}
