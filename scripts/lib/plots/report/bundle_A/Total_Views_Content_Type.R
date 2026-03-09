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
    scale_fill_sun_data(variant = "brand") +
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
    scale_fill_sun_data(variant = "brand") +
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

views_content_type_comparison_prep <- function(df) {
  total_df <- total_views_content_type_prep(df) %>%
    dplyr::rename(Total_Views_Value = Total_Views)

  avg_df <- average_views_content_type_prep(df) %>%
    dplyr::rename(Average_Views_Value = Average_Views)

  merged <- total_df %>%
    dplyr::full_join(avg_df, by = "Content Type")

  order_levels <- merged %>%
    dplyr::arrange(dplyr::desc(.data$Total_Views_Value), dplyr::desc(.data$Average_Views_Value)) %>%
    dplyr::pull(`Content Type`) %>%
    unique()

  merged %>%
    dplyr::mutate(`Content Type` = factor(`Content Type`, levels = order_levels, ordered = TRUE)) %>%
    tidyr::pivot_longer(
      cols = c("Total_Views_Value", "Average_Views_Value"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric = dplyr::case_when(
        .data$metric == "Total_Views_Value" ~ "Total views",
        .data$metric == "Average_Views_Value" ~ "Average views per video",
        TRUE ~ .data$metric
      )
    )
}

views_content_type_comparison_plot <- function(plot_df, talent) {
  plot_df %>%
    dplyr::filter(!is.na(.data$value), is.finite(.data$value)) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$`Content Type`,
      y = .data$value,
      fill = .data$`Content Type`
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(.data$value))),
      vjust = -0.4,
      size = 3.4,
      color = "grey20"
    ) +
    ggplot2::facet_wrap(~metric, ncol = 2, scales = "free_y") +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " — Views by Content Type (Total vs Average)"),
      subtitle = "Side-by-side comparison to separate volume impact from per-video performance.",
      x = "Content type",
      y = "Views"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.12))
    )
}

views_content_type_comparison <- function(df, talent) {
  plot_df <- views_content_type_comparison_prep(df)
  views_content_type_comparison_plot(plot_df, talent)
}

views_content_type_comparison_with_data <- function(df, talent) {
  plot_df <- views_content_type_comparison_prep(df)
  list(
    data = plot_df,
    plot = views_content_type_comparison_plot(plot_df, talent)
  )
}
