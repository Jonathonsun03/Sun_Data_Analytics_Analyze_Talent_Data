engagement_distribution_content_type_prep <- function(
  df,
  metric_col = "averageViewPercentage",
  content_col = "Content Type",
  min_videos = 3
) {
  if (!metric_col %in% names(df)) {
    stop("Metric column not found: ", metric_col)
  }
  if (!content_col %in% names(df)) {
    stop("Content column not found: ", content_col)
  }

  out <- df %>%
    dplyr::mutate(
      .metric = suppressWarnings(as.numeric(.data[[metric_col]])),
      .content = as.character(.data[[content_col]])
    ) %>%
    dplyr::filter(
      !is.na(.data$.metric),
      !is.na(.data$.content),
      nzchar(trimws(.data$.content))
    ) %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::mutate(.n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$.n >= min_videos)

  if (nrow(out) == 0) {
    stop("No rows available after filtering. Check min_videos or input columns.")
  }

  order_tbl <- out %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::summarize(.median = stats::median(.data$.metric, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$.median))

  out %>%
    dplyr::mutate(.content = factor(.data$.content, levels = order_tbl$.content, ordered = TRUE))
}

engagement_distribution_content_type_plot <- function(
  plot_df,
  talent,
  metric_label = "Engagement Metric",
  show_points = TRUE,
  y_as_percent = FALSE
) {
  if (!all(c(".content", ".metric") %in% names(plot_df))) {
    stop("plot_df must contain .content and .metric columns.")
  }

  p <- plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.content, y = .data$.metric, fill = .data$.content)) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.alpha = 0.2, width = 0.65) +
    scale_fill_grey(start = 0.35, end = 0.65) +
    guides(fill = "none") +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - ", metric_label, " Distribution by Content Type"),
      subtitle = paste0("Each point is a video. Box = median and IQR."),
      x = "Content type",
      y = metric_label
    )

  if (isTRUE(show_points)) {
    p <- p + ggplot2::geom_jitter(width = 0.18, alpha = 0.25, size = 1.2, color = "grey20")
  }

  if (isTRUE(y_as_percent)) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))
  } else {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_comma())
  }

  p
}

engagement_distribution_content_type <- function(
  df,
  talent,
  metric_col = "averageViewPercentage",
  metric_label = metric_col,
  content_col = "Content Type",
  min_videos = 3,
  show_points = TRUE,
  y_as_percent = grepl("percentage|rate|ratio", metric_col, ignore.case = TRUE)
) {
  plot_df <- engagement_distribution_content_type_prep(
    df = df,
    metric_col = metric_col,
    content_col = content_col,
    min_videos = min_videos
  )
  engagement_distribution_content_type_plot(
    plot_df = plot_df,
    talent = talent,
    metric_label = metric_label,
    show_points = show_points,
    y_as_percent = y_as_percent
  )
}

engagement_distribution_content_type_with_data <- function(
  df,
  talent,
  metric_col = "averageViewPercentage",
  metric_label = metric_col,
  content_col = "Content Type",
  min_videos = 3,
  show_points = TRUE,
  y_as_percent = grepl("percentage|rate|ratio", metric_col, ignore.case = TRUE)
) {
  plot_df <- engagement_distribution_content_type_prep(
    df = df,
    metric_col = metric_col,
    content_col = content_col,
    min_videos = min_videos
  )
  list(
    data = plot_df,
    plot = engagement_distribution_content_type_plot(
      plot_df = plot_df,
      talent = talent,
      metric_label = metric_label,
      show_points = show_points,
      y_as_percent = y_as_percent
    )
  )
}

engagement_summary_content_type_prep <- function(
  df,
  metric_col = "averageViewPercentage",
  content_col = "Content Type",
  min_videos = 3
) {
  engagement_distribution_content_type_prep(
    df = df,
    metric_col = metric_col,
    content_col = content_col,
    min_videos = min_videos
  ) %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::summarize(
      VideoCount = dplyr::n(),
      Median = stats::median(.data$.metric, na.rm = TRUE),
      Mean = mean(.data$.metric, na.rm = TRUE),
      Q25 = stats::quantile(.data$.metric, probs = 0.25, na.rm = TRUE),
      Q75 = stats::quantile(.data$.metric, probs = 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Median))
}

engagement_summary_content_type_plot <- function(
  summary_df,
  talent,
  metric_label = "Engagement Metric",
  y_as_percent = FALSE
) {
  p <- summary_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(.data$.content, .data$Median),
        y = .data$Median
      )
    ) +
    ggplot2::geom_col(fill = "grey55", width = 0.65) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$Q25, ymax = .data$Q75),
      width = 0.2,
      color = "grey20"
    ) +
    ggplot2::coord_flip() +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Median ", metric_label, " by Content Type"),
      subtitle = "Error bars show interquartile range (25th to 75th percentile).",
      x = "Content type",
      y = paste0("Median ", metric_label)
    )

  if (isTRUE(y_as_percent)) {
    p + ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))
  } else {
    p + ggplot2::scale_y_continuous(labels = scales::label_comma())
  }
}
