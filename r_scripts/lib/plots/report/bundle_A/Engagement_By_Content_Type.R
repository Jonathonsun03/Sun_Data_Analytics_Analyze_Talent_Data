engagement_distribution_content_type_prep <- function(
  df,
  metric_col = "averageViewPercentage",
  content_col = "Content Type",
  min_videos = 3,
  title_col = NULL,
  video_id_col = NULL
) {
  if (!metric_col %in% names(df)) {
    stop("Metric column not found: ", metric_col)
  }
  if (!content_col %in% names(df)) {
    stop("Content column not found: ", content_col)
  }

  title_col <- bundle_a_optional_col(
    df,
    col = title_col,
    candidates = c("Title", "Video Title", "title", "video_title"),
    label = "title column"
  )
  video_id_col <- bundle_a_optional_col(
    df,
    col = video_id_col,
    candidates = c("Video ID", "video_id", "videoId", "id"),
    label = "video id column"
  )

  out <- df %>%
    dplyr::mutate(
      .metric = suppressWarnings(as.numeric(.data[[metric_col]])),
      .content = as.character(.data[[content_col]]),
      .title = if (!is.null(title_col)) as.character(.data[[title_col]]) else NA_character_,
      .video_id = if (!is.null(video_id_col)) as.character(.data[[video_id_col]]) else NA_character_
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
    dplyr::mutate(
      .content = factor(.data$.content, levels = order_tbl$.content, ordered = TRUE),
      .video_label = dplyr::case_when(
        !is.na(.data$.title) & nzchar(trimws(.data$.title)) ~ trimws(.data$.title),
        !is.na(.data$.video_id) & nzchar(trimws(.data$.video_id)) ~ paste0("Video ID: ", trimws(.data$.video_id)),
        TRUE ~ "Untitled video"
      )
    )
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
  if (!".video_label" %in% names(plot_df)) {
    plot_df <- plot_df %>% dplyr::mutate(.video_label = "Untitled video")
  }

  plot_df <- plot_df %>%
    dplyr::mutate(
      .metric_fmt = if (isTRUE(y_as_percent)) {
        scales::label_percent(accuracy = 0.1)(.data$.metric)
      } else {
        scales::label_comma(accuracy = 1)(.data$.metric)
      },
      .hover_text = paste0(
        "Video: ", .data$.video_label,
        "<br>Content Type: ", .data$.content,
        "<br>", metric_label, ": ", .data$.metric_fmt
      )
    )

  p <- plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.content, y = .data$.metric, fill = .data$.content)) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.65) +
    scale_fill_sun_data(variant = "brand") +
    guides(fill = "none") +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_a_wrap_text(paste0(metric_label, " Distribution by Content Type"), width = 58),
      subtitle = bundle_a_talent_subtitle(talent, "Each point is a video. Box = median and IQR."),
      x = "Content type",
      y = metric_label
    )

  if (isTRUE(show_points)) {
    p <- p + ggplot2::geom_jitter(
      ggplot2::aes(text = .data$.hover_text),
      width = 0.18,
      alpha = 0.25,
      size = 1.2,
      color = sun_data_brand_colors()[["midnight"]]
    )
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
  title_col = NULL,
  video_id_col = NULL,
  show_points = TRUE,
  y_as_percent = grepl("percentage|rate|ratio", metric_col, ignore.case = TRUE)
) {
  plot_df <- engagement_distribution_content_type_prep(
    df = df,
    metric_col = metric_col,
    content_col = content_col,
    min_videos = min_videos,
    title_col = title_col,
    video_id_col = video_id_col
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
  title_col = NULL,
  video_id_col = NULL,
  show_points = TRUE,
  y_as_percent = grepl("percentage|rate|ratio", metric_col, ignore.case = TRUE)
) {
  plot_df <- engagement_distribution_content_type_prep(
    df = df,
    metric_col = metric_col,
    content_col = content_col,
    min_videos = min_videos,
    title_col = title_col,
    video_id_col = video_id_col
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
        y = .data$Median,
        fill = stats::reorder(.data$.content, .data$Median)
      )
    ) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$Q25, ymax = .data$Q75),
      width = 0.2,
      color = sun_data_brand_colors()[["midnight"]]
    ) +
    ggplot2::coord_flip() +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_a_wrap_text(paste0("Median ", metric_label, " by Content Type"), width = 58),
      subtitle = bundle_a_talent_subtitle(
        talent,
        "Error bars show interquartile range (25th to 75th percentile)."
      ),
      x = "Content type",
      y = paste0("Median ", metric_label)
    )

  if (isTRUE(y_as_percent)) {
    p + ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))
  } else {
    p + ggplot2::scale_y_continuous(labels = scales::label_comma())
  }
}
