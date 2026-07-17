# Reusable topic-performance and topic-distribution plots.

topic_performance_prep <- function(
  analytics_df,
  monetary_df,
  topic_col = "topic",
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue",
  top_n = 8
) {
  if (!topic_col %in% names(analytics_df) || !topic_col %in% names(monetary_df)) {
    stop("Topic column must exist in both analytics and monetary data: ", topic_col)
  }

  clean_topic <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | !nzchar(x)] <- "(unclassified)"
    x
  }

  a <- analytics_df %>% dplyr::mutate(topic_group = clean_topic(.data[[topic_col]]))
  m <- monetary_df %>% dplyr::mutate(topic_group = clean_topic(.data[[topic_col]]))

  views_sum <- bundle_a_metric_summary(
    a, group_col = "topic_group", metric_col = views_col, id_col = id_col,
    group_name = "topic_group", metric_name = "Views"
  )
  rev_sum <- bundle_a_metric_summary(
    m, group_col = "topic_group", metric_col = revenue_col, id_col = id_col,
    group_name = "topic_group", metric_name = "Revenue"
  )

  out <- bundle_a_views_revenue_join(views_sum, rev_sum, by_col = "topic_group")

  rank_tbl <- out %>%
    dplyr::mutate(rank_score = dplyr::coalesce(.data$ShareViews, 0) + dplyr::coalesce(.data$ShareRevenue, 0)) %>%
    dplyr::arrange(dplyr::desc(.data$rank_score)) %>%
    dplyr::mutate(rank_id = dplyr::row_number())

  out <- out %>%
    dplyr::left_join(rank_tbl %>% dplyr::select(topic_group, rank_id), by = "topic_group") %>%
    dplyr::mutate(
      topic_group = dplyr::if_else(.data$rank_id <= top_n, .data$topic_group, "Other topics")
    ) %>%
    dplyr::group_by(.data$topic_group) %>%
    dplyr::summarize(
      dplyr::across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  tv <- sum(out$TotalViews, na.rm = TRUE)
  tr <- sum(out$TotalRevenue, na.rm = TRUE)
  out %>%
    dplyr::mutate(
      ShareViews = if (is.finite(tv) && tv > 0) .data$TotalViews / tv else NA_real_,
      ShareRevenue = if (is.finite(tr) && tr > 0) .data$TotalRevenue / tr else NA_real_
    ) %>%
    dplyr::arrange(dplyr::desc(.data$TotalViews))
}

topic_performance_plot <- function(summary_df, talent, as_share = FALSE) {
  bundle_a_dual_metric_plot(
    summary_df = summary_df,
    group_col = "topic_group",
    talent = talent,
    title = if (isTRUE(as_share)) "Topic Share" else "Topic Performance",
    subtitle = "Views and revenue by classified topic.",
    as_share = as_share
  )
}

topic_view_distribution_prep <- function(
  analytics_df,
  topic_col = "topic",
  id_col = "Video ID",
  title_col = NULL,
  views_col = "views",
  top_n = 8,
  min_videos = 2
) {
  if (!topic_col %in% names(analytics_df)) {
    stop("Topic column must exist in analytics data: ", topic_col)
  }
  if (!id_col %in% names(analytics_df)) {
    stop("ID column must exist in analytics data: ", id_col)
  }
  if (!views_col %in% names(analytics_df)) {
    stop("Views column must exist in analytics data: ", views_col)
  }
  title_col <- bundle_a_optional_col(
    analytics_df,
    col = title_col,
    candidates = c("Title", "Video Title", "title", "video_title"),
    label = "title column"
  )

  clean_topic <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | !nzchar(x)] <- "(unclassified)"
    x
  }

  exploded <- analytics_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .video_title = if (!is.null(title_col)) as.character(.data[[title_col]]) else NA_character_,
      topic_group = clean_topic(.data[[topic_col]]),
      views = suppressWarnings(as.numeric(.data[[views_col]]))
    ) %>%
    dplyr::filter(nzchar(.data$topic_group), !is.na(.data$views), is.finite(.data$views)) %>%
    dplyr::distinct(.data$.video_id, .data$topic_group, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(c(".video_id", ".video_title", "topic_group", "views")))

  if (nrow(exploded) == 0) {
    return(list(
      plot_data = tibble::tibble(
        .video_id = character(),
        .video_title = character(),
        topic_group = character(),
        views = numeric()
      ),
      summary = tibble::tibble(
        topic_group = character(),
        VideoCount = integer(),
        TotalViews = numeric(),
        AverageViewsPerVideo = numeric(),
        MedianViewsPerVideo = numeric(),
        Q1ViewsPerVideo = numeric(),
        Q3ViewsPerVideo = numeric(),
        IQRViewsPerVideo = numeric(),
        MinViewsPerVideo = numeric(),
        MaxViewsPerVideo = numeric()
      )
    ))
  }

  summary <- exploded %>%
    dplyr::group_by(.data$topic_group) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data$.video_id),
      TotalViews = sum(.data$views, na.rm = TRUE),
      AverageViewsPerVideo = mean(.data$views, na.rm = TRUE),
      MedianViewsPerVideo = stats::median(.data$views, na.rm = TRUE),
      Q1ViewsPerVideo = stats::quantile(.data$views, probs = 0.25, na.rm = TRUE, names = FALSE),
      Q3ViewsPerVideo = stats::quantile(.data$views, probs = 0.75, na.rm = TRUE, names = FALSE),
      IQRViewsPerVideo = stats::IQR(.data$views, na.rm = TRUE),
      MinViewsPerVideo = min(.data$views, na.rm = TRUE),
      MaxViewsPerVideo = max(.data$views, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$VideoCount >= min_videos) %>%
    dplyr::arrange(dplyr::desc(.data$AverageViewsPerVideo), dplyr::desc(.data$VideoCount)) %>%
    dplyr::slice_head(n = top_n)

  if (nrow(summary) == 0) {
    return(list(
      plot_data = tibble::tibble(
        .video_id = character(),
        .video_title = character(),
        topic_group = character(),
        views = numeric()
      ),
      summary = summary
    ))
  }

  topic_levels <- summary$topic_group
  plot_data <- exploded %>%
    dplyr::filter(.data$topic_group %in% topic_levels) %>%
    dplyr::mutate(
      topic_group = factor(.data$topic_group, levels = rev(topic_levels), ordered = TRUE)
    ) %>%
    dplyr::select(dplyr::all_of(c(".video_id", ".video_title", "topic_group", "views")))

  summary <- summary %>%
    dplyr::mutate(topic_group = factor(.data$topic_group, levels = rev(topic_levels), ordered = TRUE)) %>%
    dplyr::arrange(.data$topic_group)

  list(plot_data = plot_data, summary = summary)
}

topic_view_distribution_plot <- function(topic_dist, talent) {
  if (!is.list(topic_dist) || !all(c("plot_data", "summary") %in% names(topic_dist))) {
    stop("topic_dist must be a list returned by topic_view_distribution_prep().")
  }

  plot_df <- topic_dist$plot_data
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          geom = "text",
          x = 1,
          y = 1,
          label = "No topic view distribution available for current filters."
        ) +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = bundle_a_wrap_text("Topic View Distribution", width = 58),
          subtitle = bundle_a_talent_subtitle(
            talent,
            "Top topics by average views per video."
          )
        )
    )
  }

  plot_df %>%
    dplyr::mutate(
      .video_label = dplyr::case_when(
        !is.na(.data$.video_title) & nzchar(trimws(.data$.video_title)) ~ trimws(.data$.video_title),
        !is.na(.data$.video_id) & nzchar(trimws(.data$.video_id)) ~ paste0("Video ID: ", trimws(.data$.video_id)),
        TRUE ~ "Untitled video"
      ),
      .hover_text = paste0(
        "Video: ", .data$.video_label,
        "<br>Topic: ", .data$topic_group,
        "<br>Views: ", scales::label_comma(accuracy = 1)(.data$views)
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$topic_group, y = .data$views, fill = .data$topic_group)) +
    ggplot2::geom_boxplot(
      color = sun_data_brand_colors()[["midnight"]],
      width = 0.65,
      outlier.shape = NA
    ) +
    ggplot2::geom_jitter(
      ggplot2::aes(text = .data$.hover_text),
      width = 0.18,
      alpha = 0.25,
      size = 1.2,
      color = sun_data_brand_colors()[["midnight"]]
    ) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 2.5,
      stroke = 0.2,
      fill = sun_data_brand_colors()[["midnight"]],
      color = "white"
    ) +
    ggplot2::coord_flip() +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_a_wrap_text("Topic View Distribution", width = 58),
      subtitle = bundle_a_talent_subtitle(
        talent,
        "Top topics ranked by average views per video (box = IQR, line = median, point = mean)."
      ),
      x = "",
      y = "Views per video"
    )
}

