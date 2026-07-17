# Reusable tag-performance and tag-distribution plots.

tag_performance_prep <- function(
  analytics_df,
  monetary_df,
  tags_col = "tags",
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue",
  top_n = 15,
  min_videos = 2
) {
  if (!tags_col %in% names(analytics_df) || !tags_col %in% names(monetary_df)) {
    stop("Tags column must exist in both analytics and monetary data: ", tags_col)
  }

  explode_tags <- function(df, metric_col) {
    df %>%
      dplyr::mutate(
        .metric = suppressWarnings(as.numeric(.data[[metric_col]])),
        .tag_raw = as.character(.data[[tags_col]])
      ) %>%
      dplyr::mutate(.tag_raw = ifelse(is.na(.tag_raw), "", .tag_raw)) %>%
      tidyr::separate_rows(.tag_raw, sep = ",") %>%
      dplyr::mutate(
        tag_group = trimws(tolower(.data$.tag_raw))
      ) %>%
      dplyr::filter(nzchar(.data$tag_group)) %>%
      dplyr::group_by(.data$tag_group) %>%
      dplyr::summarize(
        VideoCount = dplyr::n_distinct(.data[[id_col]]),
        Total = sum(.data$.metric, na.rm = TRUE),
        AveragePerVideo = mean(.data$.metric, na.rm = TRUE),
        .groups = "drop"
      )
  }

  v <- explode_tags(analytics_df, views_col) %>%
    dplyr::rename(
      TotalViews = Total,
      AverageViewsPerVideo = AveragePerVideo,
      VideoCountViews = VideoCount
    )

  r <- explode_tags(monetary_df, revenue_col) %>%
    dplyr::rename(
      TotalRevenue = Total,
      AverageRevenuePerVideo = AveragePerVideo,
      VideoCountRevenue = VideoCount
    )

  out <- dplyr::full_join(v, r, by = "tag_group") %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
      VideoCount = pmax(.data$VideoCountViews, .data$VideoCountRevenue)
    ) %>%
    dplyr::filter(.data$VideoCount >= min_videos)

  rank_tbl <- out %>%
    dplyr::mutate(rank_score = .data$TotalViews + .data$TotalRevenue) %>%
    dplyr::arrange(dplyr::desc(.data$rank_score))

  out <- rank_tbl %>%
    dplyr::slice_head(n = top_n)

  tv <- sum(out$TotalViews, na.rm = TRUE)
  tr <- sum(out$TotalRevenue, na.rm = TRUE)
  out %>%
    dplyr::mutate(
      ShareViews = if (is.finite(tv) && tv > 0) .data$TotalViews / tv else NA_real_,
      ShareRevenue = if (is.finite(tr) && tr > 0) .data$TotalRevenue / tr else NA_real_
    )
}

tag_performance_plot <- function(summary_df, talent, as_share = FALSE) {
  bundle_a_dual_metric_plot(
    summary_df = summary_df,
    group_col = "tag_group",
    talent = talent,
    title = if (isTRUE(as_share)) "Top Tag Share" else "Top Tag Performance",
    subtitle = "Top tags by combined view/revenue signal. Totals can overlap across tags.",
    as_share = as_share
  )
}

tag_average_views_plot <- function(summary_df, talent) {
  required <- c("tag_group", "AverageViewsPerVideo")
  if (!all(required %in% names(summary_df))) {
    stop("summary_df must contain: ", paste(required, collapse = ", "))
  }

  count_col <- if ("VideoCountViews" %in% names(summary_df)) {
    "VideoCountViews"
  } else if ("VideoCount" %in% names(summary_df)) {
    "VideoCount"
  } else {
    NULL
  }

  plot_df <- summary_df %>%
    dplyr::mutate(
      .tag_chr = as.character(.data$tag_group),
      .video_count = if (!is.null(count_col)) as.numeric(.data[[count_col]]) else NA_real_,
      .hover_text = if (!is.null(count_col)) {
        paste0(
          "Tag: ", .data$.tag_chr,
          "<br>Average views per tagged video: ", scales::label_comma(accuracy = 1)(.data$AverageViewsPerVideo),
          "<br>Tagged videos: ", scales::label_comma(accuracy = 1)(.data$.video_count)
        )
      } else {
        paste0(
          "Tag: ", .data$.tag_chr,
          "<br>Average views per tagged video: ", scales::label_comma(accuracy = 1)(.data$AverageViewsPerVideo)
        )
      }
    ) %>%
    dplyr::arrange(.data$AverageViewsPerVideo) %>%
    dplyr::mutate(
      .tag_factor = factor(.data$.tag_chr, levels = .data$.tag_chr, ordered = TRUE)
    )

  plot_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$.tag_factor,
        y = .data$AverageViewsPerVideo,
        fill = .data$.tag_factor,
        text = .data$.hover_text
      )
    ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_a_wrap_text("Average Views per Tag", width = 58),
      subtitle = bundle_a_talent_subtitle(
        talent,
        "Average views per video where each tag appears at least once."
      ),
      x = "",
      y = "Average views per tagged video"
    )
}

tag_view_distribution_prep <- function(
  analytics_df,
  tags_col = "tags",
  id_col = "Video ID",
  title_col = NULL,
  views_col = "views",
  top_n = 15,
  min_videos = 2,
  min_views_per_video = 10
) {
  if (!tags_col %in% names(analytics_df)) {
    stop("Tags column must exist in analytics data: ", tags_col)
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

  exploded <- analytics_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .video_title = if (!is.null(title_col)) as.character(.data[[title_col]]) else NA_character_,
      views = suppressWarnings(as.numeric(.data[[views_col]])),
      .tag_raw = as.character(.data[[tags_col]])
    ) %>%
    dplyr::mutate(.tag_raw = ifelse(is.na(.tag_raw), "", .tag_raw)) %>%
    tidyr::separate_rows(.tag_raw, sep = ",") %>%
    dplyr::mutate(tag_group = trimws(tolower(.data$.tag_raw))) %>%
    dplyr::filter(
      nzchar(.data$tag_group),
      !is.na(.data$views),
      is.finite(.data$views),
      .data$views >= min_views_per_video
    ) %>%
    dplyr::distinct(.data$.video_id, .data$tag_group, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(c(".video_id", ".video_title", "tag_group", "views")))

  if (nrow(exploded) == 0) {
    return(list(
      plot_data = tibble::tibble(
        .video_id = character(),
        .video_title = character(),
        tag_group = character(),
        views = numeric()
      ),
      summary = tibble::tibble(
        tag_group = character(),
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
    dplyr::group_by(.data$tag_group) %>%
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
        tag_group = character(),
        views = numeric()
      ),
      summary = summary
    ))
  }

  tag_levels <- summary$tag_group
  plot_data <- exploded %>%
    dplyr::filter(.data$tag_group %in% tag_levels) %>%
    dplyr::mutate(
      tag_group = factor(.data$tag_group, levels = rev(tag_levels), ordered = TRUE)
    ) %>%
    dplyr::select(dplyr::all_of(c(".video_id", ".video_title", "tag_group", "views")))

  summary <- summary %>%
    dplyr::mutate(tag_group = factor(.data$tag_group, levels = rev(tag_levels), ordered = TRUE)) %>%
    dplyr::arrange(.data$tag_group)

  list(plot_data = plot_data, summary = summary)
}

tag_view_distribution_plot <- function(tag_dist, talent) {
  if (!is.list(tag_dist) || !all(c("plot_data", "summary") %in% names(tag_dist))) {
    stop("tag_dist must be a list returned by tag_view_distribution_prep().")
  }

  plot_df <- tag_dist$plot_data
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          geom = "text",
          x = 1,
          y = 1,
          label = "No tag view distribution available for current filters."
        ) +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = bundle_a_wrap_text("Tag View Distribution", width = 58),
          subtitle = bundle_a_talent_subtitle(
            talent,
            "Top tags by average views per video."
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
        "<br>Tag: ", .data$tag_group,
        "<br>Views: ", scales::label_comma(accuracy = 1)(.data$views)
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$tag_group, y = .data$views, fill = .data$tag_group)) +
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
      title = bundle_a_wrap_text("Tag View Distribution", width = 58),
      subtitle = bundle_a_talent_subtitle(
        talent,
        "Top tags ranked by average views per video (box = IQR, line = median, point = mean)."
      ),
      x = "",
      y = "Views per video"
    )
}
