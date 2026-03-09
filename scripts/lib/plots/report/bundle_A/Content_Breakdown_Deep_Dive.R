bundle_a_metric_summary <- function(
  df,
  group_col,
  metric_col,
  id_col = "Video ID",
  group_name = "group",
  metric_name = "metric"
) {
  if (!group_col %in% names(df)) {
    stop("Grouping column not found: ", group_col)
  }
  if (!metric_col %in% names(df)) {
    stop("Metric column not found: ", metric_col)
  }
  if (!id_col %in% names(df)) {
    stop("ID column not found: ", id_col)
  }

  out <- df %>%
    dplyr::mutate(
      .group = as.character(.data[[group_col]]),
      .metric = suppressWarnings(as.numeric(.data[[metric_col]]))
    ) %>%
    dplyr::filter(!is.na(.group), nzchar(trimws(.group))) %>%
    dplyr::group_by(.data$.group) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data[[id_col]]),
      Total = sum(.data$.metric, na.rm = TRUE),
      AveragePerVideo = mean(.data$.metric, na.rm = TRUE),
      MedianPerVideo = stats::median(.data$.metric, na.rm = TRUE),
      .groups = "drop"
    )

  names(out)[names(out) == ".group"] <- group_name
  names(out)[names(out) == "Total"] <- paste0("Total", metric_name)
  names(out)[names(out) == "AveragePerVideo"] <- paste0("Average", metric_name, "PerVideo")
  names(out)[names(out) == "MedianPerVideo"] <- paste0("Median", metric_name, "PerVideo")
  out
}

bundle_a_views_revenue_join <- function(views_df, revenue_df, by_col) {
  out <- dplyr::full_join(views_df, revenue_df, by = by_col) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "VideoCount.x", "VideoCount.y",
          "TotalViews", "TotalRevenue",
          "AverageViewsPerVideo", "AverageRevenuePerVideo",
          "MedianViewsPerVideo", "MedianRevenuePerVideo"
        )),
        ~ tidyr::replace_na(.x, 0)
      ),
      VideoCountViews = dplyr::coalesce(.data$VideoCount.x, 0),
      VideoCountRevenue = dplyr::coalesce(.data$VideoCount.y, 0)
    ) %>%
    dplyr::select(-dplyr::any_of(c("VideoCount.x", "VideoCount.y")))

  if ("TotalViews" %in% names(out)) {
    tv <- sum(out$TotalViews, na.rm = TRUE)
    out <- out %>%
      dplyr::mutate(
        ShareViews = if (is.finite(tv) && tv > 0) .data$TotalViews / tv else NA_real_
      )
  }
  if ("TotalRevenue" %in% names(out)) {
    tr <- sum(out$TotalRevenue, na.rm = TRUE)
    out <- out %>%
      dplyr::mutate(
        ShareRevenue = if (is.finite(tr) && tr > 0) .data$TotalRevenue / tr else NA_real_
      )
  }

  out
}

bundle_a_dual_metric_plot <- function(
  summary_df,
  group_col,
  talent,
  title,
  subtitle = NULL,
  value_col_views = "TotalViews",
  value_col_revenue = "TotalRevenue",
  y_label_views = "Views",
  y_label_revenue = "Revenue",
  as_share = FALSE,
  rotate_x = c("auto", "none", "45")
) {
  rotate_x <- match.arg(rotate_x)

  if (!group_col %in% names(summary_df)) {
    stop("Grouping column not found in summary_df: ", group_col)
  }

  views_col <- if (isTRUE(as_share)) "ShareViews" else value_col_views
  rev_col <- if (isTRUE(as_share)) "ShareRevenue" else value_col_revenue
  if (!all(c(views_col, rev_col) %in% names(summary_df))) {
    stop("Summary data missing required view/revenue columns.")
  }

  plot_df <- summary_df %>%
    dplyr::transmute(
      .group = as.character(.data[[group_col]]),
      Views = .data[[views_col]],
      Revenue = .data[[rev_col]]
    ) %>%
    tidyr::pivot_longer(
      cols = c("Views", "Revenue"),
      names_to = "metric",
      values_to = "value"
    )

  group_labels <- unique(as.character(plot_df$.group))
  max_label_chars <- if (length(group_labels) > 0) max(nchar(group_labels), na.rm = TRUE) else 0
  should_rotate <- if (rotate_x == "45") {
    TRUE
  } else if (rotate_x == "none") {
    FALSE
  } else {
    length(group_labels) > 6 || max_label_chars > 12
  }

  y_scale <- if (isTRUE(as_share)) scales::label_percent(accuracy = 1) else scales::label_comma()
  y_lab <- if (isTRUE(as_share)) "Share of total" else "Total"

  p <- plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.group, y = .data$value, fill = .data$.group)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 1) +
    ggplot2::scale_fill_grey(start = 0.35, end = 0.65) +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_y_continuous(labels = y_scale) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - ", title),
      subtitle = subtitle,
      x = "",
      y = y_lab
    )

  if (isTRUE(should_rotate)) {
    p <- p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
      )
  }

  p
}

weekend_vs_weekday_prep <- function(
  analytics_df,
  monetary_df,
  weekend_col = "is_weekend",
  date_col_analytics = NULL,
  date_col_monetary = NULL,
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue"
) {
  derive_weekend <- function(df, date_col = NULL) {
    if (weekend_col %in% names(df) && is.logical(df[[weekend_col]])) {
      return(df[[weekend_col]])
    }
    if (is.null(date_col)) {
      date_col <- bundle_a_pick_col(df, c("publish_date", "Published At", "date"), label = "date column")
    }
    d <- bundle_a_as_date(df[[date_col]])
    as.logical(lubridate::wday(d) %in% c(1, 7))
  }

  a <- analytics_df %>%
    dplyr::mutate(weekend_group = ifelse(derive_weekend(analytics_df, date_col_analytics), "Weekend", "Weekday"))
  m <- monetary_df %>%
    dplyr::mutate(weekend_group = ifelse(derive_weekend(monetary_df, date_col_monetary), "Weekend", "Weekday"))

  views_sum <- bundle_a_metric_summary(
    a, group_col = "weekend_group", metric_col = views_col, id_col = id_col,
    group_name = "weekend_group", metric_name = "Views"
  )
  rev_sum <- bundle_a_metric_summary(
    m, group_col = "weekend_group", metric_col = revenue_col, id_col = id_col,
    group_name = "weekend_group", metric_name = "Revenue"
  )

  bundle_a_views_revenue_join(views_sum, rev_sum, by_col = "weekend_group") %>%
    dplyr::mutate(
      weekend_group = factor(.data$weekend_group, levels = c("Weekday", "Weekend"), ordered = TRUE)
    ) %>%
    dplyr::arrange(.data$weekend_group)
}

weekend_vs_weekday_plot <- function(summary_df, talent, as_share = FALSE) {
  bundle_a_dual_metric_plot(
    summary_df = summary_df,
    group_col = "weekend_group",
    talent = talent,
    title = if (isTRUE(as_share)) "Weekend vs Weekday Share" else "Weekend vs Weekday Performance",
    subtitle = "Views and revenue compared by publish day type.",
    as_share = as_share
  )
}

day_of_week_performance_prep <- function(
  analytics_df,
  monetary_df,
  date_col_analytics = NULL,
  date_col_monetary = NULL,
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue"
) {
  get_weekday <- function(df, date_col = NULL) {
    if (is.null(date_col)) {
      date_col <- bundle_a_pick_col(df, c("publish_date", "Published At", "date"), label = "date column")
    }
    d <- bundle_a_as_date(df[[date_col]])
    as.character(lubridate::wday(d, label = TRUE, abbr = TRUE, week_start = 1))
  }

  a <- analytics_df %>% dplyr::mutate(day_of_week = get_weekday(analytics_df, date_col_analytics))
  m <- monetary_df %>% dplyr::mutate(day_of_week = get_weekday(monetary_df, date_col_monetary))

  views_sum <- bundle_a_metric_summary(
    a, group_col = "day_of_week", metric_col = views_col, id_col = id_col,
    group_name = "day_of_week", metric_name = "Views"
  )
  rev_sum <- bundle_a_metric_summary(
    m, group_col = "day_of_week", metric_col = revenue_col, id_col = id_col,
    group_name = "day_of_week", metric_name = "Revenue"
  )

  day_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  bundle_a_views_revenue_join(views_sum, rev_sum, by_col = "day_of_week") %>%
    dplyr::mutate(day_of_week = factor(.data$day_of_week, levels = day_levels, ordered = TRUE)) %>%
    dplyr::arrange(.data$day_of_week)
}

day_of_week_performance_plot <- function(summary_df, talent, as_share = FALSE) {
  bundle_a_dual_metric_plot(
    summary_df = summary_df,
    group_col = "day_of_week",
    talent = talent,
    title = if (isTRUE(as_share)) "Day-of-Week Share" else "Day-of-Week Performance",
    subtitle = "Views and revenue compared by publish weekday.",
    as_share = as_share
  )
}

collaboration_effectiveness_prep <- function(
  analytics_df,
  monetary_df,
  collab_col = "collaborative_energy",
  tags_col = "tags",
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue"
) {
  derive_collab <- function(df) {
    if (collab_col %in% names(df) && is.logical(df[[collab_col]])) {
      return(df[[collab_col]])
    }
    if (tags_col %in% names(df)) {
      tags <- tolower(as.character(df[[tags_col]]))
      return(grepl("collab|collaboration|duo|guest", tags))
    }
    rep(FALSE, nrow(df))
  }

  a <- analytics_df %>%
    dplyr::mutate(collab_group = ifelse(derive_collab(analytics_df), "Collaborative", "Non-collaborative"))
  m <- monetary_df %>%
    dplyr::mutate(collab_group = ifelse(derive_collab(monetary_df), "Collaborative", "Non-collaborative"))

  views_sum <- bundle_a_metric_summary(
    a, group_col = "collab_group", metric_col = views_col, id_col = id_col,
    group_name = "collab_group", metric_name = "Views"
  )
  rev_sum <- bundle_a_metric_summary(
    m, group_col = "collab_group", metric_col = revenue_col, id_col = id_col,
    group_name = "collab_group", metric_name = "Revenue"
  )

  out <- bundle_a_views_revenue_join(views_sum, rev_sum, by_col = "collab_group") %>%
    dplyr::mutate(
      collab_group = factor(
        .data$collab_group,
        levels = c("Non-collaborative", "Collaborative"),
        ordered = TRUE
      )
    ) %>%
    dplyr::arrange(.data$collab_group)

  base_views <- out$AverageViewsPerVideo[out$collab_group == "Non-collaborative"]
  base_rev <- out$AverageRevenuePerVideo[out$collab_group == "Non-collaborative"]
  out %>%
    dplyr::mutate(
      AvgViewsLiftVsNonCollab = if (
        length(base_views) == 1 && is.finite(base_views) && base_views > 0
      ) {
        .data$AverageViewsPerVideo / base_views - 1
      } else {
        NA_real_
      },
      AvgRevenueLiftVsNonCollab = if (
        length(base_rev) == 1 && is.finite(base_rev) && base_rev > 0
      ) {
        .data$AverageRevenuePerVideo / base_rev - 1
      } else {
        NA_real_
      }
    )
}

collaboration_effectiveness_plot <- function(summary_df, talent, as_share = FALSE) {
  bundle_a_dual_metric_plot(
    summary_df = summary_df,
    group_col = "collab_group",
    talent = talent,
    title = if (isTRUE(as_share)) "Collaboration Share" else "Collaboration Effectiveness",
    subtitle = "Views and revenue compared for collaborative vs non-collaborative titles.",
    as_share = as_share
  )
}

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

  clean_topic <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | !nzchar(x)] <- "(unclassified)"
    x
  }

  exploded <- analytics_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      topic_group = clean_topic(.data[[topic_col]]),
      views = suppressWarnings(as.numeric(.data[[views_col]]))
    ) %>%
    dplyr::filter(nzchar(.data$topic_group), !is.na(.data$views), is.finite(.data$views)) %>%
    dplyr::distinct(.data$.video_id, .data$topic_group, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(c(".video_id", "topic_group", "views")))

  if (nrow(exploded) == 0) {
    return(list(
      plot_data = tibble::tibble(topic_group = character(), views = numeric()),
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
      plot_data = tibble::tibble(topic_group = character(), views = numeric()),
      summary = summary
    ))
  }

  topic_levels <- summary$topic_group
  plot_data <- exploded %>%
    dplyr::filter(.data$topic_group %in% topic_levels) %>%
    dplyr::mutate(
      topic_group = factor(.data$topic_group, levels = rev(topic_levels), ordered = TRUE)
    ) %>%
    dplyr::select(dplyr::all_of(c("topic_group", "views")))

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
          title = paste0(talent, " - Topic View Distribution"),
          subtitle = "Top topics by average views per video."
        )
    )
  }

  plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$topic_group, y = .data$views)) +
    ggplot2::geom_boxplot(
      fill = "grey70",
      color = "grey20",
      width = 0.65,
      outlier.alpha = 0.35
    ) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 2.5,
      stroke = 0.2,
      fill = "black",
      color = "white"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Topic View Distribution"),
      subtitle = "Top topics ranked by average views per video (box = IQR, line = median, point = mean).",
      x = "",
      y = "Views per video"
    )
}

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

tag_view_distribution_prep <- function(
  analytics_df,
  tags_col = "tags",
  id_col = "Video ID",
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

  exploded <- analytics_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
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
    dplyr::select(dplyr::all_of(c(".video_id", "tag_group", "views")))

  if (nrow(exploded) == 0) {
    return(list(
      plot_data = tibble::tibble(tag_group = character(), views = numeric()),
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
      plot_data = tibble::tibble(tag_group = character(), views = numeric()),
      summary = summary
    ))
  }

  tag_levels <- summary$tag_group
  plot_data <- exploded %>%
    dplyr::filter(.data$tag_group %in% tag_levels) %>%
    dplyr::mutate(
      tag_group = factor(.data$tag_group, levels = rev(tag_levels), ordered = TRUE)
    ) %>%
    dplyr::select(dplyr::all_of(c("tag_group", "views")))

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
          title = paste0(talent, " - Tag View Distribution"),
          subtitle = "Top tags by average views per video."
        )
    )
  }

  plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$tag_group, y = .data$views)) +
    ggplot2::geom_boxplot(
      fill = "grey70",
      color = "grey20",
      width = 0.65,
      outlier.alpha = 0.35
    ) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 2.5,
      stroke = 0.2,
      fill = "black",
      color = "white"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Tag View Distribution"),
      subtitle = "Top tags ranked by average views per video (box = IQR, line = median, point = mean).",
      x = "",
      y = "Views per video"
    )
}
