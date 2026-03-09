bundle_b_priority_rank_plot <- function(strength_df, talent) {
  if (!all(c("Content_Type", "Composite_Score", "Performance_Band") %in% names(strength_df))) {
    stop("strength_df must include: Content_Type, Composite_Score, Performance_Band")
  }

  strength_df %>%
    dplyr::mutate(Content_Type = stats::reorder(.data$Content_Type, .data$Composite_Score)) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$Content_Type,
        y = .data$Composite_Score,
        fill = .data$Performance_Band
      )
    ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(
        "Strength" = "grey35",
        "Middle" = "grey60",
        "Weakness / Improve" = "grey80"
      )
    ) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Content Priority Ranking"),
      subtitle = "Composite score combines normalized views, revenue, and median engagement.",
      x = "Content type",
      y = "Composite score",
      fill = "Band"
    )
}

bundle_b_content_position_distribution_prep <- function(
  analytics_df,
  monetary_df,
  id_col = "Video ID",
  content_col = "Content Type",
  views_col = "views",
  engagement_col = "averageViewPercentage",
  revenue_col = "Estimated Revenue"
) {
  required_analytics <- c(id_col, views_col, engagement_col)
  if (!all(required_analytics %in% names(analytics_df))) {
    stop("analytics_df must include: ", paste(required_analytics, collapse = ", "))
  }
  if (!id_col %in% names(monetary_df) || !revenue_col %in% names(monetary_df)) {
    stop("monetary_df must include: ", id_col, ", ", revenue_col)
  }

  safe_median <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    x_num <- x_num[is.finite(x_num)]
    if (length(x_num) == 0) return(NA_real_)
    stats::median(x_num, na.rm = TRUE)
  }

  analytics_video <- analytics_df %>%
    dplyr::transmute(
      .video_id = as.character(.data[[id_col]]),
      .content = if (content_col %in% names(analytics_df)) as.character(.data[[content_col]]) else NA_character_,
      .views = suppressWarnings(as.numeric(.data[[views_col]])),
      .eng_pct = suppressWarnings(as.numeric(.data[[engagement_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$.video_id), nzchar(.data$.video_id)) %>%
    dplyr::mutate(
      .content = trimws(.data$.content),
      .content = dplyr::if_else(is.na(.data$.content) | !nzchar(.data$.content), "(unclassified)", .data$.content)
    ) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(
      Content_Type = dplyr::first(.data$.content),
      Views = safe_median(.data$.views),
      EngagementPct = safe_median(.data$.eng_pct),
      .groups = "drop"
    )

  revenue_by_video <- monetary_df %>%
    dplyr::transmute(
      .video_id = as.character(.data[[id_col]]),
      .revenue = suppressWarnings(as.numeric(.data[[revenue_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$.video_id), nzchar(.data$.video_id)) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(Revenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop")

  video_df <- analytics_video %>%
    dplyr::left_join(revenue_by_video, by = ".video_id") %>%
    dplyr::mutate(Revenue = tidyr::replace_na(.data$Revenue, 0))

  metric_levels <- c("Views per video", "Revenue per video ($)", "Engagement rate (%)")
  metric_long <- dplyr::bind_rows(
    video_df %>%
      dplyr::transmute(Content_Type = .data$Content_Type, Metric = metric_levels[[1]], Value = .data$Views),
    video_df %>%
      dplyr::transmute(Content_Type = .data$Content_Type, Metric = metric_levels[[2]], Value = .data$Revenue),
    video_df %>%
      dplyr::transmute(Content_Type = .data$Content_Type, Metric = metric_levels[[3]], Value = .data$EngagementPct)
  ) %>%
    dplyr::filter(is.finite(.data$Value)) %>%
    dplyr::filter(
      !(.data$Metric %in% metric_levels[1:2] & .data$Value <= 0)
    ) %>%
    dplyr::mutate(Metric = factor(.data$Metric, levels = metric_levels, ordered = TRUE))

  if (nrow(metric_long) == 0) {
    empty_summary <- tibble::tibble(
      Metric = factor(character(), levels = metric_levels, ordered = TRUE),
      Content_Type = character(),
      VideoCount = integer(),
      MedianValue = numeric(),
      MeanValue = numeric(),
      Q25 = numeric(),
      Q75 = numeric(),
      MedianPercentile = numeric(),
      Performance_Band = character()
    )
    return(list(metric_long = metric_long, summary = empty_summary))
  }

  summary_df <- metric_long %>%
    dplyr::group_by(.data$Metric, .data$Content_Type) %>%
    dplyr::summarize(
      VideoCount = dplyr::n(),
      MedianValue = stats::median(.data$Value, na.rm = TRUE),
      MeanValue = mean(.data$Value, na.rm = TRUE),
      Q25 = stats::quantile(.data$Value, probs = 0.25, na.rm = TRUE, names = FALSE),
      Q75 = stats::quantile(.data$Value, probs = 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      metric_long %>%
        dplyr::group_by(.data$Metric) %>%
        dplyr::summarize(.metric_values = list(.data$Value), .groups = "drop"),
      by = "Metric"
    ) %>%
    dplyr::mutate(
      MedianPercentile = purrr::map2_dbl(
        .data$.metric_values,
        .data$MedianValue,
        ~ {
          if (length(.x) == 0 || !is.finite(.y)) return(NA_real_)
          as.numeric(stats::ecdf(.x)(.y))
        }
      )
    ) %>%
    dplyr::select(-dplyr::all_of(".metric_values")) %>%
    dplyr::group_by(.data$Metric) %>%
    dplyr::mutate(
      .n_content = dplyr::n_distinct(.data$Content_Type),
      .high_cut = stats::quantile(.data$MedianPercentile, probs = 0.67, na.rm = TRUE, names = FALSE),
      .low_cut = stats::quantile(.data$MedianPercentile, probs = 0.33, na.rm = TRUE, names = FALSE),
      Performance_Band = dplyr::case_when(
        .data$.n_content < 3 ~ "Middle",
        .data$MedianPercentile >= .data$.high_cut ~ "Strength",
        .data$MedianPercentile <= .data$.low_cut ~ "Weakness / Improve",
        TRUE ~ "Middle"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::any_of(c(".n_content", ".high_cut", ".low_cut"))) %>%
    dplyr::arrange(.data$Metric, dplyr::desc(.data$MedianPercentile), .data$Content_Type)

  list(
    metric_long = metric_long,
    summary = summary_df
  )
}

bundle_b_content_position_distribution_plot <- function(position_data, talent) {
  if (!is.list(position_data) || !all(c("metric_long", "summary") %in% names(position_data))) {
    stop("position_data must be the output of bundle_b_content_position_distribution_prep().")
  }

  metric_long <- position_data$metric_long
  summary_df <- position_data$summary

  if (nrow(metric_long) == 0 || nrow(summary_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No content distribution data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(title = paste0(talent, " - Content Position in Overall Distribution"))
    )
  }

  summary_df <- summary_df %>%
    dplyr::mutate(
      Content_Type = factor(
        .data$Content_Type,
        levels = unique(.data$Content_Type[order(.data$Metric, dplyr::desc(.data$MedianPercentile))])
      )
    )

  metric_long %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Value)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(count)),
      bins = 35,
      fill = "grey82",
      color = "grey45",
      linewidth = 0.4,
      alpha = 0.9
    ) +
    ggplot2::geom_vline(
      data = summary_df,
      ggplot2::aes(xintercept = .data$MedianValue, linetype = .data$Content_Type),
      linewidth = 0.6,
      color = "grey20",
      alpha = 0.9,
      show.legend = TRUE
    ) +
    ggplot2::facet_wrap(~Metric, scales = "free_x", ncol = 1) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(big.mark = ",", accuracy = 1)
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Content Type Position in Full Distribution"),
      subtitle = "Grey bars = count of videos in each value range. Vertical lines = content type medians.",
      x = "Per-video value (raw units)",
      y = "Video count",
      linetype = "Content type"
    )
}

bundle_b_content_position_overall_summary <- function(position_data) {
  if (!is.list(position_data) || !"summary" %in% names(position_data)) {
    stop("position_data must be the output of bundle_b_content_position_distribution_prep().")
  }

  summary_df <- position_data$summary
  if (nrow(summary_df) == 0) {
    return(
      tibble::tibble(
        Content_Type = character(),
        MetricsCovered = integer(),
        ViewsMedianPercentile = numeric(),
        RevenueMedianPercentile = numeric(),
        EngagementMedianPercentile = numeric(),
        AvgMedianPercentile = numeric(),
        Performance_Band = character()
      )
    )
  }

  metric_wide <- summary_df %>%
    dplyr::mutate(
      MetricKey = dplyr::case_when(
        .data$Metric == "Views per video" ~ "ViewsMedianPercentile",
        .data$Metric == "Revenue per video ($)" ~ "RevenueMedianPercentile",
        .data$Metric == "Engagement rate (%)" ~ "EngagementMedianPercentile",
        TRUE ~ as.character(.data$Metric)
      )
    ) %>%
    dplyr::select(Content_Type, MetricKey, MedianPercentile) %>%
    tidyr::pivot_wider(names_from = "MetricKey", values_from = "MedianPercentile")

  out <- summary_df %>%
    dplyr::group_by(.data$Content_Type) %>%
    dplyr::summarize(
      MetricsCovered = dplyr::n_distinct(.data$Metric),
      AvgMedianPercentile = mean(.data$MedianPercentile, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(metric_wide, by = "Content_Type")

  if (nrow(out) < 3) {
    return(
      out %>%
        dplyr::mutate(Performance_Band = "Middle") %>%
        dplyr::arrange(dplyr::desc(.data$AvgMedianPercentile))
    )
  }

  high_cut <- stats::quantile(out$AvgMedianPercentile, probs = 0.67, na.rm = TRUE, names = FALSE)
  low_cut <- stats::quantile(out$AvgMedianPercentile, probs = 0.33, na.rm = TRUE, names = FALSE)

  out %>%
    dplyr::mutate(
      Performance_Band = dplyr::case_when(
        .data$AvgMedianPercentile >= high_cut ~ "Strength",
        .data$AvgMedianPercentile <= low_cut ~ "Weakness / Improve",
        TRUE ~ "Middle"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$AvgMedianPercentile))
}

bundle_b_strength_matrix_plot <- function(strength_df, talent) {
  required_cols <- c(
    "Content_Type",
    "Median_Engagement",
    "Total_Revenue",
    "Total_Views",
    "Performance_Band"
  )
  if (!all(required_cols %in% names(strength_df))) {
    stop(
      "strength_df must include: ",
      paste(required_cols, collapse = ", ")
    )
  }

  strength_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$Median_Engagement,
        y = .data$Total_Revenue,
        size = .data$Total_Views,
        color = .data$Performance_Band
      )
    ) +
    ggplot2::geom_point(alpha = 0.85) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$Content_Type),
      hjust = 0,
      nudge_x = 0.005,
      size = 3,
      check_overlap = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Strength" = "grey20",
        "Middle" = "grey50",
        "Weakness / Improve" = "grey75"
      )
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(scale = 1)) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Content Opportunity Matrix"),
      subtitle = "X = median engagement, Y = total revenue, bubble size = total views.",
      x = "Median engagement",
      y = "Total revenue",
      size = "Total views",
      color = "Band"
    )
}

bundle_b_revenue_efficiency_plot <- function(strength_df, talent) {
  if (!all(c("Content_Type", "Total_Revenue", "Total_Views") %in% names(strength_df))) {
    stop("strength_df must include: Content_Type, Total_Revenue, Total_Views")
  }

  efficiency_df <- strength_df %>%
    dplyr::mutate(
      Revenue_Per_1K_Views = dplyr::if_else(
        .data$Total_Views > 0,
        .data$Total_Revenue / .data$Total_Views * 1000,
        NA_real_
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Revenue_Per_1K_Views))

  efficiency_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(.data$Content_Type, .data$Revenue_Per_1K_Views),
        y = .data$Revenue_Per_1K_Views
      )
    ) +
    ggplot2::geom_col(fill = "grey45", width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(scale = 1)) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Revenue Efficiency by Content Type"),
      subtitle = "Estimated revenue generated per 1,000 views.",
      x = "Content type",
      y = "Revenue per 1,000 views"
    )
}

bundle_b_collaboration_lift_plot <- function(collab_summary, talent) {
  required_cols <- c(
    "collab_group",
    "AvgViewsLiftVsNonCollab",
    "AvgRevenueLiftVsNonCollab"
  )
  if (!all(required_cols %in% names(collab_summary))) {
    stop("collab_summary must include: ", paste(required_cols, collapse = ", "))
  }

  lift_df <- collab_summary %>%
    dplyr::filter(.data$collab_group == "Collaborative")

  if (nrow(lift_df) > 0) {
    lift_df <- tibble::tibble(
      Metric = c("Average Views Lift", "Average Revenue Lift"),
      Lift = c(lift_df$AvgViewsLiftVsNonCollab[[1]], lift_df$AvgRevenueLiftVsNonCollab[[1]])
    )
  }

  if (nrow(lift_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No collaboration lift data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = paste0(talent, " - Collaboration Lift"),
          subtitle = "Lift relative to non-collaborative content."
        )
    )
  }

  lift_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$Metric,
        y = .data$Lift
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey35", linewidth = 0.3) +
    ggplot2::geom_col(fill = "grey45", width = 0.6) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Collaboration Lift vs Non-Collab"),
      subtitle = "Positive values indicate collaborative content outperforms baseline.",
      x = "",
      y = "Lift vs non-collaborative"
    )
}

bundle_b_day_of_week_lift_plot <- function(day_summary, talent) {
  required_cols <- c("day_of_week", "AverageViewsPerVideo", "AverageRevenuePerVideo")
  if (!all(required_cols %in% names(day_summary))) {
    stop("day_summary must include: ", paste(required_cols, collapse = ", "))
  }
  day_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  day_levels_present <- day_levels[day_levels %in% as.character(day_summary$day_of_week)]
  if (length(day_levels_present) == 0) {
    day_levels_present <- unique(as.character(day_summary$day_of_week))
  }

  base_views <- mean(day_summary$AverageViewsPerVideo, na.rm = TRUE)
  base_rev <- mean(day_summary$AverageRevenuePerVideo, na.rm = TRUE)

  lift_df <- day_summary %>%
    dplyr::transmute(
      day_of_week = factor(
        as.character(.data$day_of_week),
        levels = day_levels_present,
        ordered = TRUE
      ),
      ViewsLift = if (is.finite(base_views) && base_views > 0) {
        .data$AverageViewsPerVideo / base_views - 1
      } else {
        rep(NA_real_, dplyr::n())
      },
      RevenueLift = if (is.finite(base_rev) && base_rev > 0) {
        .data$AverageRevenuePerVideo / base_rev - 1
      } else {
        rep(NA_real_, dplyr::n())
      }
    ) %>%
    tidyr::pivot_longer(
      cols = c("ViewsLift", "RevenueLift"),
      names_to = "Metric",
      values_to = "Lift"
    ) %>%
    dplyr::mutate(
      Metric = dplyr::recode(
        .data$Metric,
        ViewsLift = "Views lift",
        RevenueLift = "Revenue lift"
      ),
      day_of_week = factor(
        as.character(.data$day_of_week),
        levels = day_levels_present,
        ordered = TRUE
      )
    ) %>%
    dplyr::arrange(.data$day_of_week, .data$Metric)

  lift_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$day_of_week, y = .data$Lift, fill = .data$Metric)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey35", linewidth = 0.3) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.6) +
    ggplot2::scale_fill_grey(start = 0.35, end = 0.65) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Day-of-Week Lift"),
      subtitle = "Lift in average per-video performance vs overall day-of-week mean.",
      x = "",
      y = "Lift vs baseline",
      fill = "Metric"
    )
}

bundle_b_attribute_opportunity_prep <- function(
  analytics_df,
  monetary_df,
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue",
  engagement_col = "averageViewPercentage",
  content_col = "Content Type",
  topic_col = "topic",
  primary_reference_col = "primary_reference",
  tags_col = "tags",
  top_n_content = 8,
  top_n_labels = 8,
  top_n_tags = 10,
  min_videos = 2
) {
  required_analytics <- c(id_col, views_col, engagement_col)
  if (!all(required_analytics %in% names(analytics_df))) {
    stop(
      "analytics_df must include: ",
      paste(required_analytics, collapse = ", ")
    )
  }
  if (!id_col %in% names(monetary_df) || !revenue_col %in% names(monetary_df)) {
    stop("monetary_df must include: ", id_col, ", ", revenue_col)
  }

  clean_label <- function(topic_value, ref_value) {
    t <- trimws(as.character(topic_value))
    r <- trimws(as.character(ref_value))
    t[is.na(t) | !nzchar(t)] <- ""
    r[is.na(r) | !nzchar(r)] <- ""
    out <- dplyr::if_else(nzchar(r), r, t)
    out[!nzchar(out)] <- "(unclassified)"
    tolower(out)
  }

  analytics_base <- analytics_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .views = suppressWarnings(as.numeric(.data[[views_col]])),
      .eng = suppressWarnings(as.numeric(.data[[engagement_col]])) / 100,
      .content = if (content_col %in% names(analytics_df)) as.character(.data[[content_col]]) else NA_character_,
      .topic = if (topic_col %in% names(analytics_df)) .data[[topic_col]] else NA_character_,
      .ref = if (primary_reference_col %in% names(analytics_df)) .data[[primary_reference_col]] else NA_character_,
      .label = clean_label(.topic, .ref),
      .tags = if (tags_col %in% names(analytics_df)) as.character(.data[[tags_col]]) else NA_character_
    ) %>%
    dplyr::filter(!is.na(.video_id), nzchar(.video_id), is.finite(.views), is.finite(.eng)) %>%
    dplyr::select(dplyr::all_of(c(".video_id", ".views", ".eng", ".content", ".label", ".tags")))

  revenue_by_video <- monetary_df %>%
    dplyr::transmute(
      .video_id = as.character(.data[[id_col]]),
      .revenue = suppressWarnings(as.numeric(.data[[revenue_col]]))
    ) %>%
    dplyr::filter(!is.na(.video_id), nzchar(.video_id)) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(.revenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop")

  label_video <- analytics_base %>%
    dplyr::distinct(.data$.video_id, .data$.label, .keep_all = TRUE)

  content_video <- analytics_base %>%
    dplyr::mutate(.content = trimws(as.character(.data$.content))) %>%
    dplyr::mutate(.content = dplyr::if_else(is.na(.data$.content) | !nzchar(.data$.content), "(unclassified)", .data$.content)) %>%
    dplyr::distinct(.data$.video_id, .data$.content, .keep_all = TRUE)

  content_summary <- content_video %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data$.video_id),
      TotalViews = sum(.data$.views, na.rm = TRUE),
      MedianEngagement = stats::median(.data$.eng, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      content_video %>%
        dplyr::select(dplyr::all_of(c(".video_id", ".content"))) %>%
        dplyr::left_join(revenue_by_video, by = ".video_id") %>%
        dplyr::group_by(.data$.content) %>%
        dplyr::summarize(TotalRevenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop"),
      by = ".content"
    ) %>%
    dplyr::mutate(
      TotalRevenue = tidyr::replace_na(.data$TotalRevenue, 0),
      Attribute = tolower(.data$.content),
      LabelType = "Content type"
    ) %>%
    dplyr::filter(.data$VideoCount >= min_videos) %>%
    dplyr::arrange(dplyr::desc(.data$TotalViews + .data$TotalRevenue)) %>%
    dplyr::slice_head(n = top_n_content)

  label_summary <- label_video %>%
    dplyr::group_by(.data$.label) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data$.video_id),
      TotalViews = sum(.data$.views, na.rm = TRUE),
      MedianEngagement = stats::median(.data$.eng, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      label_video %>%
        dplyr::select(dplyr::all_of(c(".video_id", ".label"))) %>%
        dplyr::left_join(revenue_by_video, by = ".video_id") %>%
        dplyr::group_by(.data$.label) %>%
        dplyr::summarize(TotalRevenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop"),
      by = ".label"
    ) %>%
    dplyr::mutate(
      TotalRevenue = tidyr::replace_na(.data$TotalRevenue, 0),
      Attribute = .data$.label,
      LabelType = "Title label"
    ) %>%
    dplyr::filter(.data$VideoCount >= min_videos) %>%
    dplyr::arrange(dplyr::desc(.data$TotalViews + .data$TotalRevenue)) %>%
    dplyr::slice_head(n = top_n_labels)

  tag_video <- analytics_base %>%
    dplyr::mutate(.tags = ifelse(is.na(.data$.tags), "", .data$.tags)) %>%
    tidyr::separate_rows(".tags", sep = ",") %>%
    dplyr::mutate(.tag = trimws(tolower(.data$.tags))) %>%
    dplyr::filter(nzchar(.data$.tag)) %>%
    dplyr::distinct(.data$.video_id, .data$.tag, .keep_all = TRUE)

  tag_summary <- tag_video %>%
    dplyr::group_by(.data$.tag) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data$.video_id),
      TotalViews = sum(.data$.views, na.rm = TRUE),
      MedianEngagement = stats::median(.data$.eng, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      tag_video %>%
        dplyr::select(dplyr::all_of(c(".video_id", ".tag"))) %>%
        dplyr::left_join(revenue_by_video, by = ".video_id") %>%
        dplyr::group_by(.data$.tag) %>%
        dplyr::summarize(TotalRevenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop"),
      by = ".tag"
    ) %>%
    dplyr::mutate(
      TotalRevenue = tidyr::replace_na(.data$TotalRevenue, 0),
      Attribute = .data$.tag,
      LabelType = "Tag"
    ) %>%
    dplyr::filter(.data$VideoCount >= min_videos) %>%
    dplyr::arrange(dplyr::desc(.data$TotalViews + .data$TotalRevenue)) %>%
    dplyr::slice_head(n = top_n_tags)

  out <- dplyr::bind_rows(
    content_summary %>%
      dplyr::transmute(
        Attribute = .data$Attribute,
        LabelType = .data$LabelType,
        VideoCount = .data$VideoCount,
        TotalViews = .data$TotalViews,
        TotalRevenue = .data$TotalRevenue,
        MedianEngagement = .data$MedianEngagement
      ),
    label_summary %>%
      dplyr::transmute(
        Attribute = .data$Attribute,
        LabelType = .data$LabelType,
        VideoCount = .data$VideoCount,
        TotalViews = .data$TotalViews,
        TotalRevenue = .data$TotalRevenue,
        MedianEngagement = .data$MedianEngagement
      ),
    tag_summary %>%
      dplyr::transmute(
        Attribute = .data$Attribute,
        LabelType = .data$LabelType,
        VideoCount = .data$VideoCount,
        TotalViews = .data$TotalViews,
        TotalRevenue = .data$TotalRevenue,
        MedianEngagement = .data$MedianEngagement
      )
  )

  if (nrow(out) == 0) {
    return(
      out %>%
        dplyr::mutate(
          Views_Z = numeric(),
          Revenue_Z = numeric(),
          Engagement_Z = numeric(),
          Composite_Score = numeric(),
          Performance_Band = character()
        )
    )
  }

  safe_zscore_local <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    if (length(stats::na.omit(x_num)) <= 1 || isTRUE(all.equal(stats::sd(x_num, na.rm = TRUE), 0))) {
      return(rep(0, length(x_num)))
    }
    as.numeric(scale(x_num))
  }

  out <- out %>%
    dplyr::mutate(
      AvgViewsPerVideo = dplyr::if_else(
        .data$VideoCount > 0,
        .data$TotalViews / .data$VideoCount,
        NA_real_
      ),
      AvgRevenuePerVideo = dplyr::if_else(
        .data$VideoCount > 0,
        .data$TotalRevenue / .data$VideoCount,
        NA_real_
      ),
      Views_Z = safe_zscore_local(.data$AvgViewsPerVideo),
      Revenue_Z = safe_zscore_local(.data$AvgRevenuePerVideo),
      Engagement_Z = safe_zscore_local(.data$MedianEngagement),
      Composite_Score = rowMeans(
        dplyr::across(c("Views_Z", "Revenue_Z", "Engagement_Z")),
        na.rm = TRUE
      )
    )

  if (nrow(out) < 3) {
    return(
      out %>%
        dplyr::mutate(Performance_Band = "Middle") %>%
        dplyr::arrange(dplyr::desc(.data$Composite_Score))
    )
  }

  high_cut <- stats::quantile(out$Composite_Score, probs = 0.67, na.rm = TRUE, names = FALSE)
  low_cut <- stats::quantile(out$Composite_Score, probs = 0.33, na.rm = TRUE, names = FALSE)

  out %>%
    dplyr::mutate(
      Performance_Band = dplyr::case_when(
        .data$Composite_Score >= high_cut ~ "Strength",
        .data$Composite_Score <= low_cut ~ "Weakness / Improve",
        TRUE ~ "Middle"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Composite_Score))
}

bundle_b_attribute_opportunity_matrix_plot <- function(
  attr_df,
  talent,
  add_labels = TRUE,
  use_repel = TRUE
) {
  required_cols <- c(
    "Attribute",
    "VideoCount",
    "AvgViewsPerVideo",
    "AvgRevenuePerVideo",
    "MedianEngagement",
    "Performance_Band"
  )
  if (!all(required_cols %in% names(attr_df))) {
    stop("attr_df must include: ", paste(required_cols, collapse = ", "))
  }

  if (nrow(attr_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No attribute opportunity data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = paste0(talent, " - Opportunity Matrix by Tags and Title Labels")
        )
    )
  }

  plot_df <- attr_df %>%
    dplyr::filter(
      is.finite(.data$MedianEngagement),
      is.finite(.data$AvgRevenuePerVideo),
      is.finite(.data$AvgViewsPerVideo),
      .data$AvgViewsPerVideo > 0,
      .data$AvgRevenuePerVideo > 0
    ) %>%
    dplyr::mutate(
      HoverText = paste0(
        "Attribute: ", .data$Attribute,
        "<br>Type: ", .data$LabelType,
        "<br>Band: ", .data$Performance_Band,
        "<br>Videos: ", scales::comma(.data$VideoCount),
        "<br>Median Engagement: ", scales::percent(.data$MedianEngagement, accuracy = 0.1),
        "<br>Avg Views/Video: ", scales::comma(.data$AvgViewsPerVideo),
        "<br>Avg Revenue/Video: ", scales::dollar(.data$AvgRevenuePerVideo),
        "<br>Total Views: ", scales::comma(.data$TotalViews),
        "<br>Total Revenue: ", scales::dollar(.data$TotalRevenue)
      )
    )

  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No non-zero revenue points available for matrix.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = paste0(talent, " - Combined Opportunity Matrix")
        )
    )
  }

  p <- plot_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$MedianEngagement,
        y = .data$AvgRevenuePerVideo,
        size = .data$VideoCount,
        shape = .data$Performance_Band,
        text = .data$HoverText
      )
    ) +
    ggplot2::geom_point(alpha = 0.8, color = "grey30") +
    ggplot2::scale_shape_manual(
      values = c(
        "Strength" = 17,
        "Middle" = 16,
        "Weakness / Improve" = 15
      )
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_y_continuous(
      labels = scales::label_dollar(scale = 1)
    ) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Combined Opportunity Matrix"),
      subtitle = "Content types, tags, and title labels in one map. Y-axis uses avg revenue per video (linear scale).",
      x = "Median engagement",
      y = "Average revenue per video",
      size = "Video count",
      shape = "Performance band"
    )

  if (isTRUE(add_labels)) {
    if (isTRUE(use_repel) && requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_label_repel(
        ggplot2::aes(label = .data$Attribute),
        size = 2.3,
        color = "grey20",
        fill = "white",
        label.size = 0.1,
        box.padding = 0.2,
        point.padding = 0.2,
        min.segment.length = 0,
        max.overlaps = Inf,
        seed = 123,
        show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data$Attribute),
        size = 2.8,
        check_overlap = TRUE,
        nudge_x = 0.002,
        show.legend = FALSE
      )
    }
  }

  p
}

bundle_b_attribute_opportunity_matrix_plotly <- function(attr_df, talent) {
  required_cols <- c(
    "Attribute",
    "LabelType",
    "VideoCount",
    "AvgViewsPerVideo",
    "AvgRevenuePerVideo",
    "MedianEngagement",
    "Performance_Band"
  )
  if (!all(required_cols %in% names(attr_df))) {
    stop("attr_df must include: ", paste(required_cols, collapse = ", "))
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive matrix.")
  }

  plot_df <- attr_df %>%
    dplyr::filter(
      is.finite(.data$MedianEngagement),
      is.finite(.data$AvgRevenuePerVideo),
      is.finite(.data$AvgViewsPerVideo),
      .data$AvgViewsPerVideo > 0,
      .data$AvgRevenuePerVideo > 0
    ) %>%
    dplyr::mutate(
      Performance_Band = factor(
        .data$Performance_Band,
        levels = c("Strength", "Middle", "Weakness / Improve")
      ),
      HoverText = paste0(
        "<b>", .data$Attribute, "</b>",
        "<br>Type: ", .data$LabelType,
        "<br>Band: ", .data$Performance_Band,
        "<br>Videos: ", scales::comma(.data$VideoCount),
        "<br>Median Engagement: ", scales::percent(.data$MedianEngagement, accuracy = 0.1),
        "<br>Avg Views/Video: ", scales::comma(.data$AvgViewsPerVideo),
        "<br>Avg Revenue/Video: ", scales::dollar(.data$AvgRevenuePerVideo),
        "<br>Total Views: ", scales::comma(.data$TotalViews),
        "<br>Total Revenue: ", scales::dollar(.data$TotalRevenue)
      )
    )

  if (nrow(plot_df) == 0) {
    return(plotly::plotly_empty(type = "scatter", mode = "markers"))
  }

  plotly::plot_ly(
    data = plot_df,
    x = ~MedianEngagement,
    y = ~AvgRevenuePerVideo,
    type = "scatter",
    mode = "markers",
    symbol = ~Performance_Band,
    symbols = c("triangle-up", "circle", "square"),
    size = ~VideoCount,
    sizes = c(8, 36),
    marker = list(
      color = "rgba(90,90,90,0.75)",
      line = list(color = "rgba(50,50,50,1)", width = 1),
      sizemode = "area"
    ),
    text = ~HoverText,
    hovertemplate = "%{text}<extra></extra>"
  ) %>%
    plotly::layout(
      title = list(text = paste0(talent, " - Combined Opportunity Matrix (Interactive)")),
      xaxis = list(
        title = "Median engagement",
        tickformat = ".0%"
      ),
      yaxis = list(
        title = "Average revenue per video",
        tickprefix = "$",
        separatethousands = TRUE,
        tickformat = ",.0f"
      ),
      legend = list(title = list(text = "Performance band"))
    )
}
