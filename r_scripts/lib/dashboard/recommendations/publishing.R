# Publishing recommendation rules.

dashboard_recommendations_publishing <- function(dashboard_data) {
  weekday <- dashboard_data$weekday_summary
  if (is.null(weekday) || nrow(weekday) == 0) {
    return(dashboard_recommendation(
      domain = "publishing",
      priority = "low",
      title = "Publishing timing evidence is unavailable",
      finding = "The active filters did not produce a weekday summary.",
      recommendation = "Avoid changing publishing cadence based on this view until more timing data is available.",
      evidence = "No weekday performance rows were available.",
      caveat = "Missing data may reflect the active date window, source exports, or content type filters.",
      confidence = "insufficient",
      supporting_plot_or_table = "Weekday Summary",
      rule_id = "publishing_weekday_missing",
      source_table = "weekday_summary",
      sort_order = 900,
      data_limit_flag = TRUE
    ))
  }

  metric_col <- dashboard_first_existing_col(weekday, c("MedianViewsPerVideo", "AverageViewsPerVideo"))
  count_col <- dashboard_views_count_col(weekday)
  if (is.null(metric_col) || is.null(count_col)) {
    return(dashboard_recommendation(
      domain = "publishing",
      priority = "low",
      title = "Weekday evidence is incomplete",
      finding = "The weekday summary is missing a per-video views metric or sample-size column.",
      recommendation = "Use the weekday table as context, but avoid a publishing recommendation from this rule.",
      evidence = "Required fields were not found in `weekday_summary`.",
      caveat = "This safeguard prevents timing recommendations from total-volume fields alone.",
      confidence = "insufficient",
      supporting_plot_or_table = "Weekday Summary",
      rule_id = "publishing_weekday_incomplete",
      source_table = "weekday_summary",
      sort_order = 901,
      data_limit_flag = TRUE
    ))
  }

  wk <- weekday %>%
    dplyr::mutate(
      .metric = suppressWarnings(as.numeric(.data[[metric_col]])),
      .n = suppressWarnings(as.numeric(.data[[count_col]])),
      .day = as.character(.data$day_of_week)
    ) %>%
    dplyr::filter(is.finite(.data$.metric), is.finite(.data$.n))
  eligible <- wk %>% dplyr::filter(.data$.n >= 3)
  if (nrow(eligible) == 0) {
    return(dashboard_recommendation(
      domain = "publishing",
      priority = "low",
      title = "Weekday sample sizes are too small",
      finding = "No weekday has at least three videos in the active filters.",
      recommendation = "Treat timing patterns as exploratory until more uploads are observed.",
      evidence = paste0("Largest weekday sample size: ", scales::comma(max(wk$.n, na.rm = TRUE))),
      caveat = "Small samples can be dominated by one unusual video, topic, event, or stream.",
      confidence = "insufficient",
      supporting_plot_or_table = "Weekday Summary",
      rule_id = "publishing_weekday_small_n",
      source_table = "weekday_summary",
      sample_size = max(wk$.n, na.rm = TRUE),
      sort_order = 902,
      data_limit_flag = TRUE
    ))
  }

  best <- eligible %>% dplyr::arrange(dplyr::desc(.data$.metric), dplyr::desc(.data$.n)) %>% dplyr::slice_head(n = 1)
  baseline <- stats::weighted.mean(wk$.metric, w = pmax(wk$.n, 1), na.rm = TRUE)
  lift <- if (is.finite(baseline) && baseline > 0) best$.metric[[1]] / baseline - 1 else NA_real_
  confidence <- dashboard_recommendation_confidence(best$.n[[1]], lift)
  priority <- dashboard_recommendation_priority(confidence, lift)
  day_label <- best$.day[[1]]
  metric_label <- if (identical(metric_col, "MedianViewsPerVideo")) "median views per video" else "average views per video"

  weekday_rec <- dashboard_recommendation(
    domain = "publishing",
    priority = priority,
    title = if (confidence %in% c("strong", "moderate")) {
      paste("Test", day_label, "for higher-priority uploads")
    } else {
      paste(day_label, "is an early timing signal")
    },
    finding = paste0(day_label, " had the strongest ", metric_label, " among weekdays with enough observations."),
    recommendation = if (confidence %in% c("strong", "moderate")) {
      paste0("Consider scheduling higher-effort uploads on ", day_label, " when the topic and production schedule fit.")
    } else {
      paste0("Treat ", day_label, " as worth testing, not as a settled publishing rule.")
    },
    evidence = paste0(
      stringr::str_to_sentence(metric_label), " were ",
      dashboard_format_metric(best$.metric[[1]], metric_label),
      if (is.finite(lift)) paste0(" (", scales::percent(lift, accuracy = 1), " vs the weighted weekday baseline)") else "",
      " across ", scales::comma(best$.n[[1]]), " videos."
    ),
    caveat = "This is descriptive, not causal. Weekday effects can be confounded with topic, event timing, format, or creator availability.",
    metric_name = metric_label,
    metric_value = best$.metric[[1]],
    comparison_value = baseline,
    sample_size = best$.n[[1]],
    confidence = confidence,
    supporting_plot_or_table = "Publishing Schedule: Weekday Summary",
    rule_id = "publishing_best_weekday",
    source_table = "weekday_summary",
    sort_order = 100,
    data_limit_flag = identical(confidence, "insufficient")
  )

  wk_dist <- dashboard_data$weekend_weekday_distribution
  weekend_rec <- NULL
  if (!is.null(wk_dist) && is.list(wk_dist) && !is.null(wk_dist$summary) && nrow(wk_dist$summary) > 0) {
    summary_df <- wk_dist$summary %>%
      dplyr::mutate(
        .metric = as.character(.data$metric),
        .group = as.character(.data$weekend_group),
        .mean = suppressWarnings(as.numeric(.data$Mean)),
        .n = suppressWarnings(as.numeric(.data$VideoCount))
      )
    views_cmp <- summary_df %>%
      dplyr::filter(.data$.metric %in% c("Views per video", "View per video"), .data$.group %in% c("Weekday", "Weekend"))
    if (nrow(views_cmp) == 2) {
      wide <- views_cmp %>%
        dplyr::select(group = ".group", mean_value = ".mean", video_count = ".n") %>%
        tidyr::pivot_wider(names_from = "group", values_from = c("mean_value", "video_count"))
      weekday_mean <- wide$mean_value_Weekday[[1]]
      weekend_mean <- wide$mean_value_Weekend[[1]]
      min_n <- min(wide$video_count_Weekday[[1]], wide$video_count_Weekend[[1]], na.rm = TRUE)
      lift2 <- if (is.finite(weekday_mean) && weekday_mean > 0) weekend_mean / weekday_mean - 1 else NA_real_
      confidence2 <- dashboard_recommendation_confidence(min_n, lift2, min_strong_n = 10, min_moderate_n = 5)
      stronger <- if (is.finite(lift2) && lift2 > 0) "weekends" else "weekdays"
      weekend_rec <- dashboard_recommendation(
        domain = "publishing",
        priority = dashboard_recommendation_priority(confidence2, lift2),
        title = paste("Compare", stronger, "as a publishing window"),
        finding = paste0(stringr::str_to_sentence(stronger), " currently show higher average views per video in the active window."),
        recommendation = if (confidence2 %in% c("strong", "moderate")) {
          paste0("Use this as a scheduling input when deciding whether to reserve stronger concepts for ", stronger, ".")
        } else {
          "Treat this as an early timing signal and continue collecting examples before changing cadence."
        },
        evidence = paste0(
          "Weekend average: ", dashboard_format_metric(weekend_mean, "views"),
          "; weekday average: ", dashboard_format_metric(weekday_mean, "views"),
          if (is.finite(lift2)) paste0("; weekend lift: ", scales::percent(lift2, accuracy = 1), ".") else "."
        ),
        caveat = "Weekend and weekday groups may differ by topic, format, promotion, or event context.",
        metric_name = "average views per video",
        metric_value = if (identical(stronger, "weekends")) weekend_mean else weekday_mean,
        comparison_value = if (identical(stronger, "weekends")) weekday_mean else weekend_mean,
        sample_size = min_n,
        confidence = confidence2,
        supporting_plot_or_table = "Publishing Schedule: Weekend vs Weekday Distribution",
        rule_id = "publishing_weekend_weekday",
        source_table = "weekend_weekday_distribution$summary",
        sort_order = 110
      )
    }
  }

  dashboard_recommendation_bind(weekday_rec, weekend_rec)
}
