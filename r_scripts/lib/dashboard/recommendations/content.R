# Content-strategy recommendation rules.

dashboard_top_group_recommendation <- function(df, domain, group_col, rule_id, title_prefix, source_table, supporting_plot_or_table, sort_order) {
  if (is.null(df) || nrow(df) == 0 || !group_col %in% names(df)) {
    return(NULL)
  }
  metric_col <- dashboard_first_existing_col(df, c("AverageViewsPerVideo", "MedianViewsPerVideo", "AverageRevenuePerVideo"))
  count_col <- dashboard_views_count_col(df)
  if (is.null(metric_col) || is.null(count_col)) {
    return(NULL)
  }
  dat <- df %>%
    dplyr::mutate(
      .group = as.character(.data[[group_col]]),
      .metric = suppressWarnings(as.numeric(.data[[metric_col]])),
      .n = suppressWarnings(as.numeric(.data[[count_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$.group), nzchar(.data$.group), .data$.group != "Other topics", is.finite(.data$.metric), is.finite(.data$.n))
  eligible <- dat %>% dplyr::filter(.data$.n >= 3)
  if (nrow(eligible) == 0) {
    return(NULL)
  }
  best <- eligible %>% dplyr::arrange(dplyr::desc(.data$.metric), dplyr::desc(.data$.n)) %>% dplyr::slice_head(n = 1)
  baseline <- stats::weighted.mean(dat$.metric, w = pmax(dat$.n, 1), na.rm = TRUE)
  lift <- if (is.finite(baseline) && baseline > 0) best$.metric[[1]] / baseline - 1 else NA_real_
  confidence <- dashboard_recommendation_confidence(best$.n[[1]], lift)
  metric_label <- dplyr::case_when(
    metric_col == "AverageRevenuePerVideo" ~ "average revenue per video",
    metric_col == "MedianViewsPerVideo" ~ "median views per video",
    TRUE ~ "average views per video"
  )
  dashboard_recommendation(
    domain = domain,
    priority = dashboard_recommendation_priority(confidence, lift),
    title = paste(title_prefix, best$.group[[1]]),
    finding = paste0("`", best$.group[[1]], "` has the strongest ", metric_label, " among groups with enough examples."),
    recommendation = if (confidence %in% c("strong", "moderate")) {
      paste0("Use `", best$.group[[1]], "` as a candidate for repeatable testing, while checking fit with audience and production constraints.")
    } else {
      paste0("Treat `", best$.group[[1]], "` as an early signal that deserves more examples before scaling.")
    },
    evidence = paste0(
      stringr::str_to_sentence(metric_label), ": ",
      dashboard_format_metric(best$.metric[[1]], metric_label),
      if (is.finite(lift)) paste0(" (", scales::percent(lift, accuracy = 1), " vs the weighted group baseline)") else "",
      " across ", scales::comma(best$.n[[1]]), " videos."
    ),
    caveat = "Groups can overlap with format, publishing timing, collaboration, and title effects. This rule is descriptive.",
    metric_name = metric_label,
    metric_value = best$.metric[[1]],
    comparison_value = baseline,
    sample_size = best$.n[[1]],
    confidence = confidence,
    supporting_plot_or_table = supporting_plot_or_table,
    rule_id = rule_id,
    source_table = source_table,
    sort_order = sort_order
  )
}

dashboard_recommendations_content_strategy <- function(dashboard_data) {
  topic_rec <- dashboard_top_group_recommendation(
    dashboard_data$content_strategy$topic_summary,
    domain = "content",
    group_col = "topic_group",
    rule_id = "content_top_topic",
    title_prefix = "Test more around topic:",
    source_table = "content_strategy$topic_summary",
    supporting_plot_or_table = "Content Strategy: Topic Performance",
    sort_order = 200
  )
  tag_rec <- dashboard_top_group_recommendation(
    dashboard_data$content_strategy$tag_summary,
    domain = "content",
    group_col = "tag_group",
    rule_id = "content_top_tag",
    title_prefix = "Watch tag signal:",
    source_table = "content_strategy$tag_summary",
    supporting_plot_or_table = "Content Strategy: Tag Performance",
    sort_order = 210
  )

  collab_rec <- NULL
  collab <- dashboard_data$content_strategy$collab_summary
  if (!is.null(collab) && nrow(collab) > 0 && all(c("collab_group", "AverageViewsPerVideo") %in% names(collab))) {
    dat <- collab %>%
      dplyr::mutate(.group = as.character(.data$collab_group), .views = suppressWarnings(as.numeric(.data$AverageViewsPerVideo)))
    collaborative <- dat %>% dplyr::filter(.data$.group == "Collaborative")
    baseline <- dat %>% dplyr::filter(.data$.group == "Non-collaborative")
    if (nrow(collaborative) == 1 && nrow(baseline) == 1) {
      lift <- if (is.finite(baseline$.views[[1]]) && baseline$.views[[1]] > 0) collaborative$.views[[1]] / baseline$.views[[1]] - 1 else NA_real_
      count_col <- dashboard_views_count_col(collab)
      n_collab <- if (!is.null(count_col)) suppressWarnings(as.numeric(collaborative[[count_col]][[1]])) else NA_real_
      confidence <- dashboard_recommendation_confidence(n_collab, lift, min_strong_n = 5, min_moderate_n = 3)
      collab_rec <- dashboard_recommendation(
        domain = "content",
        priority = dashboard_recommendation_priority(confidence, lift),
        title = "Evaluate collaboration lift",
        finding = if (is.finite(lift) && lift > 0) "Collaborative uploads are outperforming non-collaborative uploads on average views per video." else "Collaborative uploads are not clearly outperforming non-collaborative uploads on average views per video.",
        recommendation = if (is.finite(lift) && lift > 0) "Consider more collaboration tests in formats where the guest or partner naturally fits the audience." else "Do not increase collaborations based on this signal alone; prioritize fit and concept quality.",
        evidence = paste0(
          "Collaborative average: ", dashboard_format_metric(collaborative$.views[[1]], "views"),
          "; non-collaborative average: ", dashboard_format_metric(baseline$.views[[1]], "views"),
          if (is.finite(lift)) paste0("; lift: ", scales::percent(lift, accuracy = 1), ".") else "."
        ),
        caveat = "Collaboration labels can be imperfect, and collaborations may cluster in specific topics or event types.",
        metric_name = "average views per video",
        metric_value = collaborative$.views[[1]],
        comparison_value = baseline$.views[[1]],
        sample_size = n_collab,
        confidence = confidence,
        supporting_plot_or_table = "Content Strategy: Collaboration Performance",
        rule_id = "content_collaboration_signal",
        source_table = "content_strategy$collab_summary",
        sort_order = 220
      )
    }
  }

  fallback <- NULL
  if (is.null(topic_rec) && is.null(tag_rec) && is.null(collab_rec)) {
    fallback <- dashboard_recommendation(
      domain = "content",
      priority = "low",
      title = "Content strategy evidence is limited",
      finding = "Topic, tag, and collaboration summaries did not meet the first-pass recommendation thresholds.",
      recommendation = "Use the Content Strategy tab descriptively and collect more examples before making a strong content recommendation.",
      evidence = "No content recommendation rule produced a supported signal.",
      caveat = "This first pass only uses a small set of high-confidence rules.",
      confidence = "insufficient",
      supporting_plot_or_table = "Content Strategy",
      rule_id = "content_strategy_fallback",
      source_table = "content_strategy",
      sort_order = 950,
      data_limit_flag = TRUE
    )
  }
  dashboard_recommendation_bind(topic_rec, tag_rec, collab_rec, fallback)
}
