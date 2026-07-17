# Performance recommendation rules.

dashboard_recommendations_performance <- function(dashboard_data) {
  top_videos <- dashboard_data$top_videos
  analytics <- dashboard_data$source_data$analytics
  concentration_rec <- NULL
  if (!is.null(top_videos) && nrow(top_videos) > 0 && !is.null(analytics) && "views" %in% names(top_videos) && "views" %in% names(analytics)) {
    total_views <- sum(suppressWarnings(as.numeric(analytics$views)), na.rm = TRUE)
    top_n <- min(5, nrow(top_videos))
    top_share <- sum(suppressWarnings(as.numeric(top_videos$views[seq_len(top_n)])), na.rm = TRUE) / total_views
    if (is.finite(top_share)) {
      confidence <- dashboard_recommendation_confidence(top_n, top_share, min_strong_n = 5, min_moderate_n = 3)
      concentration_rec <- dashboard_recommendation(
        domain = "performance",
        priority = if (top_share >= 0.50) "high" else "medium",
        title = "Check reliance on top-performing uploads",
        finding = paste0("The top ", top_n, " videos account for ", scales::percent(top_share, accuracy = 1), " of observed views in the active window."),
        recommendation = if (top_share >= 0.50) {
          "Study the highest-performing videos for repeatable topics, titles, formats, and distribution context."
        } else {
          "Use top videos as examples, but avoid assuming performance depends on only a few outliers."
        },
        evidence = paste0("Top-", top_n, " view share: ", scales::percent(top_share, accuracy = 1), "."),
        caveat = "This is a concentration signal based on observed rows; it does not explain why those videos performed well.",
        metric_name = "top video view share",
        metric_value = top_share,
        comparison_value = 1,
        sample_size = top_n,
        confidence = confidence,
        supporting_plot_or_table = "Overview: Top Videos",
        rule_id = "performance_top_video_concentration",
        source_table = "top_videos; source_data$analytics",
        sort_order = 400
      )
    }
  }

  type_rec <- NULL
  avg_type <- dashboard_data$content_summary$average_views
  if (!is.null(avg_type) && nrow(avg_type) > 0 && all(c("Content Type", "Average_Views", "VideoCount") %in% names(avg_type))) {
    dat <- avg_type %>%
      dplyr::mutate(.avg = suppressWarnings(as.numeric(.data$Average_Views)), .n = suppressWarnings(as.numeric(.data$VideoCount))) %>%
      dplyr::filter(is.finite(.data$.avg), is.finite(.data$.n))
    eligible <- dat %>% dplyr::filter(.data$.n >= 3)
    if (nrow(eligible) > 0) {
      best <- eligible %>% dplyr::arrange(dplyr::desc(.data$.avg), dplyr::desc(.data$.n)) %>% dplyr::slice_head(n = 1)
      baseline <- stats::weighted.mean(dat$.avg, w = pmax(dat$.n, 1), na.rm = TRUE)
      lift <- if (is.finite(baseline) && baseline > 0) best$.avg[[1]] / baseline - 1 else NA_real_
      confidence <- dashboard_recommendation_confidence(best$.n[[1]], lift)
      type_rec <- dashboard_recommendation(
        domain = "performance",
        priority = dashboard_recommendation_priority(confidence, lift),
        title = paste("Repeat format tests:", best$`Content Type`[[1]]),
        finding = paste0(best$`Content Type`[[1]], " has the strongest average views per video among formats with enough examples."),
        recommendation = "Use this format as a candidate for repeatable testing, while checking whether the advantage holds by topic and timing.",
        evidence = paste0("Average views per video: ", dashboard_format_metric(best$.avg[[1]], "views"), if (is.finite(lift)) paste0(" (", scales::percent(lift, accuracy = 1), " vs format baseline)") else "", " across ", scales::comma(best$.n[[1]]), " videos."),
        caveat = "Format performance can be influenced by topic mix, upload age, collaboration, and promotion.",
        metric_name = "average views per video",
        metric_value = best$.avg[[1]],
        comparison_value = baseline,
        sample_size = best$.n[[1]],
        confidence = confidence,
        supporting_plot_or_table = "Overview: Average View % / Content Type Views",
        rule_id = "performance_content_type_average_views",
        source_table = "content_summary$average_views",
        sort_order = 410
      )
    }
  }

  fallback <- NULL
  if (is.null(concentration_rec) && is.null(type_rec)) {
    fallback <- dashboard_recommendation(
      domain = "performance",
      priority = "low",
      title = "Performance recommendation evidence is limited",
      finding = "Top-video concentration or content-type average summaries were unavailable or too sparse.",
      recommendation = "Use performance charts descriptively until more rows are available.",
      evidence = "No performance recommendation rule produced a supported signal.",
      caveat = "This first pass avoids performance recommendations from total volume alone.",
      confidence = "insufficient",
      supporting_plot_or_table = "Overview; Creator Performance",
      rule_id = "performance_fallback",
      source_table = "top_videos; content_summary",
      sort_order = 970,
      data_limit_flag = TRUE
    )
  }
  dashboard_recommendation_bind(concentration_rec, type_rec, fallback)
}
