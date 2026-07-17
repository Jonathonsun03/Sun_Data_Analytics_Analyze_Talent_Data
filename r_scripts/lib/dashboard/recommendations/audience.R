# Audience recommendation rules.

dashboard_recommendations_audience <- function(dashboard_data) {
  aud <- dashboard_data$audience_summary
  audience_rec <- NULL
  if (!is.null(aud) && nrow(aud) > 0 && all(c(".age", ".gender", "share") %in% names(aud))) {
    latest_period <- max(aud$.period, na.rm = TRUE)
    latest <- aud %>%
      dplyr::filter(.data$.period == latest_period) %>%
      dplyr::mutate(.share = suppressWarnings(as.numeric(.data$share)), .videos = suppressWarnings(as.numeric(.data$videos))) %>%
      dplyr::filter(is.finite(.data$.share)) %>%
      dplyr::arrange(dplyr::desc(.data$.share)) %>%
      dplyr::slice_head(n = 1)
    if (nrow(latest) == 1) {
      confidence <- dashboard_recommendation_confidence(latest$.videos[[1]], latest$.share[[1]], min_strong_n = 8, min_moderate_n = 4)
      audience_rec <- dashboard_recommendation(
        domain = "audience",
        priority = dashboard_recommendation_priority(confidence, latest$.share[[1]]),
        title = paste("Primary audience segment:", latest$.age[[1]], latest$.gender[[1]]),
        finding = paste0(latest$.age[[1]], " / ", latest$.gender[[1]], " is the largest observed age-gender share in the latest audience period."),
        recommendation = "Use this segment as context for packaging, topic framing, and audience-growth tests.",
        evidence = paste0("Latest observed share: ", scales::percent(latest$.share[[1]], accuracy = 0.1), " across ", scales::comma(latest$.videos[[1]]), " videos."),
        caveat = "Audience demographics are percentage/share exports, not raw viewer counts, and can shift with missing or partial exports.",
        metric_name = "audience share",
        metric_value = latest$.share[[1]],
        sample_size = latest$.videos[[1]],
        confidence = confidence,
        supporting_plot_or_table = "Audience: Age and Gender Trends",
        rule_id = "audience_top_segment",
        source_table = "audience_summary",
        sort_order = 300
      )
    }
  }

  geo_rec <- NULL
  geo <- dashboard_data$audience_geography
  if (!is.null(geo) && nrow(geo) > 0 && all(c("snapshot_date", "country_code", "metric_value") %in% names(geo))) {
    latest_geo <- geo %>%
      dplyr::filter(.data$snapshot_date == max(.data$snapshot_date, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(.data$metric_value)) %>%
      dplyr::slice_head(n = 1)
    if (nrow(latest_geo) == 1) {
      label <- if ("country_name" %in% names(latest_geo) && !is.na(latest_geo$country_name[[1]])) latest_geo$country_name[[1]] else latest_geo$country_code[[1]]
      metric_label <- attr(geo, "metric_label", exact = TRUE)
      if (is.null(metric_label)) metric_label <- "audience metric"
      geo_rec <- dashboard_recommendation(
        domain = "audience",
        priority = "medium",
        title = paste("Prioritize geography context:", label),
        finding = paste0(label, " is the largest available geography signal in the latest audience geography snapshot."),
        recommendation = "Use this region as context for scheduling, localization, reference choices, or audience-development experiments.",
        evidence = paste0(metric_label, ": ", latest_geo$metric_display[[1]], "."),
        caveat = "Geography exports may be share-based or estimated, and unmatched country codes are omitted from the map.",
        metric_name = metric_label,
        metric_value = latest_geo$metric_value[[1]],
        confidence = "moderate",
        supporting_plot_or_table = "Audience: Geography",
        rule_id = "audience_top_geography",
        source_table = "audience_geography",
        sort_order = 310
      )
    }
  }

  fallback <- NULL
  if (is.null(audience_rec) && is.null(geo_rec)) {
    fallback <- dashboard_recommendation(
      domain = "audience",
      priority = "low",
      title = "Audience recommendation data is limited",
      finding = "Audience age/gender or geography summaries were unavailable for the active filters.",
      recommendation = "Avoid audience-growth recommendations until audience export coverage improves.",
      evidence = "No audience recommendation rule produced a supported signal.",
      caveat = "Audience data availability depends on platform export coverage and privacy thresholds.",
      confidence = "insufficient",
      supporting_plot_or_table = "Audience",
      rule_id = "audience_fallback",
      source_table = "audience_summary; audience_geography",
      sort_order = 960,
      data_limit_flag = TRUE
    )
  }
  dashboard_recommendation_bind(audience_rec, geo_rec, fallback)
}
