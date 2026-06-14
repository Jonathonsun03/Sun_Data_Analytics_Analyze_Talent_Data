dashboard_recommendation_empty <- function() {
  tibble::tibble(
    domain = character(),
    priority = character(),
    title = character(),
    finding = character(),
    recommendation = character(),
    evidence = character(),
    caveat = character(),
    metric_name = character(),
    metric_value = numeric(),
    comparison_value = numeric(),
    sample_size = numeric(),
    confidence = character(),
    supporting_plot_or_table = character(),
    rule_id = character(),
    source_table = character(),
    sort_order = numeric(),
    data_limit_flag = logical()
  )
}

dashboard_recommendation <- function(
  domain,
  priority,
  title,
  finding,
  recommendation,
  evidence,
  caveat,
  metric_name = NA_character_,
  metric_value = NA_real_,
  comparison_value = NA_real_,
  sample_size = NA_real_,
  confidence = "weak",
  supporting_plot_or_table = NA_character_,
  rule_id = NA_character_,
  source_table = NA_character_,
  sort_order = 999,
  data_limit_flag = FALSE
) {
  tibble::tibble(
    domain = domain,
    priority = priority,
    title = title,
    finding = finding,
    recommendation = recommendation,
    evidence = evidence,
    caveat = caveat,
    metric_name = metric_name,
    metric_value = as.numeric(metric_value),
    comparison_value = as.numeric(comparison_value),
    sample_size = as.numeric(sample_size),
    confidence = confidence,
    supporting_plot_or_table = supporting_plot_or_table,
    rule_id = rule_id,
    source_table = source_table,
    sort_order = as.numeric(sort_order),
    data_limit_flag = isTRUE(data_limit_flag)
  )
}

dashboard_recommendation_confidence <- function(sample_size, lift = NA_real_, min_strong_n = 8, min_moderate_n = 4) {
  sample_size <- suppressWarnings(as.numeric(sample_size))
  lift <- suppressWarnings(as.numeric(lift))
  if (is.na(sample_size) || sample_size < 2) {
    return("insufficient")
  }
  if (sample_size >= min_strong_n && is.finite(lift) && abs(lift) >= 0.20) {
    return("strong")
  }
  if (sample_size >= min_moderate_n && (!is.finite(lift) || abs(lift) >= 0.10)) {
    return("moderate")
  }
  "weak"
}

dashboard_recommendation_priority <- function(confidence, lift = NA_real_) {
  lift <- suppressWarnings(as.numeric(lift))
  if (identical(confidence, "strong") && (!is.finite(lift) || abs(lift) >= 0.20)) {
    return("high")
  }
  if (confidence %in% c("strong", "moderate")) {
    return("medium")
  }
  "low"
}

dashboard_recommendation_bind <- function(...) {
  items <- list(...)
  items <- items[!purrr::map_lgl(items, is.null)]
  items <- items[purrr::map_int(items, nrow) > 0]
  if (length(items) == 0) {
    return(dashboard_recommendation_empty())
  }
  dplyr::bind_rows(items)
}

dashboard_first_existing_col <- function(df, candidates) {
  if (is.null(df)) {
    return(NULL)
  }
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit)) NULL else hit
}

dashboard_format_metric <- function(value, metric_name = "") {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    return("not available")
  }
  if (grepl("revenue|dollar|\\$", metric_name, ignore.case = TRUE)) {
    return(scales::dollar(value, accuracy = 0.01))
  }
  if (grepl("share|rate|lift|percentage|pct", metric_name, ignore.case = TRUE)) {
    return(scales::percent(value, accuracy = 0.1))
  }
  scales::comma(value, accuracy = 1)
}

dashboard_views_count_col <- function(df) {
  dashboard_first_existing_col(df, c("VideoCount", "VideoCountViews", "video_count", "videos", "tracked_videos"))
}

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

dashboard_build_recommendations <- function(dashboard_data) {
  all <- dashboard_recommendation_bind(
    dashboard_recommendations_publishing(dashboard_data),
    dashboard_recommendations_content_strategy(dashboard_data),
    dashboard_recommendations_audience(dashboard_data),
    dashboard_recommendations_performance(dashboard_data)
  ) %>%
    dplyr::mutate(
      priority = factor(.data$priority, levels = c("high", "medium", "low"), ordered = TRUE),
      confidence = factor(.data$confidence, levels = c("strong", "moderate", "weak", "insufficient"), ordered = TRUE)
    ) %>%
    dplyr::arrange(.data$data_limit_flag, .data$priority, .data$confidence, .data$sort_order) %>%
    dplyr::mutate(priority = as.character(.data$priority), confidence = as.character(.data$confidence))

  actionable <- all %>% dplyr::filter(!.data$data_limit_flag, .data$confidence != "insufficient")
  top <- actionable %>% dplyr::slice_head(n = 5)
  caveats <- all %>%
    dplyr::filter(.data$data_limit_flag | .data$confidence %in% c("weak", "insufficient")) %>%
    dplyr::select("domain", "title", "finding", "caveat", "confidence", "rule_id")

  list(
    all = all,
    top = top,
    publishing = all %>% dplyr::filter(.data$domain == "publishing"),
    content = all %>% dplyr::filter(.data$domain == "content"),
    audience = all %>% dplyr::filter(.data$domain == "audience"),
    performance = all %>% dplyr::filter(.data$domain == "performance"),
    caveats = caveats
  )
}
