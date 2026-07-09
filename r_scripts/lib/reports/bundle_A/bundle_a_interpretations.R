bundle_a_interpretation_slots <- function() {
  c(
    "05_report_bookends/01_executive_summary",
    "01_overall_performance_snapshot/00_section_synthesis",
    "01_overall_performance_snapshot/01_views_by_content_type",
    "01_overall_performance_snapshot/02_combined_performance_trends",
    "01_overall_performance_snapshot/03_total_revenue_by_content_type",
    "01_overall_performance_snapshot/04_engagement_distribution_successful_videos",
    "02_trends_over_time/00_section_synthesis",
    "02_trends_over_time/01_revenue_over_time_by_content_type",
    "02_trends_over_time/02_total_views_over_time_by_content_type",
    "03_audience_composition/00_section_synthesis",
    "03_audience_composition/01_audience_age_gender_trends",
    "04_content_strategy_deep_dive/00_section_synthesis",
    "04_content_strategy_deep_dive/01_weekend_vs_weekday",
    "04_content_strategy_deep_dive/02_day_of_week_distribution",
    "04_content_strategy_deep_dive/03_collaboration_effectiveness",
    "04_content_strategy_deep_dive/04_topic_performance",
    "04_content_strategy_deep_dive/05_tag_performance",
    "04_content_strategy_deep_dive/06_average_views_per_tag",
    "05_report_bookends/02_conclusion"
  )
}

bundle_a_priority <- function(confidence, action_level) {
  if (identical(action_level, "scale_or_prioritize") && confidence == "strong") {
    return("high")
  }
  if (action_level %in% c("controlled_test", "scale_or_prioritize") || confidence == "moderate") {
    return("medium")
  }
  "low"
}

bundle_a_action_level <- function(
  sample_size,
  lift = NA_real_,
  confidence = "weak",
  min_controlled = 4,
  min_scale = 8,
  controlled_lift = 0.10,
  scale_lift = 0.20,
  sparse_review_max = 3
) {
  sample_size <- report_interp_num(sample_size)
  lift <- abs(report_interp_num(lift))
  if (!is.finite(sample_size) || sample_size < 2 || identical(confidence, "insufficient")) {
    return("do_not_recommend")
  }
  if (sample_size <= sparse_review_max) {
    return("review_examples")
  }
  if (sample_size >= min_scale && is.finite(lift) && lift >= scale_lift && confidence == "strong") {
    return("scale_or_prioritize")
  }
  if (sample_size >= min_controlled && (!is.finite(lift) || lift >= controlled_lift) && confidence %in% c("moderate", "strong")) {
    return("controlled_test")
  }
  "monitor"
}

bundle_a_display_label <- function(x, sentence_start = FALSE) {
  x <- report_interp_clean_label(x)
  key <- tolower(trimws(x))
  known <- c(
    live = "Live streams",
    short = "Shorts",
    shorts = "Shorts",
    video = "Standard videos",
    videos = "Standard videos",
    personality_short = "Personality short-form content",
    "male age25-34" = "Male viewers ages 25-34",
    vtuber = "VTuber",
    survival = "Survival",
    collaborative = "Collaborative uploads",
    "non-collaborative" = "Non-collaborative uploads",
    noncollaborative = "Non-collaborative uploads",
    weekday = "Weekday uploads",
    weekend = "Weekend uploads"
  )
  if (key %in% names(known)) {
    return(known[[key]])
  }
  weekday <- c(
    Mon = "Monday",
    Tue = "Tuesday",
    Wed = "Wednesday",
    Thu = "Thursday",
    Fri = "Friday",
    Sat = "Saturday",
    Sun = "Sunday"
  )
  if (x %in% names(weekday)) {
    return(weekday[[x]])
  }
  if (grepl("_", x)) {
    return(paste0("`", x, "`"))
  }
  if (isTRUE(sentence_start)) {
    if (grepl("^[a-z0-9-]+$", x) && nchar(x) > 18) {
      return(paste0("`", x, "`"))
    }
    return(stringr::str_to_title(gsub("-", " ", x)))
  }
  x
}

bundle_a_recommendation_summary <- function(items) {
  items <- trimws(as.character(items))
  items <- unique(items[nzchar(items) & !is.na(items)])
  if (length(items) == 0) {
    return("Start with the strongest supported signal, then check the underlying videos before changing strategy.")
  }
  if (length(items) == 1) {
    return(items[[1]])
  }
  items <- items[seq_len(min(3, length(items)))]
  to_summary_sentence <- function(txt, index) {
    txt <- sub("[.]\\s*$", "", txt)
    if (grepl("^Prioritize this group for monetization review", txt)) {
      return("Start with a monetization review of this group, focusing on whether revenue strength comes from repeatable formats, topics, or individual high-value uploads.")
    }
    if (grepl("^Prioritize this as a repeat-testing lane", txt)) {
      return("Next, inspect this as a repeat-testing lane, focusing on title structure, topic, pacing, and posting context.")
    }
    if (grepl("^Treat this as a repeat candidate", txt)) {
      return("Also treat this as a repeat candidate after checking which title framing and format pairing supported the result.")
    }
    if (grepl("^Run a controlled timing test", txt)) {
      return("Then check per-video performance before testing the timing pattern with topic and format held as steady as practical.")
    }
    if (grepl("^Prioritize review of the strongest collaborations", txt)) {
      return("Start with the strongest collaborations, focusing on guest fit, audience overlap, format, and promotional context.")
    }
    if (grepl("^Prioritize this tag for follow-up review", txt)) {
      return("Review this tag alongside the underlying videos, since tags overlap and should not be read as independent categories.")
    }
    if (grepl("^Prioritize follow-up around", txt)) {
      txt <- sub("^Prioritize follow-up around\\s+", "Review ", txt)
    }
    if (index == 1) {
      return(paste0(txt, "."))
    }
    prefix <- c("Next, ", "Also ")[[min(index - 1, 2)]]
    paste0(prefix, tolower(substr(txt, 1, 1)), substr(txt, 2, nchar(txt)), ".")
  }
  items <- vapply(seq_along(items), function(i) {
    to_summary_sentence(items[[i]], i)
  }, character(1))
  paste(items, collapse = " ")
}

bundle_a_interpretation <- function(
  slot,
  title,
  finding,
  why_it_matters,
  evidence,
  recommendation,
  next_check,
  caveat,
  confidence = c("weak", "moderate", "strong", "insufficient"),
  action_level = c("monitor", "controlled_test", "scale_or_prioritize", "review_examples", "do_not_recommend"),
  priority = NULL,
  sample_size = NA_real_,
  metric_name = NA_character_,
  metric_value = NA_real_,
  comparison_name = NA_character_,
  comparison_value = NA_real_,
  source_table = NA_character_,
  rule_id = NA_character_,
  fallback = FALSE
) {
  confidence <- match.arg(confidence)
  action_level <- match.arg(action_level)
  if (is.null(priority)) {
    priority <- bundle_a_priority(confidence, action_level)
  }
  report_interp_result(
    slot = slot,
    title = title,
    finding = finding,
    why_it_matters = why_it_matters,
    evidence = evidence,
    implication = recommendation,
    recommendation = recommendation,
    next_check = next_check,
    caveat = caveat,
    confidence = confidence,
    priority = priority,
    action_level = action_level,
    sample_size = sample_size,
    metric_name = metric_name,
    metric_value = metric_value,
    comparison_name = comparison_name,
    comparison_value = comparison_value,
    source_table = source_table,
    rule_id = rule_id,
    fallback = fallback
  )
}

bundle_a_fallback <- function(slot, title, purpose, source_table = NA_character_, rule_id = NA_character_) {
  bundle_a_interpretation(
    slot = slot,
    title = title,
    finding = purpose,
    why_it_matters = "The report can still use this view to frame what should be checked next, but the current evidence is not stable enough for a specific planning recommendation.",
    evidence = "The required metric, comparison group, or sample size was missing, ambiguous, or too sparse for a supported finding.",
    recommendation = "Do not change content strategy from this slot alone; monitor the next report or inspect the underlying rows before acting.",
    next_check = "Check whether the selected reporting window includes enough videos, groups, or periods for this view.",
    caveat = "This fallback is intentionally conservative so missing or sparse data does not become a misleading recommendation.",
    confidence = "insufficient",
    action_level = "do_not_recommend",
    source_table = source_table,
    rule_id = rule_id,
    fallback = TRUE
  )
}

bundle_a_baseline_phrase <- function(signal, formatter = report_interp_fmt_number) {
  paste0("the median comparison group is ", formatter(signal$peer_median))
}

bundle_a_metric_safe_table <- function(
  slot,
  metric_name,
  metric_family,
  metric_unit,
  metric_value,
  group_name = NA_character_,
  sample_size = NA_real_,
  comparison_name = NA_character_,
  comparison_value = NA_real_,
  period_label = NA_character_,
  period_start = as.Date(NA),
  period_end = as.Date(NA),
  source_table = NA_character_
) {
  tibble::tibble(
    slot = slot,
    period_label = period_label,
    period_start = as.Date(period_start),
    period_end = as.Date(period_end),
    metric_name = metric_name,
    metric_family = metric_family,
    metric_unit = metric_unit,
    metric_value = as.numeric(metric_value),
    comparison_name = comparison_name,
    comparison_value = as.numeric(comparison_value),
    sample_size = as.numeric(sample_size),
    group_name = group_name,
    source_table = source_table
  )
}

bundle_a_validate_metric_family <- function(df, metric_family, metric_unit) {
  is.data.frame(df) &&
    all(c("metric_family", "metric_unit", "metric_value") %in% names(df)) &&
    all(df$metric_family == metric_family, na.rm = TRUE) &&
    all(df$metric_unit == metric_unit, na.rm = TRUE)
}

bundle_a_period_label <- function(freq = "month") {
  if (identical(freq, "week")) "weekly periods" else "monthly periods"
}

bundle_a_prep_combined_trends <- function(analytics, monetary, freq = "month") {
  date_a <- bundle_a_pick_col(analytics, c("publish_date", "Published At", "date"), label = "analytics date column")
  date_m <- bundle_a_pick_col(monetary, c("publish_date", "Published At", "date"), label = "monetary date column")
  unit <- if (identical(freq, "week")) "week" else "month"
  period_type <- bundle_a_period_label(freq)

  views <- analytics %>%
    dplyr::mutate(.period = lubridate::floor_date(bundle_a_as_date(.data[[date_a]]), unit = unit)) %>%
    dplyr::filter(!is.na(.data$.period)) %>%
    dplyr::group_by(.data$.period) %>%
    dplyr::summarize(
      metric_value = sum(report_interp_num(.data$views), na.rm = TRUE),
      sample_size = dplyr::n_distinct(.data$`Video ID`),
      .groups = "drop"
    ) %>%
    dplyr::mutate(metric_name = "views", metric_family = "views", metric_unit = "views")

  revenue <- monetary %>%
    dplyr::mutate(.period = lubridate::floor_date(bundle_a_as_date(.data[[date_m]]), unit = unit)) %>%
    dplyr::filter(!is.na(.data$.period)) %>%
    dplyr::group_by(.data$.period) %>%
    dplyr::summarize(
      metric_value = sum(report_interp_num(.data$`Estimated Revenue`), na.rm = TRUE),
      sample_size = dplyr::n_distinct(.data$`Video ID`),
      .groups = "drop"
    ) %>%
    dplyr::mutate(metric_name = "estimated revenue", metric_family = "revenue", metric_unit = "dollars")

  dplyr::bind_rows(views, revenue) %>%
    dplyr::arrange(.data$metric_family, .data$.period) %>%
    dplyr::group_by(.data$metric_family) %>%
    dplyr::mutate(
      period_count = dplyr::n(),
      first_value = dplyr::first(.data$metric_value),
      latest_value = dplyr::last(.data$metric_value),
      absolute_change = .data$latest_value - .data$first_value,
      percent_change = dplyr::if_else(.data$first_value > 0, .data$latest_value / .data$first_value - 1, NA_real_),
      peak_period = .data$.period[which.max(.data$metric_value)],
      trough_period = .data$.period[which.min(.data$metric_value)],
      period_type = period_type,
      period_label = format(.data$.period, if (identical(freq, "week")) "%Y-%m-%d" else "%b %Y")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      slot = "01_overall_performance_snapshot/02_combined_performance_trends",
      period_label = .data$period_label,
      period_start = .data$.period,
      period_end = lubridate::ceiling_date(.data$.period, unit = unit) - lubridate::days(1),
      period_type = .data$period_type,
      metric_name = .data$metric_name,
      metric_family = .data$metric_family,
      metric_unit = .data$metric_unit,
      metric_value = .data$metric_value,
      comparison_name = paste0("first ", sub("s$", "", .data$period_type)),
      comparison_value = .data$first_value,
      sample_size = .data$sample_size,
      group_name = NA_character_,
      source_table = dplyr::if_else(.data$metric_family == "views", "analytics", "monetary"),
      period_count = .data$period_count,
      first_value = .data$first_value,
      latest_value = .data$latest_value,
      absolute_change = .data$absolute_change,
      percent_change = .data$percent_change,
      peak_period = .data$peak_period,
      trough_period = .data$trough_period
    )
}

bundle_a_combined_trend_direction <- function(change) {
  if (!is.finite(change) || abs(change) < 0.05) {
    return("flat")
  }
  if (change > 0) "up" else "down"
}

bundle_a_direction_recommendation <- function(view_dir, revenue_dir, action_level) {
  if (view_dir == "up" && revenue_dir == "up") {
    return("Identify which content types, topics, or uploads contributed to the shared increase before scaling the pattern.")
  }
  if (view_dir == "up" && revenue_dir == "down") {
    return("Review monetization mix, content-type mix, and revenue-per-view proxies to see whether recent audience volume is coming from lower-monetizing formats.")
  }
  if (view_dir == "down" && revenue_dir == "up") {
    return("Inspect high-value videos or formats that may be monetizing better per viewer, rather than treating lower view volume as the whole story.")
  }
  if (view_dir == "down" && revenue_dir == "down") {
    return("Review upload cadence, content mix, and outlier periods before making format or schedule changes.")
  }
  if (action_level %in% c("review_examples", "monitor")) {
    return("Monitor whether this pattern repeats in the next report before acting on it.")
  }
  "Compare the specific periods and videos behind the movement before turning the trend into a strategy decision."
}

bundle_a_rule_recommendation <- function(action_level, family, sparse_label = "signal") {
  if (action_level == "scale_or_prioritize") {
    return(switch(
      family,
      format = "Prioritize this as a repeat-testing lane and inspect the strongest examples for title structure, topic, pacing, and posting context.",
      revenue = "Prioritize this group for monetization review and compare whether its revenue strength comes from repeatable formats, topics, or individual high-value uploads.",
      timing = "Run a controlled timing test around this pattern while holding topic and format as steady as practical.",
      collaboration = "Prioritize review of the strongest collaborations to identify repeatable guest fit, audience overlap, format, and promotional context.",
      topic = "Treat this as a repeat candidate, then verify which title framing and format pairing made the topic perform well.",
      tag = "Prioritize this tag for follow-up review, while remembering that tags overlap and should not be read as independent categories.",
      "Prioritize follow-up around this signal."
    ))
  }
  if (action_level == "controlled_test") {
    return(switch(
      family,
      format = "Run a controlled test with more uploads in this format before treating it as a reliable scale decision.",
      revenue = "Test whether the revenue pattern repeats with additional uploads and check whether revenue per video stays elevated.",
      timing = "Test the timing pattern cautiously and compare against topic, format, and event overlap.",
      collaboration = "Test similar collaboration conditions selectively and review whether audience overlap or guest fit explains the pattern.",
      topic = "Use this topic as a controlled-test candidate rather than a settled content rule.",
      tag = "Test the theme again before scaling tag-driven planning decisions.",
      "Run a controlled test before scaling this signal."
    ))
  }
  if (action_level == "review_examples") {
    return(paste0("Review the ", sparse_label, " examples qualitatively before changing strategy; the sample is too small for a scaling recommendation."))
  }
  if (action_level == "do_not_recommend") {
    return("Do not make a planning change from this slot alone; the evidence is missing, ambiguous, or too sparse.")
  }
  "Monitor this pattern in future reports and only act if it repeats with more supporting examples."
}

bundle_a_interpret_views_by_content_type <- function(ctx) {
  slot <- "01_overall_performance_snapshot/01_views_by_content_type"
  df <- ctx$tables$views_by_content_type
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(bundle_a_fallback(slot, "Views by content type", "This view separates total audience volume from average views per upload.", "views_by_content_type", "views_content_type_missing"))
  }
  wide <- df %>%
    dplyr::select(dplyr::all_of(c("Content Type", "metric", "value"))) %>%
    tidyr::pivot_wider(names_from = "metric", values_from = "value") %>%
    dplyr::left_join(ctx$analytics %>% dplyr::count(.data$`Content Type`, name = "VideoCount"), by = "Content Type")
  total_sig <- report_interp_distribution_signal(wide, "Content Type", "Total views", "VideoCount", min_sample = 2)
  avg_sig <- report_interp_distribution_signal(wide, "Content Type", "Average views per video", "VideoCount", min_sample = 2)
  if (!isTRUE(total_sig$available) || !isTRUE(avg_sig$available)) {
    return(bundle_a_fallback(slot, "Views by content type", "This view separates total audience volume from average views per upload.", "views_by_content_type", "views_content_type_sparse"))
  }

  same <- identical(total_sig$group, avg_sig$group)
  total_label <- bundle_a_display_label(total_sig$group, sentence_start = TRUE)
  avg_label <- bundle_a_display_label(avg_sig$group, sentence_start = TRUE)
  action <- bundle_a_action_level(avg_sig$sample_size, avg_sig$lift_vs_median, avg_sig$confidence)
  pattern <- if (same) {
    "both the scale leader and the per-video efficiency leader"
  } else if (avg_sig$sample_size <= 3) {
    "an early efficiency signal with limited sample support"
  } else {
    "a different efficiency leader than the total-volume leader"
  }
  bundle_a_interpretation(
    slot = slot,
    title = "Views by content type",
    finding = paste0(total_label, " lead total views, while ", avg_label, " are ", pattern, " in this report."),
    why_it_matters = "This distinction matters because a format can create reach through volume without being the strongest performer per upload.",
    evidence = paste0(
      total_label, " contribute ", report_interp_fmt_number(total_sig$metric), " total views. ",
      avg_label, " average ", report_interp_fmt_number(avg_sig$metric), " views per video across ",
      report_interp_fmt_count(avg_sig$sample_size), " videos. The median comparison format is ",
      report_interp_fmt_number(avg_sig$peer_median), " average views per video."
    ),
    recommendation = bundle_a_rule_recommendation(action, "format", "format"),
    next_check = "Review the top examples in the leading format for repeatable title framing, topic, pacing, and posting context.",
    caveat = "This is descriptive evidence; format performance can overlap with topic, timing, collaboration, and individual standout uploads.",
    confidence = avg_sig$confidence,
    action_level = action,
    sample_size = avg_sig$sample_size,
    metric_name = "average views per video",
    metric_value = avg_sig$metric,
    comparison_name = "middle compared format",
    comparison_value = avg_sig$peer_median,
    source_table = "views_by_content_type",
    rule_id = "bundle_a_views_content_type_scale_efficiency"
  )
}

bundle_a_interpret_performance_trends <- function(ctx) {
  slot <- "01_overall_performance_snapshot/02_combined_performance_trends"
  df <- ctx$tables$combined_performance_trends_safe
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(bundle_a_fallback(slot, "Combined performance trends", "This trend view compares views and estimated revenue with metric-safe period summaries.", "combined_performance_trends_safe", "combined_trends_missing"))
  }
  views <- df %>% dplyr::filter(.data$metric_family == "views")
  revenue <- df %>% dplyr::filter(.data$metric_family == "revenue")
  if (!bundle_a_validate_metric_family(views, "views", "views") || !bundle_a_validate_metric_family(revenue, "revenue", "dollars")) {
    return(bundle_a_fallback(slot, "Combined performance trends", "This trend view compares views and estimated revenue with metric-safe period summaries.", "combined_performance_trends_safe", "combined_trends_metric_guardrail"))
  }
  if (nrow(views) < 2 || nrow(revenue) < 2) {
    return(bundle_a_fallback(slot, "Combined performance trends", "This trend view needs at least two periods for each metric before making a trend recommendation.", "combined_performance_trends_safe", "combined_trends_sparse"))
  }
  v <- views[nrow(views), , drop = FALSE]
  r <- revenue[nrow(revenue), , drop = FALSE]
  view_dir <- bundle_a_combined_trend_direction(v$percent_change[[1]])
  revenue_dir <- bundle_a_combined_trend_direction(r$percent_change[[1]])
  min_periods <- min(v$period_count[[1]], r$period_count[[1]], na.rm = TRUE)
  lift <- max(abs(c(v$percent_change[[1]], r$percent_change[[1]])), na.rm = TRUE)
  confidence <- report_interp_distribution_confidence(
    sample_size = min_periods,
    group_count = 2,
    percentile_rank = if (min_periods >= 3) 0.75 else 0.50,
    lift_vs_median = lift,
    min_sample = 2,
    moderate_sample = 3,
    strong_sample = 6
  )
  action <- bundle_a_action_level(min_periods, lift, confidence, min_controlled = 3, min_scale = 6)
  period_type <- v$period_type[[1]]
  bundle_a_interpretation(
    slot = slot,
    title = "Combined performance trends",
    finding = paste0("Views are ", view_dir, " and estimated revenue is ", revenue_dir, " across the ", period_type, " in this report."),
    why_it_matters = "When views and revenue move differently, the report should separate audience demand from monetization mix instead of treating performance as one signal.",
    evidence = paste0(
      "Latest ", sub("s$", "", period_type), " views are ", report_interp_fmt_number(v$latest_value[[1]]),
      " versus ", report_interp_fmt_number(v$first_value[[1]]), " in the first ", sub("s$", "", period_type),
      " (", report_interp_fmt_percent(v$percent_change[[1]], 1), "). Latest estimated revenue is ",
      report_interp_fmt_dollar(r$latest_value[[1]]), " versus ", report_interp_fmt_dollar(r$first_value[[1]]),
      " (", report_interp_fmt_percent(r$percent_change[[1]], 1), ")."
    ),
    recommendation = bundle_a_direction_recommendation(view_dir, revenue_dir, action),
    next_check = "Inspect the specific periods, content types, and videos behind the movement before changing publishing or format strategy.",
    caveat = "This is not causal; short windows, uneven upload cadence, and individual high-impact videos can make period movement volatile.",
    confidence = confidence,
    action_level = action,
    sample_size = min_periods,
    metric_name = "views and estimated revenue trend",
    metric_value = v$latest_value[[1]],
    comparison_name = "first period views",
    comparison_value = v$first_value[[1]],
    source_table = "combined_performance_trends_safe",
    rule_id = "bundle_a_combined_trends_metric_safe"
  )
}

bundle_a_interpret_revenue_by_content_type <- function(ctx) {
  slot <- "01_overall_performance_snapshot/03_total_revenue_by_content_type"
  df <- ctx$tables$total_revenue_by_content_type
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(bundle_a_fallback(slot, "Revenue by content type", "This section separates total estimated revenue from average revenue per video.", "total_revenue_by_content_type", "revenue_content_missing"))
  }
  summary <- df %>%
    dplyr::group_by(.data$`Content Type`) %>%
    dplyr::summarize(
      TotalRevenue = sum(.data$Total, na.rm = TRUE),
      VideoCount = sum(.data$VideoCount, na.rm = TRUE),
      AverageRevenuePerVideo = dplyr::if_else(.data$VideoCount > 0, .data$TotalRevenue / .data$VideoCount, NA_real_),
      .groups = "drop"
    )
  total_sig <- report_interp_distribution_signal(summary, "Content Type", "TotalRevenue", "VideoCount", min_sample = 2)
  avg_sig <- report_interp_distribution_signal(summary, "Content Type", "AverageRevenuePerVideo", "VideoCount", min_sample = 2)
  if (!isTRUE(total_sig$available) || !isTRUE(avg_sig$available)) {
    return(bundle_a_fallback(slot, "Revenue by content type", "This section separates total estimated revenue from average revenue per video.", "total_revenue_by_content_type", "revenue_content_sparse"))
  }
  action <- bundle_a_action_level(avg_sig$sample_size, avg_sig$lift_vs_median, avg_sig$confidence)
  total_label <- bundle_a_display_label(total_sig$group, sentence_start = TRUE)
  avg_label <- bundle_a_display_label(avg_sig$group, sentence_start = TRUE)
  pattern <- if (identical(total_sig$group, avg_sig$group)) {
    "lead on both total estimated revenue and average revenue per video"
  } else {
    "lead average revenue per video, while total revenue is led by a different content type"
  }
  bundle_a_interpretation(
    slot = slot,
    title = "Revenue by content type",
    finding = if (identical(total_sig$group, avg_sig$group)) {
      paste0(avg_label, " ", pattern, " during the selected reporting window.")
    } else {
      paste0(avg_label, " ", pattern, " during the selected reporting window.")
    },
    why_it_matters = "Revenue totals can be driven by upload volume, while average revenue per video is a better signal for monetization efficiency.",
    evidence = paste0(
      total_label, " contribute ", report_interp_fmt_dollar(total_sig$metric), " in total estimated revenue. ",
      avg_label, " average ", report_interp_fmt_dollar(avg_sig$metric), " per video across ",
      report_interp_fmt_count(avg_sig$sample_size), " videos."
    ),
    recommendation = bundle_a_rule_recommendation(action, "revenue", "revenue"),
    next_check = "Compare the high-revenue videos against format, topic, and revenue-per-view proxies where available.",
    caveat = "Estimated revenue is descriptive and can be shaped by video count, age of uploads, monetization mix, and individual high-value videos.",
    confidence = avg_sig$confidence,
    action_level = action,
    sample_size = avg_sig$sample_size,
    metric_name = "average revenue per video",
    metric_value = avg_sig$metric,
    comparison_name = "middle compared content type",
    comparison_value = avg_sig$peer_median,
    source_table = "total_revenue_by_content_type",
    rule_id = "bundle_a_revenue_content_type_volume_efficiency"
  )
}

bundle_a_interpret_engagement_distribution <- function(ctx) {
  slot <- "01_overall_performance_snapshot/04_engagement_distribution_successful_videos"
  df <- ctx$tables$engagement_distribution_successful_videos
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(bundle_a_fallback(slot, "Engagement distribution", "This view compares engagement consistency and standout videos by content type.", "engagement_distribution_successful_videos", "engagement_missing"))
  }
  summary <- df %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::summarize(
      VideoCount = dplyr::n(),
      MedianEngagement = stats::median(.data$.metric, na.rm = TRUE),
      TopQuartileVideos = sum(.data$.is_success, na.rm = TRUE),
      IQR = stats::IQR(.data$.metric, na.rm = TRUE),
      .groups = "drop"
    )
  sig <- report_interp_distribution_signal(summary, ".content", "MedianEngagement", "VideoCount", min_sample = 3)
  if (!isTRUE(sig$available)) {
    return(bundle_a_fallback(slot, "Engagement distribution", "This view compares engagement consistency and standout videos by content type.", "engagement_distribution_successful_videos", "engagement_sparse"))
  }
  top <- summary[as.character(summary$.content) == sig$group, , drop = FALSE]
  action <- bundle_a_action_level(sig$sample_size, sig$lift_vs_median, sig$confidence)
  label <- bundle_a_display_label(sig$group, sentence_start = TRUE)
  bundle_a_interpretation(
    slot = slot,
    title = "Engagement distribution",
    finding = paste0(label, " have the strongest median Average View % among content types with enough examples."),
    why_it_matters = "Median engagement is useful because it points to typical retention, while highlighted top-quartile videos show examples worth reviewing.",
    evidence = paste0(
      "Median Average View % is ", report_interp_fmt_percent(sig$metric), " across ",
      report_interp_fmt_count(sig$sample_size), " videos, with ",
      report_interp_fmt_count(top$TopQuartileVideos[[1]]), " highlighted top-quartile examples."
    ),
    recommendation = bundle_a_rule_recommendation(action, "format", "engagement"),
    next_check = "Review the top-quartile videos for title promise, topic fit, pacing, format, and whether audience expectations match the opening content.",
    caveat = "A few strong highlighted points can make a content type look more repeatable than it is, especially with small groups.",
    confidence = sig$confidence,
    action_level = action,
    sample_size = sig$sample_size,
    metric_name = "median average view percentage",
    metric_value = sig$metric,
    comparison_name = "middle compared content type",
    comparison_value = sig$peer_median,
    source_table = "engagement_distribution_successful_videos",
    rule_id = "bundle_a_engagement_distribution"
  )
}

bundle_a_interpret_content_duration <- function(ctx, slot, table_name, metric_label, metric_family, formatter) {
  df <- ctx$tables[[table_name]]
  if (!is.data.frame(df) || nrow(df) == 0 || dplyr::n_distinct(df$publish_date) < 2) {
    return(bundle_a_fallback(slot, paste0(stringr::str_to_sentence(metric_label), " trend"), paste0("This chart tracks ", metric_label, " over time by content type."), table_name, paste0(table_name, "_missing")))
  }
  summary <- df %>%
    dplyr::group_by(.data$`Content Type`) %>%
    dplyr::summarize(
      Periods = dplyr::n(),
      Total = sum(.data$Total, na.rm = TRUE),
      Latest = dplyr::last(.data$Total[order(.data$publish_date)]),
      MedianPeriod = stats::median(.data$Total, na.rm = TRUE),
      .groups = "drop"
    )
  sig <- report_interp_distribution_signal(summary, "Content Type", "Total", "Periods", min_sample = 2, moderate_sample = 3, strong_sample = 6)
  if (!isTRUE(sig$available)) {
    return(bundle_a_fallback(slot, paste0(stringr::str_to_sentence(metric_label), " trend"), paste0("This chart tracks ", metric_label, " over time by content type."), table_name, paste0(table_name, "_sparse")))
  }
  action <- bundle_a_action_level(sig$sample_size, sig$lift_vs_median, sig$confidence, min_controlled = 3, min_scale = 6)
  label <- bundle_a_display_label(sig$group, sentence_start = TRUE)
  rec <- if (identical(metric_family, "revenue")) {
    "Inspect the months and videos behind this revenue concentration, then compare content type, topic, and monetization efficiency before scaling."
  } else {
    "Inspect upload cadence, content mix, and outlier videos behind this view concentration before treating it as momentum."
  }
  bundle_a_interpretation(
    slot = slot,
    title = paste0(stringr::str_to_sentence(metric_label), " over time"),
    finding = paste0(label, " contribute the largest total ", metric_label, " across the available dated trend rows."),
    why_it_matters = "This identifies where total-volume movement is concentrated, but it does not show per-video efficiency by itself.",
    evidence = paste0(
      label, " total ", formatter(sig$metric), " across ",
      report_interp_fmt_count(sig$sample_size), " dated points. The middle-ranked comparison group totals ",
      formatter(sig$peer_median), "."
    ),
    recommendation = rec,
    next_check = "Check whether the apparent trend is driven by upload count, one high-impact period, or a broader repeatable pattern across content types.",
    caveat = paste0(stringr::str_to_sentence(metric_label), " trends are total-volume signals, so they should be read alongside per-video metrics before making planning decisions."),
    confidence = sig$confidence,
    action_level = action,
    sample_size = sig$sample_size,
    metric_name = paste0("total ", metric_label),
    metric_value = sig$metric,
    comparison_name = "middle compared content type",
    comparison_value = sig$peer_median,
    source_table = table_name,
    rule_id = paste0("bundle_a_", table_name)
  )
}

bundle_a_interpret_audience_age_gender <- function(ctx) {
  slot <- "03_audience_composition/01_audience_age_gender_trends"
  df <- ctx$tables$audience_age_gender_trends
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(bundle_a_fallback(slot, "Audience age and gender", "This chart describes audience age and gender mix as percentages, not raw audience counts.", "audience_age_gender_trends", "audience_missing"))
  }
  latest_period <- max(df$.period, na.rm = TRUE)
  latest <- df %>%
    dplyr::filter(.data$.period == latest_period) %>%
    dplyr::mutate(segment = paste(.data$.gender, .data$.age, sep = " "))
  sig <- report_interp_distribution_signal(latest, "segment", "share", "videos", min_sample = 1, moderate_sample = 3, strong_sample = 8)
  if (!isTRUE(sig$available)) {
    return(bundle_a_fallback(slot, "Audience age and gender", "This chart describes audience age and gender mix as percentages, not raw audience counts.", "audience_age_gender_trends", "audience_sparse"))
  }
  action <- if (sig$sample_size >= 4) "controlled_test" else "review_examples"
  label <- bundle_a_display_label(sig$group, sentence_start = TRUE)
  bundle_a_interpretation(
    slot = slot,
    title = "Audience age and gender",
    finding = paste0(label, " are the largest observed age/gender share segment in the latest available monthly period."),
    why_it_matters = "Audience shares are useful for positioning, sponsorship fit, and content framing, but they are not raw audience counts.",
    evidence = paste0(
      "The segment represents ", report_interp_fmt_percent(sig$metric),
      " of the latest available demographic mix across ", report_interp_fmt_count(sig$sample_size), " videos."
    ),
    recommendation = "Use this audience mix to check whether content positioning, partnership assumptions, and creative framing match the observed viewer composition.",
    next_check = "Monitor whether this segment remains prominent across additional periods before describing it as an audience shift.",
    caveat = "Because the metric is a share, the report should not imply that the absolute viewer count for this segment increased or decreased.",
    confidence = sig$confidence,
    action_level = action,
    sample_size = sig$sample_size,
    metric_name = "latest audience share",
    metric_value = sig$metric,
    comparison_name = "middle compared audience segment",
    comparison_value = sig$peer_median,
    source_table = "audience_age_gender_trends",
    rule_id = "bundle_a_audience_age_gender_share"
  )
}

bundle_a_interpret_weekend_vs_weekday <- function(ctx) {
  slot <- "04_content_strategy_deep_dive/01_weekend_vs_weekday"
  summary <- ctx$tables$weekend_vs_weekday_distribution_summary
  if (!is.data.frame(summary) || nrow(summary) == 0) {
    return(bundle_a_fallback(slot, "Weekend vs weekday", "This comparison uses per-video weekend and weekday distributions for timing context.", "weekend_vs_weekday_distribution_summary", "weekend_weekday_missing"))
  }
  views <- summary %>% dplyr::filter(.data$metric == "Views per video")
  sig <- report_interp_distribution_signal(views, "weekend_group", "Median", "VideoCount", min_sample = 2, moderate_sample = 5, strong_sample = 10)
  if (!isTRUE(sig$available)) {
    return(bundle_a_fallback(slot, "Weekend vs weekday", "This comparison uses per-video weekend and weekday distributions for timing context.", "weekend_vs_weekday_distribution_summary", "weekend_weekday_sparse"))
  }
  action <- bundle_a_action_level(sig$sample_size, sig$lift_vs_median, sig$confidence, min_controlled = 5, min_scale = 10, sparse_review_max = 4)
  label <- bundle_a_display_label(sig$group, sentence_start = TRUE)
  bundle_a_interpretation(
    slot = slot,
    title = "Weekend vs weekday",
    finding = paste0(label, " show the higher median views per video in the available timing comparison."),
    why_it_matters = "Median per-video views are a better timing signal than total volume because total volume can simply reflect more uploads on one day type.",
    evidence = paste0(
      label, " have median views per video of ", report_interp_fmt_number(sig$metric),
      " across ", report_interp_fmt_count(sig$sample_size), " videos. The other day-type group is ",
      report_interp_fmt_number(sig$peer_median), "."
    ),
    recommendation = bundle_a_rule_recommendation(action, "timing", "timing"),
    next_check = "Check whether the stronger day type overlaps with specific topics, formats, events, or collaboration choices.",
    caveat = "Timing is heavily confounded with content mix and event scheduling, so this should be tested rather than treated as proof of a day-type effect.",
    confidence = sig$confidence,
    action_level = action,
    sample_size = sig$sample_size,
    metric_name = "median views per video",
    metric_value = sig$metric,
    comparison_name = "other day type",
    comparison_value = sig$peer_median,
    source_table = "weekend_vs_weekday_distribution_summary",
    rule_id = "bundle_a_weekend_weekday_timing"
  )
}

bundle_a_interpret_day_of_week <- function(ctx) {
  slot <- "04_content_strategy_deep_dive/02_day_of_week_distribution"
  df <- ctx$tables$day_of_week_distribution
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(bundle_a_fallback(slot, "Day-of-week distribution", "This view shows how total views and revenue are distributed across weekdays.", "day_of_week_distribution", "dow_missing"))
  }
  sig <- report_interp_distribution_signal(df, "day_of_week", "ShareViews", "VideoCountViews", min_sample = 1, moderate_sample = 5, strong_sample = 10)
  if (!isTRUE(sig$available)) {
    return(bundle_a_fallback(slot, "Day-of-week distribution", "This view shows how total views and revenue are distributed across weekdays.", "day_of_week_distribution", "dow_sparse"))
  }
  action <- bundle_a_action_level(sig$sample_size, sig$lift_vs_median, sig$confidence, min_controlled = 5, min_scale = 10, sparse_review_max = 4)
  day_label <- bundle_a_display_label(sig$group, sentence_start = TRUE)
  bundle_a_interpretation(
    slot = slot,
    title = "Day-of-week distribution",
    finding = paste0(day_label, " accounts for the largest share of observed views by weekday."),
    why_it_matters = "A high weekday share can point to a useful scheduling pattern, but it can also reflect upload count or content mix.",
    evidence = paste0(
      day_label, " represents ", report_interp_fmt_percent(sig$metric),
      " of weekday-attributed views across ", report_interp_fmt_count(sig$sample_size), " videos."
    ),
    recommendation = if (action %in% c("controlled_test", "scale_or_prioritize")) {
      "Check per-video performance and then test the weekday pattern while holding topic and format as steady as practical."
    } else {
      "Review upload count and content mix on this weekday before treating it as a timing signal."
    },
    next_check = "Compare the weekday's number of uploads, topics, formats, and revenue share before changing schedule priorities.",
    caveat = "Share of total views is a descriptive weekday distribution and should not be used alone to recommend publishing timing.",
    confidence = sig$confidence,
    action_level = action,
    sample_size = sig$sample_size,
    metric_name = "share of views",
    metric_value = sig$metric,
    comparison_name = "middle compared weekday",
    comparison_value = sig$peer_median,
    source_table = "day_of_week_distribution",
    rule_id = "bundle_a_day_of_week_share"
  )
}

bundle_a_interpret_collaboration <- function(ctx) {
  slot <- "04_content_strategy_deep_dive/03_collaboration_effectiveness"
  df <- ctx$tables$collaboration_effectiveness
  if (!is.data.frame(df) || nrow(df) < 2) {
    return(bundle_a_fallback(slot, "Collaboration effectiveness", "This section compares collaborative and non-collaborative uploads using per-video metrics.", "collaboration_effectiveness", "collaboration_missing"))
  }
  views_sig <- report_interp_distribution_signal(df, "collab_group", "AverageViewsPerVideo", "VideoCountViews", min_sample = 2, moderate_sample = 3, strong_sample = 5)
  rev_sig <- report_interp_distribution_signal(df, "collab_group", "AverageRevenuePerVideo", "VideoCountRevenue", min_sample = 2, moderate_sample = 3, strong_sample = 5)
  if (!isTRUE(views_sig$available)) {
    return(bundle_a_fallback(slot, "Collaboration effectiveness", "This section compares collaborative and non-collaborative uploads using per-video metrics.", "collaboration_effectiveness", "collaboration_sparse"))
  }
  action <- bundle_a_action_level(views_sig$sample_size, views_sig$lift_vs_median, views_sig$confidence, min_controlled = 3, min_scale = 5)
  views_label <- bundle_a_display_label(views_sig$group, sentence_start = TRUE)
  revenue_clause <- if (isTRUE(rev_sig$available)) {
    paste0(" ", bundle_a_display_label(rev_sig$group, sentence_start = TRUE), " also lead on average revenue per video, at ", report_interp_fmt_dollar(rev_sig$metric), ".")
  } else {
    ""
  }
  bundle_a_interpretation(
    slot = slot,
    title = "Collaboration effectiveness",
    finding = paste0(views_label, " show the higher average views per video in the available collaboration comparison."),
    why_it_matters = "Collaboration should be evaluated on per-video performance and repeatability, not only on whether collaborative uploads contribute more total volume.",
    evidence = paste0(
      views_label, " average ", report_interp_fmt_number(views_sig$metric), " views per video across ",
      report_interp_fmt_count(views_sig$sample_size), " videos. The other collaboration group averages ",
      report_interp_fmt_number(views_sig$peer_median), " average views per video.", revenue_clause
    ),
    recommendation = bundle_a_rule_recommendation(action, "collaboration", "collaboration"),
    next_check = "Review the strongest collaborations for guest fit, audience overlap, topic match, format, and promotional context.",
    caveat = "This does not isolate collaboration as the driver of performance; collaborations can differ in audience, topic, timing, and production context.",
    confidence = views_sig$confidence,
    action_level = action,
    sample_size = views_sig$sample_size,
    metric_name = "average views per video",
    metric_value = views_sig$metric,
    comparison_name = "other collaboration group",
    comparison_value = views_sig$peer_median,
    source_table = "collaboration_effectiveness",
    rule_id = "bundle_a_collaboration_per_video"
  )
}

bundle_a_interpret_topic <- function(ctx) {
  slot <- "04_content_strategy_deep_dive/04_topic_performance"
  df <- ctx$tables$topic_performance
  if (!is.data.frame(df) || nrow(df) == 0 || !"topic_group" %in% names(df)) {
    return(bundle_a_fallback(slot, "Topic performance", "This section compares topic groups using views and revenue signals.", "topic_performance", "topic_missing"))
  }
  df <- df %>% dplyr::filter(.data$topic_group != "Other topics")
  sig <- report_interp_distribution_signal(df, "topic_group", "AverageViewsPerVideo", "VideoCountViews", min_sample = 2, moderate_sample = 3, strong_sample = 8)
  if (!isTRUE(sig$available)) {
    return(bundle_a_fallback(slot, "Topic performance", "This section compares topic groups using views and revenue signals.", "topic_performance", "topic_sparse"))
  }
  action <- bundle_a_action_level(sig$sample_size, sig$lift_vs_median, sig$confidence, min_controlled = 4, min_scale = 8)
  label <- bundle_a_display_label(sig$group, sentence_start = TRUE)
  bundle_a_interpretation(
    slot = slot,
    title = "Topic performance",
    finding = paste0(label, " has the strongest average views per video among compared topic groups."),
    why_it_matters = "A topic with high average views may be a repeat candidate, but only if the sample is large enough and the result is not mainly format- or title-driven.",
    evidence = paste0(
      label, " averages ", report_interp_fmt_number(sig$metric), " views per video across ",
      report_interp_fmt_count(sig$sample_size), " videos. The median comparison topic is ",
      report_interp_fmt_number(sig$peer_median), " average views per video."
    ),
    recommendation = bundle_a_rule_recommendation(action, "topic", "topic"),
    next_check = "Review title framing, format pairing, audience promise, and whether the topic overlaps with shorts, live, or standard video formats.",
    caveat = "Topic labels can overlap with format, timing, collaboration, and title effects, so this should be treated as descriptive evidence.",
    confidence = sig$confidence,
    action_level = action,
    sample_size = sig$sample_size,
    metric_name = "average views per video",
    metric_value = sig$metric,
    comparison_name = "middle compared topic",
    comparison_value = sig$peer_median,
    source_table = "topic_performance",
    rule_id = "bundle_a_topic_average_views"
  )
}

bundle_a_interpret_tag <- function(ctx) {
  slot <- "04_content_strategy_deep_dive/05_tag_performance"
  df <- ctx$tables$tag_performance
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(bundle_a_fallback(slot, "Tag performance", "This chart summarizes top tags using views and revenue as directional signals.", "tag_performance", "tag_missing"))
  }
  total_sig <- report_interp_distribution_signal(df, "tag_group", "TotalViews", "VideoCountViews", min_sample = 2, moderate_sample = 3, strong_sample = 5)
  avg_sig <- report_interp_distribution_signal(df, "tag_group", "AverageViewsPerVideo", "VideoCountViews", min_sample = 2, moderate_sample = 3, strong_sample = 5)
  if (!isTRUE(total_sig$available)) {
    return(bundle_a_fallback(slot, "Tag performance", "This chart summarizes top tags using views and revenue as directional signals.", "tag_performance", "tag_sparse"))
  }
  action <- bundle_a_action_level(total_sig$sample_size, total_sig$lift_vs_median, total_sig$confidence, min_controlled = 3, min_scale = 5)
  total_label <- bundle_a_display_label(total_sig$group, sentence_start = TRUE)
  avg_label <- bundle_a_display_label(avg_sig$group, sentence_start = TRUE)
  bundle_a_interpretation(
    slot = slot,
    title = "Tag performance",
    finding = paste0(total_label, " is the highest-volume tag by tag-attributed views. ", avg_label, " is the high-average tag to compare against."),
    why_it_matters = "Tag volume and tag efficiency answer different questions, and tags can overlap on the same video.",
    evidence = paste0(
      total_label, " contributes ", report_interp_fmt_number(total_sig$metric), " tag-attributed views across ",
      report_interp_fmt_count(total_sig$sample_size), " tagged videos."
    ),
    recommendation = bundle_a_rule_recommendation(action, "tag", "tag"),
    next_check = "Compare high-volume tags against high-average tags, then inspect the actual videos for repeated topic, title, and format patterns.",
    caveat = "Tags are not mutually exclusive categories, so tag totals are directional and should not be added together as independent effects.",
    confidence = total_sig$confidence,
    action_level = action,
    sample_size = total_sig$sample_size,
    metric_name = "tag-attributed total views",
    metric_value = total_sig$metric,
    comparison_name = "middle compared tag",
    comparison_value = total_sig$peer_median,
    source_table = "tag_performance",
    rule_id = "bundle_a_tag_total_views"
  )
}

bundle_a_interpret_average_views_per_tag <- function(ctx) {
  slot <- "04_content_strategy_deep_dive/06_average_views_per_tag"
  df <- ctx$tables$tag_performance
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(bundle_a_fallback(slot, "Average views per tag", "This view shifts tag interpretation from total volume to average views per tagged video.", "tag_performance", "avg_tag_missing"))
  }
  sig <- report_interp_distribution_signal(df, "tag_group", "AverageViewsPerVideo", "VideoCountViews", min_sample = 2, moderate_sample = 3, strong_sample = 5)
  if (!isTRUE(sig$available)) {
    return(bundle_a_fallback(slot, "Average views per tag", "This view shifts tag interpretation from total volume to average views per tagged video.", "tag_performance", "avg_tag_sparse"))
  }
  action <- bundle_a_action_level(sig$sample_size, sig$lift_vs_median, sig$confidence, min_controlled = 3, min_scale = 5)
  if (sig$sample_size < 3 && action %in% c("controlled_test", "scale_or_prioritize")) {
    action <- "review_examples"
  }
  label <- bundle_a_display_label(sig$group, sentence_start = TRUE)
  bundle_a_interpretation(
    slot = slot,
    title = "Average views per tag",
    finding = paste0(label, " has the highest average views per tagged video among compared tags."),
    why_it_matters = "Average views per tag can surface promising themes that total-volume tag rankings miss, but small samples are especially fragile.",
    evidence = paste0(
      label, " averages ", report_interp_fmt_number(sig$metric), " views per tagged video across ",
      report_interp_fmt_count(sig$sample_size), " videos. The median comparison tag is ",
      report_interp_fmt_number(sig$peer_median), " average views per tagged video."
    ),
    recommendation = if (action == "review_examples") {
      "Review the tagged videos qualitatively before changing strategy, focusing on shared topic, title framing, and format context."
    } else {
      bundle_a_rule_recommendation(action, "tag", "tag")
    },
    next_check = "Test the theme again before treating it as repeatable.",
    caveat = "Small tag samples and overlapping tag assignments can make average-per-tag comparisons look stronger than they are.",
    confidence = sig$confidence,
    action_level = action,
    sample_size = sig$sample_size,
    metric_name = "average views per tagged video",
    metric_value = sig$metric,
    comparison_name = "middle compared tag",
    comparison_value = sig$peer_median,
    source_table = "tag_performance",
    rule_id = "bundle_a_tag_average_views"
  )
}

bundle_a_rank_results <- function(results) {
  results %>%
    dplyr::mutate(
      .action_rank = dplyr::recode(
        .data$action_level,
        scale_or_prioritize = 1L,
        controlled_test = 2L,
        review_examples = 3L,
        monitor = 4L,
        do_not_recommend = 5L,
        .default = 6L
      ),
      .confidence_rank = dplyr::recode(
        .data$confidence,
        strong = 1L,
        moderate = 2L,
        weak = 3L,
        insufficient = 4L,
        .default = 5L
      )
    ) %>%
    dplyr::arrange(.data$.action_rank, .data$.confidence_rank, dplyr::desc(.data$sample_size))
}

bundle_a_section_synthesis <- function(slot, title, children, section_focus) {
  supported <- children %>% dplyr::filter(!.data$fallback, .data$action_level != "do_not_recommend")
  if (nrow(supported) == 0) {
    return(bundle_a_fallback(slot, title, paste0(section_focus, " The available evidence is too sparse or mixed for a specific section recommendation."), "section_children", paste0(gsub("[^a-z0-9]+", "_", tolower(slot)), "_fallback")))
  }
  top <- bundle_a_rank_results(supported) %>% dplyr::slice_head(n = 2)
  bundle_a_interpretation(
    slot = slot,
    title = title,
    finding = paste0(section_focus, " The strongest supported signal is that ", top$finding[[1]]),
    why_it_matters = top$why_it_matters[[1]],
    evidence = paste(top$evidence, collapse = " "),
    recommendation = bundle_a_recommendation_summary(top$recommendation),
    next_check = paste(unique(top$next_check), collapse = " "),
    caveat = "Because this section combines several descriptive signals, decisions should still be checked against the underlying charts and sample sizes.",
    confidence = if (any(top$confidence == "strong")) "strong" else "moderate",
    action_level = top$action_level[[1]],
    sample_size = suppressWarnings(max(top$sample_size, na.rm = TRUE)),
    source_table = "section_child_interpretations",
    rule_id = paste0("bundle_a_section_synthesis_", gsub("[^a-z0-9]+", "_", tolower(title)))
  )
}

bundle_a_executive_summary <- function(children) {
  supported <- bundle_a_rank_results(children %>% dplyr::filter(!.data$fallback, .data$action_level != "do_not_recommend"))
  if (nrow(supported) == 0) {
    return(bundle_a_fallback("05_report_bookends/01_executive_summary", "Executive summary", "This report provides a baseline view of performance, monetization, audience, and content strategy, but no rule produced enough evidence for a specific planning recommendation.", "all_interpretations", "executive_summary_fallback"))
  }
  top <- supported %>% dplyr::slice_head(n = min(4, nrow(.)))
  limitation <- children %>%
    dplyr::filter(.data$fallback | .data$action_level %in% c("review_examples", "monitor", "do_not_recommend")) %>%
    dplyr::slice_head(n = 1)
  caveat <- if (nrow(limitation) > 0) {
    paste0("The main limitation is that ", tolower(substr(limitation$caveat[[1]], 1, 1)), substr(limitation$caveat[[1]], 2, nchar(limitation$caveat[[1]])))
  } else {
    "The main limitation is that all signals remain descriptive and should be checked against the underlying videos before strategy changes."
  }
  top_two <- top$title[seq_len(min(2, nrow(top)))]
  bundle_a_interpretation(
    slot = "05_report_bookends/01_executive_summary",
    title = "Executive summary",
    finding = paste0("The strongest planning signals in this report come from ", report_interp_human_list(top$title), "."),
    why_it_matters = "These signals identify where the reader should focus first instead of treating every chart as equal priority.",
    evidence = paste(top$evidence[seq_len(min(2, nrow(top)))], collapse = " "),
    recommendation = paste0(
      "Start with ", report_interp_human_list(top_two),
      ", then review the underlying videos before changing format, topic, timing, or monetization priorities."
    ),
    next_check = "Compare whether the same pattern appears across views, revenue, and per-video metrics, and separate repeatable patterns from one-off outliers.",
    caveat = caveat,
    confidence = if (any(top$confidence == "strong")) "strong" else "moderate",
    action_level = top$action_level[[1]],
    sample_size = suppressWarnings(max(top$sample_size, na.rm = TRUE)),
    source_table = "all_interpretations",
    rule_id = "bundle_a_executive_summary_top_signals"
  )
}

bundle_a_conclusion <- function(children) {
  supported <- bundle_a_rank_results(children %>% dplyr::filter(!.data$fallback, .data$action_level != "do_not_recommend"))
  if (nrow(supported) == 0) {
    return(bundle_a_fallback("05_report_bookends/02_conclusion", "Conclusion", "The next reporting cycle should prioritize filling data gaps before making content strategy changes.", "all_interpretations", "conclusion_fallback"))
  }
  top <- supported %>% dplyr::slice_head(n = min(3, nrow(.)))
  top_two <- top$title[seq_len(min(2, nrow(top)))]
  bundle_a_interpretation(
    slot = "05_report_bookends/02_conclusion",
    title = "Conclusion",
    finding = "The next report cycle should turn the strongest descriptive signals into a short review queue.",
    why_it_matters = "That keeps the follow-up focused on repeatable evidence rather than one-off highs.",
    evidence = paste0("The highest-priority supported areas are ", report_interp_human_list(top$title), "."),
    recommendation = paste0(
      "Build the next-cycle review around ", report_interp_human_list(top_two),
      ", then decide which signals deserve monitoring, controlled testing, or example-level review."
    ),
    next_check = "For the next report, check whether these same signals repeat after adding more uploads and whether the pattern holds across both total-volume and per-video metrics.",
    caveat = "Do not read this report as proof that any format, tag, timing choice, or collaboration drove performance by itself; it is a prioritized evidence map for follow-up.",
    confidence = if (any(top$confidence == "strong")) "strong" else "moderate",
    action_level = top$action_level[[1]],
    sample_size = suppressWarnings(max(top$sample_size, na.rm = TRUE)),
    source_table = "all_interpretations",
    rule_id = "bundle_a_conclusion_next_cycle"
  )
}

bundle_a_build_interpretations <- function(ctx) {
  results <- list()
  add <- function(x) {
    results[[x$slot[[1]]]] <<- x
  }
  add(bundle_a_interpret_views_by_content_type(ctx))
  add(bundle_a_interpret_performance_trends(ctx))
  add(bundle_a_interpret_revenue_by_content_type(ctx))
  add(bundle_a_interpret_engagement_distribution(ctx))
  add(bundle_a_interpret_content_duration(
    ctx,
    "02_trends_over_time/01_revenue_over_time_by_content_type",
    "revenue_over_time_by_content_type",
    "estimated revenue",
    "revenue",
    report_interp_fmt_dollar
  ))
  add(bundle_a_interpret_content_duration(
    ctx,
    "02_trends_over_time/02_total_views_over_time_by_content_type",
    "total_views_over_time_by_content_type",
    "views",
    "views",
    report_interp_fmt_number
  ))
  add(bundle_a_interpret_audience_age_gender(ctx))
  add(bundle_a_interpret_weekend_vs_weekday(ctx))
  add(bundle_a_interpret_day_of_week(ctx))
  add(bundle_a_interpret_collaboration(ctx))
  add(bundle_a_interpret_topic(ctx))
  add(bundle_a_interpret_tag(ctx))
  add(bundle_a_interpret_average_views_per_tag(ctx))

  snapshot_children <- dplyr::bind_rows(results[c(
    "01_overall_performance_snapshot/01_views_by_content_type",
    "01_overall_performance_snapshot/02_combined_performance_trends",
    "01_overall_performance_snapshot/03_total_revenue_by_content_type",
    "01_overall_performance_snapshot/04_engagement_distribution_successful_videos"
  )])
  trend_children <- dplyr::bind_rows(results[c(
    "02_trends_over_time/01_revenue_over_time_by_content_type",
    "02_trends_over_time/02_total_views_over_time_by_content_type"
  )])
  audience_children <- dplyr::bind_rows(results["03_audience_composition/01_audience_age_gender_trends"])
  strategy_children <- dplyr::bind_rows(results[c(
    "04_content_strategy_deep_dive/01_weekend_vs_weekday",
    "04_content_strategy_deep_dive/02_day_of_week_distribution",
    "04_content_strategy_deep_dive/03_collaboration_effectiveness",
    "04_content_strategy_deep_dive/04_topic_performance",
    "04_content_strategy_deep_dive/05_tag_performance",
    "04_content_strategy_deep_dive/06_average_views_per_tag"
  )])

  add(bundle_a_section_synthesis(
    "01_overall_performance_snapshot/00_section_synthesis",
    "Overall performance snapshot",
    snapshot_children,
    "This section separates scale, per-video efficiency, monetization, and engagement quality."
  ))
  add(bundle_a_section_synthesis(
    "02_trends_over_time/00_section_synthesis",
    "Trends over time",
    trend_children,
    "This section identifies where total views and estimated revenue are concentrated over time."
  ))
  add(bundle_a_section_synthesis(
    "03_audience_composition/00_section_synthesis",
    "Audience composition",
    audience_children,
    "This section interprets audience demographic mix as shares, not raw audience counts."
  ))
  add(bundle_a_section_synthesis(
    "04_content_strategy_deep_dive/00_section_synthesis",
    "Content strategy deep dive",
    strategy_children,
    "This section compares timing, collaboration, topic, and tag signals as operating hypotheses."
  ))

  all_children <- dplyr::bind_rows(snapshot_children, trend_children, audience_children, strategy_children)
  add(bundle_a_executive_summary(all_children))
  add(bundle_a_conclusion(all_children))

  out <- dplyr::bind_rows(results)
  missing_slots <- setdiff(bundle_a_interpretation_slots(), out$slot)
  for (slot in missing_slots) {
    out <- dplyr::bind_rows(out, bundle_a_fallback(slot, "Missing interpretation slot", "This slot did not produce a rule result.", "bundle_a_build_interpretations", "missing_slot_fallback"))
  }
  out %>%
    dplyr::mutate(slot = factor(.data$slot, levels = bundle_a_interpretation_slots(), ordered = TRUE)) %>%
    dplyr::arrange(.data$slot) %>%
    dplyr::mutate(slot = as.character(.data$slot))
}

bundle_a_write_interpretations <- function(results, interpret_root) {
  dir.create(interpret_root, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(results, file.path(interpret_root, "bundle_a_interpretation_results.csv"))
  for (i in seq_len(nrow(results))) {
    slot <- results$slot[[i]]
    out_dir <- file.path(interpret_root, slot)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(
      report_interp_markdown(results[i, , drop = FALSE]),
      con = file.path(out_dir, "output.md"),
      useBytes = TRUE
    )
  }
  invisible(results)
}
