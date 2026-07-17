# Recommendation schema, ranking, formatting, and binding helpers.

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
