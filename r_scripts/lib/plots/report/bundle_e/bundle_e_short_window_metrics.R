bundle_e_short_window_base <- function(
  panel_df,
  content_type = "short",
  required_launch_capture_days = 7
) {
  if (nrow(panel_df) == 0) {
    return(tibble::tibble())
  }

  panel_df %>%
    dplyr::filter(.data$`Content Type` == content_type) %>%
    dplyr::arrange(.data$`Video ID`, .data$snapshot_date) %>%
    dplyr::group_by(.data$`Video ID`) %>%
    dplyr::mutate(
      latest_views = max(.data$views_cumulative, na.rm = TRUE),
      latest_age_days = max(.data$video_age_days, na.rm = TRUE),
      launch_capture_lag_days = dplyr::first(.data$launch_capture_lag_days)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      is.finite(.data$latest_views),
      .data$latest_views > 0,
      !is.na(.data$video_age_days),
      is.finite(.data$latest_age_days),
      .data$latest_age_days >= 0,
      !is.na(.data$launch_capture_lag_days),
      .data$launch_capture_lag_days >= 0,
      .data$launch_capture_lag_days <= required_launch_capture_days
    )
}

bundle_e_short_window_snapshot_at_day <- function(short_panel, target_day) {
  if (nrow(short_panel) == 0) {
    return(tibble::tibble())
  }

  short_panel %>%
    dplyr::filter(!is.na(.data$video_age_days), .data$video_age_days <= target_day) %>%
    dplyr::group_by(.data$`Video ID`) %>%
    dplyr::slice_max(order_by = .data$video_age_days, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      views_at_window = .data$views_cumulative,
      share_of_latest = bundle_e_safe_divide(.data$views_at_window, .data$latest_views),
      remaining_share = 1 - .data$share_of_latest
    )
}

bundle_e_short_window_candidate_days <- function(
  short_panel,
  default_days = c(7, 10, 14, 21, 30, 45, 60),
  max_candidates = 8
) {
  if (nrow(short_panel) == 0 || !"latest_age_days" %in% names(short_panel)) {
    return(default_days)
  }

  max_observed_age <- suppressWarnings(stats::quantile(short_panel$latest_age_days, probs = 0.9, na.rm = TRUE))
  if (!is.finite(max_observed_age) || is.na(max_observed_age) || max_observed_age <= 0) {
    return(default_days)
  }

  auto_days <- unique(c(
    default_days,
    floor(seq(7, min(max_observed_age, 90), length.out = max_candidates))
  ))

  auto_days <- auto_days[is.finite(auto_days) & !is.na(auto_days) & auto_days >= 3]
  auto_days <- sort(unique(as.integer(auto_days)))
  auto_days[auto_days <= max_observed_age]
}

build_bundle_e_short_window_diagnostics <- function(
  panel_df,
  candidate_days = NULL,
  content_type = "short",
  required_launch_capture_days = 7,
  min_videos = 10
) {
  short_panel <- bundle_e_short_window_base(
    panel_df = panel_df,
    content_type = content_type,
    required_launch_capture_days = required_launch_capture_days
  )

  if (nrow(short_panel) == 0) {
    return(tibble::tibble())
  }

  if (is.null(candidate_days) || length(candidate_days) == 0) {
    candidate_days <- bundle_e_short_window_candidate_days(short_panel)
  }

  purrr::map_dfr(candidate_days, function(day) {
    day_panel <- bundle_e_short_window_snapshot_at_day(short_panel, target_day = day) %>%
      dplyr::filter(
        !is.na(.data$share_of_latest),
        is.finite(.data$share_of_latest),
        !is.na(.data$remaining_share),
        is.finite(.data$remaining_share),
        .data$latest_age_days >= day
      )

    if (nrow(day_panel) == 0) {
      return(tibble::tibble(
        window_days = day,
        n_videos = 0,
        median_share_of_latest = NA_real_,
        p25_share_of_latest = NA_real_,
        p75_share_of_latest = NA_real_,
        median_remaining_share = NA_real_,
        p75_remaining_share = NA_real_,
        pct_remaining_le_10 = NA_real_,
        pct_remaining_le_05 = NA_real_,
        eligible_window = FALSE
      ))
    }

    tibble::tibble(
      window_days = day,
      n_videos = dplyr::n_distinct(day_panel$`Video ID`),
      median_share_of_latest = stats::median(day_panel$share_of_latest, na.rm = TRUE),
      p25_share_of_latest = stats::quantile(day_panel$share_of_latest, probs = 0.25, na.rm = TRUE),
      p75_share_of_latest = stats::quantile(day_panel$share_of_latest, probs = 0.75, na.rm = TRUE),
      median_remaining_share = stats::median(day_panel$remaining_share, na.rm = TRUE),
      p75_remaining_share = stats::quantile(day_panel$remaining_share, probs = 0.75, na.rm = TRUE),
      pct_remaining_le_10 = mean(day_panel$remaining_share <= 0.10, na.rm = TRUE),
      pct_remaining_le_05 = mean(day_panel$remaining_share <= 0.05, na.rm = TRUE),
      eligible_window = dplyr::n_distinct(day_panel$`Video ID`) >= min_videos
    )
  }) %>%
    dplyr::arrange(.data$window_days)
}

choose_bundle_e_short_window <- function(
  panel_df,
  candidate_days = NULL,
  content_type = "short",
  required_launch_capture_days = 7,
  min_videos = 10,
  target_median_remaining = 0.05,
  target_p75_remaining = 0.10
) {
  diagnostics <- build_bundle_e_short_window_diagnostics(
    panel_df = panel_df,
    candidate_days = candidate_days,
    content_type = content_type,
    required_launch_capture_days = required_launch_capture_days,
    min_videos = min_videos
  )

  if (nrow(diagnostics) == 0) {
    return(tibble::tibble())
  }

  passing <- diagnostics %>%
    dplyr::filter(
      .data$eligible_window,
      !is.na(.data$median_remaining_share),
      !is.na(.data$p75_remaining_share),
      .data$median_remaining_share <= target_median_remaining,
      .data$p75_remaining_share <= target_p75_remaining
    ) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(
      selection_method = "first_window_meeting_remaining_share_thresholds",
      required_launch_capture_days = required_launch_capture_days,
      target_median_remaining = target_median_remaining,
      target_p75_remaining = target_p75_remaining
    )

  if (nrow(passing) > 0) {
    return(passing)
  }

  diagnostics %>%
    dplyr::filter(.data$eligible_window) %>%
    dplyr::mutate(
      stability_score = .data$median_remaining_share + .data$p75_remaining_share,
      selection_method = "fallback_lowest_remaining_share_score",
      required_launch_capture_days = required_launch_capture_days,
      target_median_remaining = target_median_remaining,
      target_p75_remaining = target_p75_remaining
    ) %>%
    dplyr::arrange(.data$stability_score, .data$window_days) %>%
    dplyr::slice_head(n = 1)
}

build_bundle_e_short_window_video_scores <- function(
  panel_df,
  window_days,
  content_type = "short",
  required_launch_capture_days = 7
) {
  short_panel <- bundle_e_short_window_base(
    panel_df = panel_df,
    content_type = content_type,
    required_launch_capture_days = required_launch_capture_days
  )

  if (nrow(short_panel) == 0) {
    return(tibble::tibble())
  }

  scored <- bundle_e_short_window_snapshot_at_day(short_panel, target_day = window_days) %>%
    dplyr::filter(.data$latest_age_days >= window_days) %>%
    dplyr::transmute(
      `Video ID`,
      Title,
      `Channel Name`,
      publish_date,
      `Published At`,
      `Content Type`,
      topic,
      tags,
      snapshot_date_at_window = .data$snapshot_date,
      video_age_days_at_window = .data$video_age_days,
      window_days = window_days,
      latest_views,
      views_at_window,
      share_of_latest,
      remaining_share
    ) %>%
    dplyr::arrange(dplyr::desc(.data$views_at_window), dplyr::desc(.data$share_of_latest))

  if (nrow(scored) == 0) {
    return(scored)
  }

  median_views_at_window <- stats::median(scored$views_at_window, na.rm = TRUE)

  scored %>%
    dplyr::mutate(
      views_at_window_percentile = dplyr::percent_rank(.data$views_at_window),
      relative_to_window_median = bundle_e_safe_divide(.data$views_at_window, median_views_at_window),
      early_performance_bucket = dplyr::case_when(
        .data$views_at_window_percentile >= 0.90 ~ "top_10pct",
        .data$views_at_window_percentile >= 0.75 ~ "top_25pct",
        .data$views_at_window_percentile >= 0.25 ~ "middle_50pct",
        TRUE ~ "bottom_25pct"
      ),
      standout_within_window = .data$views_at_window_percentile >= 0.90
    ) %>%
    dplyr::arrange(dplyr::desc(.data$views_at_window), dplyr::desc(.data$share_of_latest))
}

build_bundle_e_short_window_analysis <- function(
  panel_df,
  candidate_days = NULL,
  content_type = "short",
  required_launch_capture_days = 7,
  min_videos = 10,
  target_median_remaining = 0.05,
  target_p75_remaining = 0.10
) {
  diagnostics <- build_bundle_e_short_window_diagnostics(
    panel_df = panel_df,
    candidate_days = candidate_days,
    content_type = content_type,
    required_launch_capture_days = required_launch_capture_days,
    min_videos = min_videos
  )

  selected_window <- choose_bundle_e_short_window(
    panel_df = panel_df,
    candidate_days = candidate_days,
    content_type = content_type,
    required_launch_capture_days = required_launch_capture_days,
    min_videos = min_videos,
    target_median_remaining = target_median_remaining,
    target_p75_remaining = target_p75_remaining
  )

  chosen_day <- if (nrow(selected_window) == 0) NA_real_ else selected_window$window_days[[1]]

  video_scores <- if (is.na(chosen_day)) {
    tibble::tibble()
  } else {
    build_bundle_e_short_window_video_scores(
      panel_df = panel_df,
      window_days = chosen_day,
      content_type = content_type,
      required_launch_capture_days = required_launch_capture_days
    )
  }

  list(
    window_diagnostics = diagnostics,
    selected_window = selected_window,
    video_scores = video_scores
  )
}
