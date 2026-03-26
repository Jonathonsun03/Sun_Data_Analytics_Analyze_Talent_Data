bundle_e_window_stats <- function(df, metric_col, window_days, anchor_date = max(df$snapshot_date, na.rm = TRUE)) {
  if (!metric_col %in% names(df) || nrow(df) == 0 || all(is.na(df[[metric_col]]))) {
    return(list(
      start_date = as.Date(NA),
      end_date = as.Date(NA),
      elapsed_days = NA_real_,
      gain = NA_real_,
      avg_per_day = NA_real_
    ))
  }

  window_start <- anchor_date - as.integer(window_days) + 1L
  dat <- df %>%
    dplyr::filter(.data$snapshot_date >= window_start, .data$snapshot_date <= anchor_date) %>%
    dplyr::arrange(.data$snapshot_date)

  if (nrow(dat) == 0) {
    return(list(
      start_date = as.Date(NA),
      end_date = as.Date(NA),
      elapsed_days = NA_real_,
      gain = NA_real_,
      avg_per_day = NA_real_
    ))
  }

  start_date <- min(dat$snapshot_date, na.rm = TRUE)
  end_date <- max(dat$snapshot_date, na.rm = TRUE)
  elapsed_days <- as.numeric(end_date - start_date)
  metric_vals <- suppressWarnings(as.numeric(dat[[metric_col]]))
  gain <- if (length(metric_vals) >= 2) {
    metric_vals[[length(metric_vals)]] - metric_vals[[1]]
  } else {
    0
  }

  list(
    start_date = start_date,
    end_date = end_date,
    elapsed_days = elapsed_days,
    gain = gain,
    avg_per_day = if (!is.na(elapsed_days) && elapsed_days > 0) gain / elapsed_days else NA_real_
  )
}

bundle_e_observed_views_by_age <- function(df, max_age) {
  dat <- df %>%
    dplyr::filter(!is.na(.data$video_age_days), .data$video_age_days >= 0, .data$video_age_days <= max_age) %>%
    dplyr::arrange(.data$video_age_days, .data$snapshot_date)

  if (nrow(dat) == 0) {
    return(NA_real_)
  }

  suppressWarnings(max(dat$views_cumulative, na.rm = TRUE))
}

bundle_e_observed_revenue_by_age <- function(df, max_age) {
  if (!"revenue_cumulative" %in% names(df)) {
    return(NA_real_)
  }

  dat <- df %>%
    dplyr::filter(!is.na(.data$video_age_days), .data$video_age_days >= 0, .data$video_age_days <= max_age) %>%
    dplyr::arrange(.data$video_age_days, .data$snapshot_date)

  if (nrow(dat) == 0 || all(is.na(dat$revenue_cumulative))) {
    return(NA_real_)
  }

  suppressWarnings(max(dat$revenue_cumulative, na.rm = TRUE))
}

build_bundle_e_video_summary <- function(panel_df) {
  if (nrow(panel_df) == 0) {
    return(tibble::tibble())
  }

  panel_df %>%
    dplyr::arrange(.data$`Video ID`, .data$snapshot_date) %>%
    dplyr::group_by(.data$`Video ID`) %>%
    dplyr::group_modify(function(df, key) {
      df <- dplyr::arrange(df, .data$snapshot_date)
      latest <- dplyr::slice_tail(df, n = 1)
      first_window <- dplyr::slice_head(df, n = 1)
      latest_date <- latest$snapshot_date[[1]]

      recent_7 <- bundle_e_window_stats(df, metric_col = "views_cumulative", window_days = 7, anchor_date = latest_date)
      recent_30 <- bundle_e_window_stats(df, metric_col = "views_cumulative", window_days = 30, anchor_date = latest_date)
      prior_7 <- bundle_e_window_stats(df, metric_col = "views_cumulative", window_days = 7, anchor_date = latest_date - 7L)
      revenue_recent_30 <- bundle_e_window_stats(df, metric_col = "revenue_cumulative", window_days = 30, anchor_date = latest_date)

      views_day_7 <- bundle_e_observed_views_by_age(df, 7)
      views_day_30 <- bundle_e_observed_views_by_age(df, 30)
      views_day_60 <- bundle_e_observed_views_by_age(df, 60)
      views_day_90 <- bundle_e_observed_views_by_age(df, 90)

      revenue_day_30 <- bundle_e_observed_revenue_by_age(df, 30)
      revenue_day_90 <- bundle_e_observed_revenue_by_age(df, 90)

      latest_views <- suppressWarnings(as.numeric(latest$views_cumulative[[1]]))
      latest_revenue <- suppressWarnings(as.numeric(latest$revenue_cumulative[[1]]))
      latest_age_days <- suppressWarnings(as.numeric(latest$video_age_days[[1]]))
      first_window_views <- suppressWarnings(as.numeric(first_window$views_cumulative[[1]]))
      first_window_revenue <- suppressWarnings(as.numeric(first_window$revenue_cumulative[[1]]))
      window_span_days <- as.numeric(max(df$snapshot_date, na.rm = TRUE) - min(df$snapshot_date, na.rm = TRUE))

      tibble::tibble(
        Title = latest$Title[[1]],
        `Channel Name` = latest$`Channel Name`[[1]],
        `Channel ID` = latest$`Channel ID`[[1]],
        publish_date = latest$publish_date[[1]],
        `Published At` = latest$`Published At`[[1]],
        `Content Type` = latest$`Content Type`[[1]],
        content_type = latest$content_type[[1]],
        topic = latest$topic[[1]],
        tags = latest$tags[[1]],
        primary_reference = latest$primary_reference[[1]],
        collab_group = latest$collab_group[[1]],
        collaborative_energy = if ("collaborative_energy" %in% names(latest)) latest$collaborative_energy[[1]] else NA,
        latest_snapshot_date = latest_date,
        latest_views = latest_views,
        latest_revenue = latest_revenue,
        latest_age_days = latest_age_days,
        lifetime_avg_views_per_day = if (!is.na(latest_age_days) && latest_age_days > 0) latest_views / latest_age_days else NA_real_,
        lifetime_avg_revenue_per_day = if (!is.na(latest_age_days) && latest_age_days > 0) latest_revenue / latest_age_days else NA_real_,
        window_first_observed_date = min(df$snapshot_date, na.rm = TRUE),
        window_last_observed_date = max(df$snapshot_date, na.rm = TRUE),
        window_first_observed_views = first_window_views,
        window_first_observed_revenue = first_window_revenue,
        window_num_observations = nrow(df),
        observed_span_days = window_span_days,
        observed_view_gain = latest_views - first_window_views,
        observed_revenue_gain = latest_revenue - first_window_revenue,
        observed_avg_views_per_day = if (!is.na(window_span_days) && window_span_days > 0) (latest_views - first_window_views) / window_span_days else NA_real_,
        observed_avg_revenue_per_day = if (!is.na(window_span_days) && window_span_days > 0) (latest_revenue - first_window_revenue) / window_span_days else NA_real_,
        full_first_observed_date = latest$full_first_observed_date[[1]],
        full_last_observed_date = latest$full_last_observed_date[[1]],
        full_num_observations = latest$full_num_observations[[1]],
        full_observed_span_days = latest$full_observed_span_days[[1]],
        full_first_observed_views = latest$full_first_observed_views[[1]],
        full_first_observed_revenue = latest$full_first_observed_revenue[[1]],
        full_first_observed_age_days = latest$full_first_observed_age_days[[1]],
        launch_capture_lag_days = latest$launch_capture_lag_days[[1]],
        captured_on_publish_day = latest$captured_on_publish_day[[1]],
        captured_within_7_days = latest$captured_within_7_days[[1]],
        captured_within_30_days = latest$captured_within_30_days[[1]],
        captured_within_60_days = latest$captured_within_60_days[[1]],
        captured_within_90_days = latest$captured_within_90_days[[1]],
        late_entry_into_panel = latest$late_entry_into_panel[[1]],
        recent_7d_views_gain = recent_7$gain,
        recent_30d_views_gain = recent_30$gain,
        recent_7d_avg_views_per_day = recent_7$avg_per_day,
        recent_30d_avg_views_per_day = recent_30$avg_per_day,
        prior_7d_avg_views_per_day = prior_7$avg_per_day,
        recent_30d_revenue_gain = revenue_recent_30$gain,
        recent_30d_avg_revenue_per_day = revenue_recent_30$avg_per_day,
        growth_acceleration_ratio = bundle_e_safe_divide(recent_7$avg_per_day, prior_7$avg_per_day)[[1]],
        views_observed_by_day_7 = views_day_7,
        views_observed_by_day_30 = views_day_30,
        views_observed_by_day_60 = views_day_60,
        views_observed_by_day_90 = views_day_90,
        revenue_observed_by_day_30 = revenue_day_30,
        revenue_observed_by_day_90 = revenue_day_90,
        launch_window_7d_fully_observed = !is.na(latest$launch_capture_lag_days[[1]]) &&
          latest$launch_capture_lag_days[[1]] <= 7 &&
          !is.na(latest_age_days) &&
          latest_age_days >= 7,
        launch_window_30d_fully_observed = !is.na(latest$launch_capture_lag_days[[1]]) &&
          latest$launch_capture_lag_days[[1]] <= 30 &&
          !is.na(latest_age_days) &&
          latest_age_days >= 30,
        launch_window_60d_fully_observed = !is.na(latest$launch_capture_lag_days[[1]]) &&
          latest$launch_capture_lag_days[[1]] <= 60 &&
          !is.na(latest_age_days) &&
          latest_age_days >= 60,
        launch_window_90d_fully_observed = !is.na(latest$launch_capture_lag_days[[1]]) &&
          latest$launch_capture_lag_days[[1]] <= 90 &&
          !is.na(latest_age_days) &&
          latest_age_days >= 90
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      publish_cohort = lubridate::floor_date(.data$publish_date, unit = "month"),
      front_loaded_flag = dplyr::if_else(
        .data$launch_window_30d_fully_observed &
          !is.na(.data$views_observed_by_day_30) &
          !is.na(.data$recent_30d_avg_views_per_day) &
          (.data$views_observed_by_day_30 / 30) > 0 &
          .data$recent_30d_avg_views_per_day <= (.data$views_observed_by_day_30 / 30) * 0.10,
        TRUE,
        FALSE,
        missing = FALSE
      ),
      evergreen_flag = dplyr::if_else(
        !is.na(.data$latest_age_days) &
          .data$latest_age_days >= 90 &
          !is.na(.data$recent_30d_avg_views_per_day) &
          !is.na(.data$lifetime_avg_views_per_day) &
          .data$recent_30d_avg_views_per_day >= (.data$lifetime_avg_views_per_day * 0.25),
        TRUE,
        FALSE,
        missing = FALSE
      ),
      plateau_flag = dplyr::if_else(
        !is.na(.data$latest_age_days) &
          .data$latest_age_days >= 30 &
          !is.na(.data$recent_30d_avg_views_per_day) &
          !is.na(.data$lifetime_avg_views_per_day) &
          .data$recent_30d_avg_views_per_day <= (.data$lifetime_avg_views_per_day * 0.05),
        TRUE,
        FALSE,
        missing = FALSE
      ),
      reacceleration_flag = dplyr::if_else(
        !is.na(.data$growth_acceleration_ratio) & is.finite(.data$growth_acceleration_ratio) &
          .data$growth_acceleration_ratio >= 1.5,
        TRUE,
        FALSE,
        missing = FALSE
      )
    )
}

add_bundle_e_sleeper_flag <- function(video_summary) {
  if (nrow(video_summary) == 0) {
    return(video_summary)
  }

  early_cut <- stats::median(
    video_summary$views_observed_by_day_30[video_summary$launch_window_30d_fully_observed],
    na.rm = TRUE
  )
  recent_cut <- stats::median(video_summary$recent_30d_avg_views_per_day, na.rm = TRUE)

  if (!is.finite(early_cut)) early_cut <- NA_real_
  if (!is.finite(recent_cut)) recent_cut <- NA_real_

  video_summary %>%
    dplyr::mutate(
      sleeper_flag = dplyr::if_else(
        .data$launch_window_30d_fully_observed &
          !is.na(.data$views_observed_by_day_30) &
          !is.na(.data$recent_30d_avg_views_per_day) &
          !is.na(early_cut) &
          !is.na(recent_cut) &
          .data$views_observed_by_day_30 <= early_cut &
          .data$recent_30d_avg_views_per_day >= recent_cut,
        TRUE,
        FALSE,
        missing = FALSE
      )
    )
}

build_bundle_e_library_growth_snapshot <- function(panel_df) {
  if (nrow(panel_df) == 0) {
    return(tibble::tibble())
  }

  panel_df %>%
    dplyr::group_by(.data$snapshot_date) %>%
    dplyr::summarise(
      tracked_videos = dplyr::n_distinct(.data$`Video ID`),
      total_library_views = sum(.data$views_cumulative, na.rm = TRUE),
      total_library_revenue = sum(.data$revenue_cumulative, na.rm = TRUE),
      recent_30d_views = sum(.data$views_cumulative[.data$video_age_days <= 30], na.rm = TRUE),
      back_catalog_views = sum(.data$views_cumulative[.data$video_age_days > 30], na.rm = TRUE),
      recent_30d_share = bundle_e_safe_divide(.data$recent_30d_views, .data$total_library_views),
      back_catalog_share = bundle_e_safe_divide(.data$back_catalog_views, .data$total_library_views),
      median_video_age_days = stats::median(.data$video_age_days, na.rm = TRUE),
      .groups = "drop"
    )
}

build_bundle_e_back_catalog_contribution <- function(panel_df) {
  if (nrow(panel_df) == 0) {
    return(tibble::tibble())
  }

  panel_df %>%
    dplyr::filter(!is.na(.data$library_age_bucket)) %>%
    dplyr::group_by(.data$snapshot_date, .data$library_age_bucket) %>%
    dplyr::summarise(
      tracked_videos = dplyr::n_distinct(.data$`Video ID`),
      total_views = sum(.data$views_cumulative, na.rm = TRUE),
      total_revenue = sum(.data$revenue_cumulative, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$snapshot_date) %>%
    dplyr::mutate(
      view_share = bundle_e_safe_divide(.data$total_views, sum(.data$total_views, na.rm = TRUE)),
      revenue_share = bundle_e_safe_divide(.data$total_revenue, sum(.data$total_revenue, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()
}

build_bundle_e_panel_coverage_summary <- function(video_summary) {
  if (nrow(video_summary) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    metric = c(
      "videos",
      "captured_on_publish_day",
      "captured_within_7_days",
      "captured_within_30_days",
      "captured_within_90_days",
      "late_entry_into_panel",
      "median_launch_capture_lag_days",
      "median_full_num_observations",
      "median_latest_age_days"
    ),
    value = c(
      nrow(video_summary),
      sum(video_summary$captured_on_publish_day, na.rm = TRUE),
      sum(video_summary$captured_within_7_days, na.rm = TRUE),
      sum(video_summary$captured_within_30_days, na.rm = TRUE),
      sum(video_summary$captured_within_90_days, na.rm = TRUE),
      sum(video_summary$late_entry_into_panel, na.rm = TRUE),
      stats::median(video_summary$launch_capture_lag_days, na.rm = TRUE),
      stats::median(video_summary$full_num_observations, na.rm = TRUE),
      stats::median(video_summary$latest_age_days, na.rm = TRUE)
    )
  )
}

build_bundle_e_publish_cohort_performance <- function(video_summary) {
  if (nrow(video_summary) == 0) {
    return(tibble::tibble())
  }

  video_summary %>%
    dplyr::filter(!is.na(.data$publish_cohort)) %>%
    dplyr::group_by(.data$publish_cohort) %>%
    dplyr::summarise(
      video_count = dplyr::n(),
      total_latest_views = sum(.data$latest_views, na.rm = TRUE),
      median_latest_views = stats::median(.data$latest_views, na.rm = TRUE),
      median_lifetime_avg_views_per_day = stats::median(.data$lifetime_avg_views_per_day, na.rm = TRUE),
      median_recent_30d_avg_views_per_day = stats::median(.data$recent_30d_avg_views_per_day, na.rm = TRUE),
      evergreen_rate = mean(.data$evergreen_flag, na.rm = TRUE),
      front_loaded_rate = mean(.data$front_loaded_flag, na.rm = TRUE),
      sleeper_rate = mean(.data$sleeper_flag, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$publish_cohort)
}

build_bundle_e_attribute_summary <- function(video_summary, group_col) {
  if (nrow(video_summary) == 0 || !group_col %in% names(video_summary)) {
    return(tibble::tibble())
  }

  video_summary %>%
    dplyr::mutate(.group = as.character(.data[[group_col]])) %>%
    dplyr::filter(!is.na(.data$.group), nzchar(trimws(.data$.group))) %>%
    dplyr::group_by(.data$.group) %>%
    dplyr::summarise(
      video_count = dplyr::n(),
      median_latest_views = stats::median(.data$latest_views, na.rm = TRUE),
      median_lifetime_avg_views_per_day = stats::median(.data$lifetime_avg_views_per_day, na.rm = TRUE),
      median_recent_30d_avg_views_per_day = stats::median(.data$recent_30d_avg_views_per_day, na.rm = TRUE),
      median_launch_capture_lag_days = stats::median(.data$launch_capture_lag_days, na.rm = TRUE),
      evergreen_rate = mean(.data$evergreen_flag, na.rm = TRUE),
      front_loaded_rate = mean(.data$front_loaded_flag, na.rm = TRUE),
      reacceleration_rate = mean(.data$reacceleration_flag, na.rm = TRUE),
      sleeper_rate = mean(.data$sleeper_flag, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(group_value = ".group") %>%
    dplyr::arrange(dplyr::desc(.data$median_recent_30d_avg_views_per_day), dplyr::desc(.data$video_count))
}

build_bundle_e_tag_longevity <- function(video_summary) {
  if (nrow(video_summary) == 0 || !"tags" %in% names(video_summary)) {
    return(tibble::tibble())
  }

  video_summary %>%
    dplyr::mutate(.tag_raw = as.character(.data$tags)) %>%
    dplyr::mutate(.tag_raw = ifelse(is.na(.data$.tag_raw), "", .data$.tag_raw)) %>%
    tidyr::separate_rows(".tag_raw", sep = ",") %>%
    dplyr::mutate(tag_group = trimws(tolower(.data$.tag_raw))) %>%
    dplyr::filter(nzchar(.data$tag_group)) %>%
    dplyr::group_by(.data$tag_group) %>%
    dplyr::summarise(
      video_count = dplyr::n_distinct(.data$`Video ID`),
      median_latest_views = stats::median(.data$latest_views, na.rm = TRUE),
      median_lifetime_avg_views_per_day = stats::median(.data$lifetime_avg_views_per_day, na.rm = TRUE),
      median_recent_30d_avg_views_per_day = stats::median(.data$recent_30d_avg_views_per_day, na.rm = TRUE),
      evergreen_rate = mean(.data$evergreen_flag, na.rm = TRUE),
      front_loaded_rate = mean(.data$front_loaded_flag, na.rm = TRUE),
      sleeper_rate = mean(.data$sleeper_flag, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$median_recent_30d_avg_views_per_day), dplyr::desc(.data$video_count))
}

build_bundle_e_video_type_detail_tables <- function(
  video_summary,
  newest_n = 5,
  top_n = 10
) {
  if (nrow(video_summary) == 0 || !"Content Type" %in% names(video_summary)) {
    return(list(
      video_type_highest_overall_performing = tibble::tibble(),
      video_type_newest_five_videos = tibble::tibble(),
      video_type_top_performing_videos = tibble::tibble()
    ))
  }

  base <- video_summary %>%
    dplyr::filter(!is.na(.data$`Content Type`), nzchar(trimws(.data$`Content Type`))) %>%
    dplyr::mutate(`Content Type` = tolower(trimws(as.character(.data$`Content Type`))))

  select_cols <- c(
    "Content Type",
    "Video ID",
    "Title",
    "publish_date",
    "latest_snapshot_date",
    "latest_views",
    "latest_revenue",
    "latest_age_days",
    "lifetime_avg_views_per_day",
    "recent_30d_avg_views_per_day",
    "recent_30d_views_gain",
    "launch_capture_lag_days",
    "evergreen_flag",
    "front_loaded_flag",
    "sleeper_flag",
    "reacceleration_flag"
  )

  highest_overall <- base %>%
    dplyr::group_by(.data$`Content Type`) %>%
    dplyr::arrange(
      dplyr::desc(.data$latest_views),
      dplyr::desc(.data$recent_30d_avg_views_per_day),
      .by_group = TRUE
    ) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(select_cols))

  newest_videos <- base %>%
    dplyr::group_by(.data$`Content Type`) %>%
    dplyr::arrange(dplyr::desc(.data$publish_date), dplyr::desc(.data$latest_views), .by_group = TRUE) %>%
    dplyr::mutate(rank_within_type = dplyr::row_number()) %>%
    dplyr::slice_head(n = newest_n) %>%
    dplyr::ungroup() %>%
    dplyr::select("rank_within_type", dplyr::any_of(select_cols))

  top_performing <- base %>%
    dplyr::group_by(.data$`Content Type`) %>%
    dplyr::arrange(
      dplyr::desc(.data$latest_views),
      dplyr::desc(.data$recent_30d_avg_views_per_day),
      .by_group = TRUE
    ) %>%
    dplyr::mutate(rank_within_type = dplyr::row_number()) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::ungroup() %>%
    dplyr::select("rank_within_type", dplyr::any_of(select_cols))

  list(
    video_type_highest_overall_performing = highest_overall,
    video_type_newest_five_videos = newest_videos,
    video_type_top_performing_videos = top_performing
  )
}

build_bundle_e_leaders <- function(video_summary) {
  evergreen <- video_summary %>%
    dplyr::arrange(dplyr::desc(.data$recent_30d_avg_views_per_day), dplyr::desc(.data$latest_views)) %>%
    dplyr::slice_head(n = 25)

  sleepers <- video_summary %>%
    dplyr::filter(.data$sleeper_flag | .data$reacceleration_flag) %>%
    dplyr::arrange(dplyr::desc(.data$growth_acceleration_ratio), dplyr::desc(.data$recent_30d_avg_views_per_day)) %>%
    dplyr::slice_head(n = 25)

  list(
    evergreen_video_leaders = evergreen,
    sleeper_reacceleration_candidates = sleepers
  )
}
