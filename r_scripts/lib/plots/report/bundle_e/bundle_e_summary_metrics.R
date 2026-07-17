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


bundle_e_window_stats_vectors <- function(
  dates,
  values,
  window_days,
  anchor_date = max(dates, na.rm = TRUE)
) {
  if (length(values) == 0 || all(is.na(values))) {
    return(list(
      start_date = as.Date(NA),
      end_date = as.Date(NA),
      elapsed_days = NA_real_,
      gain = NA_real_,
      avg_per_day = NA_real_
    ))
  }

  window_start <- anchor_date - as.integer(window_days) + 1L
  keep <- which(
    !is.na(dates) &
      dates >= window_start &
      dates <= anchor_date
  )
  if (length(keep) == 0) {
    return(list(
      start_date = as.Date(NA),
      end_date = as.Date(NA),
      elapsed_days = NA_real_,
      gain = NA_real_,
      avg_per_day = NA_real_
    ))
  }

  window_dates <- dates[keep]
  metric_vals <- suppressWarnings(as.numeric(values[keep]))
  order_idx <- order(window_dates)
  window_dates <- window_dates[order_idx]
  metric_vals <- metric_vals[order_idx]

  start_date <- window_dates[[1]]
  end_date <- window_dates[[length(window_dates)]]
  elapsed_days <- as.numeric(end_date - start_date)
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

bundle_e_observed_metric_by_age <- function(
  video_age_days,
  values,
  max_age,
  require_non_missing = FALSE
) {
  keep <- which(
    !is.na(video_age_days) &
      video_age_days >= 0 &
      video_age_days <= max_age
  )
  if (length(keep) == 0) {
    return(NA_real_)
  }

  metric_vals <- suppressWarnings(as.numeric(values[keep]))
  if (isTRUE(require_non_missing) && all(is.na(metric_vals))) {
    return(NA_real_)
  }
  suppressWarnings(max(metric_vals, na.rm = TRUE))
}

build_bundle_e_video_summary <- function(panel_df) {
  if (nrow(panel_df) == 0) {
    return(tibble::tibble())
  }

  summary_df <- panel_df %>%
    dplyr::arrange(.data$`Video ID`, .data$snapshot_date) %>%
    dplyr::group_by(.data$`Video ID`) %>%
    dplyr::summarise(
      Title = dplyr::last(.data$Title),
      `Channel Name` = dplyr::last(.data$`Channel Name`),
      `Channel ID` = dplyr::last(.data$`Channel ID`),
      publish_date = dplyr::last(.data$publish_date),
      `Published At` = dplyr::last(.data$`Published At`),
      `Content Type` = dplyr::last(.data$`Content Type`),
      content_type = dplyr::last(.data$content_type),
      topic = dplyr::last(.data$topic),
      tags = dplyr::last(.data$tags),
      primary_reference = dplyr::last(.data$primary_reference),
      collab_group = dplyr::last(.data$collab_group),
      collaborative_energy = if ("collaborative_energy" %in% names(panel_df)) {
        dplyr::last(.data$collaborative_energy)
      } else {
        NA
      },
      latest_snapshot_date = dplyr::last(.data$snapshot_date),
      latest_views = suppressWarnings(as.numeric(dplyr::last(.data$views_cumulative))),
      latest_revenue = suppressWarnings(as.numeric(dplyr::last(.data$revenue_cumulative))),
      latest_age_days = suppressWarnings(as.numeric(dplyr::last(.data$video_age_days))),
      lifetime_avg_views_per_day = dplyr::if_else(
        !is.na(.data$latest_age_days) & .data$latest_age_days > 0,
        .data$latest_views / .data$latest_age_days,
        NA_real_
      ),
      lifetime_avg_revenue_per_day = dplyr::if_else(
        !is.na(.data$latest_age_days) & .data$latest_age_days > 0,
        .data$latest_revenue / .data$latest_age_days,
        NA_real_
      ),
      window_first_observed_date = min(.data$snapshot_date, na.rm = TRUE),
      window_last_observed_date = max(.data$snapshot_date, na.rm = TRUE),
      window_first_observed_views = suppressWarnings(as.numeric(dplyr::first(.data$views_cumulative))),
      window_first_observed_revenue = suppressWarnings(as.numeric(dplyr::first(.data$revenue_cumulative))),
      window_num_observations = dplyr::n(),
      observed_span_days = as.numeric(
        max(.data$snapshot_date, na.rm = TRUE) -
          min(.data$snapshot_date, na.rm = TRUE)
      ),
      observed_view_gain = .data$latest_views - .data$window_first_observed_views,
      observed_revenue_gain = .data$latest_revenue - .data$window_first_observed_revenue,
      observed_avg_views_per_day = dplyr::if_else(
        !is.na(.data$observed_span_days) & .data$observed_span_days > 0,
        .data$observed_view_gain / .data$observed_span_days,
        NA_real_
      ),
      observed_avg_revenue_per_day = dplyr::if_else(
        !is.na(.data$observed_span_days) & .data$observed_span_days > 0,
        .data$observed_revenue_gain / .data$observed_span_days,
        NA_real_
      ),
      full_first_observed_date = dplyr::last(.data$full_first_observed_date),
      full_last_observed_date = dplyr::last(.data$full_last_observed_date),
      full_num_observations = dplyr::last(.data$full_num_observations),
      full_observed_span_days = dplyr::last(.data$full_observed_span_days),
      full_first_observed_views = dplyr::last(.data$full_first_observed_views),
      full_first_observed_revenue = dplyr::last(.data$full_first_observed_revenue),
      full_first_observed_age_days = dplyr::last(.data$full_first_observed_age_days),
      launch_capture_lag_days = dplyr::last(.data$launch_capture_lag_days),
      captured_on_publish_day = dplyr::last(.data$captured_on_publish_day),
      captured_within_7_days = dplyr::last(.data$captured_within_7_days),
      captured_within_30_days = dplyr::last(.data$captured_within_30_days),
      captured_within_60_days = dplyr::last(.data$captured_within_60_days),
      captured_within_90_days = dplyr::last(.data$captured_within_90_days),
      late_entry_into_panel = dplyr::last(.data$late_entry_into_panel),
      .recent_7 = list(bundle_e_window_stats_vectors(
        .data$snapshot_date,
        .data$views_cumulative,
        window_days = 7,
        anchor_date = dplyr::last(.data$snapshot_date)
      )),
      .recent_30 = list(bundle_e_window_stats_vectors(
        .data$snapshot_date,
        .data$views_cumulative,
        window_days = 30,
        anchor_date = dplyr::last(.data$snapshot_date)
      )),
      .prior_7 = list(bundle_e_window_stats_vectors(
        .data$snapshot_date,
        .data$views_cumulative,
        window_days = 7,
        anchor_date = dplyr::last(.data$snapshot_date) - 7L
      )),
      .revenue_recent_30 = list(bundle_e_window_stats_vectors(
        .data$snapshot_date,
        .data$revenue_cumulative,
        window_days = 30,
        anchor_date = dplyr::last(.data$snapshot_date)
      )),
      .views_day_7 = bundle_e_observed_metric_by_age(
        .data$video_age_days,
        .data$views_cumulative,
        max_age = 7
      ),
      .views_day_30 = bundle_e_observed_metric_by_age(
        .data$video_age_days,
        .data$views_cumulative,
        max_age = 30
      ),
      .views_day_60 = bundle_e_observed_metric_by_age(
        .data$video_age_days,
        .data$views_cumulative,
        max_age = 60
      ),
      .views_day_90 = bundle_e_observed_metric_by_age(
        .data$video_age_days,
        .data$views_cumulative,
        max_age = 90
      ),
      .revenue_day_30 = bundle_e_observed_metric_by_age(
        .data$video_age_days,
        .data$revenue_cumulative,
        max_age = 30,
        require_non_missing = TRUE
      ),
      .revenue_day_90 = bundle_e_observed_metric_by_age(
        .data$video_age_days,
        .data$revenue_cumulative,
        max_age = 90,
        require_non_missing = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      recent_7d_views_gain = purrr::map_dbl(.data$.recent_7, "gain"),
      recent_30d_views_gain = purrr::map_dbl(.data$.recent_30, "gain"),
      recent_7d_avg_views_per_day = purrr::map_dbl(.data$.recent_7, "avg_per_day"),
      recent_30d_avg_views_per_day = purrr::map_dbl(.data$.recent_30, "avg_per_day"),
      prior_7d_avg_views_per_day = purrr::map_dbl(.data$.prior_7, "avg_per_day"),
      recent_30d_revenue_gain = purrr::map_dbl(.data$.revenue_recent_30, "gain"),
      recent_30d_avg_revenue_per_day = purrr::map_dbl(.data$.revenue_recent_30, "avg_per_day"),
      growth_acceleration_ratio = bundle_e_safe_divide(
        .data$recent_7d_avg_views_per_day,
        .data$prior_7d_avg_views_per_day
      ),
      views_observed_by_day_7 = .data$.views_day_7,
      views_observed_by_day_30 = .data$.views_day_30,
      views_observed_by_day_60 = .data$.views_day_60,
      views_observed_by_day_90 = .data$.views_day_90,
      revenue_observed_by_day_30 = .data$.revenue_day_30,
      revenue_observed_by_day_90 = .data$.revenue_day_90,
      launch_window_7d_fully_observed = !is.na(.data$launch_capture_lag_days) &
        .data$launch_capture_lag_days <= 7 &
        !is.na(.data$latest_age_days) &
        .data$latest_age_days >= 7,
      launch_window_30d_fully_observed = !is.na(.data$launch_capture_lag_days) &
        .data$launch_capture_lag_days <= 30 &
        !is.na(.data$latest_age_days) &
        .data$latest_age_days >= 30,
      launch_window_60d_fully_observed = !is.na(.data$launch_capture_lag_days) &
        .data$launch_capture_lag_days <= 60 &
        !is.na(.data$latest_age_days) &
        .data$latest_age_days >= 60,
      launch_window_90d_fully_observed = !is.na(.data$launch_capture_lag_days) &
        .data$launch_capture_lag_days <= 90 &
        !is.na(.data$latest_age_days) &
        .data$latest_age_days >= 90
    ) %>%
    dplyr::select(-dplyr::starts_with("."))

  summary_df %>%
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
        !is.na(.data$growth_acceleration_ratio) &
          is.finite(.data$growth_acceleration_ratio) &
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

build_bundle_e_publish_cohort_performance_by_type <- function(video_summary) {
  if (nrow(video_summary) == 0 || !"content_type" %in% names(video_summary)) {
    return(tibble::tibble())
  }

  video_summary %>%
    dplyr::filter(!is.na(.data$publish_cohort)) %>%
    dplyr::filter(!is.na(.data$content_type), nzchar(trimws(as.character(.data$content_type)))) %>%
    dplyr::mutate(
      content_type = tolower(trimws(as.character(.data$content_type))),
      content_type_label = dplyr::case_when(
        .data$content_type == "short" ~ "Short",
        .data$content_type == "video" ~ "Video",
        .data$content_type == "live" ~ "Stream",
        TRUE ~ tools::toTitleCase(.data$content_type)
      )
    ) %>%
    dplyr::group_by(.data$publish_cohort, .data$content_type, .data$content_type_label) %>%
    dplyr::summarise(
      video_count = dplyr::n(),
      median_recent_30d_avg_views_per_day = stats::median(.data$recent_30d_avg_views_per_day, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$content_type, .data$publish_cohort)
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
  top_n = NULL,
  top_prop = 0.10,
  min_top_n = 5
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
    {
      if (is.null(top_n)) {
        dplyr::mutate(., top_cutoff = pmax(min_top_n, ceiling(dplyr::n() * top_prop)))
      } else {
        dplyr::mutate(., top_cutoff = top_n)
      }
    } %>%
    dplyr::filter(.data$rank_within_type <= .data$top_cutoff) %>%
    dplyr::ungroup() %>%
    dplyr::select("rank_within_type", dplyr::any_of(select_cols))

  list(
    video_type_highest_overall_performing = highest_overall,
    video_type_newest_five_videos = newest_videos,
    video_type_top_performing_videos = top_performing
  )
}

build_bundle_e_type_age_curve_comparison <- function(
  panel_df,
  comparison_df,
  newest_df,
  content_type = "live",
  comparison_label = "Top-performing uploads",
  newest_label = "Newest uploads",
  overlap_label = "Top-performing + newest uploads"
) {
  if (nrow(panel_df) == 0) {
    return(tibble::tibble())
  }

  type_key <- tolower(trimws(as.character(content_type)))
  comparison_sel <- comparison_df %>%
    dplyr::filter(tolower(trimws(as.character(.data$`Content Type`))) == type_key) %>%
    dplyr::transmute(
      `Video ID` = .data$`Video ID`,
      Title = .data$Title,
      publish_date = .data$publish_date,
      comparison_role = comparison_label
    )

  newest_sel <- newest_df %>%
    dplyr::filter(tolower(trimws(as.character(.data$`Content Type`))) == type_key) %>%
    dplyr::transmute(
      `Video ID` = .data$`Video ID`,
      Title = .data$Title,
      publish_date = .data$publish_date,
      comparison_role = newest_label
    )

  selected <- dplyr::bind_rows(comparison_sel, newest_sel) %>%
    dplyr::group_by(.data$`Video ID`) %>%
    dplyr::summarise(
      Title = dplyr::first(.data$Title),
      publish_date = dplyr::first(.data$publish_date),
      comparison_role = dplyr::case_when(
        any(.data$comparison_role == comparison_label) & any(.data$comparison_role == newest_label) ~ overlap_label,
        any(.data$comparison_role == comparison_label) ~ comparison_label,
        any(.data$comparison_role == newest_label) ~ newest_label,
        TRUE ~ dplyr::first(.data$comparison_role)
      ),
      .groups = "drop"
    )

  if (nrow(selected) == 0) {
    return(tibble::tibble())
  }

  panel_df %>%
    dplyr::filter(.data$`Video ID` %in% selected$`Video ID`) %>%
    dplyr::filter(!is.na(.data$video_age_days), .data$video_age_days >= 0) %>%
    dplyr::filter(!is.na(.data$views_cumulative), is.finite(.data$views_cumulative)) %>%
    dplyr::left_join(selected, by = "Video ID", suffix = c("", "_selected")) %>%
    dplyr::mutate(
      Title = dplyr::coalesce(.data$Title_selected, .data$Title),
      publish_date = dplyr::coalesce(.data$publish_date_selected, .data$publish_date),
      comparison_role = factor(
        .data$comparison_role,
        levels = c(
          comparison_label,
          newest_label,
          overlap_label
        ),
        ordered = TRUE
      )
    ) %>%
    dplyr::arrange(.data$`Video ID`, .data$video_age_days, .data$snapshot_date) %>%
    dplyr::select(
      "Video ID",
      "Title",
      "publish_date",
      "snapshot_date",
      "video_age_days",
      "views_cumulative",
      "comparison_role"
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
