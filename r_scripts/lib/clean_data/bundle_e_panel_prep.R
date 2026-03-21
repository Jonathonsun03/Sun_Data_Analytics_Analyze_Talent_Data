bundle_e_safe_divide <- function(num, den) {
  out <- rep(NA_real_, length(num))
  keep <- !is.na(num) & !is.na(den) & is.finite(num) & is.finite(den) & den != 0
  out[keep] <- num[keep] / den[keep]
  out
}

bundle_e_parse_bool <- function(x) {
  if (is.logical(x)) {
    return(tidyr::replace_na(x, FALSE))
  }
  txt <- tolower(trimws(as.character(x)))
  txt %in% c("true", "t", "1", "yes", "y", "collab", "collaborative")
}

bundle_e_derive_collab_flag <- function(df, collab_col = "collaborative_energy", tags_col = "tags") {
  collab_from_col <- rep(FALSE, nrow(df))
  if (collab_col %in% names(df)) {
    collab_from_col <- bundle_e_parse_bool(df[[collab_col]])
  }

  collab_from_tags <- rep(FALSE, nrow(df))
  if (tags_col %in% names(df)) {
    tag_txt <- tolower(as.character(df[[tags_col]]))
    collab_from_tags <- grepl("collab|collaboration|duo|guest", tag_txt) & !is.na(tag_txt)
  }

  collab_from_col | collab_from_tags
}

bundle_e_bucket_age <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x <= 7 ~ "0_7_days",
    x <= 30 ~ "8_30_days",
    x <= 60 ~ "31_60_days",
    x <= 90 ~ "61_90_days",
    TRUE ~ "91_plus_days"
  )
}

apply_bundle_e_window <- function(df, window_mode, window_start_date, window_end_date) {
  if (identical(window_mode, "all_data") || nrow(df) == 0) {
    return(df)
  }

  df %>%
    dplyr::mutate(.window_date = as.Date(.data$snapshot_date)) %>%
    dplyr::filter(
      !is.na(.data$.window_date),
      .data$.window_date >= window_start_date,
      .data$.window_date <= window_end_date
    ) %>%
    dplyr::select(-".window_date")
}

build_bundle_e_long_panel <- function(
  files,
  titles,
  talent,
  type = "video_analytics"
) {
  analytics_raw <- .get_type_data(files, type = type)

  analytics <- analytics_raw %>%
    attach_title_classifications(titles = titles, by = "Video ID") %>%
    clean_published_at() %>%
    clean_duration_cols()

  if (exists("apply_content_type_filter", mode = "function")) {
    analytics <- apply_content_type_filter(
      analytics,
      output_col = "Content Type",
      keep_diagnostics = TRUE
    )
  }

  analytics <- analytics %>%
    dplyr::mutate(
      talent_name = talent,
      snapshot_date = suppressWarnings(as.Date(.data$date)),
      `Published At` = suppressWarnings(as.POSIXct(.data$`Published At`, tz = "UTC")),
      publish_date = as.Date(.data$publish_date),
      views_cumulative = suppressWarnings(as.numeric(.data$views)),
      collab_flag = bundle_e_derive_collab_flag(.),
      collab_group = dplyr::if_else(.data$collab_flag, "Collaborative", "Non-collaborative")
    ) %>%
    dplyr::arrange(.data$`Video ID`, .data$snapshot_date, .data$`Published At`) %>%
    dplyr::distinct(.data$`Video ID`, .data$snapshot_date, .keep_all = TRUE)

  monetary <- tryCatch(
    .get_type_data(files, type = "video_monetary"),
    error = function(e) tibble::tibble()
  )

  if (nrow(monetary) > 0) {
    monetary <- monetary %>%
      attach_title_classifications(titles = titles, by = "Video ID") %>%
      clean_published_at() %>%
      dplyr::mutate(
        snapshot_date = suppressWarnings(as.Date(.data$date)),
        revenue_cumulative = suppressWarnings(as.numeric(.data$`Estimated Revenue`)),
        cpm_value = suppressWarnings(as.numeric(.data$CPM))
      ) %>%
      dplyr::arrange(.data$`Video ID`, .data$snapshot_date) %>%
      dplyr::distinct(.data$`Video ID`, .data$snapshot_date, .keep_all = TRUE) %>%
      dplyr::select(
        dplyr::all_of(c("Video ID", "snapshot_date")),
        dplyr::any_of(c("revenue_cumulative", "cpm_value", "views_monetary_check"))
      )

    analytics <- analytics %>%
      dplyr::left_join(monetary, by = c("Video ID", "snapshot_date"))
  } else {
    analytics <- analytics %>%
      dplyr::mutate(
        revenue_cumulative = NA_real_,
        cpm_value = NA_real_
      )
  }

  analytics %>%
    dplyr::arrange(.data$`Video ID`, .data$snapshot_date) %>%
    dplyr::group_by(.data$`Video ID`) %>%
    dplyr::mutate(
      video_age_days = as.integer(.data$snapshot_date - .data$publish_date),
      library_age_bucket = bundle_e_bucket_age(.data$video_age_days),
      full_first_observed_date = min(.data$snapshot_date, na.rm = TRUE),
      full_last_observed_date = max(.data$snapshot_date, na.rm = TRUE),
      full_num_observations = dplyr::n(),
      full_first_observed_views = dplyr::first(.data$views_cumulative),
      full_last_observed_views = dplyr::last(.data$views_cumulative),
      full_first_observed_revenue = dplyr::first(.data$revenue_cumulative),
      full_last_observed_revenue = dplyr::last(.data$revenue_cumulative),
      full_observed_span_days = as.integer(.data$full_last_observed_date - .data$full_first_observed_date),
      full_first_observed_age_days = as.integer(.data$full_first_observed_date - .data$publish_date),
      launch_capture_lag_days = .data$full_first_observed_age_days,
      captured_on_publish_day = !is.na(.data$launch_capture_lag_days) & .data$launch_capture_lag_days == 0,
      captured_within_7_days = !is.na(.data$launch_capture_lag_days) & .data$launch_capture_lag_days <= 7,
      captured_within_30_days = !is.na(.data$launch_capture_lag_days) & .data$launch_capture_lag_days <= 30,
      captured_within_60_days = !is.na(.data$launch_capture_lag_days) & .data$launch_capture_lag_days <= 60,
      captured_within_90_days = !is.na(.data$launch_capture_lag_days) & .data$launch_capture_lag_days <= 90,
      late_entry_into_panel = !is.na(.data$launch_capture_lag_days) & .data$launch_capture_lag_days > 30,
      days_since_prior_snapshot = as.numeric(.data$snapshot_date - dplyr::lag(.data$snapshot_date)),
      views_delta_since_prior_snapshot = .data$views_cumulative - dplyr::lag(.data$views_cumulative),
      revenue_delta_since_prior_snapshot = .data$revenue_cumulative - dplyr::lag(.data$revenue_cumulative),
      avg_views_per_day_between_snapshots = bundle_e_safe_divide(
        .data$views_delta_since_prior_snapshot,
        .data$days_since_prior_snapshot
      ),
      avg_revenue_per_day_between_snapshots = bundle_e_safe_divide(
        .data$revenue_delta_since_prior_snapshot,
        .data$days_since_prior_snapshot
      ),
      is_latest_snapshot_for_video = .data$snapshot_date == max(.data$snapshot_date, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}
