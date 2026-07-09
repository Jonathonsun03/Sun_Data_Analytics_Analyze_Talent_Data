bundle_e_empty_plot <- function(title, subtitle = NULL, label = "No data available for current filters.") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = label, size = 4.4) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    theme_void() +
    ggplot2::labs(title = title, subtitle = subtitle)
}

bundle_e_title_case <- function(x) {
  out <- trimws(as.character(x))
  out[!nzchar(out)] <- NA_character_
  stringr::str_to_title(out)
}

bundle_e_first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  x[[1]]
}

bundle_e_age_curve_comparison_table <- function(curve_df) {
  if (nrow(curve_df) == 0) {
    return(curve_df)
  }

  curve_df %>%
    dplyr::group_by(.data$`Video ID`) %>%
    dplyr::slice_max(order_by = .data$video_age_days, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      `Comparison Group` = .data$comparison_role,
      Title = .data$Title,
      `Publish Date` = .data$publish_date,
      `Latest Age Days` = .data$video_age_days,
      `Latest Views` = .data$views_cumulative
    ) %>%
    dplyr::arrange(.data$`Comparison Group`, dplyr::desc(.data$`Latest Views`), .data$Title)
}

bundle_e_attribute_summary_table <- function(df, group_col, group_label, top_n = 12) {
  if (nrow(df) == 0 || !(group_col %in% names(df))) {
    return(df[0, , drop = FALSE])
  }

  df %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::transmute(
      !!group_label := .data[[group_col]],
      `Video Count` = .data$video_count,
      `Median Latest Views` = .data$median_latest_views,
      `Median Lifetime Avg Views/Day` = .data$median_lifetime_avg_views_per_day,
      `Median Recent 30-Day Avg Views/Day` = .data$median_recent_30d_avg_views_per_day
    )
}

bundle_e_topic_video_type_table <- function(video_summary, top_n = 12, min_videos = 2) {
  if (nrow(video_summary) == 0 || !"topic" %in% names(video_summary)) {
    return(tibble::tibble())
  }

  out <- video_summary %>%
    dplyr::mutate(
      Topic = trimws(as.character(.data$topic)),
      `Content Type` = dplyr::case_when(
        .data$`Content Type` == "live" ~ "Stream",
        .data$`Content Type` == "short" ~ "Short",
        .data$`Content Type` == "video" ~ "Video",
        TRUE ~ bundle_e_title_case(.data$`Content Type`)
      )
    ) %>%
    dplyr::filter(
      !is.na(.data$Topic),
      nzchar(.data$Topic),
      !is.na(.data$recent_30d_avg_views_per_day),
      is.finite(.data$recent_30d_avg_views_per_day)
    ) %>%
    dplyr::group_by(.data$Topic, .data$`Content Type`) %>%
    dplyr::summarise(
      `Video Count` = dplyr::n_distinct(.data$`Video ID`),
      `Median Recent 30-Day Avg Views/Day` = stats::median(.data$recent_30d_avg_views_per_day, na.rm = TRUE),
      `Median Latest Views` = stats::median(.data$latest_views, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$`Video Count` >= min_videos) %>%
    dplyr::arrange(dplyr::desc(.data$`Median Recent 30-Day Avg Views/Day`), dplyr::desc(.data$`Video Count`))

  out %>%
    dplyr::slice_head(n = top_n)
}

bundle_e_abbrev_leader_table <- function(df, classification) {
  if (nrow(df) == 0) {
    return(df)
  }

  if (identical(classification, "evergreen")) {
    return(
      df %>%
        dplyr::transmute(
          Title = .data$Title,
          `Publish Date` = .data$publish_date,
          `Content Type` = bundle_e_title_case(.data$`Content Type`),
          Classification = "Evergreen"
        )
    )
  }

  df %>%
    dplyr::transmute(
      Title = .data$Title,
      `Publish Date` = .data$publish_date,
      `Content Type` = bundle_e_title_case(.data$`Content Type`),
      Classification = dplyr::case_when(
        .data$sleeper_flag & .data$reacceleration_flag ~ "Sleeper + Re-accelerating",
        .data$reacceleration_flag ~ "Re-accelerating",
        .data$sleeper_flag ~ "Sleeper",
        TRUE ~ "Other"
      )
    )
}

bundle_e_combined_leader_table <- function(evergreen_df, sleeper_df) {
  dplyr::bind_rows(
    bundle_e_abbrev_leader_table(evergreen_df, "evergreen"),
    bundle_e_abbrev_leader_table(sleeper_df, "sleeper")
  ) %>%
    dplyr::arrange(.data$Classification, dplyr::desc(.data$`Publish Date`), .data$Title)
}

bundle_e_classification_guide_table <- function() {
  tibble::tibble(
    Classification = c(
      "Evergreen",
      "Front-loaded",
      "Re-accelerating",
      "Sleeper",
      "Sleeper + Re-accelerating"
    ),
    `Plain-language meaning` = c(
      "The video is still pulling a meaningful amount of views well after release.",
      "The video got most of its traction early and has cooled off a lot since launch.",
      "The video is gaining momentum again after an earlier slower period.",
      "The video did not stand out early, but is now showing stronger recent performance.",
      "The video started quietly and is now picking up again in a noticeable way."
    ),
    `What it usually suggests` = c(
      "This content has longer-tail value and may keep contributing to the library over time.",
      "This content depended more on launch energy than on long-run durability.",
      "Something may have renewed interest, such as search, recommendations, clips, or a new audience fit.",
      "The video may have hidden long-tail potential that was not obvious at launch.",
      "This is a stronger late-bloomer signal and is often worth a closer look for repeatable patterns."
    )
  )
}

build_bundle_e_overall_summary_table <- function(
  library_growth_snapshot,
  video_lifecycle_summary,
  dataset_sizes,
  included_date_range = "All available dates"
) {
  latest_library_row <- library_growth_snapshot %>%
    dplyr::arrange(dplyr::desc(.data$snapshot_date)) %>%
    dplyr::slice_head(n = 1)

  tibble::tibble(
    Metric = c(
      "Included date range",
      "Latest snapshot date",
      "Observed snapshots",
      "Unique videos in lifecycle summary",
      "Latest observed library views",
      "Panel rows in window"
    ),
    Value = c(
      included_date_range,
      if (nrow(latest_library_row) == 0) NA_character_ else as.character(latest_library_row$snapshot_date[[1]]),
      nrow(library_growth_snapshot),
      if (nrow(video_lifecycle_summary) == 0) 0 else dplyr::n_distinct(video_lifecycle_summary$`Video ID`),
      if (nrow(latest_library_row) == 0) NA_character_ else scales::comma(bundle_e_first_non_missing(latest_library_row$total_library_views)),
      if (nrow(dataset_sizes) == 0) NA_character_ else as.character(bundle_e_first_non_missing(dataset_sizes$rows[dataset_sizes$dataset == "panel_window"]))
    )
  )
}

build_bundle_e_window_summary_table <- function(
  library_growth_snapshot,
  video_lifecycle_summary,
  panel_coverage_summary,
  artifact_root,
  window_mode,
  window_days,
  included_date_range = "All available dates"
) {
  latest_library_row <- library_growth_snapshot %>%
    dplyr::arrange(dplyr::desc(.data$snapshot_date)) %>%
    dplyr::slice_head(n = 1)

  window_setting <- dplyr::case_when(
    identical(window_mode, "explicit_range") ~ "Explicit date range",
    identical(window_mode, "days_back") && !is.na(window_days) ~ paste0("Last ", window_days, " days"),
    TRUE ~ "All available dates"
  )

  tibble::tibble(
    Metric = c(
      "Window setting",
      "Included date range",
      "Artifact root",
      "Latest snapshot date",
      "Snapshot count",
      "Unique videos",
      "Median observations per video"
    ),
    Value = c(
      window_setting,
      included_date_range,
      artifact_root,
      if (nrow(latest_library_row) == 0) NA_character_ else as.character(latest_library_row$snapshot_date[[1]]),
      nrow(library_growth_snapshot),
      if (nrow(video_lifecycle_summary) == 0) 0 else dplyr::n_distinct(video_lifecycle_summary$`Video ID`),
      if (nrow(panel_coverage_summary) == 0 || !("median_observations_per_video" %in% names(panel_coverage_summary))) {
        NA_character_
      } else {
        as.character(bundle_e_first_non_missing(panel_coverage_summary$median_observations_per_video))
      }
    )
  )
}
