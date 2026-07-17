suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

source(file.path("r_scripts", "lib", "plots", "report", "bundle_e", "bundle_e_panel_prep.R"))
source(file.path("r_scripts", "lib", "plots", "report", "bundle_e", "bundle_e_summary_metrics.R"))

assert_true <- function(x, message) {
  if (!isTRUE(x)) {
    stop(message, call. = FALSE)
  }
}

assert_equal <- function(x, y, message, tolerance = 1e-12) {
  comparison <- all.equal(x, y, check.attributes = TRUE, tolerance = tolerance)
  if (!isTRUE(comparison)) {
    stop(message, ": ", paste(comparison, collapse = " | "), call. = FALSE)
  }
}

video_panel_rows <- function(
  video_id,
  snapshot_dates,
  views,
  revenue,
  publish_date,
  launch_capture_lag_days = 0L,
  content_type = "video"
) {
  snapshot_dates <- as.Date(snapshot_dates)
  publish_date <- as.Date(publish_date)
  video_age_days <- as.integer(snapshot_dates - publish_date)
  first_date <- min(snapshot_dates)
  last_date <- max(snapshot_dates)
  first_age <- as.integer(first_date - publish_date)

  tibble::tibble(
    `Video ID` = video_id,
    Title = paste("Video", video_id),
    `Channel Name` = "Test Channel",
    `Channel ID` = "channel-1",
    publish_date = publish_date,
    `Published At` = as.POSIXct(publish_date, tz = "UTC"),
    `Content Type` = content_type,
    content_type = content_type,
    topic = "test topic",
    tags = "test, fixture",
    primary_reference = "test reference",
    collab_group = "Non-collaborative",
    collaborative_energy = FALSE,
    snapshot_date = snapshot_dates,
    views_cumulative = views,
    revenue_cumulative = revenue,
    video_age_days = video_age_days,
    full_first_observed_date = first_date,
    full_last_observed_date = last_date,
    full_num_observations = length(snapshot_dates),
    full_observed_span_days = as.integer(last_date - first_date),
    full_first_observed_views = views[[1]],
    full_first_observed_revenue = revenue[[1]],
    full_first_observed_age_days = first_age,
    launch_capture_lag_days = launch_capture_lag_days,
    captured_on_publish_day = launch_capture_lag_days == 0,
    captured_within_7_days = launch_capture_lag_days <= 7,
    captured_within_30_days = launch_capture_lag_days <= 30,
    captured_within_60_days = launch_capture_lag_days <= 60,
    captured_within_90_days = launch_capture_lag_days <= 90,
    late_entry_into_panel = launch_capture_lag_days > 30
  )
}

panel <- dplyr::bind_rows(
  video_panel_rows(
    video_id = "A",
    snapshot_dates = c("2026-01-01", "2026-01-20", "2026-01-25", "2026-01-27", "2026-02-01"),
    views = c(10, 40, 50, 70, 100),
    revenue = c(1, 4, 5, 7, 10),
    publish_date = "2026-01-01"
  ),
  video_panel_rows(
    video_id = "B",
    snapshot_dates = "2026-02-01",
    views = 25,
    revenue = NA_real_,
    publish_date = "2026-01-30",
    launch_capture_lag_days = 2L
  ),
  video_panel_rows(
    video_id = "C",
    snapshot_dates = c("2026-01-01", "2026-01-11"),
    views = c(5, 15),
    revenue = c(NA_real_, NA_real_),
    publish_date = "2026-01-01"
  )
) %>%
  dplyr::arrange(dplyr::desc(.data$snapshot_date), .data$`Video ID`)

summary_df <- build_bundle_e_video_summary(panel)

expected_columns <- c(
  "Video ID",
  "Title",
  "Channel Name",
  "Channel ID",
  "publish_date",
  "Published At",
  "Content Type",
  "content_type",
  "topic",
  "tags",
  "primary_reference",
  "collab_group",
  "collaborative_energy",
  "latest_snapshot_date",
  "latest_views",
  "latest_revenue",
  "latest_age_days",
  "lifetime_avg_views_per_day",
  "lifetime_avg_revenue_per_day",
  "window_first_observed_date",
  "window_last_observed_date",
  "window_first_observed_views",
  "window_first_observed_revenue",
  "window_num_observations",
  "observed_span_days",
  "observed_view_gain",
  "observed_revenue_gain",
  "observed_avg_views_per_day",
  "observed_avg_revenue_per_day",
  "full_first_observed_date",
  "full_last_observed_date",
  "full_num_observations",
  "full_observed_span_days",
  "full_first_observed_views",
  "full_first_observed_revenue",
  "full_first_observed_age_days",
  "launch_capture_lag_days",
  "captured_on_publish_day",
  "captured_within_7_days",
  "captured_within_30_days",
  "captured_within_60_days",
  "captured_within_90_days",
  "late_entry_into_panel",
  "recent_7d_views_gain",
  "recent_30d_views_gain",
  "recent_7d_avg_views_per_day",
  "recent_30d_avg_views_per_day",
  "prior_7d_avg_views_per_day",
  "recent_30d_revenue_gain",
  "recent_30d_avg_revenue_per_day",
  "growth_acceleration_ratio",
  "views_observed_by_day_7",
  "views_observed_by_day_30",
  "views_observed_by_day_60",
  "views_observed_by_day_90",
  "revenue_observed_by_day_30",
  "revenue_observed_by_day_90",
  "launch_window_7d_fully_observed",
  "launch_window_30d_fully_observed",
  "launch_window_60d_fully_observed",
  "launch_window_90d_fully_observed",
  "publish_cohort",
  "front_loaded_flag",
  "evergreen_flag",
  "plateau_flag",
  "reacceleration_flag"
)

assert_equal(names(summary_df), expected_columns, "Video summary schema changed")
assert_equal(summary_df$`Video ID`, c("A", "B", "C"), "Video summary ordering changed")

video_a <- dplyr::filter(summary_df, .data$`Video ID` == "A")
assert_equal(video_a$latest_views[[1]], 100, "Latest views should use the final snapshot")
assert_equal(video_a$recent_7d_views_gain[[1]], 30, "Recent seven-day gain is incorrect")
assert_equal(video_a$recent_7d_avg_views_per_day[[1]], 6, "Recent seven-day rate is incorrect")
assert_equal(video_a$recent_30d_views_gain[[1]], 60, "Recent 30-day gain is incorrect")
assert_equal(video_a$recent_30d_avg_views_per_day[[1]], 5, "Recent 30-day rate is incorrect")
assert_equal(video_a$prior_7d_avg_views_per_day[[1]], 2, "Prior seven-day rate is incorrect")
assert_equal(video_a$growth_acceleration_ratio[[1]], 3, "Growth acceleration ratio is incorrect")
assert_equal(video_a$views_observed_by_day_7[[1]], 10, "Day-seven observed views are incorrect")
assert_equal(video_a$views_observed_by_day_30[[1]], 70, "Day-30 observed views are incorrect")
assert_equal(video_a$revenue_observed_by_day_30[[1]], 7, "Day-30 observed revenue is incorrect")
assert_true(video_a$launch_window_30d_fully_observed[[1]], "The 30-day launch window should be fully observed")
assert_true(!video_a$launch_window_60d_fully_observed[[1]], "The 60-day launch window should not be fully observed")
assert_true(video_a$reacceleration_flag[[1]], "Video A should be classified as reaccelerating")

video_b <- dplyr::filter(summary_df, .data$`Video ID` == "B")
assert_equal(video_b$recent_7d_views_gain[[1]], 0, "A single snapshot should have zero gain")
assert_true(is.na(video_b$recent_7d_avg_views_per_day[[1]]), "A single snapshot should not have a daily rate")
assert_true(is.na(video_b$recent_30d_revenue_gain[[1]]), "All-missing revenue should remain missing")
assert_true(is.na(video_b$revenue_observed_by_day_30[[1]]), "All-missing observed revenue should remain missing")

video_c <- dplyr::filter(summary_df, .data$`Video ID` == "C")
assert_true(is.na(video_c$recent_30d_revenue_gain[[1]]), "Missing revenue history should remain missing")
assert_true(is.na(video_c$revenue_observed_by_day_90[[1]]), "Missing revenue-by-age should remain missing")

summary_without_collab_col <- build_bundle_e_video_summary(
  dplyr::select(panel, -"collaborative_energy")
)
assert_true(
  all(is.na(summary_without_collab_col$collaborative_energy)),
  "Missing optional collaboration flags should produce missing summary values"
)

legacy_window <- bundle_e_window_stats(
  dplyr::filter(panel, .data$`Video ID` == "A"),
  metric_col = "views_cumulative",
  window_days = 30,
  anchor_date = as.Date("2026-02-01")
)
vector_window <- bundle_e_window_stats_vectors(
  dplyr::filter(panel, .data$`Video ID` == "A")$snapshot_date,
  dplyr::filter(panel, .data$`Video ID` == "A")$views_cumulative,
  window_days = 30,
  anchor_date = as.Date("2026-02-01")
)
assert_equal(vector_window, legacy_window, "Vector window calculations should match the legacy helper")

assert_equal(
  build_bundle_e_video_summary(tibble::tibble()),
  tibble::tibble(),
  "Empty panels should return an empty tibble"
)

cat("Bundle E video summary tests passed\n")
