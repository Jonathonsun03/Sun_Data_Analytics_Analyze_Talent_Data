# Bundle E Column Engineering Plan

Date: 2026-03-20

## Purpose

This note locks the first-pass Bundle E column plan before implementation work in:

- `r_scripts/run/bundle_E/import_data.r`

Bundle E is different from Bundle A and Bundle B because it must preserve the full daily-ish panel of repeated video snapshots over time.

## Important architectural consequence

The standard Bundle A / Bundle B analytics prep path is not sufficient for Bundle E as-is because:

- `prepare_analytics()` deduplicates to one row per video

That is correct for snapshot-style dashboards, but incorrect for longitudinal video tracking.

Bundle E therefore needs a custom prep path that:

1. preserves every observed snapshot row
2. joins title-classifier metadata onto all snapshot rows
3. engineers observation-level and video-level longitudinal variables from that full panel

## Core data model

Bundle E should be built around two layers:

### 1. Observation-level panel

One row per:

- `Video ID`
- `snapshot_date`

This is the base layer for all longitudinal calculations.

### 2. Video-level summary

One row per:

- `Video ID`

This layer should aggregate lifecycle descriptors so the report can rank, compare, bucket, and summarize videos cleanly.

## Column groups

### A. Identity and static metadata

These should be carried through to the video-level summary, usually using the latest available non-missing value.

- `Video ID`
- `Title`
- `Channel Name`
- `Channel ID`
- `Published At`
- `publish_date`
- `Content Type`
- `content_type`
- `topic`
- `tags`
- `primary_reference`
- title-classifier booleans such as:
  - `collaborative_energy`
  - `community_milestones`
  - `interactive_entertainment`
  - `meme_viral`
  - `monetization`
  - `narrative_serialization`
  - `performance_artistry`
  - `personality_conversation`

### B. Observation-level panel columns

These must exist or be engineered for each snapshot row.

- `snapshot_date`
- `publish_date`
- `video_age_days`
- `views_cumulative`
- `revenue_cumulative`
- `days_since_prior_snapshot`
- `views_delta_since_prior_snapshot`
- `revenue_delta_since_prior_snapshot`
- `avg_views_per_day_between_snapshots`
- `avg_revenue_per_day_between_snapshots`
- `is_latest_snapshot_for_video`
- `library_age_bucket`

Suggested library age buckets:

- `0_7_days`
- `8_30_days`
- `31_60_days`
- `61_90_days`
- `91_plus_days`

### C. Panel coverage / capture-quality columns

These are critical because not all videos were captured at publish day 0.

- `first_observed_date`
- `last_observed_date`
- `first_observed_views`
- `first_observed_revenue`
- `first_observed_age_days`
- `launch_capture_lag_days`
- `num_observations`
- `observed_span_days`
- `captured_on_publish_day`
- `captured_within_7_days`
- `captured_within_30_days`
- `captured_within_60_days`
- `captured_within_90_days`
- `late_entry_into_panel`

Recommended interpretation:

- `launch_capture_lag_days = first_observed_date - publish_date`

This should be one of the most important Bundle E quality-control variables.

### D. Video-level lifecycle summary columns

These summarize how a video has behaved across the observed panel.

- `latest_snapshot_date`
- `latest_views`
- `latest_revenue`
- `latest_age_days`
- `lifetime_avg_views_per_day`
- `lifetime_avg_revenue_per_day`
- `observed_view_gain`
- `observed_revenue_gain`
- `observed_avg_views_per_day`
- `observed_avg_revenue_per_day`
- `recent_7d_views_gain`
- `recent_30d_views_gain`
- `recent_7d_avg_views_per_day`
- `recent_30d_avg_views_per_day`
- `prior_7d_avg_views_per_day`
- `growth_acceleration_ratio`

### E. Early-window / launch-window metrics

These should only be trusted when early enough observations exist.

- `views_observed_by_day_7`
- `views_observed_by_day_30`
- `views_observed_by_day_60`
- `views_observed_by_day_90`
- `launch_window_7d_fully_observed`
- `launch_window_30d_fully_observed`
- `launch_window_60d_fully_observed`
- `launch_window_90d_fully_observed`

Rule:

- only calculate or interpret these as launch-window metrics when the video entered the panel early enough

Example:

- 7-day launch metrics should require `launch_capture_lag_days <= 7`
- 30-day launch metrics should require `launch_capture_lag_days <= 30`

Otherwise:

- the metric should be `NA`
- or clearly marked as partially observed

### F. Derived behavioral labels

These can start as provisional helper flags and may be refined later.

- `front_loaded_flag`
- `evergreen_flag`
- `plateau_flag`
- `reacceleration_flag`
- `sleeper_flag`

These labels should be derived conservatively from the summary metrics and should be treated as descriptive, not authoritative.

## Recommended first-pass artifact tables

The initial Bundle E import layer should likely export tables keyed around:

- `bundle_e_dataset_sizes`
- `bundle_e_panel_coverage_summary`
- `library_growth_snapshot`
- `back_catalog_vs_recent_contribution`
- `video_lifecycle_summary`
- `video_lifecycle_panel_sample`
- `publish_cohort_performance`
- `evergreen_video_leaders`
- `sleeper_reacceleration_candidates`
- `content_type_longevity`
- `topic_longevity`
- `tag_longevity`

## Recommended first-pass static plots

The initial Bundle E import layer should likely export static ggplot artifacts for:

- library total views over time
- share of views by library age bucket over time
- latest video age vs latest views
- recent momentum vs lifetime average views per day
- publish cohort performance
- content-type longevity comparison

## Validity rules to preserve

### Metrics that remain valid even if early observation is missing

- `latest_views`
- `latest_revenue`
- `latest_age_days`
- `lifetime_avg_views_per_day`
- back-catalog contribution
- recent observed momentum

### Metrics that depend on early observation

- day-7 / day-30 / day-60 / day-90 launch metrics
- launch velocity
- front-loaded vs slow-start interpretation based on launch behavior

These should never be treated as fully observed when the panel missed the launch window.
