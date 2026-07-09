# Bundle E Architecture Plan

Date: 2026-03-20

## Goal

Bundle E should follow the same overall architecture pattern used by Bundle A and Bundle B, but its analytical focus is different:

- Bundle A = broad talent-level performance and growth dashboard
- Bundle B = content strategy and optimization dashboard
- Bundle E = video lifecycle and library growth dashboard built from longitudinal video-level observations

The immediate implementation target is:

- `r_scripts/run/bundle_E/import_data.r`

This should become the canonical prep and artifact-export layer for Bundle E.

## Key Architectural Understanding

The static artifact export layer and the final client-facing HTML layer are related, but they are not the same thing.

In Bundle A and Bundle B:

- `import_data.r` loads helpers, selects talent data, applies date-window controls, builds prepared objects, and exports static evidence artifacts to datalake.
- The report Rmd rebuilds the visualizations for the final report and can render them interactively.
- The report Rmd also reads generated interpretation markdown from the `interpretations/` folder and inserts it back into the report.

For Bundle E, we should follow the same pattern.

## Why This Matters

The Bundle E import layer should not be treated as a one-off script that only dumps temporary outputs.
It should be the reusable longitudinal prep spine that supports both:

1. artifact export for AI/custom text generation
2. later interactive Rmd output for the client-facing HTML report

## Planned Bundle E Build Order

1. Define the Bundle E data contract.
   Confirm the required fields for longitudinal video analysis:
   - stable video ID
   - observation date / snapshot date
   - publish date
   - views
   - classification metadata such as content type, topic, tag, and collaboration

2. Inspect one real talent's longitudinal data.
   Do not assume the repeated-snapshot structure; verify how it actually appears in staged/datalake files first.

3. Define the Bundle E artifact objects and key names.
   These should align with future prompt keys and report section names.
   Likely object groups:
   - `library_growth_snapshot`
   - `back_catalog_vs_recent_contribution`
   - `video_lifecycle_curves`
   - `early_velocity_vs_long_tail`
   - `publish_cohort_performance`
   - `evergreen_video_leaders`
   - `sleeper_reacceleration_candidates`
   - `content_type_topic_tag_longevity`

4. Build `r_scripts/run/bundle_E/import_data.r` in the same style as Bundle A/B.
   It should:
   - load shared helpers
   - resolve talent/data roots
   - apply window controls
   - build longitudinal prep tables and static plots
   - write artifacts to datalake under `<datalake_root>/<talent>/reports/bundle_E/artifacts/`
   - export `bundle_e_ai_inputs.json`
   - export `bundle_e_artifact_manifest.json`

5. Validate the artifact stage for one talent.
   Confirm the outputs are clean and the prep logic is analytically sound before moving on.

6. Only after that, design the Bundle E Rmd/render layer.
   That layer will be responsible for the interactive HTML report that is sent to clients.

## Important Design Note

The client-facing deliverable will ultimately be the Bundle E Rmd-rendered HTML report.
However, the first thing to stabilize is the longitudinal prep/artifact layer, because the Rmd and interpretation pipeline should both sit on top of that same data contract.

## Next Step After This Note

Select one talent with sufficient repeated video snapshots and inspect the available longitudinal fields so Bundle E can be designed around real data instead of assumptions.

## Additional Bundle E Design Notes

### Title classifier enrichment should be part of Bundle E

Like Bundle A and Bundle B, Bundle E should pair `Video ID` with the title-classification metadata.
This gives Bundle E access to the same useful enrichment fields, including:

- `content_type`
- `topic`
- `tags`
- `primary_reference`
- collaboration-style signals such as `collaborative_energy`
- other boolean thematic dimensions exported by the classifier

This is important because Bundle E is not only about raw growth curves.
It also needs to answer which types of videos, topics, tags, and collaboration patterns produce better long-tail behavior.

### Confirmed sample talent for development

Use `Leia Memoria【Variance Project】` as the sample longitudinal dataset for Bundle E design.

Observed so far:

- `video_analytics` snapshot files: 57
- `video_monetary` snapshot files: 57
- snapshot date range: `2025-12-05` to `2026-03-20`
- distinct videos in analytics history: 246
- repeated videos: 245
- median observations per video: 57
- max observations for a single video: 57

This confirms that the longitudinal panel structure is strong enough for Bundle E development.

### Granularity principle

Current highest available granularity is daily snapshots.
For Bundle E, daily should be treated as the base level of observation.

However, the reporting layer will probably need both:

- daily-level internal calculations
- summarized windows for interpretation and dashboards

Likely summary windows:

- first 7 days
- first 30 days
- first 60 days
- first 90 days
- latest 7-day / 14-day / 30-day observed momentum

### Important distinction: cumulative-valid vs observation-dependent metrics

Bundle E should separate metrics into two families.

1. Metrics that remain valid because views are cumulative:
   - latest views
   - lifetime age in days at latest observation
   - lifetime average views per day as of latest observation

2. Metrics that depend on when the dataset first captured the video:
   - first observed views
   - observed growth since first capture
   - launch-window growth
   - early velocity curves

This distinction matters because some videos were first observed after publish day 0.

### Missing-early-coverage concept

We need an explicit variable for how late a video entered our observed panel.

Suggested core variable:

- `first_observed_age_days = first_observed_date - publish_date`

Related flags:

- `captured_on_publish_day`
- `captured_within_7_days`
- `captured_within_30_days`
- `late_entry_into_panel`

Suggested label:

- `launch_capture_lag_days`

Interpretation:

- if `launch_capture_lag_days = 0`, we observed the video from publish day
- if it is greater than 0, early growth before first capture is unobserved

This gives us a clean way to identify where launch-curve conclusions are safe versus partial.

### Candidate variable families for Bundle E

#### 1. Video identity + static metadata

- `Video ID`
- `Title`
- `publish_date`
- classifier fields: `content_type`, `topic`, `tags`, `primary_reference`, collaboration/thematic booleans

#### 2. Observation-level longitudinal variables

- `snapshot_date`
- `video_age_days`
- `views_cumulative`
- `revenue_cumulative` when available
- `days_since_prior_snapshot`
- `views_delta_since_prior_snapshot`
- `revenue_delta_since_prior_snapshot`
- `avg_views_per_day_between_snapshots = views_delta_since_prior_snapshot / days_since_prior_snapshot`

Important note:
Use static/cumulative views as the source of truth.
Daily growth should be derived from change between snapshots rather than replacing cumulative counts.

#### 3. Coverage / panel-entry variables

- `first_observed_date`
- `last_observed_date`
- `first_observed_age_days`
- `launch_capture_lag_days`
- `observed_span_days`
- `num_observations`
- `is_launch_window_observed_7d`
- `is_launch_window_observed_30d`
- `is_launch_window_observed_60d`
- `is_launch_window_observed_90d`

#### 4. Video-level summary descriptors

- `latest_views`
- `latest_revenue`
- `latest_age_days`
- `lifetime_avg_views_per_day = latest_views / latest_age_days`
- `observed_view_gain = latest_views - first_observed_views`
- `observed_avg_views_per_day = observed_view_gain / observed_span_days`
- `recent_7d_avg_views_per_day`
- `recent_30d_avg_views_per_day`
- `growth_acceleration_ratio = recent_7d_avg_views_per_day / prior_window_avg_views_per_day`
- `plateau_flag`
- `evergreen_flag`
- `front_loaded_flag`
- `reacceleration_flag`

#### 5. Cohort / library variables

- `publish_month`
- `publish_cohort`
- `library_age_bucket` such as `0-7`, `8-30`, `31-60`, `61-90`, `91+`
- back-catalog vs recent contribution shares
- concentration metrics showing whether a few videos dominate recent growth

### Practical rule for first-window metrics

For metrics like:

- first 7-day growth
- first 30-day growth
- early velocity ranking

only compute them as true launch-window metrics when the video was captured early enough.

Example:

- 7-day launch metrics should require `first_observed_age_days <= 7`
- 30-day launch metrics should require `first_observed_age_days <= 30`

Otherwise:

- either return `NA`
- or label the metric as not fully observed

### Bundle E questions these variables should help answer

These variable families should support the Bundle E description directly:

- which videos are still growing vs plateauing
- whether performance is driven by recent uploads or back catalog
- evergreen vs front-loaded behavior
- growth by age windows such as 7/30/60/90 days
- which content types, topics, tags, and collaboration patterns drive long-tail outcomes
- which videos re-accelerate after initial release
- whether the overall library is becoming more durable over time
