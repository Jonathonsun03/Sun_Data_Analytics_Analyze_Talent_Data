library(tidyverse)
library(dplyr)
library(here)
library(purrr)

source("r_scripts/lib/utils/staging_root.R")
source("r_scripts/lib/utils/datalake_root.r")

bundle_e_plot_dir <- here("r_scripts", "lib", "plots", "report", "bundle_e")
if (dir.exists(bundle_e_plot_dir)) {
  list.files(bundle_e_plot_dir, full.names = TRUE) %>%
    walk(source)
}

list.files(here("r_scripts", "lib", "utils"), full.names = TRUE) %>%
  walk(source)
list.files(here("r_scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
list.files(here("r_scripts", "lib", "report_tables"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
source(here("r_scripts", "lib", "plots", "PlottingThemes.R"))
source(here::here("r_scripts", "lib", "clean_data", "CleanData.R"))

load_bundle_e_titles <- function(
  talent,
  titles_path = NULL
) {
  load_title_classifications(
    path = titles_path,
    talent = talent
  )
}
runtime_root <- report_resolve_data_root_from_env(
  data_source_env = "TALENT_DATA_SOURCE",
  data_root_env = "TALENT_DATA_ROOT",
  default_source = "datalake"
)
data_source <- runtime_root$data_source
root <- runtime_root$root

Talent <- trimws(Sys.getenv("TALENT_QUERY", unset = "Leia Memoria【Variance Project】"))
if (!nzchar(Talent)) {
  Talent <- "Leia Memoria【Variance Project】"
}

runtime_window <- report_resolve_window_from_env(
  window_days_env = "TALENT_WINDOW_DAYS",
  start_date_env = "TALENT_START_DATE",
  end_date_env = "TALENT_END_DATE"
)
window_days <- runtime_window$window_days
window_start_date <- runtime_window$window_start_date
window_end_date <- runtime_window$window_end_date
window_mode <- runtime_window$window_mode

included_date_range <- if (identical(window_mode, "explicit_range")) {
  paste(as.character(window_start_date), "to", as.character(window_end_date))
} else if (identical(window_mode, "days_back")) {
  paste(as.character(window_start_date), "to", as.character(window_end_date))
} else {
  "All available dates"
}

talent_root <- select_talent(Talent, root = root)
files <- TalentFiles(talent_root)
titles <- load_bundle_e_titles(Talent)

bundle_e_artifact_root <- report_resolve_artifact_root(
  talent_root = talent_root,
  bundle_name = "bundle_e",
  report_subdir = "reports",
  artifact_subdir = "artifacts",
  env_var = "BUNDLE_E_ARTIFACT_ROOT"
)

panel_full <- build_bundle_e_long_panel(
  files = files,
  titles = titles,
  talent = Talent
)

panel_window <- apply_bundle_e_window(panel_full, window_mode, window_start_date, window_end_date)

if (nrow(panel_window) == 0) {
  stop("No Bundle E analytics rows remain after applying the selected snapshot window.")
}

video_summary <- build_bundle_e_video_summary(panel_window) %>%
  add_bundle_e_sleeper_flag()

library_growth_snapshot <- build_bundle_e_library_growth_snapshot(panel_window)
back_catalog_contribution <- build_bundle_e_back_catalog_contribution(panel_window)
panel_coverage_summary <- build_bundle_e_panel_coverage_summary(video_summary)
publish_cohort_performance <- build_bundle_e_publish_cohort_performance(video_summary)
publish_cohort_performance_by_type <- build_bundle_e_publish_cohort_performance_by_type(video_summary)
video_type_longevity <- build_bundle_e_attribute_summary(video_summary, "Content Type")
topic_longevity <- build_bundle_e_attribute_summary(video_summary, "topic")
tag_longevity <- build_bundle_e_tag_longevity(video_summary)
video_type_detail_tables <- build_bundle_e_video_type_detail_tables(video_summary)
live_top_vs_newest_age_curve_comparison <- build_bundle_e_type_age_curve_comparison(
  panel_window,
  video_type_detail_tables$video_type_top_performing_videos,
  video_type_detail_tables$video_type_newest_five_videos,
  content_type = "live",
  comparison_label = "Top-performing live uploads",
  newest_label = "Newest live uploads",
  overlap_label = "Top-performing + newest live uploads"
)
video_highest_vs_newest_age_curve_comparison <- build_bundle_e_type_age_curve_comparison(
  panel_window,
  video_type_detail_tables$video_type_top_performing_videos,
  video_type_detail_tables$video_type_newest_five_videos,
  content_type = "video",
  comparison_label = "Top-performing video uploads",
  newest_label = "Newest video uploads",
  overlap_label = "Top-performing + newest video uploads"
)
short_highest_vs_newest_age_curve_comparison <- build_bundle_e_type_age_curve_comparison(
  panel_window,
  video_type_detail_tables$video_type_top_performing_videos,
  video_type_detail_tables$video_type_newest_five_videos,
  content_type = "short",
  comparison_label = "Top-performing shorts",
  newest_label = "Newest shorts",
  overlap_label = "Top-performing + newest shorts"
)
leaders <- build_bundle_e_leaders(video_summary)
short_window_analysis <- build_bundle_e_short_window_analysis(panel_full)

bundle_e_plots <- build_bundle_e_plot_set(
  library_growth_snapshot = library_growth_snapshot,
  back_catalog_contribution = back_catalog_contribution,
  video_summary = video_summary,
  publish_cohort_performance = publish_cohort_performance,
  publish_cohort_performance_by_type = publish_cohort_performance_by_type,
  video_type_longevity = video_type_longevity,
  live_age_curve_comparison = live_top_vs_newest_age_curve_comparison,
  video_age_curve_comparison = video_highest_vs_newest_age_curve_comparison,
  short_age_curve_comparison = short_highest_vs_newest_age_curve_comparison,
  short_window_diagnostics = short_window_analysis$window_diagnostics,
  short_window_video_scores = short_window_analysis$video_scores,
  talent = Talent
)

dataset_sizes <- tibble::tibble(
  dataset = c("panel_full", "panel_window", "video_summary"),
  rows = c(nrow(panel_full), nrow(panel_window), nrow(video_summary)),
  cols = c(ncol(panel_full), ncol(panel_window), ncol(video_summary))
)

overall_summary <- build_bundle_e_overall_summary_table(
  library_growth_snapshot = library_growth_snapshot,
  video_lifecycle_summary = video_summary,
  dataset_sizes = dataset_sizes,
  included_date_range = included_date_range
)

window_summary <- build_bundle_e_window_summary_table(
  library_growth_snapshot = library_growth_snapshot,
  video_lifecycle_summary = video_summary,
  panel_coverage_summary = panel_coverage_summary,
  artifact_root = bundle_e_artifact_root,
  window_mode = window_mode,
  window_days = window_days,
  included_date_range = included_date_range
)

bundle_e_tables <- list(
  dataset_sizes = dataset_sizes,
  overall_summary = overall_summary,
  panel_coverage_summary = panel_coverage_summary,
  library_growth_snapshot = library_growth_snapshot,
  back_catalog_vs_recent_contribution = back_catalog_contribution,
  video_lifecycle_summary = video_summary,
  video_lifecycle_panel_sample = panel_window %>%
    dplyr::arrange(.data$`Video ID`, .data$snapshot_date) %>%
    dplyr::slice_head(n = 500),
  publish_cohort_performance = publish_cohort_performance,
  publish_cohort_performance_by_type = publish_cohort_performance_by_type,
  video_type_longevity = video_type_longevity,
  short_window_diagnostics = short_window_analysis$window_diagnostics,
  short_window_selection = short_window_analysis$selected_window,
  short_window_video_scores = short_window_analysis$video_scores,
  video_type_highest_overall_performing = video_type_detail_tables$video_type_highest_overall_performing,
  video_type_newest_five_videos = video_type_detail_tables$video_type_newest_five_videos,
  video_type_top_performing_videos = video_type_detail_tables$video_type_top_performing_videos,
  live_top_vs_newest_age_curve_comparison = live_top_vs_newest_age_curve_comparison,
  video_highest_vs_newest_age_curve_comparison = video_highest_vs_newest_age_curve_comparison,
  short_highest_vs_newest_age_curve_comparison = short_highest_vs_newest_age_curve_comparison,
  live_uploads_included_in_comparison = bundle_e_age_curve_comparison_table(live_top_vs_newest_age_curve_comparison),
  video_uploads_included_in_comparison = bundle_e_age_curve_comparison_table(video_highest_vs_newest_age_curve_comparison),
  short_uploads_included_in_comparison = bundle_e_age_curve_comparison_table(short_highest_vs_newest_age_curve_comparison),
  evergreen_video_leaders = leaders$evergreen_video_leaders,
  sleeper_reacceleration_candidates = leaders$sleeper_reacceleration_candidates,
  lifecycle_classification_guide = bundle_e_classification_guide_table(),
  lifecycle_classification_leaders = bundle_e_combined_leader_table(
    leaders$evergreen_video_leaders,
    leaders$sleeper_reacceleration_candidates
  ),
  topic_longevity_by_video_type = bundle_e_topic_video_type_table(video_summary),
  topic_longevity_display = bundle_e_attribute_summary_table(topic_longevity, "group_value", "Topic"),
  tag_longevity_display = bundle_e_attribute_summary_table(tag_longevity, "tag_group", "Tag", top_n = 15),
  topic_longevity = topic_longevity,
  tag_longevity = tag_longevity,
  short_window_leaders_display = short_window_analysis$video_scores %>%
    dplyr::select(dplyr::any_of(c(
      "Title",
      "window_days",
      "views_at_window",
      "views_at_window_percentile",
      "early_performance_bucket",
      "standout_within_window"
    ))),
  window_summary = window_summary,
  metadata = tibble::tibble(
    key = c(
      "talent",
      "generated_at_utc",
      "data_source",
      "data_root",
      "artifact_root",
      "window_mode",
      "window_start_date",
      "window_end_date"
    ),
    value = c(
      Talent,
      as.character(Sys.time()),
      data_source,
      normalizePath(root, winslash = "/", mustWork = FALSE),
      bundle_e_artifact_root,
      window_mode,
      if (is.na(window_start_date)) NA_character_ else as.character(window_start_date),
      if (is.na(window_end_date)) NA_character_ else as.character(window_end_date)
    )
  )
)

bundle_e_ai_inputs <- list(
  metadata = list(
    talent = Talent,
    generated_at_utc = as.character(Sys.time()),
    data_source = data_source,
    data_root = normalizePath(root, winslash = "/", mustWork = FALSE),
    window_mode = window_mode,
    window_start_date = if (is.na(window_start_date)) NA_character_ else as.character(window_start_date),
    window_end_date = if (is.na(window_end_date)) NA_character_ else as.character(window_end_date),
    sample_talent_reference = "Leia Memoria【Variance Project】"
  ),
  dataset_sizes = bundle_e_tables$dataset_sizes,
  key_tables = bundle_e_tables
)

for (nm in names(bundle_e_plots)) {
  cat("\nRendering plot:", nm, "\n")
  print(bundle_e_plots[[nm]])
}

bundle_e_export <- report_export_artifacts(
  plots = bundle_e_plots,
  tables = bundle_e_tables,
  ai_inputs = bundle_e_ai_inputs,
  out_root = bundle_e_artifact_root,
  manifest_file = "bundle_e_artifact_manifest.json",
  ai_inputs_file = "bundle_e_ai_inputs.json"
)

cat("\nBundle E panel rows (full):", nrow(panel_full), "\n")
cat("Bundle E panel rows (window):", nrow(panel_window), "\n")
cat("Bundle E videos in summary:", nrow(video_summary), "\n")
cat("Exported Bundle E artifacts to:", bundle_e_export$out_root, "\n")
cat("Exported figures to:", bundle_e_export$plots_dir, "\n")
cat("Exported tables to:", bundle_e_export$tables_dir, "\n")
cat("Exported AI inputs JSON to:", bundle_e_export$ai_inputs_path, "\n")
cat("Exported artifact manifest to:", bundle_e_export$manifest_path, "\n")
