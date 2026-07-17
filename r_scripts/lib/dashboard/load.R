# Dashboard composition root. Source this file from dashboard entrypoints.

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(DT)
  library(plotly)
})

dashboard_source <- function(...) {
  path <- here::here(...)
  if (!file.exists(path)) {
    stop("Dashboard dependency not found: ", path, call. = FALSE)
  }
  source(path)
}

# Shared project utilities and data preparation.
dashboard_source("r_scripts", "lib", "utils", "source_dir.r")
dashboard_source("r_scripts", "lib", "utils", "repo_env.R")
dashboard_source("r_scripts", "lib", "utils", "staging_root.R")
dashboard_source("r_scripts", "lib", "utils", "datalake_root.r")
dashboard_source("r_scripts", "lib", "utils", "talent_select.R")
dashboard_source("r_scripts", "lib", "utils", "dt_utils.R")
dashboard_source("r_scripts", "lib", "utils", "content_type_utils.R")
dashboard_source("r_scripts", "lib", "import_data", "talent_files.R")
dashboard_source("r_scripts", "lib", "clean_data", "CleanData.R")
dashboard_source("r_scripts", "lib", "clean_data", "title_classification_join.R")

# Only the report plotting modules used by the creator dashboard are loaded.
dashboard_source("r_scripts", "lib", "plots", "PlottingThemes.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_A", "00_Bundle_A_Helpers.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_A", "Audience_Demographic_Trends.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_A", "Content_Breakdown_Deep_Dive.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_A", "Engagement_By_Content_Type.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_A", "Performance_Trends_Overview.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_A", "Total_Metric_Content_Type.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_A", "Total_Views_Content_Type.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_B", "00_Layout_Standards.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_B", "Category_Strategy_Plots.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_e", "00_layout_standards.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_e", "00_bundle_e_plotly.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_e", "bundle_e_panel_prep.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_e", "bundle_e_summary_metrics.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_e", "lifecycle_library_growth_plots.R")

# Day-of-week and topic-weekday analysis.
dashboard_source("r_scripts", "lib", "linear_regression", "day_of_week", "weekday_transform.R")
dashboard_source("r_scripts", "lib", "linear_regression", "day_of_week", "report_helpers.R")
dashboard_source("r_scripts", "lib", "linear_regression", "day_of_week", "topic_weekday_recommendations.R")

# Dashboard data, metric, and recommendation layers.
dashboard_source("r_scripts", "lib", "dashboard", "data", "sources.R")
dashboard_source("r_scripts", "lib", "dashboard", "data", "filters.R")
dashboard_source("r_scripts", "lib", "dashboard", "metrics", "overview.R")
dashboard_source("r_scripts", "lib", "dashboard", "metrics", "publishing.R")
dashboard_source("r_scripts", "lib", "dashboard", "metrics", "lifecycle.R")
dashboard_source("r_scripts", "lib", "dashboard", "metrics", "audience.R")
dashboard_source("r_scripts", "lib", "dashboard", "recommendations", "core.R")
dashboard_source("r_scripts", "lib", "dashboard", "recommendations", "publishing.R")
dashboard_source("r_scripts", "lib", "dashboard", "recommendations", "content.R")
dashboard_source("r_scripts", "lib", "dashboard", "recommendations", "audience.R")
dashboard_source("r_scripts", "lib", "dashboard", "recommendations", "performance.R")
dashboard_source("r_scripts", "lib", "dashboard", "recommendations", "assemble.R")
dashboard_source("r_scripts", "lib", "dashboard", "data", "assemble.R")

# Dashboard UI and report-plot adapters.
dashboard_source("r_scripts", "lib", "dashboard", "ui", "core.R")
dashboard_source("r_scripts", "lib", "dashboard", "ui", "recommendations.R")
dashboard_source("r_scripts", "lib", "dashboard", "ui", "metrics.R")
dashboard_source("r_scripts", "lib", "dashboard", "adapters", "core.R")
dashboard_source("r_scripts", "lib", "dashboard", "adapters", "overview.R")
dashboard_source("r_scripts", "lib", "dashboard", "adapters", "lifecycle.R")
dashboard_source("r_scripts", "lib", "dashboard", "adapters", "content.R")
dashboard_source("r_scripts", "lib", "dashboard", "adapters", "publishing.R")
dashboard_source("r_scripts", "lib", "dashboard", "adapters", "audience.R")

invisible(TRUE)
