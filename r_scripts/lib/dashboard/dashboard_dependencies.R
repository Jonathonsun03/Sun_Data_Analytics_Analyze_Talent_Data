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

dashboard_source_dir <- function(..., pattern = "[rR]$", recursive = FALSE) {
  path <- here::here(...)
  if (!dir.exists(path)) {
    stop("Dashboard dependency directory not found: ", path, call. = FALSE)
  }
  files <- sort(list.files(path, pattern = pattern, full.names = TRUE, recursive = recursive))
  invisible(lapply(files, source))
}

# Core path and sourcing helpers first.
dashboard_source("r_scripts", "lib", "utils", "source_dir.r")
dashboard_source("r_scripts", "lib", "utils", "staging_root.R")
dashboard_source("r_scripts", "lib", "utils", "datalake_root.r")
dashboard_source("r_scripts", "lib", "utils", "talent_select.R")
dashboard_source("r_scripts", "lib", "utils", "dt_utils.R")
dashboard_source("r_scripts", "lib", "utils", "content_type_utils.R")

# Import and preparation helpers.
dashboard_source("r_scripts", "lib", "import_data", "talent_files.R")
dashboard_source("r_scripts", "lib", "clean_data", "CleanData.R")
dashboard_source("r_scripts", "lib", "clean_data", "title_classification_join.R")

# Plot themes and report plot helpers. Bundle A helpers are loaded before
# Bundle B/E because later report helpers reuse Bundle A labels and formatting.
dashboard_source("r_scripts", "lib", "plots", "PlottingThemes.R")
dashboard_source("r_scripts", "lib", "plots", "report", "bundle_A", "00_Bundle_A_Helpers.R")
dashboard_source_dir("r_scripts", "lib", "plots", "report", "bundle_A")
dashboard_source_dir("r_scripts", "lib", "plots", "report", "bundle_B")
dashboard_source_dir("r_scripts", "lib", "plots", "report", "bundle_e")

# Table helpers are sourced after plot helpers to match the existing reports.
dashboard_source_dir("r_scripts", "lib", "report_tables")

# Day-of-week and topic-weekday dashboard inputs.
dashboard_source("r_scripts", "lib", "linear_regression", "day_of_week", "weekday_transform.R")
dashboard_source("r_scripts", "lib", "linear_regression", "day_of_week", "report_helpers.R")
dashboard_source("r_scripts", "lib", "linear_regression", "day_of_week", "topic_weekday_recommendations.R")

invisible(TRUE)
