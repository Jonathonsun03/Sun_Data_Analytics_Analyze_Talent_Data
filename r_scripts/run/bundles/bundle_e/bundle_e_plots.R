library(tidyverse)
library(here)
library(purrr)

source(here("r_scripts", "lib", "utils", "staging_root.R"))
source(here("r_scripts", "lib", "utils", "datalake_root.r"))

list.files(here("r_scripts", "lib", "utils"), full.names = TRUE) %>%
  walk(source)

list.files(here("r_scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)

bundle_e_plot_dir <- here("r_scripts", "lib", "plots", "report", "bundle_e")
if (dir.exists(bundle_e_plot_dir)) {
  list.files(bundle_e_plot_dir, full.names = TRUE) %>%
    walk(source)
}

source(here("r_scripts", "lib", "plots", "PlottingThemes.R"))
source(here("r_scripts", "lib", "clean_data", "CleanData.R"))

root <- get_datalake_root()
Talent <- "Leia Memoria【Variance Project】"

talent_root <- select_talent(Talent, root = root)
files <- TalentFiles(talent_root)
titles <- load_title_classifications(talent = Talent)

panel_full <- build_bundle_e_long_panel(
  files = files,
  titles = titles,
  talent = Talent
)

short_window_analysis <- build_bundle_e_short_window_analysis(
  panel_df = panel_full,
  required_launch_capture_days = 7,
  min_videos = 10,
  target_median_remaining = 0.05,
  target_p75_remaining = 0.10
)

selected_window <- short_window_analysis$selected_window
window_diagnostics <- short_window_analysis$window_diagnostics
video_scores <- short_window_analysis$video_scores

if (nrow(selected_window) == 0) {
  stop("No eligible Shorts window could be selected for this talent.")
}

cat("\nSelected short window:\n")
print(selected_window, width = Inf)

cat("\nWindow diagnostics:\n")
print(window_diagnostics, n = Inf, width = Inf)

cat("\nTop shorts within selected window:\n")
print(
  video_scores %>%
    dplyr::select(
      `Video ID`,
      Title,
      window_days,
      views_at_window,
      views_at_window_percentile,
      early_performance_bucket,
      standout_within_window
    ) %>%
    dplyr::slice_head(n = 15),
  n = 15,
  width = Inf
)

window_plot <- plot_bundle_e_short_window_diagnostics(window_diagnostics, Talent)
leaders_plot <- plot_bundle_e_short_window_leaders(video_scores, Talent)

print(window_plot)
print(leaders_plot)
