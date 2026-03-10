library(tidyverse)
library(dplyr)
library(here)
library(purrr)

source("scripts/lib/utils/staging_root.R")
source("scripts/lib/utils/datalake_root.r")

list.files(here("scripts","lib","plots","report","bundle_A"), full.names = TRUE) %>%
    walk(source)
list.files(here("scripts","lib","utils"), full.names = TRUE) %>%
    walk(source)
list.files(here("scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
list.files(here("scripts", "lib", "report_tables"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
source(here("scripts","lib","plots","PlottingThemes.R"))
source(here::here("scripts","lib","clean_data","CleanData.R"))

load_bundle_a_titles <- function(
  talent,
  titles_path = file.path(
    "classification", "output", "title_classifications",
    "classification_export_gpt-5-mini_from_duckdb.csv"
  )
) {
  load_title_classifications(
    path = titles_path,
    talent = talent
  )
}

build_bundle_a_preps <- function(
  files,
  titles,
  talent,
  dedupe = TRUE,
  dedupe_sets = c("analytics", "monetary"),
  key_cols = "Video ID",
  sort_cols = c("confidence", "published_at", "Published At")
) {
  video_preps_with_titles(
    files = files,
    titles = titles,
    talent = talent,
    dedupe = dedupe,
    key_cols = key_cols,
    sort_cols = sort_cols,
    dedupe_sets = dedupe_sets
  )
}

build_bundle_a_deep_dive <- function(
  analytics,
  monetary,
  topic_top_n = 8,
  topic_min_videos = 2,
  tag_top_n = 15,
  tag_min_videos = 2,
  tag_min_views_per_video = 10
) {
  list(
    weekend_summary = weekend_vs_weekday_prep(analytics, monetary),
    day_of_week_summary = day_of_week_performance_prep(analytics, monetary),
    collab_summary = collaboration_effectiveness_prep(analytics, monetary),
    topic_summary = topic_performance_prep(analytics, monetary, top_n = topic_top_n),
    topic_view_distribution = topic_view_distribution_prep(
      analytics,
      top_n = topic_top_n,
      min_videos = topic_min_videos
    ),
    tag_summary = tag_performance_prep(
      analytics,
      monetary,
      top_n = tag_top_n,
      min_videos = tag_min_videos
    ),
    tag_view_distribution = tag_view_distribution_prep(
      analytics,
      top_n = tag_top_n,
      min_videos = tag_min_videos,
      min_views_per_video = tag_min_views_per_video
    )
  )
}

build_bundle_a_plot_set <- function(
  analytics,
  monetary,
  demo,
  talent,
  deep_dive = NULL
) {
  if (is.null(deep_dive)) {
    deep_dive <- build_bundle_a_deep_dive(analytics, monetary)
  }

  analytics_eng <- analytics %>%
    mutate(avg_view_prop = averageViewPercentage / 100)

  list(
    p_views = total_views_content_type(analytics, talent),
    p_rev = total_metric_content_type(
      monetary,
      talent,
      metric_col = "Estimated Revenue",
      metric_label = "Revenue",
      window_months = 1,
      bar_position = "stack",
      show_counts = TRUE
    ),
    p_views_time = Content_duration_views(analytics, talent),
    p_rev_time = content_duration_metric(monetary, talent, "Estimated Revenue"),
    p_perf_overview = performance_trends_over_time(analytics, monetary, talent),
    p_engagement_dist = engagement_distribution_content_type(
      analytics_eng,
      talent,
      metric_col = "avg_view_prop",
      metric_label = "Average View %",
      y_as_percent = TRUE
    ),
    p_audience_trends = audience_age_gender_trends(demo, talent),
    p_weekend = weekend_vs_weekday_plot(deep_dive$weekend_summary, talent),
    p_day_of_week = day_of_week_performance_plot(deep_dive$day_of_week_summary, talent, as_share = TRUE),
    p_collab = collaboration_effectiveness_plot(deep_dive$collab_summary, talent),
    p_topic = topic_performance_plot(deep_dive$topic_summary, talent),
    p_topic_view_dist = topic_view_distribution_plot(deep_dive$topic_view_distribution, talent),
    p_tag = tag_performance_plot(deep_dive$tag_summary, talent),
    p_tag_view_dist = tag_view_distribution_plot(deep_dive$tag_view_distribution, talent)
  )
}

print_bundle_a_preview <- function(preps) {
  cat("\nBundle A dataset sizes:\n")
  cat("analytics:", nrow(preps$analytics), "rows |", ncol(preps$analytics), "cols\n")
  cat("monetary :", nrow(preps$monetary), "rows |", ncol(preps$monetary), "cols\n")
  cat("demo     :", nrow(preps$demo), "rows |", ncol(preps$demo), "cols\n")
  cat("geo      :", nrow(preps$geo), "rows |", ncol(preps$geo), "cols\n")

  preview_cols <- c(
    "Video ID", "Content Type", "confidence", "topic", "primary_reference"
  )
  show_cols <- preview_cols[preview_cols %in% names(preps$analytics)]
  if (length(show_cols) > 0) {
    cat("\nAnalytics preview:\n")
    print(dplyr::slice_head(dplyr::select(preps$analytics, dplyr::all_of(show_cols)), n = 5))
  }
}

export_bundle_a_artifacts <- function(
  plots,
  tables,
  out_root = here::here("templates", "reports", "Bundle_A"),
  plot_subdir = "plots",
  table_subdir = "tables",
  plot_width = 11,
  plot_height = 7,
  plot_dpi = 150
) {
  plots_dir <- file.path(out_root, plot_subdir)
  tables_dir <- file.path(out_root, table_subdir)
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

  safe_name <- function(x) {
    gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
  }

  for (nm in names(plots)) {
    p <- plots[[nm]]
    if (inherits(p, "ggplot")) {
      ggplot2::ggsave(
        filename = file.path(plots_dir, paste0(safe_name(nm), ".png")),
        plot = p,
        width = plot_width,
        height = plot_height,
        dpi = plot_dpi
      )
    }
  }

  for (nm in names(tables)) {
    tbl <- tables[[nm]]
    if (is.data.frame(tbl)) {
      readr::write_csv(tbl, file.path(tables_dir, paste0(safe_name(nm), ".csv")))
    }
  }

  invisible(list(plots_dir = plots_dir, tables_dir = tables_dir))
}

data_source <- tolower(trimws(Sys.getenv("TALENT_DATA_SOURCE", unset = "staging")))
if (!(data_source %in% c("staging", "datalake"))) {
  stop("TALENT_DATA_SOURCE must be one of: staging, datalake.")
}
data_root_override <- trimws(Sys.getenv("TALENT_DATA_ROOT", unset = ""))
root <- if (nzchar(data_root_override)) {
  data_root_override
} else if (identical(data_source, "datalake")) {
  get_datalake_root()
} else {
  get_staging_root()
}
if (!dir.exists(root)) {
  stop("Resolved data root does not exist: ", root)
}

Talent <- trimws(Sys.getenv("TALENT_QUERY", unset = "Ava"))
if (!nzchar(Talent)) {
  Talent <- "Ava"
}

talent_root <- select_talent(Talent, root = root)
files <- TalentFiles(talent_root)

titles <- load_bundle_a_titles(Talent)
preps <- build_bundle_a_preps(
  files = files,
  titles = titles,
  talent = Talent
)

analytics <- preps$analytics
monetary  <- preps$monetary
demo      <- preps$demo
geo       <- preps$geo

# Backward-compatible alias used by some Bundle A report code.
df <- analytics

print_bundle_a_preview(preps)

# Deep-dive summary tables for downstream analysis / API interpretation.
deep_dive <- build_bundle_a_deep_dive(analytics, monetary)
weekend_summary <- deep_dive$weekend_summary
day_of_week_summary <- deep_dive$day_of_week_summary
collab_summary <- deep_dive$collab_summary
topic_summary <- deep_dive$topic_summary
topic_view_distribution <- deep_dive$topic_view_distribution
tag_summary <- deep_dive$tag_summary
tag_view_distribution <- deep_dive$tag_view_distribution

# Bundle A raw tables packaged for OpenAI prompt input or export.
bundle_a_ai_inputs <- list(
  metadata = list(
    talent = Talent,
    generated_at_utc = as.character(Sys.time())
  ),
  dataset_sizes = tibble::tibble(
    dataset = c("analytics", "monetary", "demo", "geo"),
    rows = c(nrow(analytics), nrow(monetary), nrow(demo), nrow(geo)),
    cols = c(ncol(analytics), ncol(monetary), ncol(demo), ncol(geo))
  ),
  key_tables = list(
    views_by_content_type = total_views_content_type_prep(analytics),
    revenue_by_content_type = total_metric_content_type_prep(
      monetary,
      metric_col = "Estimated Revenue",
      window_months = 1
    ),
    weekend_vs_weekday = weekend_summary,
    day_of_week = day_of_week_summary,
    collaboration = collab_summary,
    topic = topic_summary,
    topic_view_distribution = topic_view_distribution$summary,
    tag = tag_summary,
    tag_view_distribution = tag_view_distribution$summary
  )
)

# Plot objects loaded here for quick visual checks.
bundle_a_plots <- build_bundle_a_plot_set(
  analytics = analytics,
  monetary = monetary,
  demo = demo,
  talent = Talent,
  deep_dive = deep_dive
)

# Backward-compatible plot variables used in existing scripts.
p_views <- bundle_a_plots$p_views
p_rev <- bundle_a_plots$p_rev
p_views_time <- bundle_a_plots$p_views_time
p_rev_time <- bundle_a_plots$p_rev_time

for (nm in names(bundle_a_plots)) {
  cat("\nRendering plot:", nm, "\n")
  print(bundle_a_plots[[nm]])
}

bundle_a_tables <- c(
  bundle_a_ai_inputs$key_tables,
  list(dataset_sizes = bundle_a_ai_inputs$dataset_sizes)
)

bundle_a_export <- export_bundle_a_artifacts(
  plots = bundle_a_plots,
  tables = bundle_a_tables
)

cat("\nExported plots to:", bundle_a_export$plots_dir, "\n")
cat("Exported tables to:", bundle_a_export$tables_dir, "\n")
