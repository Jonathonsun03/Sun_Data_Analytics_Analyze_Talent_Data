library(tidyverse)
library(dplyr)
library(here)
library(purrr)

source("scripts/lib/utils/staging_root.R")

bundle_b_plot_dir <- here("scripts", "lib", "plots", "report", "bundle_B")
if (dir.exists(bundle_b_plot_dir)) {
  list.files(bundle_b_plot_dir, full.names = TRUE) %>%
    walk(source)
}

# Reuse shared plotting/report helpers currently implemented in bundle_A.
list.files(here("scripts", "lib", "plots", "report", "bundle_A"), full.names = TRUE) %>%
  walk(source)
list.files(here("scripts", "lib", "utils"), full.names = TRUE) %>%
  walk(source)
list.files(here("scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
list.files(here("scripts", "lib", "report_tables"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
source(here("scripts", "lib", "plots", "PlottingThemes.R"))
source(here::here("scripts", "lib", "clean_data", "CleanData.R"))

load_bundle_b_titles <- function(
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

build_bundle_b_preps <- function(
  files,
  titles,
  talent,
  dedupe = TRUE,
  dedupe_sets = c("analytics", "monetary", "demo"),
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

build_bundle_b_deep_dive <- function(
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

safe_zscore <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  if (length(stats::na.omit(x_num)) <= 1 || isTRUE(all.equal(stats::sd(x_num, na.rm = TRUE), 0))) {
    return(rep(0, length(x_num)))
  }
  as.numeric(scale(x_num))
}

build_bundle_b_strength_matrix <- function(analytics, monetary, analytics_eng) {
  views_by_content <- total_views_content_type_prep(analytics)

  revenue_by_content <- total_metric_content_type_prep(
    monetary,
    metric_col = "Estimated Revenue",
    window_months = 1
  ) %>%
    dplyr::group_by(`Content Type`) %>%
    dplyr::summarize(
      Total_Revenue = sum(Total, na.rm = TRUE),
      Revenue_Video_Count = sum(VideoCount, na.rm = TRUE),
      .groups = "drop"
    )

  engagement_summary <- engagement_summary_content_type_prep(
    analytics_eng,
    metric_col = "avg_view_prop"
  )

  strength_matrix <- views_by_content %>%
    dplyr::rename(Content_Type = `Content Type`) %>%
    dplyr::left_join(
      revenue_by_content %>% dplyr::rename(Content_Type = `Content Type`),
      by = "Content_Type"
    ) %>%
    dplyr::left_join(
      engagement_summary %>%
        dplyr::transmute(
          Content_Type = as.character(.content),
          Engagement_Video_Count = VideoCount,
          Median_Engagement = Median,
          Mean_Engagement = Mean
        ),
      by = "Content_Type"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "Total_Views",
          "Total_Revenue",
          "Revenue_Video_Count",
          "Engagement_Video_Count",
          "Median_Engagement",
          "Mean_Engagement"
        )),
        ~ tidyr::replace_na(.x, 0)
      ),
      Views_Z = safe_zscore(Total_Views),
      Revenue_Z = safe_zscore(Total_Revenue),
      Engagement_Z = safe_zscore(Median_Engagement),
      Composite_Score = rowMeans(dplyr::across(c(Views_Z, Revenue_Z, Engagement_Z)), na.rm = TRUE)
    )

  high_cut <- stats::quantile(
    strength_matrix$Composite_Score,
    probs = 0.67,
    na.rm = TRUE,
    names = FALSE
  )
  low_cut <- stats::quantile(
    strength_matrix$Composite_Score,
    probs = 0.33,
    na.rm = TRUE,
    names = FALSE
  )

  strength_matrix %>%
    dplyr::mutate(
      Performance_Band = dplyr::case_when(
        Composite_Score >= high_cut ~ "Strength",
        Composite_Score <= low_cut ~ "Weakness / Improve",
        TRUE ~ "Middle"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(Composite_Score))
}

build_bundle_b_plot_set <- function(
  analytics,
  monetary,
  talent,
  deep_dive = NULL
) {
  if (is.null(deep_dive)) {
    deep_dive <- build_bundle_b_deep_dive(analytics, monetary)
  }

  analytics_eng <- analytics %>%
    dplyr::mutate(avg_view_prop = averageViewPercentage / 100)
  engagement_summary <- engagement_summary_content_type_prep(
    analytics_eng,
    metric_col = "avg_view_prop"
  )
  strength_matrix <- build_bundle_b_strength_matrix(
    analytics = analytics,
    monetary = monetary,
    analytics_eng = analytics_eng
  )

  list(
    p_engagement_dist = engagement_distribution_content_type(
      analytics_eng,
      talent,
      metric_col = "avg_view_prop",
      metric_label = "Average View %",
      y_as_percent = TRUE
    ),
    p_format_consistency = engagement_summary_content_type_plot(
      engagement_summary,
      talent,
      metric_label = "Average View %",
      y_as_percent = TRUE
    ),
    p_priority_rank = bundle_b_priority_rank_plot(strength_matrix, talent),
    p_strength_matrix = bundle_b_strength_matrix_plot(strength_matrix, talent),
    p_revenue_efficiency = bundle_b_revenue_efficiency_plot(strength_matrix, talent),
    p_collab_lift = bundle_b_collaboration_lift_plot(deep_dive$collab_summary, talent),
    p_schedule_lift = bundle_b_day_of_week_lift_plot(deep_dive$day_of_week_summary, talent),
    p_topic_view_dist = topic_view_distribution_plot(deep_dive$topic_view_distribution, talent),
    p_tag_view_dist = tag_view_distribution_plot(deep_dive$tag_view_distribution, talent)
  )
}

build_bundle_b_tables <- function(
  analytics,
  monetary,
  demo,
  geo,
  talent,
  deep_dive = NULL
) {
  if (is.null(deep_dive)) {
    deep_dive <- build_bundle_b_deep_dive(analytics, monetary)
  }

  analytics_eng <- analytics %>%
    dplyr::mutate(avg_view_prop = averageViewPercentage / 100)
  engagement_summary <- engagement_summary_content_type_prep(
    analytics_eng,
    metric_col = "avg_view_prop"
  ) %>%
    dplyr::transmute(
      Content_Type = as.character(.content),
      Video_Count = VideoCount,
      Median_Engagement = Median,
      Mean_Engagement = Mean,
      Q25_Engagement = Q25,
      Q75_Engagement = Q75
    )

  list(
    dataset_sizes = tibble::tibble(
      dataset = c("analytics", "monetary", "demo", "geo"),
      rows = c(nrow(analytics), nrow(monetary), nrow(demo), nrow(geo)),
      cols = c(ncol(analytics), ncol(monetary), ncol(demo), ncol(geo))
    ),
    views_by_content_type = total_views_content_type_prep(analytics),
    revenue_by_content_type = total_metric_content_type_prep(
      monetary,
      metric_col = "Estimated Revenue",
      window_months = 1
    ),
    engagement_summary = engagement_summary,
    strength_matrix = build_bundle_b_strength_matrix(
      analytics = analytics,
      monetary = monetary,
      analytics_eng = analytics_eng
    ),
    weekend_vs_weekday = deep_dive$weekend_summary,
    day_of_week = deep_dive$day_of_week_summary,
    collaboration = deep_dive$collab_summary,
    topic = deep_dive$topic_summary,
    topic_view_distribution = deep_dive$topic_view_distribution$summary,
    tag = deep_dive$tag_summary,
    tag_view_distribution = deep_dive$tag_view_distribution$summary,
    metadata = tibble::tibble(
      key = c("talent", "generated_at_utc"),
      value = c(as.character(talent), as.character(Sys.time()))
    )
  )
}

print_bundle_b_preview <- function(preps) {
  cat("\nBundle B dataset sizes:\n")
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

export_bundle_b_artifacts <- function(
  plots,
  tables,
  out_root = here::here("templates", "reports", "Bundle_B"),
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
  unlink(file.path(plots_dir, "*.png"))
  unlink(file.path(tables_dir, "*.csv"))

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

render_bundle_b_test <- function(
  input = here::here("templates", "reports", "Bundle_B", "Bundle_B.Rmd"),
  output_file = "Bundle_B_test.html"
) {
  if (!file.exists(input)) {
    warning("Bundle B report template not found: ", input)
    return(invisible(NULL))
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("Package 'rmarkdown' is not installed; skipping test render.")
    return(invisible(NULL))
  }

  rmarkdown::render(
    input = input,
    output_file = output_file,
    output_dir = dirname(input)
  )
}

root <- get_staging_root()
talent_root <- list.files(root, full.names = TRUE)

Talent <- c("Avaritia")

talent_root <- select_talent(Talent)
files <- TalentFiles(talent_root)

titles <- load_bundle_b_titles(Talent)
preps <- build_bundle_b_preps(
  files = files,
  titles = titles,
  talent = Talent
)

analytics <- preps$analytics
monetary <- preps$monetary
demo <- preps$demo
geo <- preps$geo

# Backward-compatible alias used by some Bundle report code.
df <- analytics

print_bundle_b_preview(preps)

deep_dive <- build_bundle_b_deep_dive(analytics, monetary)

bundle_b_ai_inputs <- list(
  metadata = list(
    talent = Talent,
    generated_at_utc = as.character(Sys.time())
  ),
  key_tables = build_bundle_b_tables(
    analytics = analytics,
    monetary = monetary,
    demo = demo,
    geo = geo,
    talent = Talent,
    deep_dive = deep_dive
  )
)

bundle_b_plots <- build_bundle_b_plot_set(
  analytics = analytics,
  monetary = monetary,
  talent = Talent,
  deep_dive = deep_dive
)

# Backward-compatible plot variables used in existing scripts.
plot_or_null <- function(name) {
  if (name %in% names(bundle_b_plots)) bundle_b_plots[[name]] else NULL
}

p_views <- plot_or_null("p_views")
p_rev <- plot_or_null("p_rev")
p_weekend <- plot_or_null("p_weekend")
p_day_of_week <- plot_or_null("p_day_of_week")
p_collab <- plot_or_null("p_collab")
p_topic <- plot_or_null("p_topic")
p_tag <- plot_or_null("p_tag")

for (nm in names(bundle_b_plots)) {
  cat("\nRendering plot:", nm, "\n")
  print(bundle_b_plots[[nm]])
}

bundle_b_export <- export_bundle_b_artifacts(
  plots = bundle_b_plots,
  tables = bundle_b_ai_inputs$key_tables
)

cat("\nExported plots to:", bundle_b_export$plots_dir, "\n")
cat("Exported tables to:", bundle_b_export$tables_dir, "\n")

render_result <- render_bundle_b_test()
if (!is.null(render_result)) {
  cat("Rendered test report:", render_result, "\n")
}
