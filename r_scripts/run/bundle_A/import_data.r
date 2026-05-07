library(tidyverse)
library(dplyr)
library(here)
library(purrr)

source("r_scripts/lib/utils/staging_root.R")
source("r_scripts/lib/utils/datalake_root.r")

list.files(here("r_scripts","lib","plots","report","bundle_A"), full.names = TRUE) %>%
    walk(source)
list.files(here("r_scripts","lib","utils"), full.names = TRUE) %>%
    walk(source)
list.files(here("r_scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
list.files(here("r_scripts", "lib", "report_tables"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
source(here("r_scripts","lib","plots","PlottingThemes.R"))
source(here::here("r_scripts","lib","clean_data","CleanData.R"))

parse_bundle_a_optional_date <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(as.Date(NA))
  }
  d <- suppressWarnings(as.Date(trimws(x)))
  if (is.na(d)) {
    stop("Date must be YYYY-MM-DD when provided.")
  }
  d
}

apply_bundle_a_window <- function(df, window_mode, window_start_date, window_end_date) {
  if (identical(window_mode, "all_data") || nrow(df) == 0) {
    return(df)
  }

  date_col <- bundle_a_optional_col(
    df,
    candidates = c("publish_date", "Published At", "date"),
    label = "analysis window date column"
  )
  if (is.null(date_col)) {
    return(df)
  }

  df %>%
    dplyr::mutate(.window_date = bundle_a_as_date(.data[[date_col]])) %>%
    dplyr::filter(
      !is.na(.data$.window_date),
      .data$.window_date >= window_start_date,
      .data$.window_date <= window_end_date
    ) %>%
    dplyr::select(-".window_date")
}

load_bundle_a_titles <- function(
  talent,
  titles_path = NULL
) {
  override <- trimws(Sys.getenv("BUNDLE_A_TITLE_CLASSIFICATIONS_PATH", unset = ""))
  if (!nzchar(override)) {
    override <- trimws(Sys.getenv("TITLE_CLASSIFICATIONS_PATH", unset = ""))
  }
  if (nzchar(override)) {
    titles_path <- override
  }

  load_title_classifications(
    path = titles_path,
    talent = talent
  )
}

build_bundle_a_audience_trends_artifact <- function(
  demo_df,
  talent,
  freq = "month"
) {
  unavailable <- list(
    available = FALSE,
    reason = "Audience demographics were not available for this talent.",
    data = NULL,
    plot = NULL
  )

  if (!is.data.frame(demo_df) || nrow(demo_df) == 0) {
    return(unavailable)
  }

  prepared <- tryCatch(
    audience_age_gender_trends_prep(
      demo_df = demo_df,
      freq = freq
    ),
    error = function(e) {
      unavailable$reason <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(prepared) || nrow(prepared) == 0) {
    unavailable$reason <- "Audience demographics export only contained no-data age/gender rows."
    return(unavailable)
  }

  list(
    available = TRUE,
    reason = NA_character_,
    data = prepared,
    plot = audience_age_gender_trends_plot(
      plot_df = prepared,
      talent = talent
    )
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

build_bundle_a_engagement_success_artifact <- function(analytics, talent) {
  analytics_eng <- analytics %>%
    mutate(avg_view_prop = averageViewPercentage / 100)

  eng_success_df <- engagement_distribution_content_type_prep(
    analytics_eng,
    metric_col = "avg_view_prop"
  )

  eng_success_df <- eng_success_df %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::mutate(
      .success_cut = stats::quantile(.data$.metric, probs = 0.75, na.rm = TRUE),
      .is_success = .data$.metric >= .data$.success_cut
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .metric_fmt = scales::label_percent(accuracy = 0.1)(.data$.metric),
      .hover_text = paste0(
        "Video: ", .data$.video_label,
        "<br>Content Type: ", .data$.content,
        "<br>Average View %: ", .data$.metric_fmt
      )
    )

  p_eng_success <- ggplot2::ggplot(
    eng_success_df,
    ggplot2::aes(x = .data$.content, y = .data$.metric, fill = .data$.content)
  ) +
    ggplot2::geom_boxplot(
      color = sun_data_brand_colors()[["midnight"]],
      alpha = 0.65,
      outlier.shape = NA,
      width = 0.62
    ) +
    ggplot2::geom_jitter(
      data = dplyr::filter(eng_success_df, .data$.is_success),
      ggplot2::aes(text = .data$.hover_text),
      width = 0.16,
      alpha = 0.85,
      size = 2.0,
      color = sun_data_brand_colors()[["orange"]]
    ) +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_a_wrap_text("Average View % Distribution by Content Type", width = 58),
      subtitle = bundle_a_talent_subtitle(
        talent,
        "Highlighted points are top-quartile videos within each content type."
      ),
      x = "Content type",
      y = "Average View %"
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))

  list(
    data = eng_success_df,
    plot = p_eng_success
  )
}

build_bundle_a_plot_set <- function(
  analytics,
  monetary,
  demo,
  talent,
  deep_dive = NULL,
  audience_trends = NULL
) {
  if (is.null(deep_dive)) {
    deep_dive <- build_bundle_a_deep_dive(analytics, monetary)
  }
  if (is.null(audience_trends)) {
    audience_trends <- build_bundle_a_audience_trends_artifact(
      demo_df = demo,
      talent = talent,
      freq = "month"
    )
  }

  views_content <- views_content_type_comparison_with_data(analytics, talent)
  perf_overview <- performance_trends_over_time_with_data(
    analytics_df = analytics,
    monetary_df = monetary,
    talent = talent,
    freq = "month",
    value_mode = "raw"
  )
  revenue_content <- total_metric_content_type_with_data(
    monetary,
    talent,
    metric_col = "Estimated Revenue",
    metric_label = "Revenue",
    window_months = 1,
    bar_position = "stack",
    show_counts = TRUE,
    unique_bar_colors = TRUE
  )
  engagement_success <- build_bundle_a_engagement_success_artifact(analytics, talent)
  revenue_over_time <- content_duration_metric_with_data(
    monetary,
    talent,
    metric_col = "Estimated Revenue",
    metric_label = "Estimated Revenue"
  )
  views_over_time <- content_duration_metric_with_data(
    analytics,
    talent,
    metric_col = "views",
    metric_label = "Views"
  )
  weekend_dist <- weekend_vs_weekday_distribution_prep(analytics, monetary)
  plots <- list(
    views_by_content_type = views_content$plot,
    combined_performance_trends = perf_overview$plot,
    total_revenue_by_content_type = revenue_content$plot,
    engagement_distribution_successful_videos = engagement_success$plot,
    revenue_over_time_by_content_type = revenue_over_time$plot,
    total_views_over_time_by_content_type = views_over_time$plot,
    weekend_vs_weekday = weekend_vs_weekday_distribution_plot(weekend_dist, talent),
    day_of_week_distribution = day_of_week_performance_plot(deep_dive$day_of_week_summary, talent, as_share = TRUE),
    collaboration_effectiveness = collaboration_effectiveness_plot(deep_dive$collab_summary, talent),
    topic_performance = topic_performance_plot(deep_dive$topic_summary, talent),
    tag_performance = tag_performance_plot(deep_dive$tag_summary, talent),
    average_views_per_tag = tag_average_views_plot(deep_dive$tag_summary, talent)
  )

  if (isTRUE(audience_trends$available) && inherits(audience_trends$plot, "ggplot")) {
    plots$audience_age_gender_trends <- audience_trends$plot
  }

  plots
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
  ai_inputs = NULL,
  out_root = here::here("templates", "reports", "Bundle_A"),
  plot_subdir = "figures",
  table_subdir = "tables",
  manifest_file = "bundle_a_artifact_manifest.json",
  ai_inputs_file = "bundle_a_ai_inputs.json",
  plot_width = 11,
  plot_height = 7,
  plot_dpi = 150
) {
  plots_dir <- file.path(out_root, plot_subdir)
  tables_dir <- file.path(out_root, table_subdir)
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(plots_dir, pattern = "\\.png$", full.names = TRUE))
  unlink(list.files(tables_dir, pattern = "\\.csv$", full.names = TRUE))

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

  ai_inputs_path <- NA_character_
  if (!is.null(ai_inputs)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package `jsonlite` is required to export Bundle A AI inputs.")
    }
    ai_inputs_path <- file.path(out_root, ai_inputs_file)
    jsonlite::write_json(
      ai_inputs,
      path = ai_inputs_path,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
  }

  manifest <- list(
    talent = if (!is.null(ai_inputs$metadata$talent)) ai_inputs$metadata$talent else NA_character_,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    out_root = normalizePath(out_root, winslash = "/", mustWork = FALSE),
    figures_dir = normalizePath(plots_dir, winslash = "/", mustWork = FALSE),
    tables_dir = normalizePath(tables_dir, winslash = "/", mustWork = FALSE),
    ai_inputs_json = if (is.na(ai_inputs_path)) NA_character_ else normalizePath(ai_inputs_path, winslash = "/", mustWork = FALSE),
    figure_files = unname(vapply(
      names(plots),
      function(nm) file.path(normalizePath(plots_dir, winslash = "/", mustWork = FALSE), paste0(safe_name(nm), ".png")),
      character(1)
    )),
    table_files = unname(vapply(
      names(tables),
      function(nm) file.path(normalizePath(tables_dir, winslash = "/", mustWork = FALSE), paste0(safe_name(nm), ".csv")),
      character(1)
    ))
  )
  manifest_path <- file.path(out_root, manifest_file)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to export Bundle A artifact manifest.")
  }
  jsonlite::write_json(
    manifest,
    path = manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  invisible(list(
    out_root = normalizePath(out_root, winslash = "/", mustWork = FALSE),
    plots_dir = normalizePath(plots_dir, winslash = "/", mustWork = FALSE),
    tables_dir = normalizePath(tables_dir, winslash = "/", mustWork = FALSE),
    ai_inputs_path = if (is.na(ai_inputs_path)) NA_character_ else normalizePath(ai_inputs_path, winslash = "/", mustWork = FALSE),
    manifest_path = normalizePath(manifest_path, winslash = "/", mustWork = FALSE)
  ))
}

resolve_bundle_a_artifact_root <- function(
  talent_root,
  bundle_name = "bundle_A",
  report_subdir = "reports",
  artifact_subdir = "artifacts"
) {
  override <- trimws(Sys.getenv("BUNDLE_A_ARTIFACT_ROOT", unset = ""))
  if (nzchar(override)) {
    return(normalizePath(override, winslash = "/", mustWork = FALSE))
  }

  datalake_root <- normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)
  talent_folder <- safe_basename(talent_root)
  normalizePath(
    file.path(datalake_root, talent_folder, report_subdir, bundle_name, artifact_subdir),
    winslash = "/",
    mustWork = FALSE
  )
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

window_days <- suppressWarnings(as.integer(trimws(Sys.getenv("TALENT_WINDOW_DAYS", unset = ""))))
if (is.na(window_days)) {
  window_days <- NA_integer_
} else if (window_days <= 0) {
  stop("TALENT_WINDOW_DAYS must be a positive integer when provided.")
}
start_date_param <- parse_bundle_a_optional_date(Sys.getenv("TALENT_START_DATE", unset = ""))
end_date_param <- parse_bundle_a_optional_date(Sys.getenv("TALENT_END_DATE", unset = ""))
if (!is.na(start_date_param) || !is.na(end_date_param)) {
  if (is.na(start_date_param)) {
    start_date_param <- as.Date("1900-01-01")
  }
  if (is.na(end_date_param)) {
    end_date_param <- Sys.Date()
  }
  if (start_date_param > end_date_param) {
    stop("TALENT_START_DATE cannot be after TALENT_END_DATE.")
  }
  window_start_date <- start_date_param
  window_end_date <- end_date_param
  window_mode <- "explicit_range"
} else if (!is.na(window_days)) {
  window_end_date <- Sys.Date()
  window_start_date <- window_end_date - as.integer(window_days) + 1L
  window_mode <- "days_back"
} else {
  window_start_date <- as.Date(NA)
  window_end_date <- as.Date(NA)
  window_mode <- "all_data"
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

analytics <- apply_bundle_a_window(analytics, window_mode, window_start_date, window_end_date)
monetary  <- apply_bundle_a_window(monetary, window_mode, window_start_date, window_end_date)
demo      <- apply_bundle_a_window(demo, window_mode, window_start_date, window_end_date)
geo       <- apply_bundle_a_window(geo, window_mode, window_start_date, window_end_date)

if (nrow(analytics) == 0 || nrow(monetary) == 0) {
  stop(
    "No data after applying the selected window. ",
    "At least analytics and monetary data are required."
  )
}

# Backward-compatible alias used by some Bundle A report code.
df <- analytics

print_bundle_a_preview(preps)

# Deep-dive summary tables for downstream analysis / API interpretation.
deep_dive <- build_bundle_a_deep_dive(analytics, monetary)
audience_trends <- build_bundle_a_audience_trends_artifact(
  demo_df = demo,
  talent = Talent,
  freq = "month"
)
weekend_summary <- deep_dive$weekend_summary
day_of_week_summary <- deep_dive$day_of_week_summary
collab_summary <- deep_dive$collab_summary
topic_summary <- deep_dive$topic_summary
topic_view_distribution <- deep_dive$topic_view_distribution
tag_summary <- deep_dive$tag_summary
tag_view_distribution <- deep_dive$tag_view_distribution

views_content_comparison <- views_content_type_comparison_with_data(analytics, Talent)
performance_overview <- performance_trends_over_time_with_data(
  analytics_df = analytics,
  monetary_df = monetary,
  talent = Talent,
  freq = "month",
  value_mode = "raw"
)
revenue_content_with_data <- total_metric_content_type_with_data(
  monetary,
  Talent,
  metric_col = "Estimated Revenue",
  metric_label = "Revenue",
  window_months = 1,
  bar_position = "stack",
  show_counts = TRUE,
  unique_bar_colors = TRUE
)
engagement_success <- build_bundle_a_engagement_success_artifact(analytics, Talent)
revenue_over_time <- content_duration_metric_with_data(
  monetary,
  Talent,
  metric_col = "Estimated Revenue",
  metric_label = "Estimated Revenue"
)
views_over_time <- content_duration_metric_with_data(
  analytics,
  Talent,
  metric_col = "views",
  metric_label = "Views"
)

key_tables <- list(
  views_by_content_type = views_content_comparison$data,
  combined_performance_trends = performance_overview$data,
  total_revenue_by_content_type = revenue_content_with_data$data,
  engagement_distribution_successful_videos = engagement_success$data,
  revenue_over_time_by_content_type = revenue_over_time$data,
  total_views_over_time_by_content_type = views_over_time$data,
  weekend_vs_weekday = weekend_summary,
  day_of_week_distribution = day_of_week_summary,
  collaboration_effectiveness = collab_summary,
  topic_performance = topic_summary,
  topic_view_distribution = topic_view_distribution$summary,
  tag_performance = tag_summary,
  tag_view_distribution = tag_view_distribution$summary,
  average_views_per_tag = tag_summary %>%
    dplyr::select(dplyr::any_of(c("tag_group", "AverageViewsPerVideo", "VideoCountViews", "VideoCount")))
)
if (isTRUE(audience_trends$available) && is.data.frame(audience_trends$data)) {
  key_tables$audience_age_gender_trends <- audience_trends$data
}

# Bundle A raw tables packaged for OpenAI prompt input or export.
bundle_a_ai_inputs <- list(
  metadata = list(
    talent = Talent,
    generated_at_utc = as.character(Sys.time()),
    data_source = data_source,
    data_root = normalizePath(root, winslash = "/", mustWork = FALSE),
    window_mode = window_mode,
    window_start_date = if (is.na(window_start_date)) NA_character_ else as.character(window_start_date),
    window_end_date = if (is.na(window_end_date)) NA_character_ else as.character(window_end_date),
    audience_demographics_available = isTRUE(audience_trends$available),
    audience_demographics_reason = audience_trends$reason
  ),
  dataset_sizes = tibble::tibble(
    dataset = c("analytics", "monetary", "demo", "geo"),
    rows = c(nrow(analytics), nrow(monetary), nrow(demo), nrow(geo)),
    cols = c(ncol(analytics), ncol(monetary), ncol(demo), ncol(geo))
  ),
  key_tables = key_tables
)

# Plot objects loaded here for quick visual checks.
bundle_a_plots <- build_bundle_a_plot_set(
  analytics = analytics,
  monetary = monetary,
  demo = demo,
  talent = Talent,
  deep_dive = deep_dive,
  audience_trends = audience_trends
)

# Backward-compatible plot variables used in existing scripts.
p_views <- bundle_a_plots$views_by_content_type
p_rev <- bundle_a_plots$total_revenue_by_content_type
p_views_time <- bundle_a_plots$total_views_over_time_by_content_type
p_rev_time <- bundle_a_plots$revenue_over_time_by_content_type

for (nm in names(bundle_a_plots)) {
  cat("\nRendering plot:", nm, "\n")
  print(bundle_a_plots[[nm]])
}

bundle_a_tables <- c(
  bundle_a_ai_inputs$key_tables,
  list(dataset_sizes = bundle_a_ai_inputs$dataset_sizes)
)

bundle_a_artifact_root <- resolve_bundle_a_artifact_root(talent_root)
bundle_a_export <- export_bundle_a_artifacts(
  plots = bundle_a_plots,
  tables = bundle_a_tables,
  ai_inputs = bundle_a_ai_inputs,
  out_root = bundle_a_artifact_root
)

cat("\nExported Bundle A artifacts to:", bundle_a_export$out_root, "\n")
cat("Exported figures to:", bundle_a_export$plots_dir, "\n")
cat("Exported tables to:", bundle_a_export$tables_dir, "\n")
cat("Exported AI inputs JSON to:", bundle_a_export$ai_inputs_path, "\n")
cat("Exported artifact manifest to:", bundle_a_export$manifest_path, "\n")
