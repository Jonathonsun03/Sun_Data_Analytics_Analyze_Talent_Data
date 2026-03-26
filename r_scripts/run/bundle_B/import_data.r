library(tidyverse)
library(dplyr)
library(here)
library(purrr)

source("r_scripts/lib/utils/staging_root.R")
source("r_scripts/lib/utils/datalake_root.r")

bundle_b_plot_dir <- here("r_scripts", "lib", "plots", "report", "bundle_B")
if (dir.exists(bundle_b_plot_dir)) {
  list.files(bundle_b_plot_dir, full.names = TRUE) %>%
    walk(source)
}

# Reuse shared plotting/report helpers currently implemented in bundle_A.
list.files(here("r_scripts", "lib", "plots", "report", "bundle_A"), full.names = TRUE) %>%
  walk(source)
list.files(here("r_scripts", "lib", "utils"), full.names = TRUE) %>%
  walk(source)
list.files(here("r_scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
list.files(here("r_scripts", "lib", "report_tables"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
source(here("r_scripts", "lib", "plots", "PlottingThemes.R"))
source(here::here("r_scripts", "lib", "clean_data", "CleanData.R"))

parse_bundle_b_optional_date <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(as.Date(NA))
  }
  d <- suppressWarnings(as.Date(trimws(x)))
  if (is.na(d)) {
    stop("Date must be YYYY-MM-DD when provided.")
  }
  d
}

apply_bundle_b_window <- function(df, window_mode, window_start_date, window_end_date) {
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

load_bundle_b_titles <- function(
  talent,
  titles_path = file.path(
    "classification", "output", "title_classifications",
    "classification_export_gpt-5-mini_from_duckdb.csv"
  )
) {
  override <- trimws(Sys.getenv("BUNDLE_B_TITLE_CLASSIFICATIONS_PATH", unset = ""))
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

bundle_b_attribute_metric_matrix_static_plot <- function(
  attr_df,
  talent,
  x_col,
  y_col,
  title_suffix,
  x_label,
  y_label,
  x_axis_type = c("percent", "dollar", "number"),
  y_axis_type = c("percent", "dollar", "number")
) {
  x_axis_type <- match.arg(x_axis_type)
  y_axis_type <- match.arg(y_axis_type)

  plot_df <- bundle_b_attribute_opportunity_plotly_data(attr_df)
  if (!(x_col %in% names(plot_df)) || !(y_col %in% names(plot_df))) {
    stop("plot_df must include x and y columns: ", x_col, ", ", y_col)
  }

  plot_df <- plot_df %>%
    dplyr::filter(
      is.finite(.data[[x_col]]),
      is.finite(.data[[y_col]]),
      .data[[x_col]] > 0,
      .data[[y_col]] > 0
    )

  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No attribute matrix data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(title = bundle_b_title_text(talent, title_suffix))
    )
  }

  add_axis_scale <- function(p, axis_name, axis_type) {
    if (identical(axis_name, "x")) {
      if (identical(axis_type, "percent")) {
        return(p + ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)))
      }
      if (identical(axis_type, "dollar")) {
        return(p + ggplot2::scale_x_continuous(labels = scales::label_dollar(scale = 1)))
      }
      return(p + ggplot2::scale_x_continuous(labels = scales::label_comma()))
    }

    if (identical(axis_type, "percent")) {
      return(p + ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)))
    }
    if (identical(axis_type, "dollar")) {
      return(p + ggplot2::scale_y_continuous(labels = scales::label_dollar(scale = 1)))
    }
    p + ggplot2::scale_y_continuous(labels = scales::label_comma())
  }

  p <- plot_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[x_col]],
        y = .data[[y_col]],
        size = .data$SizeProxy,
        shape = .data$Performance_Band,
        color = .data$Performance_Band,
        text = .data$HoverText
      )
    ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::scale_color_manual(
      values = c(
        "Strength" = sun_data_brand_colors()[["blue"]],
        "Middle" = sun_data_brand_colors()[["steel"]],
        "Weakness / Improve" = sun_data_brand_colors()[["orange"]]
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "Strength" = 17,
        "Middle" = 16,
        "Weakness / Improve" = 15
      )
    ) +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, title_suffix),
      x = x_label,
      y = y_label,
      size = "Video count",
      shape = "Performance band"
    ) +
    ggplot2::guides(color = "none")

  p <- add_axis_scale(p, "x", x_axis_type)
  add_axis_scale(p, "y", y_axis_type)
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
  revenue_eff_dist <- bundle_b_revenue_efficiency_distribution_prep(
    analytics,
    monetary
  )
  content_position <- bundle_b_content_position_distribution_prep(
    analytics_df = analytics,
    monetary_df = monetary
  )
  attribute_opportunity <- bundle_b_attribute_opportunity_prep(
    analytics_df = analytics,
    monetary_df = monetary
  )
  collab_dist <- bundle_b_collaboration_distribution_prep(
    analytics_df = analytics,
    monetary_df = monetary
  )

  list(
    engagement_distribution_by_content_type = engagement_distribution_content_type(
      analytics_eng,
      talent,
      metric_col = "avg_view_prop",
      metric_label = "Average View %",
      y_as_percent = TRUE
    ),
    median_engagement_by_content_type = bundle_b_engagement_summary_distribution_plot(
      engagement_distribution_content_type_prep(
        analytics_eng,
        metric_col = "avg_view_prop"
      ),
      talent,
      metric_label = "Average View %",
      y_as_percent = TRUE
    ),
    content_type_position_in_full_distribution = bundle_b_content_position_distribution_plot(content_position, talent),
    combined_opportunity_matrix_content_types_tags_and_title_labels = bundle_b_attribute_opportunity_matrix_plot(attribute_opportunity, talent),
    combined_opportunity_matrix_median_engagement_vs_average_views = bundle_b_attribute_metric_matrix_static_plot(
      attr_df = attribute_opportunity,
      talent = talent,
      x_col = "MedianEngagement",
      y_col = "AvgViewsPerVideo",
      title_suffix = "Engagement vs Average Views Matrix",
      x_label = "Median engagement",
      y_label = "Average views per video",
      x_axis_type = "percent",
      y_axis_type = "number"
    ),
    combined_opportunity_matrix_average_views_vs_average_revenue = bundle_b_attribute_metric_matrix_static_plot(
      attr_df = attribute_opportunity,
      talent = talent,
      x_col = "AvgViewsPerVideo",
      y_col = "AvgRevenuePerVideo",
      title_suffix = "Average Views vs Average Revenue Matrix",
      x_label = "Average views per video",
      y_label = "Average revenue per video",
      x_axis_type = "number",
      y_axis_type = "dollar"
    ),
    revenue_efficiency_by_content_type = bundle_b_revenue_efficiency_plot(revenue_eff_dist, talent),
    collaboration_distribution_vs_non_collaborative_baseline = bundle_b_collaboration_lift_plot(collab_dist, talent),
    topic_view_distribution = bundle_b_topic_view_distribution_plot(deep_dive$topic_view_distribution, talent),
    tag_view_distribution = bundle_b_tag_view_distribution_plot(deep_dive$tag_view_distribution, talent),
    day_of_week_deviation_from_average = bundle_b_day_of_week_lift_plot(deep_dive$day_of_week_summary, talent),
    weekend_vs_weekday_summary = bundle_b_weekend_weekday_distribution_plot(
      weekend_vs_weekday_distribution_prep(analytics, monetary),
      talent
    )
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
  content_position <- bundle_b_content_position_distribution_prep(
    analytics_df = analytics,
    monetary_df = monetary
  )
  attribute_opportunity <- bundle_b_attribute_opportunity_prep(
    analytics_df = analytics,
    monetary_df = monetary
  )

  list(
    dataset_sizes = tibble::tibble(
      dataset = c("analytics", "monetary", "demo", "geo"),
      rows = c(nrow(analytics), nrow(monetary), nrow(demo), nrow(geo)),
      cols = c(ncol(analytics), ncol(monetary), ncol(demo), ncol(geo))
    ),
    engagement_distribution_by_content_type = engagement_distribution_content_type_prep(
      analytics_eng,
      metric_col = "avg_view_prop"
    ),
    median_engagement_by_content_type = engagement_summary,
    content_type_position_in_full_distribution = bundle_b_content_position_metric_table(content_position),
    distribution_based_strength_weakness_summary_by_content_type = bundle_b_content_position_overall_table(content_position),
    combined_opportunity_matrix_content_types_tags_and_title_labels = bundle_b_attribute_opportunity_table(attribute_opportunity),
    combined_opportunity_matrix_median_engagement_vs_average_views = bundle_b_attribute_opportunity_table(attribute_opportunity),
    combined_opportunity_matrix_average_views_vs_average_revenue = bundle_b_attribute_opportunity_table(attribute_opportunity),
    revenue_efficiency_by_content_type = bundle_b_revenue_efficiency_distribution_prep(analytics, monetary) %>%
      dplyr::select(
        dplyr::any_of(c("Content_Type", "Views", "Revenue", "Revenue_Per_1K_Views", "VideoTitle", ".video_label"))
      ),
    collaboration_distribution_vs_non_collaborative_baseline = bundle_b_collaboration_distribution_prep(analytics, monetary)$summary,
    topic_view_distribution = deep_dive$topic_view_distribution$summary,
    tag_view_distribution = deep_dive$tag_view_distribution$summary,
    day_of_week_deviation_from_average = bundle_b_day_of_week_deviation_table(deep_dive$day_of_week_summary),
    weekend_vs_weekday_summary = deep_dive$weekend_summary,
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
  ai_inputs = NULL,
  out_root = here::here("templates", "reports", "Bundle_B"),
  plot_subdir = "figures",
  table_subdir = "tables",
  manifest_file = "bundle_b_artifact_manifest.json",
  ai_inputs_file = "bundle_b_ai_inputs.json",
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
      stop("Package `jsonlite` is required to export Bundle B AI inputs.")
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
    stop("Package `jsonlite` is required to export Bundle B artifact manifest.")
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

resolve_bundle_b_artifact_root <- function(
  talent_root,
  bundle_name = "bundle_B",
  report_subdir = "reports",
  artifact_subdir = "artifacts"
) {
  override <- trimws(Sys.getenv("BUNDLE_B_ARTIFACT_ROOT", unset = ""))
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

data_source <- tolower(trimws(Sys.getenv("TALENT_DATA_SOURCE", unset = "datalake")))
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

Talent <- trimws(Sys.getenv("TALENT_QUERY", unset = "Avaritia Hawthorne 【Variance Project】"))
if (!nzchar(Talent)) {
  Talent <- "Avaritia Hawthorne 【Variance Project】"
}

window_days <- suppressWarnings(as.integer(trimws(Sys.getenv("TALENT_WINDOW_DAYS", unset = ""))))
if (is.na(window_days)) {
  window_days <- NA_integer_
} else if (window_days <= 0) {
  stop("TALENT_WINDOW_DAYS must be a positive integer when provided.")
}
start_date_param <- parse_bundle_b_optional_date(Sys.getenv("TALENT_START_DATE", unset = ""))
end_date_param <- parse_bundle_b_optional_date(Sys.getenv("TALENT_END_DATE", unset = ""))
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

analytics <- apply_bundle_b_window(analytics, window_mode, window_start_date, window_end_date)
monetary <- apply_bundle_b_window(monetary, window_mode, window_start_date, window_end_date)
demo <- apply_bundle_b_window(demo, window_mode, window_start_date, window_end_date)
geo <- apply_bundle_b_window(geo, window_mode, window_start_date, window_end_date)

if (nrow(analytics) == 0 || nrow(monetary) == 0) {
  stop(
    "No data after applying the selected window. ",
    "At least analytics and monetary data are required."
  )
}

# Backward-compatible alias used by some Bundle report code.
df <- analytics

print_bundle_b_preview(preps)

deep_dive <- build_bundle_b_deep_dive(analytics, monetary)

bundle_b_ai_inputs <- list(
  metadata = list(
    talent = Talent,
    generated_at_utc = as.character(Sys.time()),
    data_source = data_source,
    data_root = normalizePath(root, winslash = "/", mustWork = FALSE),
    window_mode = window_mode,
    window_start_date = if (is.na(window_start_date)) NA_character_ else as.character(window_start_date),
    window_end_date = if (is.na(window_end_date)) NA_character_ else as.character(window_end_date)
  ),
  dataset_sizes = tibble::tibble(
    dataset = c("analytics", "monetary", "demo", "geo"),
    rows = c(nrow(analytics), nrow(monetary), nrow(demo), nrow(geo)),
    cols = c(ncol(analytics), ncol(monetary), ncol(demo), ncol(geo))
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

p_views <- plot_or_null("engagement_distribution_by_content_type")
p_rev <- plot_or_null("revenue_efficiency_by_content_type")
p_weekend <- plot_or_null("weekend_vs_weekday_summary")
p_day_of_week <- plot_or_null("day_of_week_deviation_from_average")
p_collab <- plot_or_null("collaboration_distribution_vs_non_collaborative_baseline")
p_topic <- plot_or_null("topic_view_distribution")
p_tag <- plot_or_null("tag_view_distribution")

for (nm in names(bundle_b_plots)) {
  cat("\nRendering plot:", nm, "\n")
  print(bundle_b_plots[[nm]])
}

bundle_b_tables <- c(
  bundle_b_ai_inputs$key_tables,
  list(dataset_sizes = bundle_b_ai_inputs$dataset_sizes)
)

bundle_b_artifact_root <- resolve_bundle_b_artifact_root(talent_root)
bundle_b_export <- export_bundle_b_artifacts(
  plots = bundle_b_plots,
  tables = bundle_b_tables,
  ai_inputs = bundle_b_ai_inputs,
  out_root = bundle_b_artifact_root
)

cat("\nExported Bundle B artifacts to:", bundle_b_export$out_root, "\n")
cat("Exported figures to:", bundle_b_export$plots_dir, "\n")
cat("Exported tables to:", bundle_b_export$tables_dir, "\n")
cat("Exported AI inputs JSON to:", bundle_b_export$ai_inputs_path, "\n")
cat("Exported artifact manifest to:", bundle_b_export$manifest_path, "\n")
