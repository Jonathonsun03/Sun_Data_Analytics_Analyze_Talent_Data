bootstrap_get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

bootstrap_find_repo_root <- function(start_dirs = c(bootstrap_get_script_dir(), getwd())) {
  starts <- unique(normalizePath(start_dirs, winslash = "/", mustWork = FALSE))
  for (start in starts) {
    current <- start
    repeat {
      git_marker <- file.path(current, ".git")
      if (dir.exists(git_marker) || file.exists(git_marker)) {
        return(current)
      }
      parent <- dirname(current)
      if (identical(parent, current)) {
        break
      }
      current <- parent
    }
  }
  stop("Could not locate repository root.")
}

repo_root <- bootstrap_find_repo_root()
setwd(repo_root)

library(tidyverse)
library(here)

source(file.path(repo_root, "r_scripts", "lib", "utils", "staging_root.R"))
source(file.path(repo_root, "r_scripts", "lib", "utils", "datalake_root.r"))
source(file.path(repo_root, "r_scripts", "lib", "utils", "talent_select.R"))

list.files(file.path(repo_root, "r_scripts", "lib", "plots", "report", "bundle_A"), pattern = "[rR]$", full.names = TRUE) %>%
  purrr::walk(source)
list.files(file.path(repo_root, "r_scripts", "lib", "utils"), pattern = "[rR]$", full.names = TRUE) %>%
  purrr::walk(source)
list.files(file.path(repo_root, "r_scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  purrr::walk(source)
list.files(file.path(repo_root, "r_scripts", "lib", "report_tables"), pattern = "[rR]$", full.names = TRUE) %>%
  purrr::walk(source)
source(file.path(repo_root, "r_scripts", "lib", "plots", "PlottingThemes.R"))

clean_data_dir <- file.path(repo_root, "r_scripts", "lib", "clean_data")
source(file.path(clean_data_dir, "analytics_core.R"))
source(file.path(clean_data_dir, "analytics_join.R"))
source(file.path(clean_data_dir, "clean_columns.R"))
source(file.path(clean_data_dir, "video_prep.R"))
source(file.path(clean_data_dir, "title_classification_join.R"))

source(file.path(repo_root, "r_scripts", "lib", "reports", "report_interpretation_core.R"))
source(file.path(repo_root, "r_scripts", "lib", "reports", "bundle_A", "bundle_a_interpretations.R"))

arg_value <- function(args, flag, default = "") {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit[[1]] == length(args)) {
    return(default)
  }
  args[[hit[[1]] + 1]]
}

has_flag <- function(args, flag) {
  any(args == flag)
}

parse_optional_date <- function(x, label) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(as.Date(NA))
  }
  d <- suppressWarnings(as.Date(trimws(x)))
  if (is.na(d)) {
    stop(label, " must be YYYY-MM-DD.")
  }
  d
}

apply_bundle_a_window <- function(df, window_mode, window_start_date, window_end_date) {
  if (identical(window_mode, "all_data") || !is.data.frame(df) || nrow(df) == 0) {
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

load_bundle_a_titles <- function(talent) {
  titles_path <- trimws(Sys.getenv("BUNDLE_A_TITLE_CLASSIFICATIONS_PATH", unset = ""))
  if (!nzchar(titles_path)) {
    titles_path <- trimws(Sys.getenv("TITLE_CLASSIFICATIONS_PATH", unset = ""))
  }
  load_title_classifications(
    path = if (nzchar(titles_path)) titles_path else NULL,
    talent = talent
  )
}

build_bundle_a_engagement_table <- function(analytics) {
  analytics_eng <- analytics %>%
    dplyr::mutate(avg_view_prop = .data$averageViewPercentage / 100)
  engagement_distribution_content_type_prep(
    analytics_eng,
    metric_col = "avg_view_prop"
  ) %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::mutate(
      .success_cut = stats::quantile(.data$.metric, probs = 0.75, na.rm = TRUE, names = FALSE),
      .is_success = .data$.metric >= .data$.success_cut
    ) %>%
    dplyr::ungroup()
}

safe_table <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      warning(conditionMessage(e), call. = FALSE)
      tibble::tibble()
    }
  )
}

build_bundle_a_interpretation_context <- function(talent, data_source, data_root, window_days, start_date, end_date) {
  root <- if (nzchar(data_root)) {
    data_root
  } else if (identical(data_source, "datalake")) {
    get_datalake_root()
  } else {
    get_staging_root()
  }
  if (!dir.exists(root)) {
    stop("Resolved data root does not exist: ", root)
  }

  if (!is.na(start_date) || !is.na(end_date)) {
    window_start_date <- if (is.na(start_date)) as.Date("1900-01-01") else start_date
    window_end_date <- if (is.na(end_date)) Sys.Date() else end_date
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

  talent_root <- select_talent(talent, root = root)
  files <- TalentFiles(talent_root)
  titles <- load_bundle_a_titles(talent)
  preps <- video_preps_with_titles(
    files = files,
    titles = titles,
    talent = talent,
    dedupe = TRUE,
    key_cols = "Video ID",
    sort_cols = c("confidence", "published_at", "Published At"),
    dedupe_sets = c("analytics", "monetary")
  )

  analytics <- apply_bundle_a_window(preps$analytics, window_mode, window_start_date, window_end_date)
  monetary <- apply_bundle_a_window(preps$monetary, window_mode, window_start_date, window_end_date)
  demo <- apply_bundle_a_window(preps$demo, window_mode, window_start_date, window_end_date)

  if (nrow(analytics) == 0 || nrow(monetary) == 0) {
    stop("No analytics or monetary data after applying the selected window.")
  }

  audience_plot_df <- safe_table(audience_age_gender_trends_prep(demo_df = demo, freq = "month"))
  revenue_monthly_ct <- safe_table(total_metric_content_type_with_data(
    monetary,
    talent,
    metric_col = "Estimated Revenue",
    metric_label = "Revenue",
    window_months = 1,
    bar_position = "stack",
    show_counts = TRUE,
    unique_bar_colors = TRUE
  )$data)
  wk_dist <- tryCatch(weekend_vs_weekday_distribution_prep(analytics, monetary), error = function(e) list(plot_data = tibble::tibble(), summary = tibble::tibble()))

  tables <- list(
    views_by_content_type = safe_table(views_content_type_comparison_with_data(analytics, talent)$data),
    combined_performance_trends_safe = safe_table(bundle_a_prep_combined_trends(analytics, monetary, freq = "month")),
    combined_performance_trends = safe_table(performance_trends_over_time_with_data(
      analytics_df = analytics,
      monetary_df = monetary,
      talent = talent,
      freq = "month",
      value_mode = "raw"
    )$data),
    total_revenue_by_content_type = revenue_monthly_ct,
    engagement_distribution_successful_videos = safe_table(build_bundle_a_engagement_table(analytics)),
    revenue_over_time_by_content_type = safe_table(content_duration_metric_prep(monetary, "Estimated Revenue")),
    total_views_over_time_by_content_type = safe_table(content_duration_metric_prep(analytics, "views")),
    audience_age_gender_trends = audience_plot_df,
    weekend_vs_weekday = safe_table(weekend_vs_weekday_prep(analytics, monetary)),
    weekend_vs_weekday_distribution_summary = wk_dist$summary,
    day_of_week_distribution = safe_table(day_of_week_performance_prep(analytics, monetary)),
    collaboration_effectiveness = safe_table(collaboration_effectiveness_prep(analytics, monetary)),
    topic_performance = safe_table(topic_performance_prep(analytics, monetary, top_n = 8)),
    tag_performance = safe_table(tag_performance_prep(analytics, monetary, top_n = 15, min_videos = 2))
  )

  list(
    talent = talent,
    data_source = data_source,
    data_root = root,
    window_mode = window_mode,
    window_start_date = window_start_date,
    window_end_date = window_end_date,
    analytics = analytics,
    monetary = monetary,
    demo = demo,
    tables = tables
  )
}

usage <- function() {
  cat(
    "Usage: Rscript r_scripts/run/bundles/bundle_A/generate_bundle_A_interpretations.R --talent NAME --output-dir DIR [options]\n",
    "\nOptions:\n",
    "  --talent NAME\n",
    "  --output-dir DIR       Bundle A report output dir, containing/receiving interpretations/\n",
    "  --data-source NAME     staging|datalake (default: datalake)\n",
    "  --data-root PATH\n",
    "  --window-days N\n",
    "  --start-date YYYY-MM-DD\n",
    "  --end-date YYYY-MM-DD\n",
    "  --dry-run\n",
    "  -h, --help\n",
    sep = ""
  )
}

args <- commandArgs(trailingOnly = TRUE)
if (has_flag(args, "-h") || has_flag(args, "--help")) {
  usage()
  quit(status = 0)
}

talent <- trimws(arg_value(args, "--talent", Sys.getenv("TALENT_QUERY", unset = "Ava")))
output_dir <- trimws(arg_value(args, "--output-dir", Sys.getenv("BUNDLE_A_OUTPUT_DIR", unset = "")))
data_source <- tolower(trimws(arg_value(args, "--data-source", Sys.getenv("TALENT_DATA_SOURCE", unset = "datalake"))))
data_root <- trimws(arg_value(args, "--data-root", Sys.getenv("TALENT_DATA_ROOT", unset = "")))
window_days_txt <- trimws(arg_value(args, "--window-days", Sys.getenv("TALENT_WINDOW_DAYS", unset = "")))
window_days <- suppressWarnings(as.integer(window_days_txt))
if (!nzchar(window_days_txt)) {
  window_days <- NA_integer_
}
if (!is.na(window_days) && window_days <= 0) {
  stop("--window-days must be positive when provided.")
}
start_date <- parse_optional_date(arg_value(args, "--start-date", Sys.getenv("TALENT_START_DATE", unset = "")), "--start-date")
end_date <- parse_optional_date(arg_value(args, "--end-date", Sys.getenv("TALENT_END_DATE", unset = "")), "--end-date")
dry_run <- has_flag(args, "--dry-run")

if (!nzchar(talent)) {
  stop("--talent is required.")
}
if (!nzchar(output_dir)) {
  stop("--output-dir is required.")
}
if (!(data_source %in% c("staging", "datalake"))) {
  stop("--data-source must be staging or datalake.")
}
if (!is.na(start_date) && !is.na(end_date) && start_date > end_date) {
  stop("--start-date cannot be after --end-date.")
}

ctx <- build_bundle_a_interpretation_context(
  talent = talent,
  data_source = data_source,
  data_root = data_root,
  window_days = window_days,
  start_date = start_date,
  end_date = end_date
)
results <- bundle_a_build_interpretations(ctx)
interpret_root <- file.path(output_dir, "interpretations")

cat("[bundle-a-rule-interpretations] Talent:", talent, "\n")
cat("[bundle-a-rule-interpretations] Output:", interpret_root, "\n")
cat("[bundle-a-rule-interpretations] Slots:", nrow(results), "\n")
if (isTRUE(dry_run)) {
  print(results %>% dplyr::select(dplyr::all_of(c("slot", "confidence", "priority", "action_level", "fallback"))))
} else {
  bundle_a_write_interpretations(results, interpret_root)
  cat("[bundle-a-rule-interpretations] Wrote interpretation markdown files.\n")
}
