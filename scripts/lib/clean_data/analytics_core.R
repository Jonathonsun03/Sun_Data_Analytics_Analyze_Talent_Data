.normalize_talent_files <- function(files, talent_index = NULL) {
  if (is.data.frame(files)) return(files)
  if (!is.list(files)) {
    stop("`files` must be a data frame or a list returned by TalentFiles().")
  }

  # If this already looks like a per-type list, return as-is.
  if (!is.null(names(files)) && any(nzchar(names(files)))) {
    return(files)
  }

  # If TalentFiles() was called on a single path, unwrap the single element.
  if (length(files) == 1 && is.list(files[[1]])) {
    return(files[[1]])
  }

  # If multiple talents were passed, require an explicit index.
  if (!is.null(talent_index)) {
    if (!is.numeric(talent_index) || length(talent_index) != 1) {
      stop("`talent_index` must be a single numeric index.")
    }
    if (talent_index < 1 || talent_index > length(files)) {
      stop("`talent_index` out of range for provided `files` list.")
    }
    return(files[[talent_index]])
  }

  stop(
    "Multiple talent folders detected. ",
    "Pass a single talent folder to TalentFiles(), or provide `talent_index`."
  )
}

.get_type_data <- function(files, type, talent_index = NULL) {
  files <- .normalize_talent_files(files, talent_index)

  if (!is.list(files) || is.null(names(files)) || !(type %in% names(files))) {
    stop("Type not found in `files`: ", type)
  }

  dfs <- files[[type]]
  if (is.data.frame(dfs)) return(dfs)
  if (is.list(dfs)) return(dplyr::bind_rows(dfs))

  stop("Unexpected data structure for type: ", type)
}

prepare_analytics <- function(files,
                              talent = NULL,
                              analytics_type = "video_analytics",
                              talent_index = NULL) {

  if (is.null(talent)) talent <- "unknown"
  message("Preparing analytics data for: ", talent,
          " (type = ", analytics_type, ")")

  # 1) Get raw analytics for a given talent
  analytics_raw <- .get_type_data(
    files = files,
    type = analytics_type,
    talent_index = talent_index
  )
  
  # 2) Keep only useful columns & deduplicate by video/channel
  analytics_clean <- analytics_raw %>%
    dplyr::select(
      dplyr::all_of(c(
        "Video ID",
        "Channel ID",
        "Channel Name",
        "Title",
        "Published At",
        "date",
        "Content Type",
        "views",
        "estimatedMinutesWatched",
        "averageViewDuration",
        "averageViewPercentage",
        "subscribersGained",
        "subscribersLost"
      )),
      dplyr::any_of(c(
        "Estimated Revenue",
        "CPM",
        "DurationSeconds",
        "DurationISO"
      ))
    ) %>%
    dplyr::distinct(`Video ID`, `Channel ID`, `Channel Name`, .keep_all = TRUE)
  
  # 3) Return cleaned analytics (stream titles not available yet)
  analytics_clean
}

.safe_zscore <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sdv <- stats::sd(x, na.rm = TRUE)

  if (!is.finite(sdv) || sdv <= 0) {
    return(rep(0, length(x)))
  }

  (x - mu) / sdv
}

# Add log-view z scores and performance bins with group-aware fallback.
add_view_performance_bins <- function(df,
                                      views_col = "views",
                                      group_cols = c("talent_name", "content_type"),
                                      min_n = 10,
                                      fallback = c("global", "talent", "none"),
                                      talent_col = "talent_name") {
  fallback <- match.arg(fallback)

  if (!all(group_cols %in% names(df))) {
    stop("Missing required group columns: ", paste(setdiff(group_cols, names(df)), collapse = ", "))
  }
  if (!views_col %in% names(df)) {
    stop("Missing views column: ", views_col)
  }

  out <- df %>%
    dplyr::mutate(log_views = log1p(.data[[views_col]])) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(
      n_group = dplyr::n(),
      mu = mean(log_views, na.rm = TRUE),
      sdv = stats::sd(log_views, na.rm = TRUE),
      .z_group = ifelse(sdv > 0, (log_views - mu) / sdv, 0)
    ) %>%
    dplyr::ungroup()

  if (fallback == "global") {
    out <- out %>%
      dplyr::mutate(.z_fallback = .safe_zscore(log_views))
  } else if (fallback == "talent") {
    if (!talent_col %in% names(out)) {
      stop("fallback = 'talent' requires column: ", talent_col)
    }
    out <- out %>%
      dplyr::group_by(.data[[talent_col]]) %>%
      dplyr::mutate(.z_fallback = .safe_zscore(log_views)) %>%
      dplyr::ungroup()
  } else {
    out <- out %>%
      dplyr::mutate(.z_fallback = .z_group)
  }

  out %>%
    dplyr::mutate(
      z_views = dplyr::if_else(n_group < min_n, .z_fallback, .z_group),
      view_perf_bin = dplyr::case_when(
        z_views <= -1.5 ~ "very_low",
        z_views <= -0.5 ~ "low",
        z_views < 0.5 ~ "typical",
        z_views < 1.5 ~ "high",
        TRUE ~ "very_high"
      )
    ) %>%
    dplyr::select(-dplyr::any_of(c(".z_group", ".z_fallback")))
}

# Add a z-score column to a numeric weight-like column.
add_weight_z_score <- function(tbl, weight_col = "weight", z_col = "z_score") {
  if (!weight_col %in% names(tbl)) {
    stop("Column not found: ", weight_col)
  }

  w <- tbl[[weight_col]]
  sd_w <- stats::sd(w, na.rm = TRUE)
  mu_w <- mean(w, na.rm = TRUE)

  z_vals <- if (is.finite(sd_w) && sd_w > 0) {
    (w - mu_w) / sd_w
  } else {
    rep(0, length(w))
  }

  tbl[[z_col]] <- z_vals
  tbl
}
