# Individual-video longitudinal metric preparation.

dashboard_individual_video_metric_definitions <- function() {
  tibble::tribble(
    ~metric, ~label, ~axis_label, ~format, ~metric_kind, ~definition,
    "views", "Views", "Cumulative views", "number", "cumulative",
    "The number of recorded views attributed to the selected video.",
    "estimated_revenue", "Estimated revenue", "Cumulative estimated revenue", "currency", "cumulative",
    "Estimated net revenue from Google-sold advertising and non-advertising sources, in the reporting currency. It can change after month-end adjustments.",
    "estimated_minutes_watched", "Watch time", "Cumulative minutes watched", "number", "cumulative",
    "The estimated total number of minutes that viewers watched the selected video.",
    "average_view_duration", "Average view duration", "Average view duration (seconds)", "duration", "level",
    "The average length of a video playback, measured in seconds. YouTube excludes looping-clips traffic from this metric.",
    "average_view_percentage", "Average view percentage", "Average view percentage", "percent", "level",
    "The average percentage of the video watched during a playback. YouTube excludes looping-clips traffic from this metric.",
    "subscribers_gained", "Subscribers gained", "Cumulative subscribers gained", "number", "cumulative",
    "The number of subscriptions attributed to the selected video's watch page.",
    "subscribers_lost", "Subscribers lost", "Cumulative subscribers lost", "number", "cumulative",
    "The number of unsubscriptions attributed to the selected video's watch page.",
    "net_subscribers", "Net subscribers", "Cumulative net subscribers", "number", "cumulative",
    "A dashboard-derived measure calculated as subscribers gained minus subscribers lost for the selected video.",
    "cpm", "CPM", "CPM", "currency", "level",
    "Estimated gross revenue per thousand ad impressions, in the reporting currency."
  )
}

dashboard_individual_video_metric_info <- function(metric) {
  definitions <- dashboard_individual_video_metric_definitions()
  metric_id <- metric
  match <- definitions %>% dplyr::filter(.data$metric == .env$metric_id)
  if (nrow(match) != 1) {
    stop("Unsupported individual-video metric: ", metric, call. = FALSE)
  }
  match
}

dashboard_individual_video_prepare <- function(history) {
  if (is.null(history) || nrow(history) == 0) {
    return(tibble::tibble())
  }

  required <- c(
    "video_id", "snapshot_date", "published_at", "views",
    "estimated_minutes_watched", "average_view_duration",
    "average_view_percentage", "subscribers_gained", "subscribers_lost",
    "estimated_revenue", "cpm"
  )
  missing <- setdiff(required, names(history))
  if (length(missing) > 0) {
    stop(
      "Individual-video history is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  history %>%
    dplyr::mutate(
      snapshot_date = as.Date(.data$snapshot_date),
      publish_date = as.Date(.data$published_at),
      dplyr::across(
        dplyr::all_of(c(
          "views", "estimated_minutes_watched", "average_view_duration",
          "average_view_percentage", "subscribers_gained", "subscribers_lost",
          "estimated_revenue", "cpm"
        )),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) %>%
    dplyr::filter(!is.na(.data$snapshot_date)) %>%
    dplyr::arrange(.data$snapshot_date) %>%
    dplyr::distinct(.data$snapshot_date, .keep_all = TRUE) %>%
    dplyr::mutate(
      video_age_days = as.integer(.data$snapshot_date - .data$publish_date),
      net_subscribers = .data$subscribers_gained - .data$subscribers_lost,
      days_since_prior_snapshot = as.numeric(
        .data$snapshot_date - dplyr::lag(.data$snapshot_date)
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "views", "estimated_revenue", "estimated_minutes_watched",
          "average_view_duration", "average_view_percentage",
          "subscribers_gained", "subscribers_lost", "net_subscribers", "cpm"
        )),
        ~ .x - dplyr::lag(.x),
        .names = "{.col}_change"
      )
    )
}

dashboard_individual_video_metric_series <- function(history, metric) {
  info <- dashboard_individual_video_metric_info(metric)
  if (is.null(history) || nrow(history) == 0 || !metric %in% names(history)) {
    return(tibble::tibble())
  }

  change_col <- paste0(metric, "_change")
  history %>%
    dplyr::transmute(
      snapshot_date = .data$snapshot_date,
      video_age_days = .data$video_age_days,
      value = .data[[metric]],
      change = .data[[change_col]],
      days_since_prior_snapshot = .data$days_since_prior_snapshot,
      change_per_day = bundle_e_safe_divide(
        .data[[change_col]],
        .data$days_since_prior_snapshot
      ),
      metric = info$metric[[1]],
      metric_label = info$label[[1]],
      axis_label = info$axis_label[[1]],
      format = info$format[[1]],
      metric_kind = info$metric_kind[[1]]
    ) %>%
    dplyr::filter(!is.na(.data$value), is.finite(.data$value))
}

dashboard_individual_video_summary <- function(history, metric) {
  series <- dashboard_individual_video_metric_series(history, metric)
  if (nrow(series) == 0) {
    return(NULL)
  }

  first_value <- dplyr::first(series$value)
  latest_value <- dplyr::last(series$value)
  span_days <- as.numeric(
    dplyr::last(series$snapshot_date) - dplyr::first(series$snapshot_date)
  )
  total_change <- latest_value - first_value

  list(
    latest_value = latest_value,
    total_change = total_change,
    average_change_per_day = if (is.finite(span_days) && span_days > 0) {
      total_change / span_days
    } else {
      NA_real_
    },
    snapshot_count = nrow(series),
    span_days = span_days,
    first_snapshot_date = dplyr::first(series$snapshot_date),
    latest_snapshot_date = dplyr::last(series$snapshot_date),
    metric_info = dashboard_individual_video_metric_info(metric)
  )
}

dashboard_individual_video_snapshot_table <- function(history, metric) {
  series <- dashboard_individual_video_metric_series(history, metric)
  if (nrow(series) == 0) {
    return(tibble::tibble())
  }

  series %>%
    dplyr::transmute(
      `Snapshot Date` = .data$snapshot_date,
      `Video Age (Days)` = .data$video_age_days,
      Value = .data$value,
      `Change Since Prior Snapshot` = .data$change,
      `Days Since Prior Snapshot` = .data$days_since_prior_snapshot,
      `Average Change Per Day` = .data$change_per_day
    ) %>%
    dplyr::arrange(dplyr::desc(.data$`Snapshot Date`))
}

dashboard_individual_video_descriptive_statistics <- function(
  history,
  metrics
) {
  metrics <- unique(as.character(metrics))
  definitions <- dashboard_individual_video_metric_definitions()
  invalid_metrics <- setdiff(metrics, definitions$metric)
  if (length(invalid_metrics) > 0) {
    stop(
      "Unsupported individual-video metrics: ",
      paste(invalid_metrics, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(history) || nrow(history) == 0 || length(metrics) == 0) {
    return(list(
      variables = tibble::tibble(),
      paired_observations = 0L,
      pearson_correlation = NA_real_
    ))
  }

  variable_statistics <- purrr::map_dfr(metrics, function(metric) {
    info <- dashboard_individual_video_metric_info(metric)
    values <- if (metric %in% names(history)) {
      suppressWarnings(as.numeric(history[[metric]]))
    } else {
      numeric()
    }
    values <- values[is.finite(values)]
    tibble::tibble(
      metric = metric,
      label = info$label[[1]],
      format = info$format[[1]],
      observations = length(values),
      minimum = if (length(values) > 0) min(values) else NA_real_,
      mean = if (length(values) > 0) mean(values) else NA_real_,
      median = if (length(values) > 0) stats::median(values) else NA_real_,
      maximum = if (length(values) > 0) max(values) else NA_real_,
      standard_deviation = if (length(values) > 1) stats::sd(values) else NA_real_
    )
  })

  paired_observations <- 0L
  pearson_correlation <- NA_real_
  if (length(metrics) == 2 && all(metrics %in% names(history))) {
    x <- suppressWarnings(as.numeric(history[[metrics[[1]]]]))
    y <- suppressWarnings(as.numeric(history[[metrics[[2]]]]))
    keep <- is.finite(x) & is.finite(y)
    x <- x[keep]
    y <- y[keep]
    paired_observations <- length(x)
    if (paired_observations > 1 && stats::sd(x) > 0 && stats::sd(y) > 0) {
      pearson_correlation <- stats::cor(x, y)
    }
  }

  list(
    variables = variable_statistics,
    paired_observations = as.integer(paired_observations),
    pearson_correlation = pearson_correlation
  )
}
