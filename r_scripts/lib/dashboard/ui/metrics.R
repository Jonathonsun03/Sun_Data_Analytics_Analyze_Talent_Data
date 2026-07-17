# Overview metric card UI.

dashboard_metric_value <- function(dashboard_data, metric) {
  analytics <- dashboard_data$source_data$analytics
  monetary <- dashboard_data$source_data$monetary

  switch(
    metric,
    total_views = sum(suppressWarnings(as.numeric(analytics$views)), na.rm = TRUE),
    total_revenue = if (!is.null(monetary) && "Estimated Revenue" %in% names(monetary)) {
      sum(suppressWarnings(as.numeric(monetary$`Estimated Revenue`)), na.rm = TRUE)
    } else {
      NA_real_
    },
    video_count = if (!is.null(analytics) && "Video ID" %in% names(analytics)) {
      dplyr::n_distinct(analytics$`Video ID`)
    } else {
      nrow(analytics)
    },
    average_views = mean(suppressWarnings(as.numeric(analytics$views)), na.rm = TRUE),
    stop("Unsupported dashboard metric: ", metric, call. = FALSE)
  )
}

dashboard_metric_card <- function(dashboard_data, metric) {
  value <- dashboard_metric_value(dashboard_data, metric)
  display_value <- switch(
    metric,
    total_revenue = if (is.na(value)) "N/A" else scales::dollar(value, accuracy = 1),
    average_views = if (is.nan(value) || is.na(value)) "N/A" else scales::comma(round(value)),
    if (is.na(value)) "N/A" else scales::comma(round(value))
  )
  metric_label <- switch(
    metric,
    total_views = "Total Views",
    total_revenue = "Total Revenue",
    video_count = "Videos / Streams",
    average_views = "Average Views",
    metric
  )

  if (requireNamespace("bslib", quietly = TRUE)) {
    return(bslib::value_box(title = metric_label, value = display_value))
  }

  htmltools::div(
    class = "dashboard-metric-card",
    htmltools::tags$div(class = "dashboard-metric-label", metric_label),
    htmltools::tags$div(class = "dashboard-metric-value", display_value)
  )
}
