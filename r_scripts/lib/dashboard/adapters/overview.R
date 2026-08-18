# Overview plot and table adapters.

dashboard_monthly_performance_plot <- function(dashboard_data, talent, value_mode = c("raw", "index")) {
  value_mode <- match.arg(value_mode)
  if (is.null(dashboard_data$monthly_performance)) {
    return(NULL)
  }
  analytics <- dashboard_data$source_data$analytics
  snapshot_dates <- suppressWarnings(as.Date(analytics$date))
  snapshot_dates <- snapshot_dates[!is.na(snapshot_dates)]
  snapshot_label <- if (length(snapshot_dates) == 0) {
    "the latest selected analytics snapshot"
  } else {
    paste("the", format(max(snapshot_dates), "%Y-%m-%d"), "analytics snapshot")
  }
  interpretation <- paste(
    "Cumulative totals recorded in",
    snapshot_label,
    "grouped by video publication month; these are not views or revenue earned during that month."
  )

  plot <- performance_trends_over_time_plot(
    dashboard_data$monthly_performance,
    talent = talent,
    value_mode = value_mode
  ) +
    ggplot2::labs(
      subtitle = bundle_a_talent_subtitle(talent, interpretation),
      x = "Video publication month"
    )

  dashboard_ggplotly(plot)
}


dashboard_top_videos_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(dashboard_data$top_videos, page_length = page_length, scroll_x = TRUE)
}
