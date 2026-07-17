# Overview plot and table adapters.

dashboard_monthly_performance_plot <- function(dashboard_data, talent, value_mode = c("raw", "index")) {
  value_mode <- match.arg(value_mode)
  if (is.null(dashboard_data$monthly_performance)) {
    return(NULL)
  }
  dashboard_ggplotly(performance_trends_over_time_plot(
    dashboard_data$monthly_performance,
    talent = talent,
    value_mode = value_mode
  ))
}


dashboard_top_videos_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(dashboard_data$top_videos, page_length = page_length, scroll_x = TRUE)
}
