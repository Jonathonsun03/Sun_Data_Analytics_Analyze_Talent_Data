# Audience plot and geography-map adapters.

dashboard_audience_plot <- function(dashboard_data, talent) {
  if (is.null(dashboard_data$audience_summary) || nrow(dashboard_data$audience_summary) == 0) {
    return(dashboard_empty_state("Audience age and gender data are unavailable for this selection."))
  }
  dashboard_ggplotly(audience_age_gender_trends_plot(
    plot_df = dashboard_data$audience_summary,
    talent = talent
  ))
}

dashboard_audience_geography_map <- function(dashboard_data, talent = NULL) {
  audience_geography_plotly(
    dashboard_data$audience_geography,
    talent = talent,
    empty_state = dashboard_empty_state
  )
}
