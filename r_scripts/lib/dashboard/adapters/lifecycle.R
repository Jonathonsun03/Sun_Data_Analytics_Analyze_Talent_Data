# Lifecycle plot and table adapters.

dashboard_lifecycle_data_available <- function(dashboard_data) {
  !is.null(dashboard_data$lifecycle) &&
    !is.null(dashboard_data$lifecycle$video_summary) &&
    nrow(dashboard_data$lifecycle$video_summary) > 0
}

dashboard_lifecycle_latest_views_age_plot <- function(dashboard_data, talent) {
  if (!dashboard_lifecycle_data_available(dashboard_data)) {
    return(dashboard_empty_state("Lifecycle data is unavailable for the active filters."))
  }

  bundle_e_ggplotly(
    dashboard_lifecycle_card_ggplot(
      plot_bundle_e_latest_views_vs_age(dashboard_data$lifecycle$video_summary, talent)
    ),
    tooltip = "text",
    include_title = FALSE,
    compact = TRUE,
    height = 520,
    tickangle = -35
  )
}

dashboard_lifecycle_recent_vs_lifetime_plot <- function(dashboard_data, talent) {
  if (!dashboard_lifecycle_data_available(dashboard_data)) {
    return(dashboard_empty_state("Lifecycle data is unavailable for the active filters."))
  }

  bundle_e_ggplotly(
    dashboard_lifecycle_card_ggplot(
      plot_bundle_e_recent_vs_lifetime_rate(dashboard_data$lifecycle$video_summary, talent)
    ),
    tooltip = "text",
    include_title = FALSE,
    compact = TRUE,
    height = 520,
    tickangle = -35
  )
}

dashboard_lifecycle_topic_longevity_plot <- function(dashboard_data, talent) {
  if (!dashboard_lifecycle_data_available(dashboard_data)) {
    return(dashboard_empty_state("Lifecycle data is unavailable for the active filters."))
  }

  bundle_e_ggplotly(
    dashboard_lifecycle_card_ggplot(
      plot_bundle_e_topic_video_type_longevity(
        dashboard_data$lifecycle$video_summary,
        talent,
        top_n = 5,
        min_videos = 2
      )
    ),
    tooltip = "text",
    include_title = FALSE,
    compact = TRUE,
    height = 520,
    tickangle = -35
  )
}

dashboard_lifecycle_classification_guide <- function() {
  dashboard_static_table(
    tibble::tibble(
      `Lifecycle category` = c(
        "Evergreen",
        "Front-loaded",
        "Re-accelerating",
        "Other"
      ),
      `What it means` = c(
        "The upload is still drawing a meaningful recent pace well after release.",
        "The upload depended heavily on launch-period traction and has cooled substantially.",
        "The upload is gaining momentum compared with the immediately prior period.",
        "The upload does not meet the re-accelerating, evergreen, or front-loaded rule."
      ),
      `Classification rule` = c(
        "Age is at least 90 days and recent 30-day average views/day is at least 25% of lifetime average views/day.",
        "The 30-day launch window is fully observed and recent 30-day average views/day is no more than 10% of observed day-30 views/day.",
        "Growth acceleration ratio is finite and at least 1.5.",
        "Fallback category after applying the momentum plot priority order."
      ),
      `How to read it` = c(
        "Long-tail content that is still contributing after the initial release window.",
        "Launch-driven content that may need strong release timing or promotion to repeat.",
        "Content with renewed activity, possibly from search, recommendations, clips, or a new audience fit.",
        "Neutral lifecycle profile under the current data window."
      )
    )
  )
}

dashboard_lifecycle_type_age_curve_plot <- function(dashboard_data, talent, content_type) {
  if (is.null(dashboard_data$lifecycle) || is.null(dashboard_data$lifecycle$type_curves)) {
    return(dashboard_empty_state("Lifecycle curve data is unavailable for the active filters."))
  }

  curve_df <- dashboard_data$lifecycle$type_curves[[content_type]]
  if (is.null(curve_df)) {
    curve_df <- tibble::tibble()
  }
  type_label <- switch(
    content_type,
    short = "Short uploads",
    video = "Video uploads",
    live = "Live uploads",
    tools::toTitleCase(content_type)
  )

  bundle_e_ggplotly(
    dashboard_lifecycle_card_ggplot(
      plot_bundle_e_type_age_curve_comparison(
        curve_df,
        talent,
        content_type_label = type_label
      )
    ),
    tooltip = "text",
    include_title = FALSE,
    compact = TRUE,
    height = 520,
    tickangle = -35
  )
}
dashboard_lifecycle_recommendations_table <- function(dashboard_data) {
  # TODO: wire this after Bundle E artifact loading is extracted from bundle_e.Rmd.
  dashboard_datatable(dashboard_data$recommendations$lifecycle, page_length = 10, scroll_x = TRUE)
}
