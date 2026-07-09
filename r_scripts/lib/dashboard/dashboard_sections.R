dashboard_datatable <- function(df, caption = NULL, page_length = 10, scroll_x = TRUE) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  DTSettings(
    df,
    class = "display stripe hover",
    scroll_x = scroll_x,
    options = list(pageLength = page_length),
    filter = "none"
  )
}

dashboard_static_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  htmltools::tags$table(
    class = "table table-sm table-striped dashboard-static-table",
    style = "width:100%; font-size:0.86rem; margin:0;",
    htmltools::tags$thead(
      htmltools::tags$tr(
        lapply(names(df), function(nm) {
          htmltools::tags$th(style = "vertical-align:top;", nm)
        })
      )
    ),
    htmltools::tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        htmltools::tags$tr(
          lapply(names(df), function(nm) {
            htmltools::tags$td(style = "vertical-align:top;", as.character(df[[nm]][[i]]))
          })
        )
      })
    )
  )
}

dashboard_overview_table <- function(dashboard_data) {
  dashboard_datatable(dashboard_data$overview, page_length = nrow(dashboard_data$overview), scroll_x = TRUE)
}

dashboard_empty_state <- function(message) {
  htmltools::div(
    class = "dashboard-empty-state",
    style = "padding: 1rem; color: #555;",
    message
  )
}

dashboard_filter_context_card <- function(filter_summary) {
  if (is.null(filter_summary) || nrow(filter_summary) == 0) {
    return(dashboard_empty_state("No active filter summary is available."))
  }

  htmltools::div(
    class = "dashboard-filter-context",
    style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(9rem,1fr)); gap:0.5rem 1rem; padding:0.65rem 0.9rem;",
    lapply(seq_len(nrow(filter_summary)), function(i) {
      htmltools::div(
        style = "min-width:0;",
        htmltools::tags$div(
          style = "font-size:0.72rem; color:#666; text-transform:uppercase; letter-spacing:0.04em;",
          filter_summary$setting[[i]]
        ),
        htmltools::tags$div(
          style = "font-size:0.92rem; font-weight:600; overflow-wrap:anywhere;",
          filter_summary$value[[i]]
        )
      )
    })
  )
}

dashboard_recommendation_cards <- function(recommendations, domain = NULL, limit = NULL) {
  recs <- recommendations
  if (is.list(recommendations) && !is.data.frame(recommendations)) {
    recs <- if (is.null(domain)) recommendations$all else recommendations[[domain]]
  } else if (!is.null(domain) && "domain" %in% names(recs)) {
    recs <- recs %>% dplyr::filter(.data$domain == .env$domain)
  }

  if (is.null(recs) || nrow(recs) == 0) {
    return(dashboard_empty_state("Not enough evidence for a recommendation in this area for the active filters."))
  }
  if (!is.null(limit) && is.finite(limit)) {
    recs <- recs %>% dplyr::slice_head(n = limit)
  }

  badge_color <- function(confidence) {
    switch(
      confidence,
      strong = "#1f7a4d",
      moderate = "#2f6f9f",
      weak = "#8a6d1d",
      insufficient = "#777777",
      "#777777"
    )
  }
  priority_label <- function(priority) {
    switch(
      priority,
      high = "High priority",
      medium = "Medium priority",
      low = "Low priority",
      "Priority"
    )
  }

  cards <- lapply(seq_len(nrow(recs)), function(i) {
    rec <- recs[i, , drop = FALSE]
    confidence <- as.character(rec$confidence[[1]])
    htmltools::tags$article(
      class = "dashboard-recommendation-card",
      style = paste(
        "border:1px solid #d9d9d9; border-radius:6px; padding:0.9rem 1rem;",
        "background:#fff; display:flex; flex-direction:column; gap:0.55rem;"
      ),
      htmltools::div(
        style = "display:flex; justify-content:space-between; gap:0.75rem; align-items:flex-start;",
        htmltools::tags$h3(
          style = "font-size:1rem; line-height:1.25; margin:0;",
          as.character(rec$title[[1]])
        ),
        htmltools::tags$span(
          style = paste0(
            "background:", badge_color(confidence), "; color:white; border-radius:999px;",
            "font-size:0.72rem; line-height:1; padding:0.32rem 0.5rem; text-transform:uppercase;",
            "letter-spacing:0.04em; white-space:nowrap;"
          ),
          confidence
        )
      ),
      htmltools::tags$div(
        style = "font-size:0.78rem; color:#555; text-transform:uppercase; letter-spacing:0.04em;",
        paste(priority_label(as.character(rec$priority[[1]])), "|", as.character(rec$domain[[1]]))
      ),
      htmltools::tags$p(
        style = "margin:0; color:#333;",
        htmltools::tags$strong("Finding: "),
        as.character(rec$finding[[1]])
      ),
      htmltools::tags$p(
        style = "margin:0; color:#333;",
        htmltools::tags$strong("Recommendation: "),
        as.character(rec$recommendation[[1]])
      ),
      htmltools::tags$p(
        style = "margin:0; color:#333;",
        htmltools::tags$strong("Evidence: "),
        as.character(rec$evidence[[1]])
      ),
      htmltools::tags$p(
        style = "margin:0; color:#666; font-size:0.9rem;",
        htmltools::tags$strong("Caveat: "),
        as.character(rec$caveat[[1]])
      )
    )
  })

  htmltools::div(
    class = "dashboard-recommendation-grid",
    style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(19rem,1fr)); gap:0.85rem; padding:0.25rem;",
    cards
  )
}

dashboard_recommendation_caveat_cards <- function(recommendation_story) {
  caveats <- recommendation_story$caveats
  if (is.null(caveats) || nrow(caveats) == 0) {
    return(dashboard_empty_state("No additional caveats were generated for the active filters."))
  }

  caveats <- caveats %>% dplyr::slice_head(n = 6)
  htmltools::tags$ul(
    style = "margin:0; padding:0.75rem 1.25rem; color:#444;",
    lapply(seq_len(nrow(caveats)), function(i) {
      htmltools::tags$li(
        style = "margin-bottom:0.45rem;",
        paste0(caveats$title[[i]], ": ", caveats$caveat[[i]])
      )
    })
  )
}

dashboard_overview_value_boxes <- function(dashboard_data) {
  # index.qmd can map this compact table into Quarto value boxes.
  dashboard_data$overview
}

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

dashboard_ggplotly <- function(plot_obj, tooltip = NULL, compact = FALSE, tickangle = -45) {
  if (is.null(plot_obj) || !requireNamespace("plotly", quietly = TRUE)) {
    return(plot_obj)
  }

  if (inherits(plot_obj, "ggplot")) {
    if (!is.null(tickangle)) {
      plot_obj <- plot_obj +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = abs(tickangle), hjust = 1, vjust = 1)
        )
    }
  }

  p <- if (is.null(tooltip)) {
    plotly::ggplotly(plot_obj)
  } else {
    plotly::ggplotly(plot_obj, tooltip = tooltip)
  }

  p <- plotly::layout(
    p,
    margin = if (isTRUE(compact)) {
      list(l = 54, r = 16, b = 54, t = 16)
    } else {
      list(l = 70, r = 30, b = 95, t = 85)
    },
    xaxis = list(automargin = TRUE, tickangle = tickangle),
    yaxis = list(automargin = TRUE),
    autosize = TRUE
  )

  axis_keys <- names(p$x$layout)[grepl("^xaxis[0-9]*$", names(p$x$layout))]
  for (axis_key in axis_keys) {
    if (is.null(p$x$layout[[axis_key]])) {
      p$x$layout[[axis_key]] <- list()
    }
    p$x$layout[[axis_key]]$automargin <- TRUE
    p$x$layout[[axis_key]]$tickangle <- tickangle
  }

  plotly::config(p, responsive = TRUE, displaylogo = FALSE)
}

dashboard_card_ggplot <- function(plot_obj) {
  if (is.null(plot_obj) || !inherits(plot_obj, "ggplot")) {
    return(plot_obj)
  }

  plot_obj +
    ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(t = 4, r = 8, b = 4, l = 4)
    )
}

dashboard_lifecycle_card_ggplot <- function(plot_obj) {
  if (is.null(plot_obj) || !inherits(plot_obj, "ggplot")) {
    return(plot_obj)
  }

  dashboard_card_ggplot(plot_obj) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      strip.text = ggplot2::element_text(size = 8),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = grid::unit(0.45, "lines")
    )
}

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

dashboard_content_views_plot <- function(dashboard_data, talent) {
  analytics <- dashboard_data$source_data$analytics
  if (is.null(analytics) || nrow(analytics) == 0) {
    return(NULL)
  }
  dashboard_ggplotly(views_content_type_comparison(analytics, talent))
}

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

dashboard_revenue_content_type_plot <- function(dashboard_data, talent) {
  monetary <- dashboard_data$source_data$monetary
  if (is.null(monetary) || nrow(monetary) == 0 || !("Estimated Revenue" %in% names(monetary))) {
    return(NULL)
  }
  plot_df <- total_metric_content_type_prep(
    monetary,
    metric_col = "Estimated Revenue",
    window_months = 1
  )
  dashboard_ggplotly(total_metric_content_type_plot(
    plot_df,
    talent = talent,
    metric_label = "Revenue",
    subtitle_text = NULL,
    x_axis_label = "Month",
    bar_position = "stack",
    show_counts = TRUE,
    unique_bar_colors = TRUE
  ))
}

dashboard_content_engagement_plot <- function(dashboard_data, talent) {
  analytics <- dashboard_data$source_data$analytics
  if (is.null(analytics) || nrow(analytics) == 0 || !("averageViewPercentage" %in% names(analytics))) {
    return(NULL)
  }
  dashboard_ggplotly(engagement_distribution_content_type(
    analytics %>% dplyr::mutate(avg_view_prop = .data$averageViewPercentage / 100),
    talent,
    metric_col = "avg_view_prop",
    metric_label = "Average View %",
    y_as_percent = TRUE
  ))
}

dashboard_topic_performance_plot <- function(dashboard_data, talent, as_share = FALSE) {
  topic_summary <- dashboard_data$content_strategy$topic_summary
  if (is.null(topic_summary) || nrow(topic_summary) == 0) {
    return(NULL)
  }
  dashboard_ggplotly(topic_performance_plot(
    topic_summary,
    talent = talent,
    as_share = as_share
  ))
}

dashboard_topic_performance_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(
    dashboard_data$content_strategy$topic_summary,
    page_length = page_length,
    scroll_x = TRUE
  )
}

dashboard_tag_performance_plot <- function(dashboard_data, talent, as_share = FALSE) {
  tag_summary <- dashboard_data$content_strategy$tag_summary
  if (is.null(tag_summary) || nrow(tag_summary) == 0) {
    return(NULL)
  }
  dashboard_ggplotly(tag_performance_plot(
    tag_summary,
    talent = talent,
    as_share = as_share
  ))
}

dashboard_tag_performance_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(
    dashboard_data$content_strategy$tag_summary,
    page_length = page_length,
    scroll_x = TRUE
  )
}

dashboard_collaboration_performance_plot <- function(dashboard_data, talent, as_share = FALSE) {
  collab_summary <- dashboard_data$content_strategy$collab_summary
  if (is.null(collab_summary) || nrow(collab_summary) == 0) {
    return(NULL)
  }
  dashboard_ggplotly(collaboration_effectiveness_plot(
    collab_summary,
    talent = talent,
    as_share = as_share
  ))
}

dashboard_collaboration_performance_compact_plot <- function(dashboard_data) {
  collab_summary <- dashboard_data$content_strategy$collab_summary
  if (is.null(collab_summary) || nrow(collab_summary) == 0 || !requireNamespace("plotly", quietly = TRUE)) {
    return(NULL)
  }

  plot_df <- collab_summary %>%
    dplyr::mutate(collab_group = as.character(.data$collab_group)) %>%
    dplyr::select(
      collab_group,
      AverageViewsPerVideo,
      AverageRevenuePerVideo
    ) %>%
    tidyr::pivot_longer(
      cols = c("AverageViewsPerVideo", "AverageRevenuePerVideo"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric = dplyr::recode(
        .data$metric,
        AverageViewsPerVideo = "Avg views/video",
        AverageRevenuePerVideo = "Avg revenue/video"
      ),
      hover_text = paste0(
        .data$collab_group,
        "<br>", .data$metric, ": ",
        dplyr::if_else(
          .data$metric == "Avg revenue/video",
          scales::dollar(.data$value, accuracy = 0.01),
          scales::comma(.data$value, accuracy = 1)
        )
      )
    )

  brand_colors <- sun_data_brand_colors()
  collab_colors <- c(
    "Non-collaborative" = brand_colors[["steel"]],
    "Collaborative" = brand_colors[["blue"]]
  )
  present_groups <- unique(plot_df$collab_group)
  missing_groups <- setdiff(present_groups, names(collab_colors))
  if (length(missing_groups) > 0) {
    fallback_colors <- sun_data_palette(length(missing_groups), variant = "brand")
    names(fallback_colors) <- missing_groups
    collab_colors <- c(collab_colors, fallback_colors)
  }
  collab_colors <- collab_colors[intersect(names(collab_colors), present_groups)]

  views_df <- plot_df %>% dplyr::filter(.data$metric == "Avg views/video")
  revenue_df <- plot_df %>% dplyr::filter(.data$metric == "Avg revenue/video")

  views_plot <- plotly::plot_ly(
    views_df,
    x = ~collab_group,
    y = ~value,
    color = ~collab_group,
    colors = collab_colors,
    text = ~hover_text,
    hoverinfo = "text",
    type = "bar",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      title = list(text = "Avg views/video", font = list(size = 12)),
      xaxis = list(title = "", automargin = TRUE, tickangle = -35),
      yaxis = list(title = "", automargin = TRUE, tickformat = ",")
    )

  revenue_plot <- plotly::plot_ly(
    revenue_df,
    x = ~collab_group,
    y = ~value,
    color = ~collab_group,
    colors = collab_colors,
    text = ~hover_text,
    hoverinfo = "text",
    type = "bar",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      title = list(text = "Avg revenue/video", font = list(size = 12)),
      xaxis = list(title = "", automargin = TRUE, tickangle = -35),
      yaxis = list(title = "", automargin = TRUE, tickprefix = "$", tickformat = ",.0f")
    )

  plotly::subplot(
    views_plot,
    revenue_plot,
    nrows = 1,
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) %>%
    plotly::layout(
      margin = list(l = 55, r = 18, b = 80, t = 30),
      autosize = TRUE
    ) %>%
    plotly::config(responsive = TRUE, displaylogo = FALSE)
}

dashboard_collaboration_performance_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(
    dashboard_data$content_strategy$collab_summary,
    page_length = page_length,
    scroll_x = TRUE
  )
}

dashboard_weekday_table <- function(dashboard_data) {
  if (is.null(dashboard_data$weekday_summary) || nrow(dashboard_data$weekday_summary) == 0) {
    return(dashboard_empty_state("Weekday summary is unavailable for the active filters."))
  }
  dashboard_datatable(dashboard_data$weekday_summary, page_length = 7, scroll_x = TRUE)
}

dashboard_weekday_plot <- function(dashboard_data, talent, as_share = FALSE) {
  if (is.null(dashboard_data$weekday_summary) || nrow(dashboard_data$weekday_summary) == 0) {
    return(dashboard_empty_state("Day-of-week performance is unavailable for the active filters."))
  }
  dashboard_ggplotly(day_of_week_performance_plot(
    dashboard_data$weekday_summary,
    talent = talent,
    as_share = as_share
  ), compact = TRUE)
}

dashboard_day_of_week_deviation_plot <- function(dashboard_data, talent) {
  if (is.null(dashboard_data$weekday_summary) || nrow(dashboard_data$weekday_summary) == 0) {
    return(dashboard_empty_state("Day-of-week deviation is unavailable for the active filters."))
  }
  dashboard_ggplotly(
    dashboard_card_ggplot(bundle_b_day_of_week_lift_plot(
      dashboard_data$weekday_summary,
      talent = talent
    )),
    tooltip = "text",
    compact = TRUE,
    tickangle = -35
  )
}

dashboard_weekend_weekday_distribution_plot <- function(dashboard_data, talent) {
  wk_dist <- dashboard_data$weekend_weekday_distribution
  if (is.null(wk_dist)) {
    return(dashboard_empty_state("Weekend versus weekday distribution is unavailable for the active filters."))
  }
  dashboard_ggplotly(
    dashboard_card_ggplot(bundle_b_weekend_weekday_distribution_plot(
      wk_dist,
      talent = talent
    )),
    tooltip = "text",
    compact = TRUE,
    tickangle = 0
  )
}

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
  geo_df <- dashboard_data$audience_geography
  if (is.null(geo_df) || nrow(geo_df) == 0) {
    return(dashboard_empty_state("Audience geography data are unavailable for this selection."))
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    return(dashboard_empty_state("Package 'plotly' is required for the audience geography map."))
  }

  metric_label <- attr(geo_df, "metric_label", exact = TRUE)
  if (is.null(metric_label) || !nzchar(metric_label)) {
    metric_label <- "Audience Metric"
  }
  unmatched_count <- attr(geo_df, "unmatched_country_count", exact = TRUE)
  if (is.null(unmatched_count)) {
    unmatched_count <- sum(is.na(geo_df$country_iso3))
  }
  unmatched_codes <- attr(geo_df, "unmatched_country_codes", exact = TRUE)
  unmatched_note <- if (unmatched_count > 0) {
    paste0(
      unmatched_count,
      " country code",
      if (unmatched_count == 1) "" else "s",
      " could not be matched to map geometry",
      if (length(unmatched_codes) > 0) paste0(": ", paste(unmatched_codes, collapse = ", ")) else "",
      "."
    )
  } else {
    NULL
  }

  plot_df <- geo_df %>%
    dplyr::filter(!is.na(.data$country_iso3), !is.na(.data$metric_value)) %>%
    dplyr::mutate(
      country_label = dplyr::if_else(
        is.na(.data$country_name) | !nzchar(.data$country_name),
        .data$country_code,
        paste0(.data$country_name, " (", .data$country_code, ")")
      ),
      hover_text = paste0(
        .data$country_label,
        "<br>", metric_label, ": ", .data$metric_display,
        "<br>Snapshot date: ", .data$snapshot_date_label
      )
    )

  if (nrow(plot_df) == 0) {
    msg <- "Audience geography rows were available, but no country codes matched map geometry."
    if (!is.null(unmatched_note)) {
      msg <- paste(msg, unmatched_note)
    }
    return(dashboard_empty_state(msg))
  }

  title_text <- paste0("Audience Geography by Country - ", metric_label)
  if (!is.null(talent) && length(talent) > 0 && !is.na(talent[[1]]) && nzchar(as.character(talent[[1]]))) {
    title_text <- paste0(title_text, "<br><sup>Talent: ", htmltools::htmlEscape(as.character(talent[[1]])), "</sup>")
  }

  snapshot_count <- dplyr::n_distinct(plot_df$snapshot_date_label)
  plot_args <- list(
    data = plot_df,
    type = "choropleth",
    locations = ~country_iso3,
    z = ~metric_value,
    text = ~hover_text,
    hoverinfo = "text",
    locationmode = "ISO-3",
    colorscale = "Blues",
    marker = list(line = list(color = "white", width = 0.4)),
    colorbar = list(title = metric_label)
  )
  if (snapshot_count > 1) {
    plot_args$frame <- ~snapshot_date_label
  }

  p <- do.call(plotly::plot_ly, plot_args) %>%
    plotly::layout(
      title = list(text = title_text, x = 0, xanchor = "left"),
      geo = list(
        projection = list(type = "natural earth"),
        showframe = FALSE,
        showcoastlines = TRUE,
        coastlinecolor = "#999999",
        landcolor = "#f7f7f7",
        bgcolor = "rgba(0,0,0,0)"
      ),
      margin = list(l = 0, r = 0, t = 80, b = if (snapshot_count > 1) 55 else 10),
      autosize = TRUE
    ) %>%
    plotly::config(responsive = TRUE, displaylogo = FALSE)

  if (!is.null(unmatched_note)) {
    p <- htmltools::tagList(
      p,
      htmltools::div(
        style = "font-size: 0.85rem; color: #666; padding: 0 0.75rem 0.75rem;",
        unmatched_note
      )
    )
  }
  p
}

dashboard_top_videos_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(dashboard_data$top_videos, page_length = page_length, scroll_x = TRUE)
}

dashboard_topic_weekday_recommendation_table <- function(dashboard_data, content_type, page_length = 15) {
  recs <- dashboard_data$recommendations$topic_weekday[[content_type]]
  if (is.null(recs) || nrow(recs) == 0) {
    return(NULL)
  }
  dashboard_datatable(recs, page_length = page_length, scroll_x = TRUE)
}

dashboard_topic_weekday_heatmap <- function(dashboard_data, content_type, content_label = content_type) {
  topic_result <- dashboard_data$topic_weekday_summary[[content_type]]
  if (is.null(topic_result) || is.null(topic_result$topic_weekday) || nrow(topic_result$topic_weekday) == 0) {
    return(dashboard_empty_state("Topic-weekday heatmap is unavailable for the active filters."))
  }

  heatmap_df <- topic_result$topic_weekday %>%
    dplyr::mutate(
      .hover_text = paste0(
        "Topic: ", .data$topic_lumped,
        "<br>Weekday: ", .data$publish_wday,
        "<br>Median views: ", scales::comma(round(.data$median_views)),
        "<br>Streams: ", scales::comma(.data$streams)
      )
    )

  dashboard_ggplotly(
    ggplot2::ggplot(
      heatmap_df,
      ggplot2::aes(
        x = .data$publish_wday,
        y = .data$topic_lumped,
        fill = .data$median_views,
        text = .data$.hover_text
      )
    ) +
      ggplot2::geom_tile(color = "white", linewidth = 0.35) +
      ggplot2::scale_fill_viridis_c(option = "C", labels = scales::comma) +
      ggplot2::labs(x = "", y = "", fill = "Median views") +
      theme_nyt() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.margin = ggplot2::margin(t = 4, r = 8, b = 4, l = 4),
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
      ),
    tooltip = "text",
    compact = TRUE,
    tickangle = 0
  )
}

dashboard_lifecycle_recommendations_table <- function(dashboard_data) {
  # TODO: wire this after Bundle E artifact loading is extracted from bundle_e.Rmd.
  dashboard_datatable(dashboard_data$recommendations$lifecycle, page_length = 10, scroll_x = TRUE)
}
