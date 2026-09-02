# Individual-video plot and value-card adapters.

dashboard_individual_video_format_value <- function(value, format) {
  if (length(value) == 0 || is.na(value[[1]]) || !is.finite(value[[1]])) {
    return("N/A")
  }
  value <- value[[1]]
  switch(
    format,
    currency = scales::dollar(value, accuracy = 0.01),
    percent = scales::percent(value / 100, accuracy = 0.1),
    duration = paste0(scales::number(value, accuracy = 0.1), " sec"),
    scales::comma(value, accuracy = 0.1)
  )
}

dashboard_individual_video_value_box <- function(title, value, note = NULL) {
  displayed_value <- if (is.null(note)) {
    value
  } else {
    htmltools::tagList(
      htmltools::tags$span(value),
      htmltools::tags$small(
        style = "display:block; margin-top:0.25rem; font-size:0.72rem; font-weight:400;",
        note
      )
    )
  }
  if (requireNamespace("bslib", quietly = TRUE)) {
    return(bslib::value_box(title = title, value = displayed_value))
  }
  htmltools::div(
    class = "dashboard-metric-card",
    htmltools::tags$div(class = "dashboard-metric-label", title),
    htmltools::tags$div(class = "dashboard-metric-value", displayed_value)
  )
}

dashboard_individual_video_metric_definition <- function(metric) {
  info <- dashboard_individual_video_metric_info(metric)
  snapshot_note <- if (identical(info$metric_kind[[1]], "cumulative")) {
    "The chart uses the cumulative value recorded at each daily snapshot."
  } else {
    "The chart uses the average or rate recorded at each daily snapshot."
  }

  htmltools::div(
    class = "plot-builder-definition",
    htmltools::div(class = "plot-builder-definition-label", "Definition"),
    htmltools::tags$p(info$definition[[1]]),
    htmltools::tags$p(
      class = "plot-builder-definition-note",
      snapshot_note
    )
  )
}

dashboard_individual_video_statistics_panel <- function(history, metrics) {
  statistics <- dashboard_individual_video_descriptive_statistics(
    history,
    metrics
  )
  if (nrow(statistics$variables) == 0) {
    return(NULL)
  }

  statistic_item <- function(label, value) {
    htmltools::div(
      class = "plot-builder-stat-item",
      htmltools::div(class = "plot-builder-stat-label", label),
      htmltools::div(class = "plot-builder-stat-value", value)
    )
  }

  variable_sections <- lapply(seq_len(nrow(statistics$variables)), function(index) {
    row <- statistics$variables[index, , drop = FALSE]
    format <- row$format[[1]]
    htmltools::div(
      class = "plot-builder-stat-section",
      htmltools::tags$h6(row$label[[1]]),
      htmltools::div(
        class = "plot-builder-stat-grid",
        statistic_item("Observations", scales::comma(row$observations[[1]])),
        statistic_item(
          "Minimum",
          dashboard_individual_video_format_value(row$minimum, format)
        ),
        statistic_item(
          "Mean",
          dashboard_individual_video_format_value(row$mean, format)
        ),
        statistic_item(
          "Median",
          dashboard_individual_video_format_value(row$median, format)
        ),
        statistic_item(
          "Maximum",
          dashboard_individual_video_format_value(row$maximum, format)
        ),
        statistic_item(
          "Standard deviation",
          dashboard_individual_video_format_value(row$standard_deviation, format)
        )
      )
    )
  })

  correlation_value <- if (is.finite(statistics$pearson_correlation)) {
    scales::number(statistics$pearson_correlation, accuracy = 0.001)
  } else {
    "N/A"
  }
  htmltools::div(
    class = "plot-builder-statistics",
    htmltools::tags$h5("Descriptive statistics"),
    htmltools::tags$p(
      class = "plot-builder-stat-note",
      "Across snapshots in the selected date window"
    ),
    variable_sections,
    htmltools::div(
      class = "plot-builder-relationship",
      htmltools::tags$h6("Paired relationship"),
      htmltools::div(
        class = "plot-builder-stat-grid",
        statistic_item(
          "Paired snapshots",
          scales::comma(statistics$paired_observations)
        ),
        statistic_item("Pearson correlation", correlation_value)
      )
    )
  )
}

dashboard_individual_video_trend_plot <- function(history, metric) {
  series <- dashboard_individual_video_metric_series(history, metric)
  if (nrow(series) == 0) {
    return(NULL)
  }
  info <- dashboard_individual_video_metric_info(metric)
  label_value <- function(x) {
    vapply(x, dashboard_individual_video_format_value, character(1), format = info$format[[1]])
  }
  plot_df <- series %>%
    dplyr::mutate(
      tooltip_text = paste0(
        "Snapshot: ", format(.data$snapshot_date, "%Y-%m-%d"),
        "<br>Video age: ", scales::comma(.data$video_age_days), " days",
        "<br>", info$label[[1]], ": ", label_value(.data$value)
      )
    )

  plot <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$snapshot_date,
      y = .data$value,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_line(color = sun_data_brand_colors()[["blue"]], linewidth = 0.9) +
    ggplot2::geom_point(color = sun_data_brand_colors()[["blue"]], size = 1.8) +
    theme_nyt() +
    ggplot2::labs(x = "Analytics snapshot date", y = info$axis_label[[1]]) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d")

  dashboard_ggplotly(plot, tooltip = "text")
}

dashboard_individual_video_change_plot <- function(history, metric) {
  series <- dashboard_individual_video_metric_series(history, metric) %>%
    dplyr::filter(
      !is.na(.data$change_per_day),
      is.finite(.data$change_per_day)
    )
  if (nrow(series) == 0) {
    return(NULL)
  }
  info <- dashboard_individual_video_metric_info(metric)
  label_value <- function(x) {
    vapply(x, dashboard_individual_video_format_value, character(1), format = info$format[[1]])
  }
  plot_df <- series %>%
    dplyr::mutate(
      tooltip_text = paste0(
        "Snapshot: ", format(.data$snapshot_date, "%Y-%m-%d"),
        "<br>Days since prior snapshot: ", scales::comma(.data$days_since_prior_snapshot),
        "<br>Total change: ", label_value(.data$change),
        "<br>Average change/day: ", label_value(.data$change_per_day)
      )
    )

  plot <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$snapshot_date,
      y = .data$change_per_day,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
    ggplot2::geom_col(fill = sun_data_brand_colors()[["orange"]], alpha = 0.88) +
    theme_nyt() +
    ggplot2::labs(
      x = "Analytics snapshot date",
      y = paste("Average daily change in", tolower(info$label[[1]]))
    ) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d")

  dashboard_ggplotly(plot, tooltip = "text")
}

dashboard_individual_video_axis_range <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(c(0, 1))
  }
  limits <- range(x)
  span <- diff(limits)
  padding <- if (span > 0) span * 0.08 else max(abs(limits[[1]]) * 0.08, 1)
  c(limits[[1]] - padding, limits[[2]] + padding)
}

dashboard_individual_video_axis_config <- function(metric_info, values) {
  config <- list(
    title = metric_info$axis_label[[1]],
    range = dashboard_individual_video_axis_range(values),
    automargin = TRUE,
    zeroline = FALSE
  )
  format <- metric_info$format[[1]]
  if (identical(format, "currency")) {
    config$tickprefix <- "$"
    config$tickformat <- ",.2f"
  } else if (identical(format, "percent")) {
    config$ticksuffix <- "%"
    config$tickformat <- ",.1f"
  } else if (identical(format, "duration")) {
    config$ticksuffix <- " sec"
    config$tickformat <- ",.1f"
  } else {
    config$tickformat <- ",.1f"
  }
  config
}

dashboard_individual_video_scatter_plot <- function(
  history,
  x_metric,
  y_metric
) {
  x_info <- dashboard_individual_video_metric_info(x_metric)
  y_info <- dashboard_individual_video_metric_info(y_metric)
  if (is.null(history) || nrow(history) == 0 ||
      !all(c(x_metric, y_metric) %in% names(history))) {
    return(NULL)
  }

  x_label_value <- function(x) {
    vapply(
      x,
      dashboard_individual_video_format_value,
      character(1),
      format = x_info$format[[1]]
    )
  }
  y_label_value <- function(x) {
    vapply(
      x,
      dashboard_individual_video_format_value,
      character(1),
      format = y_info$format[[1]]
    )
  }

  plot_df <- history %>%
    dplyr::transmute(
      snapshot_date = as.Date(.data$snapshot_date),
      snapshot_label = format(.data$snapshot_date, "%Y-%m-%d"),
      video_age_days = .data$video_age_days,
      x_value = suppressWarnings(as.numeric(.data[[x_metric]])),
      y_value = suppressWarnings(as.numeric(.data[[y_metric]]))
    ) %>%
    dplyr::filter(
      !is.na(.data$snapshot_date),
      is.finite(.data$x_value),
      is.finite(.data$y_value)
    ) %>%
    dplyr::arrange(.data$snapshot_date) %>%
    dplyr::mutate(
      tooltip_text = paste0(
        "Snapshot: ", .data$snapshot_label,
        "<br>Video age: ", scales::comma(.data$video_age_days), " days",
        "<br>", x_info$label[[1]], ": ", x_label_value(.data$x_value),
        "<br>", y_info$label[[1]], ": ", y_label_value(.data$y_value)
      )
    )

  if (nrow(plot_df) == 0) {
    return(NULL)
  }

  plot <- plotly::plot_ly()
  plot <- plotly::add_trace(
    plot,
    data = plot_df,
    x = ~x_value,
    y = ~y_value,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = sun_data_brand_colors()[["steel"]], width = 1.5),
    marker = list(color = sun_data_brand_colors()[["steel"]], size = 6),
    opacity = 0.42,
    hoverinfo = "skip",
    name = "Snapshot path",
    inherit = FALSE
  )
  plot <- plotly::add_trace(
    plot,
    data = plot_df,
    x = ~x_value,
    y = ~y_value,
    frame = ~snapshot_label,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = sun_data_brand_colors()[["orange"]],
      size = 15,
      line = list(color = sun_data_brand_colors()[["midnight"]], width = 1.5)
    ),
    text = ~tooltip_text,
    hoverinfo = "text",
    name = "Selected snapshot",
    inherit = FALSE
  )
  plot <- plotly::layout(
    plot,
    xaxis = dashboard_individual_video_axis_config(x_info, plot_df$x_value),
    yaxis = dashboard_individual_video_axis_config(y_info, plot_df$y_value),
    margin = list(l = 80, r = 30, b = 105, t = 25),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.06,
      yanchor = "bottom"
    ),
    hovermode = "closest"
  )
  plot <- plotly::animation_opts(
    plot,
    frame = 550,
    transition = 250,
    redraw = FALSE
  )
  plot <- plotly::animation_slider(
    plot,
    currentvalue = list(prefix = "Snapshot: ")
  )
  plot <- plotly::animation_button(
    plot,
    x = 1,
    xanchor = "right",
    y = 0,
    yanchor = "top"
  )
  plotly::config(plot, responsive = TRUE, displaylogo = FALSE)
}
