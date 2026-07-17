# Reusable audience geography Plotly widget.

audience_geography_plotly <- function(
  geo_df,
  talent = NULL,
  empty_state = function(message) htmltools::div(class = "plot-empty-state", message)
) {
  if (is.null(geo_df) || nrow(geo_df) == 0) {
    return(empty_state("Audience geography data are unavailable for this selection."))
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    return(empty_state("Package 'plotly' is required for the audience geography map."))
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
    return(empty_state(msg))
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
