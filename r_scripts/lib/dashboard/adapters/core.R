# Shared ggplot-to-dashboard and Plotly adaptation helpers.

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
