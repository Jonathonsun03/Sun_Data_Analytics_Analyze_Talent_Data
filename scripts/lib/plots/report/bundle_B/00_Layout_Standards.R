## Bundle B: shared layout standards for plot readability and consistent positioning.

bundle_b_title_text <- function(talent, title_suffix, width = 62) {
  # Keep title helper signature for compatibility, but do not prefix with talent name.
  stringr::str_wrap(as.character(title_suffix), width = width)
}

bundle_b_compact_discrete_labels <- function(values, width = 24) {
  stringr::str_trunc(as.character(values), width = width, side = "right", ellipsis = "...")
}

bundle_b_theme_standard <- function() {
  ggplot2::theme(
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 16,
      hjust = 0,
      lineheight = 1.1,
      margin = ggplot2::margin(b = 6)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 10,
      hjust = 0,
      lineheight = 1.2,
      margin = ggplot2::margin(b = 8)
    ),
    axis.title.x = ggplot2::element_text(size = 11, margin = ggplot2::margin(t = 8)),
    axis.title.y = ggplot2::element_text(
      size = 11,
      angle = 90,
      vjust = 0.5,
      margin = ggplot2::margin(r = 8)
    ),
    axis.text = ggplot2::element_text(size = 10),
    strip.text = ggplot2::element_text(size = 11, face = "bold"),
    legend.title = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 9),
    plot.margin = ggplot2::margin(t = 8, r = 16, b = 8, l = 8)
  )
}

bundle_b_plotly_layout <- function(
  plot_obj,
  margin_l = 120,
  margin_r = 30,
  margin_b = 64,
  margin_t = 88
) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    return(plot_obj)
  }
  plot_obj <- plot_obj %>%
    plotly::layout(
      margin = list(l = margin_l, r = margin_r, b = margin_b, t = margin_t),
      xaxis = list(automargin = TRUE),
      yaxis = list(automargin = TRUE),
      autosize = TRUE,
      title = list(x = 0.01, xanchor = "left")
    )

  axis_keys <- names(plot_obj$x$layout)[grepl("^(x|y)axis[0-9]*$", names(plot_obj$x$layout))]
  for (axis_key in axis_keys) {
    if (is.null(plot_obj$x$layout[[axis_key]])) {
      plot_obj$x$layout[[axis_key]] <- list()
    }
    plot_obj$x$layout[[axis_key]]$automargin <- TRUE
    axis_title <- plot_obj$x$layout[[axis_key]]$title
    if (!is.null(axis_title)) {
      if (is.list(axis_title)) {
        if (is.null(axis_title$standoff)) {
          axis_title$standoff <- 8
        }
      } else {
        axis_title <- list(text = as.character(axis_title), standoff = 8)
      }
      plot_obj$x$layout[[axis_key]]$title <- axis_title
    }
  }

  plot_obj
}

bundle_b_ggplotly <- function(plot_obj, tooltip = c("text", "x", "y")) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive charts.")
  }
  bundle_b_plotly_layout(
    plotly::ggplotly(plot_obj, tooltip = tooltip)
  )
}

bundle_b_topic_view_distribution_plot <- function(topic_dist, talent) {
  topic_view_distribution_plot(topic_dist, talent) +
    ggplot2::scale_x_discrete(labels = bundle_b_compact_discrete_labels) +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Topic View Distribution"),
      y = "Views"
    )
}

bundle_b_tag_view_distribution_plot <- function(tag_dist, talent) {
  tag_view_distribution_plot(tag_dist, talent) +
    ggplot2::scale_x_discrete(labels = bundle_b_compact_discrete_labels) +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Tag View Distribution"),
      y = "Views"
    )
}
