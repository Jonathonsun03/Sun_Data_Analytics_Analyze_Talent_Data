bundle_e_empty_plot <- function(title, subtitle = NULL, label = "No data available for current filters.") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = label, size = 4.4) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    theme_void() +
    ggplot2::labs(title = title, subtitle = subtitle)
}
