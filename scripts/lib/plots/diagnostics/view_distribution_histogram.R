plot_view_distribution_histogram <- function(df,
                                             views_col = "views",
                                             fill_col = "content_type",
                                             facet_col = "talent_name",
                                             facet = FALSE,
                                             bins = 30,
                                             log_breaks = c(10, 30, 100, 300, 1e3, 3e3, 1e4, 3e4, 1e5),
                                             title = "Distribution of Views by Content Type",
                                             x_label = "Views",
                                             y_label = "Count") {
  required_cols <- c(views_col, fill_col)
  if (isTRUE(facet)) {
    required_cols <- c(required_cols, facet_col)
  }
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[views_col]], fill = .data[[fill_col]])) +
    ggplot2::geom_histogram(position = "dodge", bins = bins) +
    ggplot2::scale_x_log10(
      breaks = log_breaks,
      labels = scales::label_comma()
    ) +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )

  if (isTRUE(facet)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)))
  }

  p
}
