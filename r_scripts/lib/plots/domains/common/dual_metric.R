# Shared dual-metric preparation and plotting helpers.

bundle_a_metric_summary <- function(
  df,
  group_col,
  metric_col,
  id_col = "Video ID",
  group_name = "group",
  metric_name = "metric"
) {
  if (!group_col %in% names(df)) {
    stop("Grouping column not found: ", group_col)
  }
  if (!metric_col %in% names(df)) {
    stop("Metric column not found: ", metric_col)
  }
  if (!id_col %in% names(df)) {
    stop("ID column not found: ", id_col)
  }

  out <- df %>%
    dplyr::mutate(
      .group = as.character(.data[[group_col]]),
      .metric = suppressWarnings(as.numeric(.data[[metric_col]]))
    ) %>%
    dplyr::filter(!is.na(.group), nzchar(trimws(.group))) %>%
    dplyr::group_by(.data$.group) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data[[id_col]]),
      Total = sum(.data$.metric, na.rm = TRUE),
      AveragePerVideo = mean(.data$.metric, na.rm = TRUE),
      MedianPerVideo = stats::median(.data$.metric, na.rm = TRUE),
      .groups = "drop"
    )

  names(out)[names(out) == ".group"] <- group_name
  names(out)[names(out) == "Total"] <- paste0("Total", metric_name)
  names(out)[names(out) == "AveragePerVideo"] <- paste0("Average", metric_name, "PerVideo")
  names(out)[names(out) == "MedianPerVideo"] <- paste0("Median", metric_name, "PerVideo")
  out
}

bundle_a_views_revenue_join <- function(views_df, revenue_df, by_col) {
  out <- dplyr::full_join(views_df, revenue_df, by = by_col) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "VideoCount.x", "VideoCount.y",
          "TotalViews", "TotalRevenue",
          "AverageViewsPerVideo", "AverageRevenuePerVideo",
          "MedianViewsPerVideo", "MedianRevenuePerVideo"
        )),
        ~ tidyr::replace_na(.x, 0)
      ),
      VideoCountViews = dplyr::coalesce(.data$VideoCount.x, 0),
      VideoCountRevenue = dplyr::coalesce(.data$VideoCount.y, 0)
    ) %>%
    dplyr::select(-dplyr::any_of(c("VideoCount.x", "VideoCount.y")))

  if ("TotalViews" %in% names(out)) {
    tv <- sum(out$TotalViews, na.rm = TRUE)
    out <- out %>%
      dplyr::mutate(
        ShareViews = if (is.finite(tv) && tv > 0) .data$TotalViews / tv else NA_real_
      )
  }
  if ("TotalRevenue" %in% names(out)) {
    tr <- sum(out$TotalRevenue, na.rm = TRUE)
    out <- out %>%
      dplyr::mutate(
        ShareRevenue = if (is.finite(tr) && tr > 0) .data$TotalRevenue / tr else NA_real_
      )
  }

  out
}

bundle_a_dual_metric_plot <- function(
  summary_df,
  group_col,
  talent,
  title,
  subtitle = NULL,
  value_col_views = "TotalViews",
  value_col_revenue = "TotalRevenue",
  y_label_views = "Views",
  y_label_revenue = "Revenue",
  y_axis_label = NULL,
  as_share = FALSE,
  rotate_x = c("auto", "none", "45")
) {
  rotate_x <- match.arg(rotate_x)

  if (!group_col %in% names(summary_df)) {
    stop("Grouping column not found in summary_df: ", group_col)
  }

  views_col <- if (isTRUE(as_share)) "ShareViews" else value_col_views
  rev_col <- if (isTRUE(as_share)) "ShareRevenue" else value_col_revenue
  if (!all(c(views_col, rev_col) %in% names(summary_df))) {
    stop("Summary data missing required view/revenue columns.")
  }

  if (nrow(summary_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          geom = "text",
          x = 1,
          y = 1,
          label = "No data available for current filters."
        ) +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = bundle_a_wrap_text(title, width = 58),
          subtitle = bundle_a_talent_subtitle(talent, subtitle),
          x = "",
          y = if (isTRUE(as_share)) "Share of total" else if (!is.null(y_axis_label)) y_axis_label else "Total"
        )
    )
  }

  group_vals <- summary_df[[group_col]]
  group_levels <- if (is.factor(group_vals)) {
    levels(group_vals)
  } else {
    unique(as.character(group_vals))
  }

  plot_df <- summary_df %>%
    dplyr::transmute(
      .group = factor(as.character(.data[[group_col]]), levels = group_levels, ordered = TRUE),
      .views = .data[[views_col]],
      .revenue = .data[[rev_col]]
    ) %>%
    tidyr::pivot_longer(
      cols = c(".views", ".revenue"),
      names_to = "metric_key",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric = dplyr::case_when(
        .data$metric_key == ".views" ~ y_label_views,
        .data$metric_key == ".revenue" ~ y_label_revenue,
        TRUE ~ .data$metric_key
      )
    )

  group_labels <- unique(as.character(plot_df$.group))
  max_label_chars <- if (length(group_labels) > 0) max(nchar(group_labels), na.rm = TRUE) else 0
  should_rotate <- if (rotate_x == "45") {
    TRUE
  } else if (rotate_x == "none") {
    FALSE
  } else {
    length(group_labels) > 6 || max_label_chars > 12
  }

  y_scale <- if (isTRUE(as_share)) scales::label_percent(accuracy = 1) else scales::label_comma()
  y_lab <- if (isTRUE(as_share)) {
    "Share of total"
  } else if (!is.null(y_axis_label)) {
    y_axis_label
  } else {
    "Total"
  }

  p <- plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.group, y = .data$value, fill = .data$.group)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 1) +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_y_continuous(labels = y_scale) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_a_wrap_text(title, width = 58),
      subtitle = bundle_a_talent_subtitle(talent, subtitle),
      x = "",
      y = y_lab
    )

  if (isTRUE(should_rotate)) {
    p <- p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
      )
  }

  p
}
