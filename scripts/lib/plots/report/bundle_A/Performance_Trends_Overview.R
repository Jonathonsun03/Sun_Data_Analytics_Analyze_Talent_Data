performance_trends_over_time_prep <- function(
  analytics_df,
  monetary_df,
  analytics_date_col = NULL,
  monetary_date_col = NULL,
  views_col = "views",
  revenue_col = "Estimated Revenue",
  freq = c("month", "week")
) {
  freq <- match.arg(freq)

  if (is.null(analytics_date_col)) {
    analytics_date_col <- bundle_a_pick_col(
      analytics_df,
      c("publish_date", "Published At", "date"),
      label = "analytics date column"
    )
  }
  if (is.null(monetary_date_col)) {
    monetary_date_col <- bundle_a_pick_col(
      monetary_df,
      c("publish_date", "Published At", "date"),
      label = "monetary date column"
    )
  }
  if (!views_col %in% names(analytics_df)) {
    stop("Missing analytics views column: ", views_col)
  }
  if (!revenue_col %in% names(monetary_df)) {
    stop("Missing monetary revenue column: ", revenue_col)
  }

  unit <- if (freq == "month") "month" else "week"

  views_ts <- analytics_df %>%
    dplyr::mutate(
      .date = bundle_a_as_date(.data[[analytics_date_col]]),
      .period = lubridate::floor_date(.date, unit = unit)
    ) %>%
    dplyr::filter(!is.na(.period)) %>%
    dplyr::group_by(.period) %>%
    dplyr::summarize(value = sum(.data[[views_col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(metric = "Views")

  revenue_ts <- monetary_df %>%
    dplyr::mutate(
      .date = bundle_a_as_date(.data[[monetary_date_col]]),
      .period = lubridate::floor_date(.date, unit = unit)
    ) %>%
    dplyr::filter(!is.na(.period)) %>%
    dplyr::group_by(.period) %>%
    dplyr::summarize(value = sum(.data[[revenue_col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(metric = "Revenue")

  dplyr::bind_rows(views_ts, revenue_ts) %>%
    dplyr::arrange(.data$metric, .data$.period)
}

performance_trends_over_time_plot <- function(
  plot_df,
  talent,
  value_mode = c("raw", "index"),
  show_points = TRUE
) {
  value_mode <- match.arg(value_mode)

  if (!all(c(".period", "value", "metric") %in% names(plot_df))) {
    stop("plot_df must contain columns: .period, value, metric")
  }

  use_df <- plot_df
  y_label <- "Total"
  y_scale <- scales::label_comma()
  facet_scales <- "free_y"

  if (value_mode == "index") {
    use_df <- use_df %>%
      dplyr::group_by(.data$metric) %>%
      dplyr::arrange(.data$.period, .by_group = TRUE) %>%
      dplyr::mutate(
        .base = dplyr::first(.data$value[is.finite(.data$value) & !is.na(.data$value)]),
        value = dplyr::if_else(.data$.base > 0, 100 * .data$value / .data$.base, NA_real_)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-dplyr::any_of(".base"))
    y_label <- "Index (first period = 100)"
    y_scale <- scales::label_number(accuracy = 1)
    facet_scales <- "fixed"
  }

  p <- use_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.period, y = .data$value, color = .data$metric, group = .data$metric)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~metric, ncol = 1, scales = facet_scales) +
    scale_color_sun_data(variant = "brand") +
    ggplot2::guides(color = "none") +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::scale_y_continuous(labels = y_scale) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Performance Trends Over Time"),
      subtitle = bundle_a_date_range_subtitle(use_df$.period),
      x = "Period",
      y = y_label
    )

  if (isTRUE(show_points)) {
    p <- p + ggplot2::geom_point(size = 1.6)
  }

  p
}

performance_trends_over_time <- function(
  analytics_df,
  monetary_df,
  talent,
  analytics_date_col = NULL,
  monetary_date_col = NULL,
  views_col = "views",
  revenue_col = "Estimated Revenue",
  freq = c("month", "week"),
  value_mode = c("raw", "index"),
  show_points = TRUE
) {
  plot_df <- performance_trends_over_time_prep(
    analytics_df = analytics_df,
    monetary_df = monetary_df,
    analytics_date_col = analytics_date_col,
    monetary_date_col = monetary_date_col,
    views_col = views_col,
    revenue_col = revenue_col,
    freq = freq
  )
  performance_trends_over_time_plot(
    plot_df = plot_df,
    talent = talent,
    value_mode = value_mode,
    show_points = show_points
  )
}

performance_trends_over_time_with_data <- function(
  analytics_df,
  monetary_df,
  talent,
  analytics_date_col = NULL,
  monetary_date_col = NULL,
  views_col = "views",
  revenue_col = "Estimated Revenue",
  freq = c("month", "week"),
  value_mode = c("raw", "index"),
  show_points = TRUE
) {
  plot_df <- performance_trends_over_time_prep(
    analytics_df = analytics_df,
    monetary_df = monetary_df,
    analytics_date_col = analytics_date_col,
    monetary_date_col = monetary_date_col,
    views_col = views_col,
    revenue_col = revenue_col,
    freq = freq
  )
  list(
    data = plot_df,
    plot = performance_trends_over_time_plot(
      plot_df = plot_df,
      talent = talent,
      value_mode = value_mode,
      show_points = show_points
    )
  )
}
