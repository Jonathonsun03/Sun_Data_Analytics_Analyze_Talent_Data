plot_bundle_e_short_window_diagnostics <- function(window_diagnostics, talent) {
  if (nrow(window_diagnostics) == 0) {
    return(bundle_e_empty_plot("Short Stability Window", bundle_e_subtitle(talent), "No eligible Shorts window diagnostics available."))
  }

  plot_df <- window_diagnostics %>%
    dplyr::filter(
      !is.na(.data$window_days),
      !is.na(.data$median_remaining_share),
      !is.na(.data$p75_remaining_share),
      .data$eligible_window
    )

  if (nrow(plot_df) == 0) {
    return(bundle_e_empty_plot("Short Stability Window", bundle_e_subtitle(talent), "No eligible Shorts windows met the minimum coverage requirement."))
  }

  plot_long <- plot_df %>%
    dplyr::transmute(
      window_days = .data$window_days,
      `Median remaining share` = .data$median_remaining_share,
      `75th percentile remaining share` = .data$p75_remaining_share
    ) %>%
    tidyr::pivot_longer(
      cols = c("Median remaining share", "75th percentile remaining share"),
      names_to = "series",
      values_to = "remaining_share"
    )

  max_window <- max(plot_df$window_days, na.rm = TRUE)
  x_breaks <- seq(0, ceiling(max_window / 10) * 10, by = 10)

  ggplot2::ggplot(
    plot_long,
    ggplot2::aes(
      x = .data$window_days,
      y = .data$remaining_share,
      color = .data$series
    )
  ) +
    ggplot2::geom_line(linewidth = 1.0) +
    ggplot2::geom_point(size = 2.2) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Short Stability Window"),
      subtitle = bundle_e_subtitle(talent, "Lower remaining-share values indicate the short is closer to a stable read by that day."),
      x = "Candidate window (days)",
      y = "Remaining share of latest observed views",
      color = NULL
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))
}

plot_bundle_e_short_window_leaders <- function(video_scores, talent, top_n = 12) {
  if (nrow(video_scores) == 0) {
    return(bundle_e_empty_plot("Top Shorts Within Window", bundle_e_subtitle(talent), "No Shorts were eligible for within-window scoring."))
  }

  label_width <- 34
  plot_df <- video_scores %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      full_title = stringr::str_squish(as.character(.data$Title)),
      title_label = stringr::str_wrap(.data$full_title, width = label_width),
      title_label = paste0(dplyr::row_number(), ". ", .data$title_label),
      title_lines = stringr::str_count(.data$title_label, "\n") + 1L,
      title_label = factor(.data$title_label, levels = rev(.data$title_label), ordered = TRUE),
      tooltip_text = paste0(
        "Title: ", .data$full_title,
        "<br>Views at selected window: ", scales::comma(.data$views_at_window),
        "<br>Selected window: ", .data$window_days, " days",
        "<br>Standout: ", dplyr::if_else(.data$standout_within_window, "Top 10%", "Other eligible Shorts")
      )
    )

  label_line_count <- sum(plot_df$title_lines, na.rm = TRUE)
  plot_height <- max(620, 260 + (label_line_count * 26))
  plot_left_margin <- max(132, 150 + (label_width * 4.2))

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$title_label,
      y = .data$views_at_window,
      fill = .data$standout_within_window,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_col(alpha = 0.9) +
    ggplot2::coord_flip() +
    theme_nyt() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8.5, lineheight = 0.82)
    ) +
    ggplot2::labs(
      title = bundle_e_title_text("Top Shorts Within Window"),
      subtitle = bundle_e_subtitle(talent, paste0("Highest-performing Shorts at the selected early-read window (", unique(plot_df$window_days)[1], " days).")),
      x = NULL,
      y = "Views at selected window",
      fill = "Standout"
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = sun_data_brand_colors()[["orange"]], `FALSE` = sun_data_brand_colors()[["blue"]]),
      labels = c(`TRUE` = "Top 10%", `FALSE` = "Other eligible Shorts")
    )

  attr(p, "bundle_e_plotly_height") <- plot_height
  attr(p, "bundle_e_plotly_margin_l") <- plot_left_margin
  p
}
