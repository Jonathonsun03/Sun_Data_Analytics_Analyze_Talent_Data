content_duration_metric_prep <- function(df, metric_col) {
  df %>%
    group_by(`Content Type`, publish_date) %>%
    summarize(Total = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop")
}

content_duration_metric <- function(
  df,
  talent,
  metric_col,
  metric_label = metric_col,
  span = 0.35,
  show_ci = TRUE,
  raw_linewidth = 0.55,
  smooth_linewidth = 0.9,
  smooth_alpha = 0.18,
  min_points_per_content = 2,
  facet_scales = c("fixed", "free_y"),
  drop_sparse_content = TRUE
) {
  plot_df <- content_duration_metric_prep(df, metric_col)
  content_duration_metric_plot(
    plot_df,
    talent,
    metric_label = metric_label,
    span = span,
    show_ci = show_ci,
    raw_linewidth = raw_linewidth,
    smooth_linewidth = smooth_linewidth,
    smooth_alpha = smooth_alpha,
    min_points_per_content = min_points_per_content,
    facet_scales = facet_scales,
    drop_sparse_content = drop_sparse_content
  )
}

content_duration_metric_plot <- function(
  plot_df,
  talent,
  metric_label,
  span = 0.35,
  show_ci = TRUE,
  raw_linewidth = 0.55,
  smooth_linewidth = 0.9,
  smooth_alpha = 0.18,
  min_points_per_content = 2,
  facet_scales = c("fixed", "free_y"),
  drop_sparse_content = TRUE
) {
  facet_scales <- match.arg(facet_scales)

  facet_counts <- plot_df %>%
    dplyr::group_by(`Content Type`) %>%
    dplyr::summarise(
      n_points = sum(!is.na(.data$Total)),
      .groups = "drop"
    )

  if (isTRUE(drop_sparse_content) && nrow(facet_counts) > 0) {
    keep_types <- facet_counts %>%
      dplyr::filter(.data$n_points >= min_points_per_content) %>%
      dplyr::pull(`Content Type`)

    if (length(keep_types) > 0) {
      plot_df <- plot_df %>%
        dplyr::filter(`Content Type` %in% keep_types)
    }
  }

  if (nrow(plot_df) == 0) {
    stop("No content-type trend rows available after sparse-content filtering.")
  }

  start_date <- min(plot_df$publish_date, na.rm = TRUE)
  end_date <- max(plot_df$publish_date, na.rm = TRUE)
  subtitle_text <- paste0(
    "Date range: ",
    format(start_date, "%b %Y"),
    " to ",
    format(end_date, "%b %Y")
  )

  plot_df %>%
    ggplot(aes(
      x = publish_date,
      y = Total
    )) +
    geom_line(
      aes(color = `Content Type`),
      linewidth = raw_linewidth,
      alpha = 0.6
    ) +
    geom_smooth(
      method = "loess",
      span = span,
      se = show_ci,
      color = sun_data_brand_colors()[["midnight"]],
      fill = sun_data_brand_colors()[["cloud"]],
      alpha = smooth_alpha,
      linewidth = smooth_linewidth
    ) +
    facet_wrap(~`Content Type`, ncol = 1, scales = facet_scales) +
    scale_color_sun_data(variant = "brand") +
    guides(color = "none") +
    theme_nyt() +
    theme(
      axis.title.y = element_text(angle = 90, vjust = 0.5)
    ) +
    labs(
      title = bundle_a_wrap_text(
        paste0(metric_label, " Over Time by Content Type"),
        width = 58
      ),
      subtitle = bundle_a_talent_subtitle(talent, subtitle_text),
      x = "Publish date",
      y = paste0("Total ", metric_label)
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    )
}

content_duration_metric_with_data <- function(
  df,
  talent,
  metric_col,
  metric_label = metric_col,
  span = 0.35,
  show_ci = TRUE,
  raw_linewidth = 0.55,
  smooth_linewidth = 0.9,
  smooth_alpha = 0.18,
  min_points_per_content = 2,
  facet_scales = c("fixed", "free_y"),
  drop_sparse_content = TRUE
) {
  plot_df <- content_duration_metric_prep(df, metric_col)
  facet_scales <- match.arg(facet_scales)
  list(
    data = {
      facet_counts <- plot_df %>%
        dplyr::group_by(`Content Type`) %>%
        dplyr::summarise(
          n_points = sum(!is.na(.data$Total)),
          .groups = "drop"
        )
      if (isTRUE(drop_sparse_content) && nrow(facet_counts) > 0) {
        keep_types <- facet_counts %>%
          dplyr::filter(.data$n_points >= min_points_per_content) %>%
          dplyr::pull(`Content Type`)
        if (length(keep_types) > 0) {
          plot_df <- plot_df %>%
            dplyr::filter(`Content Type` %in% keep_types)
        }
      }
      plot_df
    },
    plot = content_duration_metric_plot(
      plot_df,
      talent,
      metric_label = metric_label,
      span = span,
      show_ci = show_ci,
      raw_linewidth = raw_linewidth,
      smooth_linewidth = smooth_linewidth,
      smooth_alpha = smooth_alpha,
      min_points_per_content = min_points_per_content,
      facet_scales = facet_scales,
      drop_sparse_content = drop_sparse_content
    )
  )
}
