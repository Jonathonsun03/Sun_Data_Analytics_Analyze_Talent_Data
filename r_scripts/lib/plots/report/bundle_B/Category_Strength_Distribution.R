## Bundle B: Distribution-based category strength prep, plots, and table helpers.
## Scope: content-type position in full distribution + percentile-based summaries.

bundle_b_content_position_distribution_prep <- function(
  analytics_df,
  monetary_df,
  id_col = "Video ID",
  content_col = "Content Type",
  views_col = "views",
  engagement_col = "averageViewPercentage",
  revenue_col = "Estimated Revenue"
) {
  required_analytics <- c(id_col, views_col, engagement_col)
  if (!all(required_analytics %in% names(analytics_df))) {
    stop("analytics_df must include: ", paste(required_analytics, collapse = ", "))
  }
  if (!id_col %in% names(monetary_df) || !revenue_col %in% names(monetary_df)) {
    stop("monetary_df must include: ", id_col, ", ", revenue_col)
  }

  safe_median <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    x_num <- x_num[is.finite(x_num)]
    if (length(x_num) == 0) return(NA_real_)
    stats::median(x_num, na.rm = TRUE)
  }

  analytics_video <- analytics_df %>%
    dplyr::transmute(
      .video_id = as.character(.data[[id_col]]),
      .content = if (content_col %in% names(analytics_df)) as.character(.data[[content_col]]) else NA_character_,
      .views = suppressWarnings(as.numeric(.data[[views_col]])),
      .eng_pct = suppressWarnings(as.numeric(.data[[engagement_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$.video_id), nzchar(.data$.video_id)) %>%
    dplyr::mutate(
      .content = trimws(.data$.content),
      .content = dplyr::if_else(is.na(.data$.content) | !nzchar(.data$.content), "(unclassified)", .data$.content)
    ) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(
      Content_Type = dplyr::first(.data$.content),
      Views = safe_median(.data$.views),
      EngagementPct = safe_median(.data$.eng_pct),
      .groups = "drop"
    )

  revenue_by_video <- monetary_df %>%
    dplyr::transmute(
      .video_id = as.character(.data[[id_col]]),
      .revenue = suppressWarnings(as.numeric(.data[[revenue_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$.video_id), nzchar(.data$.video_id)) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(Revenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop")

  video_df <- analytics_video %>%
    dplyr::left_join(revenue_by_video, by = ".video_id") %>%
    dplyr::mutate(Revenue = tidyr::replace_na(.data$Revenue, 0))

  metric_levels <- c("Views per video", "Revenue per video ($)", "Engagement rate (%)")
  metric_long <- dplyr::bind_rows(
    video_df %>%
      dplyr::transmute(Content_Type = .data$Content_Type, Metric = metric_levels[[1]], Value = .data$Views),
    video_df %>%
      dplyr::transmute(Content_Type = .data$Content_Type, Metric = metric_levels[[2]], Value = .data$Revenue),
    video_df %>%
      dplyr::transmute(Content_Type = .data$Content_Type, Metric = metric_levels[[3]], Value = .data$EngagementPct)
  ) %>%
    dplyr::filter(is.finite(.data$Value)) %>%
    dplyr::filter(
      !(.data$Metric %in% metric_levels[1:2] & .data$Value <= 0)
    ) %>%
    dplyr::mutate(Metric = factor(.data$Metric, levels = metric_levels, ordered = TRUE))

  if (nrow(metric_long) == 0) {
    empty_summary <- tibble::tibble(
      Metric = factor(character(), levels = metric_levels, ordered = TRUE),
      Content_Type = character(),
      VideoCount = integer(),
      MedianValue = numeric(),
      MeanValue = numeric(),
      Q25 = numeric(),
      Q75 = numeric(),
      MedianPercentile = numeric(),
      Performance_Band = character()
    )
    return(list(metric_long = metric_long, summary = empty_summary))
  }

  summary_df <- metric_long %>%
    dplyr::group_by(.data$Metric, .data$Content_Type) %>%
    dplyr::summarize(
      VideoCount = dplyr::n(),
      MedianValue = stats::median(.data$Value, na.rm = TRUE),
      MeanValue = mean(.data$Value, na.rm = TRUE),
      Q25 = stats::quantile(.data$Value, probs = 0.25, na.rm = TRUE, names = FALSE),
      Q75 = stats::quantile(.data$Value, probs = 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      metric_long %>%
        dplyr::group_by(.data$Metric) %>%
        dplyr::summarize(.metric_values = list(.data$Value), .groups = "drop"),
      by = "Metric"
    ) %>%
    dplyr::mutate(
      MedianPercentile = purrr::map2_dbl(
        .data$.metric_values,
        .data$MedianValue,
        ~ {
          if (length(.x) == 0 || !is.finite(.y)) return(NA_real_)
          as.numeric(stats::ecdf(.x)(.y))
        }
      )
    ) %>%
    dplyr::select(-dplyr::all_of(".metric_values")) %>%
    dplyr::group_by(.data$Metric) %>%
    dplyr::mutate(
      .n_content = dplyr::n_distinct(.data$Content_Type),
      .high_cut = stats::quantile(.data$MedianPercentile, probs = 0.67, na.rm = TRUE, names = FALSE),
      .low_cut = stats::quantile(.data$MedianPercentile, probs = 0.33, na.rm = TRUE, names = FALSE),
      Performance_Band = dplyr::case_when(
        .data$.n_content < 3 ~ "Middle",
        .data$MedianPercentile >= .data$.high_cut ~ "Strength",
        .data$MedianPercentile <= .data$.low_cut ~ "Weakness / Improve",
        TRUE ~ "Middle"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::any_of(c(".n_content", ".high_cut", ".low_cut"))) %>%
    dplyr::arrange(.data$Metric, dplyr::desc(.data$MedianPercentile), .data$Content_Type)

  list(
    metric_long = metric_long,
    summary = summary_df
  )
}

bundle_b_content_position_distribution_plot <- function(position_data, talent) {
  if (!is.list(position_data) || !all(c("metric_long", "summary") %in% names(position_data))) {
    stop("position_data must be the output of bundle_b_content_position_distribution_prep().")
  }

  metric_long <- position_data$metric_long
  summary_df <- position_data$summary

  if (nrow(metric_long) == 0 || nrow(summary_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No content distribution data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(title = bundle_b_title_text(talent, "Content Position in Overall Distribution"))
    )
  }

  summary_df <- summary_df %>%
    dplyr::mutate(
      Content_Type = factor(
        .data$Content_Type,
        levels = unique(.data$Content_Type[order(.data$Metric, dplyr::desc(.data$MedianPercentile))])
      )
    )

  metric_long %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Value)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(count)),
      bins = 35,
      fill = sun_data_brand_colors()[["cloud"]],
      color = sun_data_brand_colors()[["steel"]],
      linewidth = 0.4,
      alpha = 0.9
    ) +
    ggplot2::geom_vline(
      data = summary_df,
      ggplot2::aes(xintercept = .data$MedianValue, linetype = .data$Content_Type),
      linewidth = 0.6,
      color = sun_data_brand_colors()[["midnight"]],
      alpha = 0.9,
      show.legend = TRUE
    ) +
    ggplot2::facet_wrap(~Metric, scales = "free_x", ncol = 1) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(big.mark = ",", accuracy = 1)
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Content Type Position in Full Distribution"),
      subtitle = "Bars = count of videos in each value range. Vertical lines = content type medians.",
      x = "Per-video value (raw units)",
      y = "Count",
      linetype = "Content type"
    )
}

bundle_b_content_position_distribution_plotly <- function(position_data, talent, bins = 35) {
  if (!is.list(position_data) || !all(c("metric_long", "summary") %in% names(position_data))) {
    stop("position_data must be the output of bundle_b_content_position_distribution_prep().")
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive chart.")
  }

  metric_long <- position_data$metric_long
  summary_df <- position_data$summary

  if (nrow(metric_long) == 0 || nrow(summary_df) == 0) {
    return(plotly::plotly_empty(type = "scatter", mode = "markers"))
  }

  metric_levels <- if (is.factor(metric_long$Metric)) {
    levels(metric_long$Metric)[levels(metric_long$Metric) %in% as.character(metric_long$Metric)]
  } else {
    unique(as.character(metric_long$Metric))
  }

  format_metric_value <- function(metric_name, x) {
    if (!is.finite(x)) return("NA")
    if (identical(metric_name, "Revenue per video ($)")) {
      return(as.character(scales::dollar(x, accuracy = 0.01)))
    }
    if (identical(metric_name, "Engagement rate (%)")) {
      return(paste0(scales::number(x, accuracy = 0.1), "%"))
    }
    as.character(scales::comma(round(x)))
  }

  content_levels <- sort(unique(as.character(summary_df$Content_Type)))
  dash_patterns <- c("solid", "dash", "dot", "dashdot", "longdash", "longdashdot")
  dash_map <- setNames(rep(dash_patterns, length.out = length(content_levels)), content_levels)

  panel_plots <- purrr::map2(
    metric_levels,
    seq_along(metric_levels),
    function(metric_name, panel_idx) {
      panel_values <- metric_long %>%
        dplyr::filter(as.character(.data$Metric) == metric_name)
      panel_summary <- summary_df %>%
        dplyr::filter(as.character(.data$Metric) == metric_name) %>%
        dplyr::arrange(dplyr::desc(.data$MedianPercentile), .data$Content_Type)

      x_axis_cfg <- dplyr::case_when(
        identical(metric_name, "Views per video") ~ "views",
        identical(metric_name, "Revenue per video ($)") ~ "revenue",
        identical(metric_name, "Engagement rate (%)") ~ "engagement",
        TRUE ~ "default"
      )

      hist_counts <- graphics::hist(panel_values$Value, breaks = bins, plot = FALSE)$counts
      y_max <- suppressWarnings(max(hist_counts, na.rm = TRUE))
      if (!is.finite(y_max) || y_max <= 0) y_max <- 1

      panel <- plotly::plot_ly() %>%
        plotly::add_histogram(
          data = panel_values,
          x = ~Value,
          nbinsx = bins,
          name = "All videos",
          legendgroup = "all_videos",
          showlegend = panel_idx == 1,
          marker = list(
            color = "rgba(233,237,242,0.80)",
            line = list(color = "rgba(154,166,184,1)", width = 1)
          ),
          hovertemplate = paste0(
            "<b>", metric_name, "</b>",
            "<br>Value range: %{x}",
            "<br>Video count: %{y}<extra></extra>"
          )
        ) %>%
        plotly::layout(
          barmode = "overlay",
          title = list(
            text = metric_name,
            x = 0.02,
            xanchor = "left",
            y = 0.96,
            yanchor = "top",
            font = list(size = 18)
          ),
          xaxis = list(
            title = dplyr::case_when(
              x_axis_cfg == "views" ~ "Views per video",
              x_axis_cfg == "revenue" ~ "Revenue per video ($)",
              x_axis_cfg == "engagement" ~ "Engagement rate (%)",
              TRUE ~ "Per-video value"
            ),
            tickformat = ",.0f",
            tickprefix = if (x_axis_cfg == "revenue") "$" else "",
            ticksuffix = if (x_axis_cfg == "engagement") "%" else ""
          ),
          yaxis = list(
            title = "Video count",
            tickformat = ",d",
            rangemode = "tozero"
          )
        )

      if (nrow(panel_summary) > 0) {
        for (j in seq_len(nrow(panel_summary))) {
          row <- panel_summary[j, , drop = FALSE]
          ct <- as.character(row$Content_Type[[1]])
          hover_txt <- paste0(
            "<b>", ct, "</b>",
            "<br>Metric: ", metric_name,
            "<br>Median: ", format_metric_value(metric_name, row$MedianValue[[1]]),
            "<br>Q25: ", format_metric_value(metric_name, row$Q25[[1]]),
            "<br>Q75: ", format_metric_value(metric_name, row$Q75[[1]]),
            "<br>Mean: ", format_metric_value(metric_name, row$MeanValue[[1]]),
            "<br>Video count: ", scales::comma(row$VideoCount[[1]]),
            "<br>Median percentile: ", scales::percent(row$MedianPercentile[[1]], accuracy = 0.1),
            "<br>Band: ", as.character(row$Performance_Band[[1]])
          )

          panel <- panel %>%
            plotly::add_segments(
              x = row$MedianValue[[1]],
              xend = row$MedianValue[[1]],
              y = 0,
              yend = y_max * 1.05,
              name = ct,
              legendgroup = paste0("content_", ct),
              showlegend = panel_idx == 1,
              line = list(
                color = "rgba(8,22,58,1)",
                width = 1.6,
                dash = unname(dash_map[ct])
              ),
              text = hover_txt,
              hoverinfo = "text",
              inherit = FALSE
            )
        }
      }

      panel
    }
  )

  panel_metric_label <- function(metric_name) {
    dplyr::case_when(
      identical(metric_name, "Views per video") ~ "Views per video",
      identical(metric_name, "Revenue per video ($)") ~ "Revenue per video ($)",
      identical(metric_name, "Engagement rate (%)") ~ "Engagement rate (%)",
      TRUE ~ as.character(metric_name)
    )
  }

  p <- plotly::subplot(
    panel_plots,
    nrows = length(panel_plots),
    shareX = FALSE,
    shareY = FALSE,
    margin = 0.05,
    titleY = TRUE
  ) %>%
    plotly::layout(
      title = list(
        text = bundle_b_title_text(talent, "Content Type Position in Full Distribution"),
        x = 0.02,
        xanchor = "left"
      ),
      legend = list(title = list(text = "Content type"))
    )

  # Add facet-strip style bars + labels so each panel clearly states its metric.
  strip_shapes <- list()
  strip_annotations <- list()
  for (i in seq_along(metric_levels)) {
    suffix <- if (i == 1) "" else as.character(i)
    x_axis_key <- paste0("xaxis", suffix)
    y_axis_key <- paste0("yaxis", suffix)
    x_dom <- p$x$layout[[x_axis_key]]$domain
    y_dom <- p$x$layout[[y_axis_key]]$domain
    if (is.null(x_dom) || is.null(y_dom) || length(x_dom) != 2 || length(y_dom) != 2) {
      next
    }

    strip_y0 <- min(0.995, y_dom[[2]] + 0.004)
    strip_y1 <- min(0.999, strip_y0 + 0.020)
    strip_x0 <- x_dom[[1]]
    strip_x1 <- x_dom[[2]]

    strip_shapes[[length(strip_shapes) + 1]] <- list(
      type = "rect",
      xref = "paper",
      yref = "paper",
      x0 = strip_x0,
      x1 = strip_x1,
      y0 = strip_y0,
      y1 = strip_y1,
      line = list(color = "rgba(0,0,0,0)"),
      fillcolor = "rgba(239,242,247,0.95)",
      layer = "above"
    )

    strip_annotations[[length(strip_annotations) + 1]] <- list(
      x = (strip_x0 + strip_x1) / 2,
      y = (strip_y0 + strip_y1) / 2,
      xref = "paper",
      yref = "paper",
      text = paste0("<b>", panel_metric_label(metric_levels[[i]]), "</b>"),
      showarrow = FALSE,
      xanchor = "center",
      yanchor = "middle",
      font = list(size = 12, color = "rgba(33,37,41,1)")
    )
  }

  existing_shapes <- p$x$layout$shapes
  if (is.null(existing_shapes)) existing_shapes <- list()
  existing_annotations <- p$x$layout$annotations
  if (is.null(existing_annotations)) existing_annotations <- list()

  p %>%
    plotly::layout(
      shapes = c(existing_shapes, strip_shapes),
      annotations = c(existing_annotations, strip_annotations)
    ) %>%
    bundle_b_plotly_layout(margin_l = 85)
}

bundle_b_engagement_summary_distribution_plot <- function(
  plot_df,
  talent,
  metric_label = "Average View %",
  y_as_percent = TRUE
) {
  required_cols <- c(".content", ".metric")
  if (!all(required_cols %in% names(plot_df))) {
    stop("plot_df must include: ", paste(required_cols, collapse = ", "))
  }

  if (!".video_label" %in% names(plot_df)) {
    plot_df <- plot_df %>%
      dplyr::mutate(.video_label = "Untitled video")
  }

  order_tbl <- plot_df %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::summarize(.median = stats::median(.data$.metric, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$.median))

  plot_df <- plot_df %>%
    dplyr::mutate(
      .content = factor(as.character(.data$.content), levels = order_tbl$.content, ordered = TRUE),
      .metric_fmt = if (isTRUE(y_as_percent)) {
        scales::label_percent(accuracy = 0.1)(.data$.metric)
      } else {
        scales::label_comma(accuracy = 1)(.data$.metric)
      },
      .hover_text = paste0(
        "Video: ", .data$.video_label,
        "<br>Content Type: ", .data$.content,
        "<br>", metric_label, ": ", .data$.metric_fmt
      )
    )

  median_df <- plot_df %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::summarize(.median = stats::median(.data$.metric, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      .hover_text = if (isTRUE(y_as_percent)) {
        paste0(
          "Content Type: ", .data$.content,
          "<br>Median ", metric_label, ": ",
          scales::label_percent(accuracy = 0.1)(.data$.median)
        )
      } else {
        paste0(
          "Content Type: ", .data$.content,
          "<br>Median ", metric_label, ": ",
          scales::label_comma(accuracy = 1)(.data$.median)
        )
      }
    )

  p <- plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.metric, y = .data$.content, fill = .data$.content)) +
    ggplot2::geom_boxplot(
      outlier.shape = NA,
      alpha = 0.6,
      width = 0.62,
      linewidth = 0.55,
      color = sun_data_brand_colors()[["midnight"]]
    ) +
    ggplot2::geom_jitter(
      ggplot2::aes(text = .data$.hover_text),
      inherit.aes = TRUE,
      height = 0.14,
      width = 0,
      size = 1.5,
      alpha = 0.3,
      color = sun_data_brand_colors()[["midnight"]],
      stroke = 0
    ) +
    ggplot2::geom_point(
      data = median_df,
      ggplot2::aes(x = .data$.median, y = .data$.content, text = .data$.hover_text),
      inherit.aes = FALSE,
      shape = 23,
      size = 3.6,
      stroke = 0.25,
      fill = "white",
      color = sun_data_brand_colors()[["midnight"]]
    ) +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, paste0("Median ", metric_label, " by Content Type")),
      subtitle = "Box = quartiles/median. Points = individual videos.",
      x = metric_label,
      y = "Content type"
    )

  if (isTRUE(y_as_percent)) {
    p + ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1))
  } else {
    p + ggplot2::scale_x_continuous(labels = scales::label_comma())
  }
}

bundle_b_content_position_overall_summary <- function(position_data) {
  if (!is.list(position_data) || !"summary" %in% names(position_data)) {
    stop("position_data must be the output of bundle_b_content_position_distribution_prep().")
  }

  summary_df <- position_data$summary
  if (nrow(summary_df) == 0) {
    return(
      tibble::tibble(
        Content_Type = character(),
        MetricsCovered = integer(),
        ViewsMedianPercentile = numeric(),
        RevenueMedianPercentile = numeric(),
        EngagementMedianPercentile = numeric(),
        AvgMedianPercentile = numeric(),
        Performance_Band = character()
      )
    )
  }

  metric_wide <- summary_df %>%
    dplyr::mutate(
      MetricKey = dplyr::case_when(
        .data$Metric == "Views per video" ~ "ViewsMedianPercentile",
        .data$Metric == "Revenue per video ($)" ~ "RevenueMedianPercentile",
        .data$Metric == "Engagement rate (%)" ~ "EngagementMedianPercentile",
        TRUE ~ as.character(.data$Metric)
      )
    ) %>%
    dplyr::select(Content_Type, MetricKey, MedianPercentile) %>%
    tidyr::pivot_wider(names_from = "MetricKey", values_from = "MedianPercentile")

  out <- summary_df %>%
    dplyr::group_by(.data$Content_Type) %>%
    dplyr::summarize(
      MetricsCovered = dplyr::n_distinct(.data$Metric),
      AvgMedianPercentile = mean(.data$MedianPercentile, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(metric_wide, by = "Content_Type")

  if (nrow(out) < 3) {
    return(
      out %>%
        dplyr::mutate(Performance_Band = "Middle") %>%
        dplyr::arrange(dplyr::desc(.data$AvgMedianPercentile))
    )
  }

  high_cut <- stats::quantile(out$AvgMedianPercentile, probs = 0.67, na.rm = TRUE, names = FALSE)
  low_cut <- stats::quantile(out$AvgMedianPercentile, probs = 0.33, na.rm = TRUE, names = FALSE)

  out %>%
    dplyr::mutate(
      Performance_Band = dplyr::case_when(
        .data$AvgMedianPercentile >= high_cut ~ "Strength",
        .data$AvgMedianPercentile <= low_cut ~ "Weakness / Improve",
        TRUE ~ "Middle"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$AvgMedianPercentile))
}


bundle_b_content_position_metric_table <- function(position_data) {
  if (!is.list(position_data) || !"summary" %in% names(position_data)) {
    stop("position_data must be the output of bundle_b_content_position_distribution_prep().")
  }

  position_data$summary %>%
    dplyr::arrange(.data$Metric, dplyr::desc(.data$MedianPercentile), .data$Content_Type) %>%
    dplyr::select(
      .data$Metric,
      .data$Content_Type,
      .data$VideoCount,
      .data$MedianValue,
      .data$Q25,
      .data$Q75,
      .data$MedianPercentile,
      .data$Performance_Band
    )
}

bundle_b_content_position_overall_table <- function(position_data) {
  overall <- if (is.list(position_data)) {
    bundle_b_content_position_overall_summary(position_data)
  } else {
    as.data.frame(position_data)
  }

  # Some talents can be missing one metric family (for example revenue),
  # so ensure the output table keeps a stable schema with NA placeholders.
  required_numeric <- c(
    "ViewsMedianPercentile",
    "RevenueMedianPercentile",
    "EngagementMedianPercentile",
    "AvgMedianPercentile"
  )
  for (nm in required_numeric) {
    if (!nm %in% names(overall)) {
      overall[[nm]] <- NA_real_
    }
  }
  if (!"MetricsCovered" %in% names(overall)) {
    overall[["MetricsCovered"]] <- NA_integer_
  }
  if (!"Performance_Band" %in% names(overall)) {
    overall[["Performance_Band"]] <- "Middle"
  }

  overall %>%
    dplyr::arrange(dplyr::desc(.data$AvgMedianPercentile), .data$Content_Type) %>%
    dplyr::select(
      .data$Content_Type,
      .data$MetricsCovered,
      .data$ViewsMedianPercentile,
      .data$RevenueMedianPercentile,
      .data$EngagementMedianPercentile,
      .data$AvgMedianPercentile,
      .data$Performance_Band
    )
}
