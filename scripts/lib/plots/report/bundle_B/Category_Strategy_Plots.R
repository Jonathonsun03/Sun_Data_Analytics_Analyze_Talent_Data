## Bundle B: category strategy charts.
## Scope: priority ranking, revenue efficiency, collaboration lift, and day-of-week lift.

bundle_b_priority_rank_plot <- function(strength_df, talent) {
  if (!all(c("Content_Type", "Composite_Score", "Performance_Band") %in% names(strength_df))) {
    stop("strength_df must include: Content_Type, Composite_Score, Performance_Band")
  }

  strength_df %>%
    dplyr::mutate(Content_Type = stats::reorder(.data$Content_Type, .data$Composite_Score)) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$Content_Type,
        y = .data$Composite_Score,
        fill = .data$Performance_Band
      )
    ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(
        "Strength" = sun_data_brand_colors()[["blue"]],
        "Middle" = sun_data_brand_colors()[["steel"]],
        "Weakness / Improve" = sun_data_brand_colors()[["orange"]]
      )
    ) +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Content Priority Ranking"),
      subtitle = "Composite score combines normalized views, revenue, and median engagement.",
      x = "Content type",
      y = "Score",
      fill = "Band"
    )
}


bundle_b_strength_matrix_plot <- function(strength_df, talent) {
  required_cols <- c(
    "Content_Type",
    "Median_Engagement",
    "Total_Revenue",
    "Total_Views",
    "Performance_Band"
  )
  if (!all(required_cols %in% names(strength_df))) {
    stop(
      "strength_df must include: ",
      paste(required_cols, collapse = ", ")
    )
  }

  strength_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$Median_Engagement,
        y = .data$Total_Revenue,
        size = .data$Total_Views,
        color = .data$Performance_Band
      )
    ) +
    ggplot2::geom_point(alpha = 0.85) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$Content_Type),
      hjust = 0,
      nudge_x = 0.005,
      size = 3,
      check_overlap = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Strength" = sun_data_brand_colors()[["blue"]],
        "Middle" = sun_data_brand_colors()[["steel"]],
        "Weakness / Improve" = sun_data_brand_colors()[["orange"]]
      )
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(scale = 1)) +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Content Opportunity Matrix"),
      subtitle = "X = median engagement, Y = total revenue, bubble size = total views.",
      x = "Median engagement",
      y = "Revenue",
      size = "Total views",
      color = "Band"
    )
}

bundle_b_revenue_efficiency_distribution_prep <- function(
  analytics_df,
  monetary_df,
  id_col = "Video ID",
  content_col = "Content Type",
  views_col = "views",
  revenue_col = "Estimated Revenue"
) {
  required_analytics <- c(id_col, content_col, views_col)
  required_monetary <- c(id_col, revenue_col)
  if (!all(required_analytics %in% names(analytics_df))) {
    stop("analytics_df must include: ", paste(required_analytics, collapse = ", "))
  }
  if (!all(required_monetary %in% names(monetary_df))) {
    stop("monetary_df must include: ", paste(required_monetary, collapse = ", "))
  }

  title_col_analytics <- bundle_a_optional_col(
    analytics_df,
    candidates = c("Title", "Video Title", "title", "video_title"),
    label = "analytics title column"
  )

  analytics_video <- analytics_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .content = as.character(.data[[content_col]]),
      .views = suppressWarnings(as.numeric(.data[[views_col]])),
      .title = if (!is.null(title_col_analytics)) as.character(.data[[title_col_analytics]]) else NA_character_
    ) %>%
    dplyr::filter(!is.na(.data$.video_id), nzchar(.data$.video_id)) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(
      Content_Type = dplyr::first(.data$.content),
      Views = stats::median(.data$.views, na.rm = TRUE),
      VideoTitle = dplyr::first(.data$.title),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Content_Type = trimws(.data$Content_Type),
      Content_Type = dplyr::if_else(
        is.na(.data$Content_Type) | !nzchar(.data$Content_Type),
        "(unclassified)",
        .data$Content_Type
      )
    )

  revenue_video <- monetary_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .revenue = suppressWarnings(as.numeric(.data[[revenue_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$.video_id), nzchar(.data$.video_id), is.finite(.data$.revenue)) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(Revenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop")

  analytics_video %>%
    dplyr::left_join(revenue_video, by = ".video_id") %>%
    dplyr::mutate(
      Revenue = tidyr::replace_na(.data$Revenue, 0),
      Revenue_Per_1K_Views = dplyr::if_else(
        is.finite(.data$Views) & .data$Views > 0,
        (.data$Revenue / .data$Views) * 1000,
        NA_real_
      ),
      .video_label = dplyr::case_when(
        !is.na(.data$VideoTitle) & nzchar(trimws(.data$VideoTitle)) ~ trimws(.data$VideoTitle),
        TRUE ~ paste0("Video ID: ", .data$.video_id)
      )
    ) %>%
    dplyr::filter(is.finite(.data$Revenue_Per_1K_Views))
}

bundle_b_revenue_efficiency_plot <- function(efficiency_df, talent) {
  required_cols <- c("Content_Type", "Revenue_Per_1K_Views", ".video_label")
  if (!all(required_cols %in% names(efficiency_df))) {
    stop("efficiency_df must include: ", paste(required_cols, collapse = ", "))
  }

  if (nrow(efficiency_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No revenue efficiency data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(title = bundle_b_title_text(talent, "Revenue Efficiency by Content Type"))
    )
  }

  order_tbl <- efficiency_df %>%
    dplyr::group_by(.data$Content_Type) %>%
    dplyr::summarize(.median = stats::median(.data$Revenue_Per_1K_Views, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$.median))

  efficiency_df <- efficiency_df %>%
    dplyr::mutate(
      Content_Type = factor(.data$Content_Type, levels = order_tbl$Content_Type, ordered = TRUE),
      .hover_text = paste0(
        "Video: ", .data$.video_label,
        "<br>Content Type: ", .data$Content_Type,
        "<br>$ per 1K views: ", scales::dollar(.data$Revenue_Per_1K_Views, accuracy = 0.01)
      )
    )

  median_df <- efficiency_df %>%
    dplyr::group_by(.data$Content_Type) %>%
    dplyr::summarize(.median = stats::median(.data$Revenue_Per_1K_Views, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      .hover_text = paste0(
        "Content Type: ", .data$Content_Type,
        "<br>Median $ per 1K views: ", scales::dollar(.data$.median, accuracy = 0.01)
      )
    )

  efficiency_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$Revenue_Per_1K_Views,
        y = .data$Content_Type,
        fill = .data$Content_Type
      )
    ) +
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
      size = 1.4,
      alpha = 0.3,
      color = sun_data_brand_colors()[["midnight"]],
      stroke = 0
    ) +
    ggplot2::geom_point(
      data = median_df,
      ggplot2::aes(x = .data$.median, y = .data$Content_Type, text = .data$.hover_text),
      inherit.aes = FALSE,
      shape = 23,
      size = 3.6,
      stroke = 0.25,
      fill = "white",
      color = sun_data_brand_colors()[["midnight"]]
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_dollar(scale = 1)) +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Revenue Efficiency by Content Type"),
      subtitle = "Box = quartiles/median of per-video $ per 1K views. Points = individual videos.",
      x = "$ per 1K views",
      y = "Content type"
    )
}

bundle_b_collaboration_distribution_prep <- function(
  analytics_df,
  monetary_df,
  collab_col = "collaborative_energy",
  tags_col = "tags",
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue"
) {
  derive_collab <- function(df) {
    if (collab_col %in% names(df)) {
      raw_col <- df[[collab_col]]
      if (is.logical(raw_col)) {
        return(tidyr::replace_na(raw_col, FALSE))
      }
      if (is.numeric(raw_col) || is.integer(raw_col)) {
        return(!is.na(raw_col) & raw_col != 0)
      }
      if (is.character(raw_col) || is.factor(raw_col)) {
        txt <- tolower(trimws(as.character(raw_col)))
        return(txt %in% c("true", "t", "1", "yes", "y", "collab", "collaborative"))
      }
    }
    if (tags_col %in% names(df)) {
      tags <- tolower(as.character(df[[tags_col]]))
      return(grepl("collab|collaboration|duo|guest", tags) & !is.na(tags))
    }
    rep(FALSE, nrow(df))
  }

  title_col_analytics <- bundle_a_optional_col(
    analytics_df,
    candidates = c("Title", "Video Title", "title", "video_title"),
    label = "analytics title column"
  )
  title_col_monetary <- bundle_a_optional_col(
    monetary_df,
    candidates = c("Title", "Video Title", "title", "video_title"),
    label = "monetary title column"
  )

  views_df <- analytics_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .value = suppressWarnings(as.numeric(.data[[views_col]])),
      .group = ifelse(derive_collab(analytics_df), "Collaborative", "Non-collaborative"),
      .title = if (!is.null(title_col_analytics)) as.character(.data[[title_col_analytics]]) else NA_character_
    ) %>%
    dplyr::filter(!is.na(.data$.video_id), nzchar(.data$.video_id), is.finite(.data$.value)) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(
      Metric = "Views per video",
      collab_group = dplyr::first(.data$.group),
      Value = stats::median(.data$.value, na.rm = TRUE),
      VideoTitle = dplyr::first(.data$.title),
      .groups = "drop"
    )

  revenue_df <- monetary_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .value = suppressWarnings(as.numeric(.data[[revenue_col]])),
      .group = ifelse(derive_collab(monetary_df), "Collaborative", "Non-collaborative"),
      .title = if (!is.null(title_col_monetary)) as.character(.data[[title_col_monetary]]) else NA_character_
    ) %>%
    dplyr::filter(!is.na(.data$.video_id), nzchar(.data$.video_id), is.finite(.data$.value)) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(
      Metric = "Revenue per video ($)",
      collab_group = dplyr::first(.data$.group),
      Value = sum(.data$.value, na.rm = TRUE),
      VideoTitle = dplyr::first(.data$.title),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$Value > 0)

  plot_df <- dplyr::bind_rows(views_df, revenue_df) %>%
    dplyr::mutate(
      collab_group = factor(
        .data$collab_group,
        levels = c("Non-collaborative", "Collaborative"),
        ordered = TRUE
      ),
      Metric = factor(
        .data$Metric,
        levels = c("Views per video", "Revenue per video ($)"),
        ordered = TRUE
      ),
      .video_label = dplyr::case_when(
        !is.na(.data$VideoTitle) & nzchar(trimws(.data$VideoTitle)) ~ trimws(.data$VideoTitle),
        TRUE ~ paste0("Video ID: ", .data$.video_id)
      )
    ) %>%
    dplyr::filter(is.finite(.data$Value))

  summary_df <- plot_df %>%
    dplyr::group_by(.data$Metric, .data$collab_group) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data$.video_id),
      MeanValue = mean(.data$Value, na.rm = TRUE),
      MedianValue = stats::median(.data$Value, na.rm = TRUE),
      Q25 = stats::quantile(.data$Value, probs = 0.25, na.rm = TRUE, names = FALSE),
      Q75 = stats::quantile(.data$Value, probs = 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )

  list(plot_data = plot_df, summary = summary_df)
}

bundle_b_collaboration_delta_summary <- function(summary_df) {
  summary_df %>%
    dplyr::select(.data$Metric, .data$collab_group, .data$MeanValue) %>%
    tidyr::pivot_wider(names_from = .data$collab_group, values_from = .data$MeanValue) %>%
    dplyr::mutate(
      AbsDelta = .data$Collaborative - .data$`Non-collaborative`,
      LiftPct = dplyr::if_else(
        is.finite(.data$`Non-collaborative`) & .data$`Non-collaborative` > 0,
        .data$Collaborative / .data$`Non-collaborative` - 1,
        NA_real_
      ),
      Span = pmax(abs(.data$Collaborative), abs(.data$`Non-collaborative`), abs(.data$AbsDelta), 1, na.rm = TRUE),
      LabelAnchorY = (.data$Collaborative + .data$`Non-collaborative`) / 2,
      LabelNudge = 0.08 * .data$Span,
      LabelY = .data$LabelAnchorY + .data$LabelNudge,
      XStart = 1,
      XEnd = 2,
      LabelX = 1.52,
      DeltaLabel = dplyr::case_when(
        .data$Metric == "Revenue per video ($)" & is.finite(.data$LiftPct) ~ paste0(
          "Delta: ",
          ifelse(.data$AbsDelta > 0, "+", ""),
          scales::dollar(.data$AbsDelta, accuracy = 0.01),
          " (", scales::percent(.data$LiftPct, accuracy = 0.1), ")"
        ),
        .data$Metric == "Revenue per video ($)" ~ paste0(
          "Delta: ",
          ifelse(.data$AbsDelta > 0, "+", ""),
          scales::dollar(.data$AbsDelta, accuracy = 0.01), " (n/a)"
        ),
        is.finite(.data$LiftPct) ~ paste0(
          "Delta: ",
          ifelse(.data$AbsDelta > 0, "+", ""),
          scales::comma(round(.data$AbsDelta)),
          " (", scales::percent(.data$LiftPct, accuracy = 0.1), ")"
        ),
        TRUE ~ paste0(
          "Delta: ",
          ifelse(.data$AbsDelta > 0, "+", ""),
          scales::comma(round(.data$AbsDelta)),
          " (n/a)"
        )
      )
    ) %>%
    dplyr::arrange(.data$Metric)
}

bundle_b_add_collaboration_delta_annotations <- function(plot_obj, collab_dist) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    return(plot_obj)
  }
  if (!is.list(collab_dist) || !"summary" %in% names(collab_dist)) {
    return(plot_obj)
  }

  summary_df <- collab_dist$summary
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(plot_obj)
  }

  delta_df <- bundle_b_collaboration_delta_summary(summary_df)
  if (nrow(delta_df) == 0) {
    return(plot_obj)
  }

  ann <- vector("list", nrow(delta_df))
  for (i in seq_len(nrow(delta_df))) {
    axis_suffix <- if (i == 1) "" else as.character(i)
    ann[[i]] <- list(
      x = delta_df$LabelX[[i]],
      y = delta_df$LabelY[[i]],
      xref = paste0("x", axis_suffix),
      yref = paste0("y", axis_suffix),
      text = delta_df$DeltaLabel[[i]],
      showarrow = TRUE,
      arrowhead = 0,
      ax = 16,
      ay = -18,
      font = list(size = 11, color = sun_data_brand_colors()[["midnight"]]),
      bgcolor = "rgba(255,255,255,0.92)",
      bordercolor = sun_data_brand_colors()[["midnight"]],
      borderwidth = 0.6,
      borderpad = 2
    )
  }

  existing <- plot_obj$x$layout$annotations
  if (is.null(existing)) {
    existing <- list()
  }

  plotly::layout(plot_obj, annotations = c(existing, ann))
}

bundle_b_collaboration_lift_plot <- function(collab_dist, talent) {
  if (!is.list(collab_dist) || !all(c("plot_data", "summary") %in% names(collab_dist))) {
    stop("collab_dist must be output from bundle_b_collaboration_distribution_prep().")
  }

  plot_df <- collab_dist$plot_data
  summary_df <- collab_dist$summary

  if (nrow(plot_df) == 0 || nrow(summary_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No collaboration distribution data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = bundle_b_title_text(talent, "Collaboration Distribution vs Baseline"),
          subtitle = "Unable to compare because one group is missing."
        )
    )
  }

  delta_df <- bundle_b_collaboration_delta_summary(summary_df)

  mean_df <- summary_df %>%
    dplyr::transmute(
      Metric = .data$Metric,
      collab_group = .data$collab_group,
      XPos = dplyr::if_else(as.character(.data$collab_group) == "Non-collaborative", 1, 2),
      MeanValue = .data$MeanValue,
      HoverText = dplyr::case_when(
        .data$Metric == "Revenue per video ($)" ~ paste0(
          "Metric: ", .data$Metric,
          "<br>Group mean: ", .data$collab_group,
          "<br>Mean revenue/video: ", scales::dollar(.data$MeanValue, accuracy = 0.01)
        ),
        TRUE ~ paste0(
          "Metric: ", .data$Metric,
          "<br>Group mean: ", .data$collab_group,
          "<br>Mean views/video: ", scales::comma(round(.data$MeanValue))
        )
      )
    )

  plot_df <- plot_df %>%
    dplyr::mutate(
      XPos = dplyr::if_else(as.character(.data$collab_group) == "Non-collaborative", 1, 2),
      HoverText = dplyr::case_when(
        .data$Metric == "Revenue per video ($)" ~ paste0(
          "Metric: ", .data$Metric,
          "<br>Group: ", .data$collab_group,
          "<br>Video: ", .data$.video_label,
          "<br>Revenue/video: ", scales::dollar(.data$Value, accuracy = 0.01)
        ),
        TRUE ~ paste0(
          "Metric: ", .data$Metric,
          "<br>Group: ", .data$collab_group,
          "<br>Video: ", .data$.video_label,
          "<br>Views/video: ", scales::comma(round(.data$Value))
        )
      )
    )

  label_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_label_repel(
      data = delta_df,
      ggplot2::aes(x = .data$LabelX, y = .data$LabelAnchorY, label = .data$DeltaLabel),
      inherit.aes = FALSE,
      nudge_y = delta_df$LabelNudge,
      direction = "y",
      seed = 123,
      box.padding = 0.28,
      point.padding = 0.08,
      min.segment.length = 0,
      label.size = 0.18,
      size = 3.0,
      fontface = "bold",
      fill = "white",
      color = sun_data_brand_colors()[["midnight"]],
      segment.color = sun_data_brand_colors()[["midnight"]],
      alpha = 0.98,
      show.legend = FALSE
    )
  } else {
    ggplot2::geom_text(
      data = delta_df,
      ggplot2::aes(
        x = .data$LabelX,
        y = .data$LabelAnchorY + .data$LabelNudge,
        label = .data$DeltaLabel
      ),
      inherit.aes = FALSE,
      size = 3.2,
      fontface = "bold",
      color = sun_data_brand_colors()[["midnight"]],
      alpha = 0.98
    )
  }

  plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$XPos, y = .data$Value, fill = .data$collab_group, color = .data$collab_group, text = .data$HoverText)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = .data$collab_group),
      outlier.shape = NA,
      width = 0.58,
      alpha = 0.55,
      linewidth = 0.6
    ) +
    ggplot2::geom_jitter(width = 0.13, alpha = 0.33, size = 1.4, stroke = 0) +
    ggplot2::geom_segment(
      data = delta_df,
      ggplot2::aes(
        x = .data$XStart,
        xend = .data$XEnd,
        y = .data$`Non-collaborative`,
        yend = .data$Collaborative
      ),
      inherit.aes = FALSE,
      linewidth = 0.8,
      color = sun_data_brand_colors()[["midnight"]],
      alpha = 0.9
    ) +
    ggplot2::geom_point(
      data = mean_df,
      ggplot2::aes(x = .data$XPos, y = .data$MeanValue, text = .data$HoverText),
      inherit.aes = FALSE,
      shape = 23,
      size = 4.4,
      stroke = 0.35,
      fill = "white",
      color = sun_data_brand_colors()[["midnight"]]
    ) +
    label_layer +
    ggplot2::facet_wrap(~Metric, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 2),
      labels = c("Non-collaborative", "Collaborative"),
      limits = c(0.5, 2.5)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Non-collaborative" = sun_data_brand_colors()[["steel"]],
        "Collaborative" = sun_data_brand_colors()[["blue"]]
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Non-collaborative" = sun_data_brand_colors()[["steel"]],
        "Collaborative" = sun_data_brand_colors()[["blue"]]
      )
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(t = 8, r = 40, b = 8, l = 8)
    ) +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Collaboration Distribution vs Non-Collab Baseline"),
      subtitle = "Box = spread by group, circles = videos, diamonds = group means, connector = mean shift, center label = delta.",
      x = "",
      y = "Value",
      color = "",
      fill = ""
    )
}

bundle_b_day_of_week_deviation_table <- function(day_summary) {
  required_cols <- c("day_of_week", "AverageViewsPerVideo", "AverageRevenuePerVideo")
  if (!all(required_cols %in% names(day_summary))) {
    stop("day_summary must include: ", paste(required_cols, collapse = ", "))
  }
  day_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  day_levels_present <- day_levels[day_levels %in% as.character(day_summary$day_of_week)]
  if (length(day_levels_present) == 0) {
    day_levels_present <- unique(as.character(day_summary$day_of_week))
  }

  base_views <- mean(day_summary$AverageViewsPerVideo, na.rm = TRUE)
  base_rev <- mean(day_summary$AverageRevenuePerVideo, na.rm = TRUE)

  day_summary %>%
    dplyr::transmute(
      day_of_week = factor(
        as.character(.data$day_of_week),
        levels = day_levels_present,
        ordered = TRUE
      ),
      OverallAverageViewsPerVideo = base_views,
      OverallAverageRevenuePerVideo = base_rev,
      AverageViewsPerVideo = .data$AverageViewsPerVideo,
      AverageRevenuePerVideo = .data$AverageRevenuePerVideo,
      ViewsDeviation = if (is.finite(base_views)) {
        .data$AverageViewsPerVideo - base_views
      } else {
        rep(NA_real_, dplyr::n())
      },
      RevenueDeviation = if (is.finite(base_rev)) {
        .data$AverageRevenuePerVideo - base_rev
      } else {
        rep(NA_real_, dplyr::n())
      }
    ) %>%
    dplyr::arrange(.data$day_of_week)
}

bundle_b_day_of_week_lift_plot <- function(day_summary, talent) {
  dev_tbl <- bundle_b_day_of_week_deviation_table(day_summary)

  plot_df <- dev_tbl %>%
    tidyr::pivot_longer(
      cols = c("ViewsDeviation", "RevenueDeviation"),
      names_to = "Metric",
      values_to = "Deviation"
    ) %>%
    dplyr::mutate(
      Metric = dplyr::recode(
        .data$Metric,
        ViewsDeviation = "Views deviation",
        RevenueDeviation = "Revenue deviation"
      ),
      HoverText = dplyr::case_when(
        .data$Metric == "Views deviation" ~ paste0(
          "Day: ", .data$day_of_week,
          "<br>Average views/video: ", scales::comma(round(.data$AverageViewsPerVideo)),
          "<br>Overall views baseline: ", scales::comma(round(.data$OverallAverageViewsPerVideo)),
          "<br>Deviation: ", scales::comma(round(.data$Deviation))
        ),
        TRUE ~ paste0(
          "Day: ", .data$day_of_week,
          "<br>Average revenue/video: ", scales::dollar(.data$AverageRevenuePerVideo),
          "<br>Overall revenue baseline: ", scales::dollar(.data$OverallAverageRevenuePerVideo),
          "<br>Deviation: ", scales::dollar(.data$Deviation)
        )
      )
    )

  plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$day_of_week, y = .data$Deviation, fill = .data$Metric, text = .data$HoverText)) +
    ggplot2::geom_hline(yintercept = 0, color = sun_data_brand_colors()[["steel"]], linewidth = 0.3) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::facet_wrap(~Metric, scales = "free_y", ncol = 1) +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",", accuracy = 1)) +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Day-of-Week Deviation from Average"),
      subtitle = "Positive = above average day, negative = below average day (raw units, not z-score).",
      x = "",
      y = "Deviation"
    )
}

bundle_b_weekend_weekday_delta_summary <- function(summary_df) {
  summary_df %>%
    dplyr::select(.data$Metric, .data$weekend_group, .data$MeanValue) %>%
    tidyr::pivot_wider(names_from = .data$weekend_group, values_from = .data$MeanValue) %>%
    dplyr::mutate(
      AbsDelta = .data$Weekend - .data$Weekday,
      LiftPct = dplyr::if_else(
        is.finite(.data$Weekday) & .data$Weekday > 0,
        .data$Weekend / .data$Weekday - 1,
        NA_real_
      ),
      Span = pmax(abs(.data$Weekend), abs(.data$Weekday), abs(.data$AbsDelta), 1, na.rm = TRUE),
      LabelAnchorY = (.data$Weekend + .data$Weekday) / 2,
      LabelNudge = 0.08 * .data$Span,
      XStart = 1,
      XEnd = 2,
      LabelX = 1.52,
      DeltaLabel = dplyr::case_when(
        .data$Metric == "Revenue per video ($)" & is.finite(.data$LiftPct) ~ paste0(
          "Delta: ",
          ifelse(.data$AbsDelta > 0, "+", ""),
          scales::dollar(.data$AbsDelta, accuracy = 0.01),
          " (", scales::percent(.data$LiftPct, accuracy = 0.1), ")"
        ),
        .data$Metric == "Revenue per video ($)" ~ paste0(
          "Delta: ",
          ifelse(.data$AbsDelta > 0, "+", ""),
          scales::dollar(.data$AbsDelta, accuracy = 0.01), " (n/a)"
        ),
        is.finite(.data$LiftPct) ~ paste0(
          "Delta: ",
          ifelse(.data$AbsDelta > 0, "+", ""),
          scales::comma(round(.data$AbsDelta)),
          " (", scales::percent(.data$LiftPct, accuracy = 0.1), ")"
        ),
        TRUE ~ paste0(
          "Delta: ",
          ifelse(.data$AbsDelta > 0, "+", ""),
          scales::comma(round(.data$AbsDelta)),
          " (n/a)"
        )
      )
    )
}

bundle_b_weekend_weekday_distribution_plot <- function(wk_dist, talent) {
  if (!is.list(wk_dist) || !all(c("plot_data", "summary") %in% names(wk_dist))) {
    stop("wk_dist must be output from weekend_vs_weekday_distribution_prep().")
  }

  plot_df <- wk_dist$plot_data
  summary_raw <- wk_dist$summary

  if (nrow(plot_df) == 0 || nrow(summary_raw) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No weekend vs weekday distribution data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = bundle_b_title_text(talent, "Weekend vs Weekday Distribution"),
          subtitle = "Unable to compare because one group is missing."
        )
    )
  }

  pretty_metric <- function(x) {
    dplyr::case_when(
      x %in% c("Revenue per video", "Revenue per video ($)") ~ "Revenue per video ($)",
      x %in% c("Views per video", "View per video") ~ "Views per video",
      TRUE ~ as.character(x)
    )
  }

  plot_df <- plot_df %>%
    dplyr::mutate(
      Metric = pretty_metric(.data$metric),
      weekend_group = factor(as.character(.data$weekend_group), levels = c("Weekday", "Weekend"), ordered = TRUE),
      XPos = dplyr::if_else(as.character(.data$weekend_group) == "Weekday", 1, 2),
      HoverText = dplyr::case_when(
        .data$Metric == "Revenue per video ($)" ~ paste0(
          "Metric: ", .data$Metric,
          "<br>Day type: ", .data$weekend_group,
          "<br>Video: ", .data$.video_label,
          "<br>Revenue/video: ", scales::dollar(.data$metric_value, accuracy = 0.01)
        ),
        TRUE ~ paste0(
          "Metric: ", .data$Metric,
          "<br>Day type: ", .data$weekend_group,
          "<br>Video: ", .data$.video_label,
          "<br>Views/video: ", scales::comma(round(.data$metric_value))
        )
      )
    )

  summary_df <- summary_raw %>%
    dplyr::transmute(
      Metric = pretty_metric(.data$metric),
      weekend_group = factor(as.character(.data$weekend_group), levels = c("Weekday", "Weekend"), ordered = TRUE),
      MeanValue = .data$Mean
    )

  delta_df <- bundle_b_weekend_weekday_delta_summary(summary_df)

  mean_df <- summary_df %>%
    dplyr::transmute(
      Metric = .data$Metric,
      weekend_group = .data$weekend_group,
      XPos = dplyr::if_else(as.character(.data$weekend_group) == "Weekday", 1, 2),
      MeanValue = .data$MeanValue,
      HoverText = dplyr::case_when(
        .data$Metric == "Revenue per video ($)" ~ paste0(
          "Metric: ", .data$Metric,
          "<br>Group mean: ", .data$weekend_group,
          "<br>Mean revenue/video: ", scales::dollar(.data$MeanValue, accuracy = 0.01)
        ),
        TRUE ~ paste0(
          "Metric: ", .data$Metric,
          "<br>Group mean: ", .data$weekend_group,
          "<br>Mean views/video: ", scales::comma(round(.data$MeanValue))
        )
      )
    )

  label_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_label_repel(
      data = delta_df,
      ggplot2::aes(x = .data$LabelX, y = .data$LabelAnchorY, label = .data$DeltaLabel),
      inherit.aes = FALSE,
      nudge_y = delta_df$LabelNudge,
      direction = "y",
      seed = 123,
      box.padding = 0.28,
      point.padding = 0.08,
      min.segment.length = 0,
      label.size = 0.18,
      size = 3.0,
      fontface = "bold",
      fill = "white",
      color = sun_data_brand_colors()[["midnight"]],
      segment.color = sun_data_brand_colors()[["midnight"]],
      alpha = 0.98,
      show.legend = FALSE
    )
  } else {
    ggplot2::geom_text(
      data = delta_df,
      ggplot2::aes(
        x = .data$LabelX,
        y = .data$LabelAnchorY + .data$LabelNudge,
        label = .data$DeltaLabel
      ),
      inherit.aes = FALSE,
      size = 3.2,
      fontface = "bold",
      color = sun_data_brand_colors()[["midnight"]],
      alpha = 0.98
    )
  }

  plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$XPos, y = .data$metric_value, fill = .data$weekend_group, color = .data$weekend_group, text = .data$HoverText)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = .data$weekend_group),
      outlier.shape = NA,
      width = 0.58,
      alpha = 0.55,
      linewidth = 0.6
    ) +
    ggplot2::geom_jitter(width = 0.13, alpha = 0.33, size = 1.4, stroke = 0) +
    ggplot2::geom_segment(
      data = delta_df,
      ggplot2::aes(
        x = .data$XStart,
        xend = .data$XEnd,
        y = .data$Weekday,
        yend = .data$Weekend
      ),
      inherit.aes = FALSE,
      linewidth = 0.8,
      color = sun_data_brand_colors()[["midnight"]],
      alpha = 0.9
    ) +
    ggplot2::geom_point(
      data = mean_df,
      ggplot2::aes(x = .data$XPos, y = .data$MeanValue, text = .data$HoverText),
      inherit.aes = FALSE,
      shape = 23,
      size = 4.4,
      stroke = 0.35,
      fill = "white",
      color = sun_data_brand_colors()[["midnight"]]
    ) +
    label_layer +
    ggplot2::facet_wrap(~Metric, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 2),
      labels = c("Weekday", "Weekend"),
      limits = c(0.5, 2.5)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Weekday" = sun_data_brand_colors()[["steel"]],
        "Weekend" = sun_data_brand_colors()[["blue"]]
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Weekday" = sun_data_brand_colors()[["steel"]],
        "Weekend" = sun_data_brand_colors()[["blue"]]
      )
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(t = 8, r = 40, b = 8, l = 8)
    ) +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Weekend vs Weekday Distribution"),
      subtitle = "Box = spread by day type, circles = videos, diamonds = group means, connector = mean shift, label = weekend delta vs weekday.",
      x = "",
      y = "Value",
      color = "",
      fill = ""
    )
}

# Backward-compatible alias retained for code paths still calling the old helper name.
bundle_b_day_of_week_index_table <- function(day_summary) {
  bundle_b_day_of_week_deviation_table(day_summary)
}
