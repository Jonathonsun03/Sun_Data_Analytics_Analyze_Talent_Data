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
    ggplot2::labs(
      title = paste0(talent, " - Content Priority Ranking"),
      subtitle = "Composite score combines normalized views, revenue, and median engagement.",
      x = "Content type",
      y = "Composite score",
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
    ggplot2::labs(
      title = paste0(talent, " - Content Opportunity Matrix"),
      subtitle = "X = median engagement, Y = total revenue, bubble size = total views.",
      x = "Median engagement",
      y = "Total revenue",
      size = "Total views",
      color = "Band"
    )
}

bundle_b_revenue_efficiency_plot <- function(strength_df, talent) {
  if (!all(c("Content_Type", "Total_Revenue", "Total_Views") %in% names(strength_df))) {
    stop("strength_df must include: Content_Type, Total_Revenue, Total_Views")
  }

  efficiency_df <- strength_df %>%
    dplyr::mutate(
      Revenue_Per_1K_Views = dplyr::if_else(
        .data$Total_Views > 0,
        .data$Total_Revenue / .data$Total_Views * 1000,
        NA_real_
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Revenue_Per_1K_Views))

  efficiency_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(.data$Content_Type, .data$Revenue_Per_1K_Views),
        y = .data$Revenue_Per_1K_Views
      )
    ) +
    ggplot2::geom_col(fill = sun_data_brand_colors()[["blue"]], width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(scale = 1)) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Revenue Efficiency by Content Type"),
      subtitle = "Estimated revenue generated per 1,000 views.",
      x = "Content type",
      y = "Revenue per 1,000 views"
    )
}

bundle_b_collaboration_lift_plot <- function(collab_summary, talent) {
  required_cols <- c(
    "collab_group",
    "AvgViewsLiftVsNonCollab",
    "AvgRevenueLiftVsNonCollab"
  )
  if (!all(required_cols %in% names(collab_summary))) {
    stop("collab_summary must include: ", paste(required_cols, collapse = ", "))
  }

  lift_df <- collab_summary %>%
    dplyr::filter(.data$collab_group == "Collaborative")

  if (nrow(lift_df) > 0) {
    lift_df <- tibble::tibble(
      Metric = c("Average Views Lift", "Average Revenue Lift"),
      Lift = c(lift_df$AvgViewsLiftVsNonCollab[[1]], lift_df$AvgRevenueLiftVsNonCollab[[1]])
    )
  }

  if (nrow(lift_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No collaboration lift data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = paste0(talent, " - Collaboration Lift"),
          subtitle = "Lift relative to non-collaborative content."
        )
    )
  }

  lift_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$Metric,
        y = .data$Lift
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, color = sun_data_brand_colors()[["steel"]], linewidth = 0.3) +
    ggplot2::geom_col(fill = sun_data_brand_colors()[["orange"]], width = 0.6) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Collaboration Lift vs Non-Collab"),
      subtitle = "Positive values indicate collaborative content outperforms baseline.",
      x = "",
      y = "Lift vs non-collaborative"
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
    ggplot2::labs(
      title = paste0(talent, " - Day-of-Week Deviation from Average"),
      subtitle = "Positive = above average day, negative = below average day (raw units, not z-score).",
      x = "",
      y = "Deviation from overall average per video"
    )
}

# Backward-compatible alias retained for code paths still calling the old helper name.
bundle_b_day_of_week_index_table <- function(day_summary) {
  bundle_b_day_of_week_deviation_table(day_summary)
}
