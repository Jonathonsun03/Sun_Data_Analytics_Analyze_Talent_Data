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
        "Strength" = "grey35",
        "Middle" = "grey60",
        "Weakness / Improve" = "grey80"
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
        "Strength" = "grey20",
        "Middle" = "grey50",
        "Weakness / Improve" = "grey75"
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
    ggplot2::geom_col(fill = "grey45", width = 0.7) +
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
    ggplot2::geom_hline(yintercept = 0, color = "grey35", linewidth = 0.3) +
    ggplot2::geom_col(fill = "grey45", width = 0.6) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Collaboration Lift vs Non-Collab"),
      subtitle = "Positive values indicate collaborative content outperforms baseline.",
      x = "",
      y = "Lift vs non-collaborative"
    )
}

bundle_b_day_of_week_lift_plot <- function(day_summary, talent) {
  required_cols <- c("day_of_week", "AverageViewsPerVideo", "AverageRevenuePerVideo")
  if (!all(required_cols %in% names(day_summary))) {
    stop("day_summary must include: ", paste(required_cols, collapse = ", "))
  }

  base_views <- mean(day_summary$AverageViewsPerVideo, na.rm = TRUE)
  base_rev <- mean(day_summary$AverageRevenuePerVideo, na.rm = TRUE)

  lift_df <- day_summary %>%
    dplyr::transmute(
      day_of_week = as.character(.data$day_of_week),
      ViewsLift = if (is.finite(base_views) && base_views > 0) {
        .data$AverageViewsPerVideo / base_views - 1
      } else {
        rep(NA_real_, dplyr::n())
      },
      RevenueLift = if (is.finite(base_rev) && base_rev > 0) {
        .data$AverageRevenuePerVideo / base_rev - 1
      } else {
        rep(NA_real_, dplyr::n())
      }
    ) %>%
    tidyr::pivot_longer(
      cols = c("ViewsLift", "RevenueLift"),
      names_to = "Metric",
      values_to = "Lift"
    ) %>%
    dplyr::mutate(
      Metric = dplyr::recode(
        .data$Metric,
        ViewsLift = "Views lift",
        RevenueLift = "Revenue lift"
      )
    )

  lift_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$day_of_week, y = .data$Lift, fill = .data$Metric)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey35", linewidth = 0.3) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.6) +
    ggplot2::scale_fill_grey(start = 0.35, end = 0.65) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Day-of-Week Lift"),
      subtitle = "Lift in average per-video performance vs overall day-of-week mean.",
      x = "",
      y = "Lift vs baseline",
      fill = "Metric"
    )
}
