# Reusable collaboration-effectiveness preparation and plots.

collaboration_effectiveness_prep <- function(
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

  a <- analytics_df %>%
    dplyr::mutate(collab_group = ifelse(derive_collab(analytics_df), "Collaborative", "Non-collaborative"))
  m <- monetary_df %>%
    dplyr::mutate(collab_group = ifelse(derive_collab(monetary_df), "Collaborative", "Non-collaborative"))

  views_sum <- bundle_a_metric_summary(
    a, group_col = "collab_group", metric_col = views_col, id_col = id_col,
    group_name = "collab_group", metric_name = "Views"
  )
  rev_sum <- bundle_a_metric_summary(
    m, group_col = "collab_group", metric_col = revenue_col, id_col = id_col,
    group_name = "collab_group", metric_name = "Revenue"
  )

  out <- bundle_a_views_revenue_join(views_sum, rev_sum, by_col = "collab_group") %>%
    dplyr::mutate(
      collab_group = factor(
        .data$collab_group,
        levels = c("Non-collaborative", "Collaborative"),
        ordered = TRUE
      )
    ) %>%
    dplyr::arrange(.data$collab_group)

  base_views <- out$AverageViewsPerVideo[out$collab_group == "Non-collaborative"]
  base_rev <- out$AverageRevenuePerVideo[out$collab_group == "Non-collaborative"]
  out %>%
    dplyr::mutate(
      AvgViewsLiftVsNonCollab = if (
        length(base_views) == 1 && is.finite(base_views) && base_views > 0
      ) {
        .data$AverageViewsPerVideo / base_views - 1
      } else {
        NA_real_
      },
      AvgRevenueLiftVsNonCollab = if (
        length(base_rev) == 1 && is.finite(base_rev) && base_rev > 0
      ) {
        .data$AverageRevenuePerVideo / base_rev - 1
      } else {
        NA_real_
      }
    )
}

collaboration_effectiveness_plot <- function(summary_df, talent, as_share = FALSE) {
  use_average <- !isTRUE(as_share)
  bundle_a_dual_metric_plot(
    summary_df = summary_df,
    group_col = "collab_group",
    talent = talent,
    title = if (isTRUE(as_share)) "Collaboration Share" else "Collaboration Effectiveness",
    subtitle = if (use_average) {
      "Average views and average revenue per video for collaborative vs non-collaborative titles."
    } else {
      "Views and revenue compared for collaborative vs non-collaborative titles."
    },
    value_col_views = if (use_average) "AverageViewsPerVideo" else "TotalViews",
    value_col_revenue = if (use_average) "AverageRevenuePerVideo" else "TotalRevenue",
    y_label_views = if (use_average) "Average views per video" else "Views",
    y_label_revenue = if (use_average) "Average revenue per video" else "Revenue",
    y_axis_label = if (use_average) "Average per video" else "Total",
    as_share = as_share
  )
}

collaboration_performance_plotly <- function(collab_summary) {
  if (is.null(collab_summary) || nrow(collab_summary) == 0 || !requireNamespace("plotly", quietly = TRUE)) {
    return(NULL)
  }

  plot_df <- collab_summary %>%
    dplyr::mutate(collab_group = as.character(.data$collab_group)) %>%
    dplyr::select(
      collab_group,
      AverageViewsPerVideo,
      AverageRevenuePerVideo
    ) %>%
    tidyr::pivot_longer(
      cols = c("AverageViewsPerVideo", "AverageRevenuePerVideo"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric = dplyr::recode(
        .data$metric,
        AverageViewsPerVideo = "Avg views/video",
        AverageRevenuePerVideo = "Avg revenue/video"
      ),
      hover_text = paste0(
        .data$collab_group,
        "<br>", .data$metric, ": ",
        dplyr::if_else(
          .data$metric == "Avg revenue/video",
          scales::dollar(.data$value, accuracy = 0.01),
          scales::comma(.data$value, accuracy = 1)
        )
      )
    )

  brand_colors <- sun_data_brand_colors()
  collab_colors <- c(
    "Non-collaborative" = brand_colors[["steel"]],
    "Collaborative" = brand_colors[["blue"]]
  )
  present_groups <- unique(plot_df$collab_group)
  missing_groups <- setdiff(present_groups, names(collab_colors))
  if (length(missing_groups) > 0) {
    fallback_colors <- sun_data_palette(length(missing_groups), variant = "brand")
    names(fallback_colors) <- missing_groups
    collab_colors <- c(collab_colors, fallback_colors)
  }
  collab_colors <- collab_colors[intersect(names(collab_colors), present_groups)]

  views_df <- plot_df %>% dplyr::filter(.data$metric == "Avg views/video")
  revenue_df <- plot_df %>% dplyr::filter(.data$metric == "Avg revenue/video")

  views_plot <- plotly::plot_ly(
    views_df,
    x = ~collab_group,
    y = ~value,
    color = ~collab_group,
    colors = collab_colors,
    text = ~hover_text,
    hoverinfo = "text",
    type = "bar",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      title = list(text = "Avg views/video", font = list(size = 12)),
      xaxis = list(title = "", automargin = TRUE, tickangle = -35),
      yaxis = list(title = "", automargin = TRUE, tickformat = ",")
    )

  revenue_plot <- plotly::plot_ly(
    revenue_df,
    x = ~collab_group,
    y = ~value,
    color = ~collab_group,
    colors = collab_colors,
    text = ~hover_text,
    hoverinfo = "text",
    type = "bar",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      title = list(text = "Avg revenue/video", font = list(size = 12)),
      xaxis = list(title = "", automargin = TRUE, tickangle = -35),
      yaxis = list(title = "", automargin = TRUE, tickprefix = "$", tickformat = ",.0f")
    )

  plotly::subplot(
    views_plot,
    revenue_plot,
    nrows = 1,
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) %>%
    plotly::layout(
      margin = list(l = 55, r = 18, b = 80, t = 30),
      autosize = TRUE
    ) %>%
    plotly::config(responsive = TRUE, displaylogo = FALSE)
}
