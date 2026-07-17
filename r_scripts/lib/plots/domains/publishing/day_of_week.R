# Reusable weekend and day-of-week preparation and plots.

weekend_vs_weekday_prep <- function(
  analytics_df,
  monetary_df,
  weekend_col = "is_weekend",
  date_col_analytics = NULL,
  date_col_monetary = NULL,
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue"
) {
  derive_weekend <- function(df, date_col = NULL) {
    if (weekend_col %in% names(df) && is.logical(df[[weekend_col]])) {
      return(df[[weekend_col]])
    }
    if (is.null(date_col)) {
      date_col <- bundle_a_pick_col(df, c("publish_date", "Published At", "date"), label = "date column")
    }
    d <- bundle_a_as_date(df[[date_col]])
    as.logical(lubridate::wday(d) %in% c(1, 7))
  }

  a <- analytics_df %>%
    dplyr::mutate(weekend_group = ifelse(derive_weekend(analytics_df, date_col_analytics), "Weekend", "Weekday"))
  m <- monetary_df %>%
    dplyr::mutate(weekend_group = ifelse(derive_weekend(monetary_df, date_col_monetary), "Weekend", "Weekday"))

  views_sum <- bundle_a_metric_summary(
    a, group_col = "weekend_group", metric_col = views_col, id_col = id_col,
    group_name = "weekend_group", metric_name = "Views"
  )
  rev_sum <- bundle_a_metric_summary(
    m, group_col = "weekend_group", metric_col = revenue_col, id_col = id_col,
    group_name = "weekend_group", metric_name = "Revenue"
  )

  bundle_a_views_revenue_join(views_sum, rev_sum, by_col = "weekend_group") %>%
    dplyr::mutate(
      weekend_group = factor(.data$weekend_group, levels = c("Weekday", "Weekend"), ordered = TRUE)
    ) %>%
    dplyr::arrange(.data$weekend_group)
}

weekend_vs_weekday_distribution_prep <- function(
  analytics_df,
  monetary_df,
  weekend_col = "is_weekend",
  date_col_analytics = NULL,
  date_col_monetary = NULL,
  id_col = "Video ID",
  title_col_analytics = NULL,
  title_col_monetary = NULL,
  views_col = "views",
  revenue_col = "Estimated Revenue"
) {
  if (!id_col %in% names(analytics_df)) {
    stop("ID column not found in analytics data: ", id_col)
  }
  if (!id_col %in% names(monetary_df)) {
    stop("ID column not found in monetary data: ", id_col)
  }
  if (!views_col %in% names(analytics_df)) {
    stop("Views column not found in analytics data: ", views_col)
  }
  if (!revenue_col %in% names(monetary_df)) {
    stop("Revenue column not found in monetary data: ", revenue_col)
  }

  derive_weekend <- function(df, date_col = NULL) {
    if (weekend_col %in% names(df) && is.logical(df[[weekend_col]])) {
      return(df[[weekend_col]])
    }
    if (is.null(date_col)) {
      date_col <- bundle_a_pick_col(df, c("publish_date", "Published At", "date"), label = "date column")
    }
    d <- bundle_a_as_date(df[[date_col]])
    as.logical(lubridate::wday(d) %in% c(1, 7))
  }

  title_col_analytics <- bundle_a_optional_col(
    analytics_df,
    col = title_col_analytics,
    candidates = c("Title", "Video Title", "title", "video_title"),
    label = "analytics title column"
  )
  title_col_monetary <- bundle_a_optional_col(
    monetary_df,
    col = title_col_monetary,
    candidates = c("Title", "Video Title", "title", "video_title"),
    label = "monetary title column"
  )

  views_plot <- analytics_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .video_title = if (!is.null(title_col_analytics)) as.character(.data[[title_col_analytics]]) else NA_character_,
      weekend_group = ifelse(derive_weekend(analytics_df, date_col_analytics), "Weekend", "Weekday"),
      metric = "Views per video",
      metric_value = suppressWarnings(as.numeric(.data[[views_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$metric_value), is.finite(.data$metric_value), !is.na(.data$weekend_group)) %>%
    dplyr::group_by(.data$.video_id, .data$.video_title, .data$weekend_group, .data$metric) %>%
    dplyr::summarize(metric_value = mean(.data$metric_value, na.rm = TRUE), .groups = "drop")

  revenue_plot <- monetary_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .video_title = if (!is.null(title_col_monetary)) as.character(.data[[title_col_monetary]]) else NA_character_,
      weekend_group = ifelse(derive_weekend(monetary_df, date_col_monetary), "Weekend", "Weekday"),
      metric = "Revenue per video",
      metric_value = suppressWarnings(as.numeric(.data[[revenue_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$metric_value), is.finite(.data$metric_value), !is.na(.data$weekend_group)) %>%
    dplyr::group_by(.data$.video_id, .data$.video_title, .data$weekend_group, .data$metric) %>%
    dplyr::summarize(metric_value = mean(.data$metric_value, na.rm = TRUE), .groups = "drop")

  plot_df <- dplyr::bind_rows(views_plot, revenue_plot) %>%
    dplyr::mutate(
      weekend_group = factor(.data$weekend_group, levels = c("Weekday", "Weekend"), ordered = TRUE),
      .video_label = dplyr::case_when(
        !is.na(.data$.video_title) & nzchar(trimws(.data$.video_title)) ~ trimws(.data$.video_title),
        !is.na(.data$.video_id) & nzchar(trimws(.data$.video_id)) ~ paste0("Video ID: ", trimws(.data$.video_id)),
        TRUE ~ "Untitled video"
      ),
      .hover_text = paste0(
        "Video: ", .data$.video_label,
        "<br>Day Type: ", .data$weekend_group,
        "<br>", .data$metric, ": ", scales::label_comma(accuracy = 1)(.data$metric_value)
      )
    )

  summary_df <- plot_df %>%
    dplyr::group_by(.data$metric, .data$weekend_group) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data$.video_id),
      Mean = mean(.data$metric_value, na.rm = TRUE),
      Median = stats::median(.data$metric_value, na.rm = TRUE),
      Q1 = stats::quantile(.data$metric_value, probs = 0.25, na.rm = TRUE),
      Q3 = stats::quantile(.data$metric_value, probs = 0.75, na.rm = TRUE),
      IQR = stats::IQR(.data$metric_value, na.rm = TRUE),
      .groups = "drop"
    )

  list(plot_data = plot_df, summary = summary_df)
}

weekend_vs_weekday_distribution_plot <- function(wk_dist, talent) {
  if (!is.list(wk_dist) || !all(c("plot_data", "summary") %in% names(wk_dist))) {
    stop("wk_dist must be a list returned by weekend_vs_weekday_distribution_prep().")
  }

  plot_df <- wk_dist$plot_data
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          geom = "text",
          x = 1,
          y = 1,
          label = "No weekend vs weekday distribution available for current filters."
        ) +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = bundle_a_wrap_text("Weekend vs Weekday Distribution", width = 58),
          subtitle = bundle_a_talent_subtitle(talent)
        )
    )
  }

  plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$weekend_group, y = .data$metric_value, fill = .data$weekend_group)) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.62) +
    ggplot2::geom_jitter(
      ggplot2::aes(text = .data$.hover_text),
      width = 0.14,
      alpha = 0.25,
      size = 1.2,
      color = sun_data_brand_colors()[["midnight"]]
    ) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 2.6,
      stroke = 0.2,
      fill = sun_data_brand_colors()[["midnight"]],
      color = "white"
    ) +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 1) +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_a_wrap_text("Weekend vs Weekday Distribution", width = 58),
      subtitle = bundle_a_talent_subtitle(
        talent,
        "Each point is a video. Box = IQR, line = median, white point = mean."
      ),
      x = "",
      y = "Per-video value"
    )
}

day_of_week_performance_prep <- function(
  analytics_df,
  monetary_df,
  date_col_analytics = NULL,
  date_col_monetary = NULL,
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue"
) {
  get_weekday <- function(df, date_col = NULL) {
    if (is.null(date_col)) {
      date_col <- bundle_a_pick_col(df, c("publish_date", "Published At", "date"), label = "date column")
    }
    d <- bundle_a_as_date(df[[date_col]])
    as.character(lubridate::wday(d, label = TRUE, abbr = TRUE, week_start = 1))
  }

  a <- analytics_df %>% dplyr::mutate(day_of_week = get_weekday(analytics_df, date_col_analytics))
  m <- monetary_df %>% dplyr::mutate(day_of_week = get_weekday(monetary_df, date_col_monetary))

  views_sum <- bundle_a_metric_summary(
    a, group_col = "day_of_week", metric_col = views_col, id_col = id_col,
    group_name = "day_of_week", metric_name = "Views"
  )
  rev_sum <- bundle_a_metric_summary(
    m, group_col = "day_of_week", metric_col = revenue_col, id_col = id_col,
    group_name = "day_of_week", metric_name = "Revenue"
  )

  day_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  bundle_a_views_revenue_join(views_sum, rev_sum, by_col = "day_of_week") %>%
    dplyr::mutate(day_of_week = factor(.data$day_of_week, levels = day_levels, ordered = TRUE)) %>%
    dplyr::arrange(.data$day_of_week)
}

day_of_week_performance_plot <- function(summary_df, talent, as_share = FALSE) {
  bundle_a_dual_metric_plot(
    summary_df = summary_df,
    group_col = "day_of_week",
    talent = talent,
    title = if (isTRUE(as_share)) "Day-of-Week Share" else "Day-of-Week Performance",
    subtitle = "Views and revenue compared by publish weekday.",
    as_share = as_share
  )
}

