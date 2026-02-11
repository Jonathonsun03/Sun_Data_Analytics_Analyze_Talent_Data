total_metric_content_type_prep <- function(df,
                                           metric_col,
                                           window_months = 2,
                                           date_col = "publish_date",
                                           max_months = NULL) {
  if (!date_col %in% names(df)) {
    stop("Date column not found. Expected: ", date_col)
  }
  if (!metric_col %in% names(df)) {
    stop("Metric column not found. Expected: ", metric_col)
  }

  plot_df <- df %>%
    mutate(
      .date = .data[[date_col]],
      .month_start = lubridate::floor_date(.date, unit = "month"),
      .window_start = .month_start - months((month(.month_start) - 1) %% window_months),
      window_label = format(.window_start, "%b %Y")
    ) %>%
    {
      if (!is.null(max_months)) {
        cutoff <- max(.$.month_start, na.rm = TRUE) %m-% months(max_months - 1)
        dplyr::filter(., .month_start >= cutoff)
      } else {
        .
      }
    } %>%
    group_by(window_label, `Content Type`, .window_start) %>%
    summarize(
      Total = sum(.data[[metric_col]], na.rm = TRUE),
      VideoCount = dplyr::n(),
      .groups = "drop"
    )

  window_levels <- plot_df %>%
    dplyr::distinct(window_label, .window_start) %>%
    arrange(.window_start) %>%
    pull(window_label)

  plot_df %>%
    mutate(window_label = factor(window_label, levels = window_levels, ordered = TRUE))
}

total_metric_content_type_plot <- function(plot_df,
                                           talent,
                                           metric_label,
                                           subtitle_text = NULL,
                                           bar_position = c("dodge", "stack"),
                                           show_counts = FALSE) {
  bar_position <- match.arg(bar_position)

  if (bar_position == "stack") {
    plot_df %>%
      ggplot(aes(
        x = window_label,
        y = Total,
        fill = `Content Type`
      )) +
      geom_col(position = "stack") +
      geom_text(
        aes(label = if (show_counts) {
          paste0(scales::comma(Total), "\n", "n=", scales::comma(VideoCount))
        } else {
          scales::comma(Total)
        }),
        position = position_stack(vjust = 1.02),
        size = 3.5,
        color = "grey20"
      ) +
      scale_fill_grey(start = 0.35, end = 0.65) +
      theme_nyt() +
      labs(
        title = paste0(talent, " - Total ", metric_label, " by Content Type"),
        subtitle = subtitle_text,
        x = "Window (start month)",
        y = paste0("Total ", metric_label)
      ) +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.12))
      )
  } else {
    plot_df %>%
      ggplot(aes(
        x = `Content Type`,
        y = Total,
        fill = `Content Type`
      )) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = if (show_counts) {
          paste0(scales::comma(Total), "\n", "n=", scales::comma(VideoCount))
        } else {
          scales::comma(Total)
        }),
        vjust = -0.4,
        size = 3.5,
        color = "grey20"
      ) +
      facet_wrap(~window_label, scales = "free_y") +
      scale_fill_grey(start = 0.35, end = 0.65) +
      guides(fill = "none") +
      theme_nyt() +
      labs(
        title = paste0(talent, " - Total ", metric_label, " by Content Type"),
        subtitle = subtitle_text,
        x = "Content type",
        y = paste0("Total ", metric_label)
      ) +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.12))
      )
  }
}

total_metric_content_type <- function(df,
                                      talent,
                                      metric_col,
                                      metric_label = metric_col,
                                      window_months = 2,
                                      date_col = "publish_date",
                                      max_months = NULL,
                                      bar_position = c("dodge", "stack"),
                                      show_counts = FALSE) {
  plot_df <- total_metric_content_type_prep(
    df,
    metric_col,
    window_months = window_months,
    date_col = date_col,
    max_months = max_months
  )
  subtitle_text <- if (!is.null(max_months)) {
    paste0("Most recent ", max_months, " months")
  } else {
    NULL
  }
  total_metric_content_type_plot(
    plot_df,
    talent,
    metric_label = metric_label,
    subtitle_text = subtitle_text,
    bar_position = bar_position,
    show_counts = show_counts
  )
}

total_metric_content_type_with_data <- function(df,
                                                talent,
                                                metric_col,
                                                metric_label = metric_col,
                                                window_months = 2,
                                                date_col = "publish_date",
                                                max_months = NULL,
                                                bar_position = c("dodge", "stack"),
                                                show_counts = FALSE) {
  plot_df <- total_metric_content_type_prep(
    df,
    metric_col,
    window_months = window_months,
    date_col = date_col,
    max_months = max_months
  )
  subtitle_text <- if (!is.null(max_months)) {
    paste0("Most recent ", max_months, " months")
  } else {
    NULL
  }
  list(
    data = plot_df,
    plot = total_metric_content_type_plot(
      plot_df,
      talent,
      metric_label = metric_label,
      subtitle_text = subtitle_text,
      bar_position = bar_position,
      show_counts = show_counts
    )
  )
}
