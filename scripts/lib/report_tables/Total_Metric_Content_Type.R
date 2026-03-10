assert_required_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}

add_window_columns <- function(df, date_col = "publish_date", window_months = 2) {
  df %>%
    mutate(
      .date = .data[[date_col]],
      .month_start = lubridate::floor_date(.date, unit = "month"),
      .window_start = .month_start - months((month(.month_start) - 1) %% window_months),
      .window_end = .window_start %m+% months(window_months) - lubridate::days(1),
      window_label = if (window_months > 1) {
        paste0(format(.window_start, "%b %Y"), " to ", format(.window_end, "%b %Y"))
      } else {
        format(.window_start, "%b %Y")
      }
    )
}

filter_recent_windows <- function(df, max_months = NULL) {
  if (is.null(max_months)) {
    return(df)
  }

  cutoff <- max(df$.month_start, na.rm = TRUE) %m-% months(max_months - 1)
  dplyr::filter(df, .month_start >= cutoff)
}

report_tables_wrap_title <- function(x, width = 58) {
  if (exists("bundle_a_wrap_text", mode = "function")) {
    return(bundle_a_wrap_text(x, width = width))
  }
  paste(strwrap(as.character(x), width = width), collapse = "\n")
}

report_tables_talent_subtitle <- function(talent, detail = NULL) {
  if (exists("bundle_a_talent_subtitle", mode = "function")) {
    return(bundle_a_talent_subtitle(talent, detail))
  }
  t <- trimws(as.character(talent[[1]]))
  if (!nzchar(t)) {
    t <- "Unknown"
  }
  if (is.null(detail) || !nzchar(trimws(as.character(detail[[1]])))) {
    return(paste0("Talent: ", t))
  }
  paste0("Talent: ", t, " | ", trimws(as.character(detail[[1]])))
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
  assert_required_cols(df, c(date_col, metric_col, "Content Type"))

  bar_position <- match.arg(bar_position)
  subtitle_text <- if (!is.null(max_months)) {
    paste0("Most recent ", max_months, " months")
  } else {
    NULL
  }

  plot_df <- df %>%
    add_window_columns(date_col = date_col, window_months = window_months) %>%
    filter_recent_windows(max_months = max_months) %>%
    group_by(window_label, `Content Type`, .window_start, .window_end) %>%
    summarize(
      Total = sum(.data[[metric_col]], na.rm = TRUE),
      VideoCount = dplyr::n(),
      .groups = "drop"
    )

  window_levels <- plot_df %>%
    dplyr::distinct(window_label, .window_start, .window_end) %>%
    arrange(.window_start) %>%
    pull(window_label)

  plot_df <- plot_df %>%
    mutate(window_label = factor(window_label, levels = window_levels, ordered = TRUE))

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
      scale_fill_sun_data(variant = "brand") +
      theme_nyt() +
      labs(
        title = report_tables_wrap_title(paste0("Total ", metric_label, " by Content Type"), width = 58),
        subtitle = report_tables_talent_subtitle(talent, subtitle_text),
        x = "Window (start to end month)",
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
      scale_fill_sun_data(variant = "brand") +
      guides(fill = "none") +
      theme_nyt() +
      labs(
        title = report_tables_wrap_title(paste0("Total ", metric_label, " by Content Type"), width = 58),
        subtitle = report_tables_talent_subtitle(talent, subtitle_text),
        x = "Content type",
        y = paste0("Total ", metric_label)
      ) +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.12))
      )
  }
}

revenue_stream_ratio_table <- function(df,
                                       metric_col = "Estimated Revenue",
                                       metric_label = "Revenue",
                                       video_id_col = "Video ID",
                                       date_col = "publish_date",
                                       window_months = 2,
                                       max_months = NULL) {
  assert_required_cols(df, c(date_col, metric_col, video_id_col, "Content Type"))

  df %>%
    add_window_columns(date_col = date_col, window_months = window_months) %>%
    filter_recent_windows(max_months = max_months) %>%
    group_by(window_label, `Content Type`, .window_start) %>%
    summarize(
      TotalRevenue = sum(.data[[metric_col]], na.rm = TRUE),
      StreamCount = dplyr::n_distinct(.data[[video_id_col]]),
      .groups = "drop"
    ) %>%
    mutate(
      RevenuePerStream = dplyr::if_else(StreamCount > 0, TotalRevenue / StreamCount, NA_real_),
      Metric = metric_label
    ) %>%
    rename(`Revenue Generating Stream count` = StreamCount) %>%
    arrange(.window_start, `Content Type`) %>%
    select(
      window_label,
      `Content Type`,
      Metric,
      TotalRevenue,
      `Revenue Generating Stream count`,
      RevenuePerStream
    )
}
