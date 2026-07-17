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
