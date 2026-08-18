# Dashboard filter normalization and guarded evaluation helpers.

dashboard_apply_snapshot_window <- function(df, start_date = NULL, end_date = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  start_date <- dashboard_parse_optional_date(start_date, "start_date")
  end_date <- dashboard_parse_optional_date(end_date, "end_date")
  if (!is.na(start_date) && !is.na(end_date) && start_date > end_date) {
    stop("`start_date` cannot be after `end_date`.", call. = FALSE)
  }
  if (is.na(start_date) && is.na(end_date)) {
    return(df)
  }

  date_col <- bundle_a_optional_col(
    df,
    candidates = c("snapshot_date", "date", "Date", "Report Date", "report_date"),
    label = "dashboard analytics snapshot date column"
  )
  if (is.null(date_col)) {
    return(df)
  }

  out <- df %>%
    dplyr::mutate(.dashboard_snapshot_date = bundle_a_as_date(.data[[date_col]])) %>%
    dplyr::filter(!is.na(.data$.dashboard_snapshot_date))
  if (!is.na(start_date)) {
    out <- out %>% dplyr::filter(.data$.dashboard_snapshot_date >= start_date)
  }
  if (!is.na(end_date)) {
    out <- out %>% dplyr::filter(.data$.dashboard_snapshot_date <= end_date)
  }
  out %>% dplyr::select(-dplyr::all_of(".dashboard_snapshot_date"))
}

dashboard_latest_snapshot_rows <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  date_col <- bundle_a_optional_col(
    df,
    candidates = c("snapshot_date", "date", "Date", "Report Date", "report_date"),
    label = "dashboard analytics snapshot date column"
  )
  if (is.null(date_col)) {
    return(df)
  }

  snapshot_dates <- bundle_a_as_date(df[[date_col]])
  valid_dates <- snapshot_dates[!is.na(snapshot_dates)]
  if (length(valid_dates) == 0) {
    return(df[0, , drop = FALSE])
  }
  df[snapshot_dates == max(valid_dates), , drop = FALSE]
}

dashboard_canonical_content_types <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(c("live", "video", "short"))
  }
  x <- tolower(trimws(as.character(unlist(x))))
  x <- x[nzchar(x)]
  x <- dplyr::recode(x, videos = "video", shorts = "short", all = "all", .default = x)
  if ("all" %in% x) {
    return(c("live", "video", "short"))
  }
  unique(x)
}

dashboard_try <- function(expr, todo = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      if (!is.null(todo)) {
        attr(todo, "dashboard_error") <- conditionMessage(e)
      }
      todo
    }
  )
}
