# Dashboard filter normalization and guarded evaluation helpers.

dashboard_apply_publish_window <- function(df, start_date = NULL, end_date = NULL) {
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
    candidates = c("publish_date", "Published At", "date"),
    label = "dashboard publish date column"
  )
  if (is.null(date_col)) {
    return(df)
  }

  out <- df %>%
    dplyr::mutate(.dashboard_publish_date = bundle_a_as_date(.data[[date_col]])) %>%
    dplyr::filter(!is.na(.data$.dashboard_publish_date))
  if (!is.na(start_date)) {
    out <- out %>% dplyr::filter(.data$.dashboard_publish_date >= start_date)
  }
  if (!is.na(end_date)) {
    out <- out %>% dplyr::filter(.data$.dashboard_publish_date <= end_date)
  }
  out %>% dplyr::select(-dplyr::all_of(".dashboard_publish_date"))
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
