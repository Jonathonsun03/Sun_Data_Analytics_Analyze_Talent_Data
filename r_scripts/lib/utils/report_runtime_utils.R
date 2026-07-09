report_parse_optional_date <- function(x, label = "DATE") {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(as.Date(NA))
  }
  d <- suppressWarnings(as.Date(trimws(x)))
  if (is.na(d)) {
    stop(label, " must be YYYY-MM-DD when provided.")
  }
  d
}

report_resolve_data_root_from_env <- function(
  data_source_env = "TALENT_DATA_SOURCE",
  data_root_env = "TALENT_DATA_ROOT",
  default_source = "datalake"
) {
  data_source <- tolower(trimws(Sys.getenv(data_source_env, unset = default_source)))
  if (!(data_source %in% c("staging", "datalake"))) {
    stop(data_source_env, " must be one of: staging, datalake.")
  }

  data_root_override <- trimws(Sys.getenv(data_root_env, unset = ""))
  root <- if (nzchar(data_root_override)) {
    data_root_override
  } else if (identical(data_source, "datalake")) {
    get_datalake_root()
  } else {
    get_staging_root()
  }

  if (!dir.exists(root)) {
    stop("Resolved data root does not exist: ", root)
  }

  list(
    data_source = data_source,
    root = root
  )
}

report_resolve_window_from_env <- function(
  window_days_env = "TALENT_WINDOW_DAYS",
  start_date_env = "TALENT_START_DATE",
  end_date_env = "TALENT_END_DATE"
) {
  window_days <- suppressWarnings(as.integer(trimws(Sys.getenv(window_days_env, unset = ""))))
  if (is.na(window_days)) {
    window_days <- NA_integer_
  } else if (window_days <= 0) {
    stop(window_days_env, " must be a positive integer when provided.")
  }

  start_date_param <- report_parse_optional_date(Sys.getenv(start_date_env, unset = ""), start_date_env)
  end_date_param <- report_parse_optional_date(Sys.getenv(end_date_env, unset = ""), end_date_env)

  if (!is.na(start_date_param) || !is.na(end_date_param)) {
    if (is.na(start_date_param)) {
      start_date_param <- as.Date("1900-01-01")
    }
    if (is.na(end_date_param)) {
      end_date_param <- Sys.Date()
    }
    if (start_date_param > end_date_param) {
      stop(start_date_env, " cannot be after ", end_date_env, ".")
    }
    return(list(
      window_days = window_days,
      window_start_date = start_date_param,
      window_end_date = end_date_param,
      window_mode = "explicit_range"
    ))
  }

  if (!is.na(window_days)) {
    window_end_date <- Sys.Date()
    window_start_date <- window_end_date - as.integer(window_days) + 1L
    return(list(
      window_days = window_days,
      window_start_date = window_start_date,
      window_end_date = window_end_date,
      window_mode = "days_back"
    ))
  }

  list(
    window_days = NA_integer_,
    window_start_date = as.Date(NA),
    window_end_date = as.Date(NA),
    window_mode = "all_data"
  )
}
