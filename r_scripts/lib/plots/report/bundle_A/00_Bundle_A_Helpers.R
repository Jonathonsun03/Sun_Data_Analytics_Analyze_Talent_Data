bundle_a_pick_col <- function(df, candidates, label = "column") {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop("Could not find ", label, ". Tried: ", paste(candidates, collapse = ", "))
  }
  hit[[1]]
}

bundle_a_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  out <- suppressWarnings(as.Date(x))
  if (any(!is.na(out))) {
    return(out)
  }

  out_dt <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  if (any(!is.na(out_dt))) {
    return(as.Date(out_dt))
  }

  out
}

bundle_a_percent_to_prop <- function(x) {
  vals <- suppressWarnings(readr::parse_number(as.character(x)))
  if (all(is.na(vals))) {
    return(vals)
  }

  mx <- suppressWarnings(max(vals, na.rm = TRUE))
  if (is.finite(mx) && mx > 1.5) {
    vals <- vals / 100
  }
  vals
}

bundle_a_date_range_subtitle <- function(dates) {
  d <- as.Date(dates)
  d <- d[!is.na(d)]
  if (length(d) == 0) {
    return(NULL)
  }
  paste0(
    "Date range: ",
    format(min(d), "%b %Y"),
    " to ",
    format(max(d), "%b %Y")
  )
}

bundle_a_optional_col <- function(df, col = NULL, candidates = character(), label = "column") {
  if (!is.null(col)) {
    if (!col %in% names(df)) {
      stop("Specified ", label, " not found: ", col)
    }
    return(col)
  }

  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    return(NULL)
  }
  hit[[1]]
}

bundle_a_clean_talent_name <- function(talent) {
  if (is.null(talent) || length(talent) == 0) {
    return("Unknown")
  }
  t <- trimws(as.character(talent[[1]]))
  if (!nzchar(t)) {
    return("Unknown")
  }
  t <- gsub("\\s*[\\[【].*$", "", t)
  t <- trimws(t)
  if (!nzchar(t)) "Unknown" else t
}

bundle_a_wrap_text <- function(x, width = 70) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NULL)
  }
  txt <- trimws(as.character(x[[1]]))
  if (!nzchar(txt)) {
    return(NULL)
  }
  paste(strwrap(txt, width = width), collapse = "\n")
}

bundle_a_talent_subtitle <- function(talent, detail = NULL, width = 110) {
  pieces <- c(paste0("Talent: ", bundle_a_clean_talent_name(talent)))
  if (!is.null(detail) && length(detail) > 0 && !is.na(detail) && nzchar(trimws(as.character(detail[[1]])))) {
    pieces <- c(pieces, trimws(as.character(detail[[1]])))
  }
  bundle_a_wrap_text(paste(pieces, collapse = " | "), width = width)
}
