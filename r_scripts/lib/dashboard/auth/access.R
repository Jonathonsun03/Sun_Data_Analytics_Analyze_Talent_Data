# Cloudflare Access entitlement helpers for the Shiny dashboard.

dashboard_request_header <- function(request, header_name) {
  request_name <- paste0(
    "HTTP_",
    gsub("-", "_", toupper(header_name), fixed = TRUE)
  )
  value <- request[[request_name]]
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return("")
  }
  trimws(as.character(value[[1]]))
}

dashboard_parse_talent_codes <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return(character())
  }
  value <- trimws(as.character(value[[1]]))
  if (!nzchar(value)) {
    return(character())
  }

  codes <- unique(trimws(strsplit(value, ",", fixed = TRUE)[[1]]))
  codes <- codes[nzchar(codes)]
  valid <- grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,79}$", codes)
  if (!all(valid)) {
    stop(
      "The dashboard received an invalid talent entitlement header.",
      call. = FALSE
    )
  }
  codes
}

dashboard_access_mode <- function() {
  mode <- tolower(trimws(Sys.getenv("DASHBOARD_AUTH_MODE", unset = "production")))
  if (!mode %in% c("production", "development")) {
    stop("DASHBOARD_AUTH_MODE must be `production` or `development`.", call. = FALSE)
  }
  mode
}

dashboard_access_context <- function(request, available_talent_codes) {
  verified_email <- dashboard_request_header(request, "X-SDA-Verified-Email")
  allowed_codes <- dashboard_parse_talent_codes(
    dashboard_request_header(request, "X-SDA-Allowed-Talent-Codes")
  )

  if (length(allowed_codes) == 0 && identical(dashboard_access_mode(), "development")) {
    allowed_codes <- dashboard_parse_talent_codes(
      Sys.getenv("DASHBOARD_DEV_ALLOWED_TALENT_CODES", unset = "")
    )
    verified_email <- Sys.getenv(
      "DASHBOARD_DEV_EMAIL",
      unset = "local-development"
    )
  }

  available_talent_codes <- unique(as.character(available_talent_codes))
  unknown_codes <- setdiff(allowed_codes, available_talent_codes)
  allowed_codes <- intersect(allowed_codes, available_talent_codes)

  list(
    verified_email = verified_email,
    allowed_talent_codes = allowed_codes,
    unknown_talent_codes = unknown_codes,
    authorized = nzchar(verified_email) && length(allowed_codes) > 0
  )
}

dashboard_talent_is_authorized <- function(talent_code, access_context) {
  length(talent_code) == 1 &&
    !is.na(talent_code) &&
    talent_code %in% access_context$allowed_talent_codes
}
