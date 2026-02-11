clean_published_at <- function(Data){
  Data <- Data %>%
    mutate(
      publish_date  = lubridate::as_date(`Published At`),
      publish_year  = lubridate::year(`Published At`),
      publish_month = lubridate::month(`Published At`, label = TRUE, abbr = TRUE),
      publish_month_num = lubridate::month(`Published At`),
      publish_day   = lubridate::day(`Published At`),
      publish_wday       = lubridate::wday(`Published At`, label = TRUE, abbr = TRUE),
      publish_wday_num   = lubridate::wday(`Published At`),
      is_weekend         = publish_wday_num %in% c(1, 7),
      publish_hour  = lubridate::hour(`Published At`),
      publish_min   = lubridate::minute(`Published At`),
      publish_ampm  = if_else(publish_hour < 12, "AM", "PM"),
      publish_hour_sin = sin(2 * pi * publish_hour / 24),
      publish_hour_cos = cos(2 * pi * publish_hour / 24),
      publish_week   = lubridate::isoweek(`Published At`),
      publish_quarter = lubridate::quarter(`Published At`)
    )
  
  return(Data)
}

# --- helper to parse ISO8601 duration strings like "PT3H22M48S" ---
parse_iso8601_duration <- function(x) {
  # x: character vector, e.g. "PT3H22M48S"
  pattern <- "^PT(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)S)?$"
  m <- str_match(x, pattern)
  # m[,1] is full match; m[,2:4] are H/M/S
  hrs  <- suppressWarnings(as.numeric(m[, 2]))
  mins <- suppressWarnings(as.numeric(m[, 3]))
  secs <- suppressWarnings(as.numeric(m[, 4]))
  
  hrs[is.na(hrs)]   <- 0
  mins[is.na(mins)] <- 0
  secs[is.na(secs)] <- 0
  
  out <- hrs * 3600 + mins * 60 + secs
  # if pattern didn't match at all, set to NA
  out[is.na(m[, 1])] <- NA_real_
  out
}

# --- main cleaner for duration columns ---
clean_duration_cols <- function(Data) {
  Data %>%
    mutate(
      # ensure numeric seconds
      DurationSeconds = suppressWarnings(as.numeric(DurationSeconds)),
      
      # if DurationSeconds is missing but ISO is present, parse ISO
      DurationSeconds = if_else(
        is.na(DurationSeconds) & !is.na(DurationISO),
        parse_iso8601_duration(DurationISO),
        DurationSeconds
      ),
      
      # lubridate period object (good for plotting, summarising, etc.)
      duration_period = seconds_to_period(DurationSeconds),
      
      # convenience numeric columns
      duration_seconds = DurationSeconds,
      duration_minutes = DurationSeconds / 60,
      duration_hours   = DurationSeconds / 3600
    )
}
