suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(stringr)
})

prep_chat_time <- function(df,
                           tz = "America/New_York",
                           origin = ymd_hms("1970-01-01 00:00:00", tz = tz)) {

  df %>%
    mutate(
      time_in_seconds = as.numeric(time_in_seconds),

      # ---- EST anchored POSIX ----
      t_posix = origin + seconds(time_in_seconds),

      # ---- Period (human readable) ----
      t_hms = seconds_to_period(time_in_seconds),

      # ---- Bins ----
      t_floor_1s  = floor_date(t_posix, "second"),
      t_floor_5s  = floor_date(t_posix, "5 seconds"),
      t_floor_10s = floor_date(t_posix, "10 seconds"),

      # ---- HH:MM:SS label ----
      timecode = sprintf(
        "%02d:%02d:%02d",
        floor(time_in_seconds / 3600),
        floor((time_in_seconds %% 3600) / 60),
        floor(time_in_seconds %% 60)
      )
    )
}

add_chat_datetime <- function(df,
                              video_start_time,
                              tz = "America/New_York") {
  start_time <- ymd_hms(video_start_time, tz = tz)
  if (is.na(start_time)) {
    stop("`video_start_time` must be parseable by lubridate::ymd_hms().")
  }

  df %>%
    mutate(
      time_in_seconds = as.numeric(time_in_seconds),
      chat_datetime_est = start_time + seconds(time_in_seconds)
    )
}
