suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
})

prep_subtitle_time <- function(df,
                               start_col = "start_sec",
                               stop_col = "stop_sec",
                               tz = "America/New_York",
                               origin = ymd_hms("1970-01-01 00:00:00", tz = tz)) {
  if (!start_col %in% names(df)) {
    stop("Missing subtitle start column: ", start_col)
  }
  if (!stop_col %in% names(df)) {
    stop("Missing subtitle stop column: ", stop_col)
  }

  df <- tibble::as_tibble(df)
  start_sec_vec <- as.numeric(df[[start_col]])
  stop_sec_vec <- as.numeric(df[[stop_col]])

  if (length(start_sec_vec) != nrow(df) || length(stop_sec_vec) != nrow(df)) {
    stop("Time columns could not be coerced to vectors matching row count.")
  }

  df %>%
    mutate(
      start_sec = start_sec_vec,
      stop_sec = stop_sec_vec,

      # Chat-compatible timeline aliases based on subtitle start time
      t_posix = origin + seconds(start_sec),
      t_hms = seconds_to_period(start_sec),
      t_floor_1s = floor_date(t_posix, "second"),
      t_floor_5s = floor_date(t_posix, "5 seconds"),
      t_floor_10s = floor_date(t_posix, "10 seconds"),
      timecode = sprintf(
        "%02d:%02d:%02d",
        floor(start_sec / 3600),
        floor((start_sec %% 3600) / 60),
        floor(start_sec %% 60)
      ),

      start_posix = origin + seconds(start_sec),
      stop_posix = origin + seconds(stop_sec),

      start_hms = seconds_to_period(start_sec),
      stop_hms = seconds_to_period(stop_sec),

      start_floor_1s = floor_date(start_posix, "second"),
      start_floor_5s = floor_date(start_posix, "5 seconds"),
      start_floor_10s = floor_date(start_posix, "10 seconds"),

      stop_floor_1s = floor_date(stop_posix, "second"),
      stop_floor_5s = floor_date(stop_posix, "5 seconds"),
      stop_floor_10s = floor_date(stop_posix, "10 seconds"),

      start_timecode = sprintf(
        "%02d:%02d:%02d",
        floor(start_sec / 3600),
        floor((start_sec %% 3600) / 60),
        floor(start_sec %% 60)
      ),
      stop_timecode = sprintf(
        "%02d:%02d:%02d",
        floor(stop_sec / 3600),
        floor((stop_sec %% 3600) / 60),
        floor(stop_sec %% 60)
      )
    )
}
