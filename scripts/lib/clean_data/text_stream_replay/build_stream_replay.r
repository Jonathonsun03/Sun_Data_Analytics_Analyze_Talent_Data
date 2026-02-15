library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

build_stream_replay <- function(chat_clean, subs) {
  parse_hms_to_sec <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    out <- rep(NA_real_, length(x))
    ok <- grepl("^\\d{1,2}:\\d{2}:\\d{2}$", x)
    if (any(ok)) {
      parts <- strsplit(x[ok], ":", fixed = TRUE)
      out[ok] <- vapply(parts, function(p) {
        as.numeric(p[1]) * 3600 + as.numeric(p[2]) * 60 + as.numeric(p[3])
      }, numeric(1))
    }
    out
  }

  choose_best_chat_sec <- function(candidates, sub_min, sub_max) {
    valid <- lengths(candidates) > 0
    candidates <- candidates[valid]
    if (length(candidates) == 0) return(rep(NA_real_, nrow(chat_clean)))

    score_candidate <- function(x) {
      ok <- is.finite(x)
      if (!any(ok)) return(c(-Inf, Inf))
      if (is.finite(sub_min) && is.finite(sub_max)) {
        in_window <- x[ok] >= (sub_min - 300) & x[ok] <= (sub_max + 300)
        c(mean(in_window), min(x[ok], na.rm = TRUE))
      } else {
        c(0, min(x[ok], na.rm = TRUE))
      }
    }

    scores <- lapply(candidates, score_candidate)
    overlap <- vapply(scores, `[[`, numeric(1), 1)
    starts <- vapply(scores, `[[`, numeric(1), 2)

    best <- which(overlap == max(overlap, na.rm = TRUE))
    if (length(best) > 1) {
      best <- best[which.min(starts[best])]
    }
    chosen <- candidates[[best[1]]]

    # Guardrail for timezone-origin offsets (e.g., +5h) in epoch-like chat time.
    if (is.finite(sub_max)) {
      min_chosen <- suppressWarnings(min(chosen, na.rm = TRUE))
      if (is.finite(min_chosen) && min_chosen > (sub_max + 1800)) {
        hour_offset <- floor(min_chosen / 3600) * 3600
        chosen <- chosen - hour_offset
      }
    }

    chosen
  }

  sub_min <- suppressWarnings(min(as.numeric(subs$start_sec), na.rm = TRUE))
  sub_max <- suppressWarnings(max(as.numeric(subs$stop_sec), na.rm = TRUE))

  candidates <- list(
    time_in_seconds = if ("time_in_seconds" %in% names(chat_clean)) as.numeric(chat_clean$time_in_seconds) else numeric(0),
    timecode_hms = if ("timecode" %in% names(chat_clean)) parse_hms_to_sec(chat_clean$timecode) else numeric(0),
    timestamp = if ("timestamp" %in% names(chat_clean)) as.numeric(chat_clean$timestamp) else numeric(0),
    t_posix = if ("t_posix" %in% names(chat_clean)) as.numeric(chat_clean$t_posix) else numeric(0)
  )

  chat_sec <- choose_best_chat_sec(candidates, sub_min, sub_max)

  # Chat: choose the timeline that best matches subtitle range.
  chat_events <- chat_clean %>%
    mutate(
      video_id = video_id,
      sec = chat_sec,
      source = "chat",
      speaker = username,
      text = message,
      message_type = as.character(message_type)
    ) %>%
    select(video_id, sec, source, speaker, text, message_type)

  # Subtitles: already have start_sec
  sub_events <- subs %>%
    transmute(
      video_id = VideoID,
      sec = as.numeric(start_sec),
      source = "subtitle",
      speaker = "STREAM",
      text = Text,
      message_type = NA_character_
    )

  # Merge + clean + order
  replay <- bind_rows(sub_events, chat_events) %>%
    filter(!is.na(sec), !is.na(text), str_trim(text) != "") %>%
    arrange(video_id, sec, source) %>%
    mutate(
      sec_i = as.integer(floor(sec)),
      timecode = sprintf(
        "%02d:%02d:%02d",
        sec_i %/% 3600L,
        (sec_i %% 3600L) %/% 60L,
        sec_i %% 60L
      ),
      replay_line = case_when(
        source == "subtitle" ~ paste0("[", timecode, "] ", text),
        TRUE ~ paste0("[", timecode, "] ", speaker, ": ", text)
      )
    ) %>%
    select(-sec_i)

  replay
}
