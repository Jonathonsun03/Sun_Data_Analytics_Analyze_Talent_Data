library(here)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)

# Helpers
source(here("scripts", "lib", "utils", "datalake_root.r"))
source(here("scripts", "lib", "utils", "talent_select.R"))

list.files(here("scripts", "lib", "clean_data", "clean_chat"), pattern = "[rR]$", full.names = TRUE) %>% walk(source)
list.files(here("scripts", "lib", "clean_data", "clean_subtitles"), pattern = "[rR]$", full.names = TRUE) %>% walk(source)
list.files(here("scripts", "lib", "clean_data", "text_stream_replay"), pattern = "[rR]$", full.names = TRUE) %>% walk(source)

# Ensure UTF-8 locale before filesystem lookups
cur <- suppressWarnings(Sys.getlocale("LC_CTYPE"))
if (!grepl("UTF-8", cur, ignore.case = TRUE)) {
  suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
}

# Resolve talent path fresh from helper
datalake_root <- get_datalake_root()
talent_path <- as.character(select_talent("Leia", root = datalake_root))
text_playback_path <- file.path(talent_path, "text_playback")

if (!file.exists(text_playback_path)) {
  stop("text_playback path not found: ", text_playback_path)
}

files <- list.files(text_playback_path, full.names = TRUE, recursive = TRUE)
if (length(files) == 0) {
  stop("No files found under: ", text_playback_path)
}

lst <- lapply(files, function(f) {
  fread(f)
})

lst <- lapply(files, function(f) {
  fread(f) %>% select(video_id, source, timecode, message_type, speaker, text, paid_amount_text, paid_amount_value, paid_currency)
})

titles <- basename(files) %>%
  str_remove("\\.csv$") %>%
  str_replace_all("_", " ")

stream_paid <- map2_dfr(files, titles, \(f, t) {
  fread(f) %>%
    transmute(
      stream = t,
      paid_amount_value = suppressWarnings(as.numeric(as.character(paid_amount_value))),
      paid_amount_text = as.character(paid_amount_text),
      paid_currency = as.character(paid_currency)
    ) %>%
    # fallback: parse from text if value is missing
    mutate(
      paid_amount_value = coalesce(
        paid_amount_value,
        parse_number(paid_amount_text)
      )
    )
})

paid_totals <- stream_paid %>%
  filter(!is.na(paid_amount_value)) %>%
  group_by(stream, paid_currency) %>%
  summarise(
    total_paid = sum(paid_amount_value, na.rm = TRUE),
    n_paid_messages = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_paid))

paid_totals
