library(here)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)

source(here("scripts", "lib", "utils", "staging_root.R"))
source(here("scripts", "lib", "utils", "datalake_root.r"))
source(here("scripts", "lib", "utils", "talent_select.R"))

list.files(here("scripts", "lib", "clean_data", "clean_chat"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)

list.files(here("scripts", "lib", "clean_data", "clean_subtitles"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)

list.files(here("scripts", "lib", "clean_data", "text_stream_replay"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)

datalake_root <- get_datalake_root()
talent <- as.character(select_talent("Avaritia", root = datalake_root))[1]

#Retrieve Chat Data

f <- list.files(file.path(talent, "Chat", "Original"), full.names=TRUE, pattern="_chat\\.csv$", recursive=TRUE)[1]

df <- data.table::fread(f)

# run your cleaners manually in order
chat_clean <- df %>%
  prep_chat_time() %>%
  select(username, user_id, message, timestamp, message_type, video_id, starts_with("t_"), timecode)

glimpse(chat_clean)

# Retreive Subtitles
f <- list.files(
  file.path(talent, "Subtitles", "Processed"),
  full.names = TRUE,
  pattern = "_subtitles\\.csv$",
  recursive = TRUE
)[1]

subs <- data.table::fread(f)
subs_clean <- normalize_subs_for_replay(subs, f)
subs_time <- prep_subtitle_time(subs_clean)
replay <- build_stream_replay(chat_clean, subs_time)

View(replay)


glimpse(chat_clean)
glimpse(subs)
glimpse(subs_clean)
glimpse(replay)

replay %>%
  dplyr::filter(video_id == chat_clean$video_id[[1]], source == "chat") %>%
  dplyr::select(timecode, speaker, text) %>%
  View()

# Full text replay for one video:
replay_text <- replay %>%
  filter(video_id == chat_clean$video_id[[1]]) %>%
  pull(replay_line) %>%
  paste(collapse = "\n")

View(replay)
cat(replay_text)
