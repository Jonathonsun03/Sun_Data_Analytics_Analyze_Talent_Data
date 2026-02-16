library(here)
library(dplyr)
library(purrr)
library(readr)
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

files <- list.files(text_playback_path, full.names = TRUE, recursive = TRUE, pattern = "\\.csv$")
if (length(files) == 0) {
  stop("No files found under: ", text_playback_path)
}

titles <- basename(files) %>%
  str_remove("\\.csv$") %>%
  str_replace_all("_", " ")

streams <- lapply(files, fread)

streams[[104]] %>% 
glimpse()


View(streams[[104]])

# confirm the exact file path you're reading
files[str_detect(basename(files), "gk5fu8l5PDI")]

i <- which(str_detect(basename(files), "gk5fu8l5PDI"))[1]
df <- fread(files[i])

# verify paid rows exist
# data.table-native
# dplyr + tibble print
df %>%
  filter(source == "chat", message_type == "paid_message") %>%
  select(timecode, speaker, text, paid_amount_text, paid_amount_value, paid_currency) %>%
  as_tibble() %>%
  print(n = 50)
