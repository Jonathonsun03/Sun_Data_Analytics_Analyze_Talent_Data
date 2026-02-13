library(tidyverse)
library(dplyr)
library(here)
library(purrr)

source("scripts/lib/utils/staging_root.R")

list.files(here("scripts","lib","plots","report","bundle_A"), full.names = TRUE) %>%
    walk(source)
list.files(here("scripts","lib","utils"), full.names = TRUE) %>%
    walk(source)
list.files(here("scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
list.files(here("scripts", "lib", "report_tables"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
source(here("scripts","lib","plots","PlottingThemes.R"))
source(here::here("scripts","lib","clean_data","CleanData.R"))

root <- get_staging_root()
talent_root <- select_talent("all", root = root)

titles <- lapply(talent_root, function(talent_path) {
  Talent <- basename(talent_path)
  files <- TalentFiles(talent_path)

  analytics <- video_analytics_prep(files, talent = Talent)
  analytics %>%
    select(video_id, title) %>%
    distinct() %>%
    filter(!is.na(title)) %>%
    mutate(talent = Talent)
}) %>%
  bind_rows() %>%
  distinct(talent, video_id, title)
