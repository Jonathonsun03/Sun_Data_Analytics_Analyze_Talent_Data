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

root <- get_staging_root()
talent_root <- list.files(root, full.names = TRUE)

Talent <- c("Avaritia")

talent_root <- select_talent(Talent)
files <- TalentFiles(talent_root)



analytics <- video_analytics_prep(files, talent = Talent)
monetary  <- video_monetary_prep(files, talent = Talent)
demo      <- video_demographic_prep(files, talent = Talent)
geo       <- video_geographic_prep(files, talent = Talent)
