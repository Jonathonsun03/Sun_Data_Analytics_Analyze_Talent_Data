library(tidyverse)
library(dplyr)
library(here)

source("scripts/lib/utils/staging_root.R")
source("scripts/lib/utils/datalake_root.r")
list.files(here("scripts","lib","utils"), full.names = TRUE) %>%
    walk(source)

root <- get_staging_root()
talent_root <- list.files(root, full.names = TRUE)

talent_root <- select_talent("Ter")

files <- TalentFiles(talent_root)

