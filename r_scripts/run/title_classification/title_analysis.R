library(tidyverse)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)

df <- fread(file.path("classification", "output", "v6_only.csv"))
View(df)
codes <- df[, which(colnames(df) == "collaborative_energy"):
       which(colnames(df) == "personality_conversation")] %>%
  colnames()

df %>%
    select(talent_name, title_raw, content_type, topic, primary_reference, all_of(codes)) %>%
    View()
