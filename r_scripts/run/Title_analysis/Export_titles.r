library(dplyr)
library(here)
library(purrr)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) {
    return(default)
  }
  pos <- idx[[1]] + 1L
  if (pos > length(args)) {
    return(default)
  }
  args[[pos]]
}

out_csv <- arg_value("--out", here("notes", "titles.csv"))

source("r_scripts/lib/utils/staging_root.R")
list.files(here("r_scripts", "lib", "utils"), full.names = TRUE) %>%
  walk(source)
list.files(here("r_scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
source(here::here("r_scripts", "lib", "clean_data", "CleanData.R"))

root <- get_staging_root()
talent_root <- select_talent("all", root = root)

titles <- lapply(talent_root, function(talent_path) {
  talent_name <- basename(talent_path)
  files <- TalentFiles(talent_path)

  analytics <- video_analytics_prep(files, talent = talent_name)
  analytics %>%
    select(`Video ID`, Title, `Content Type`, `Published At`) %>%
    distinct() %>%
    filter(!is.na(Title), nzchar(trimws(as.character(Title)))) %>%
    mutate(talent = talent_name, .before = `Video ID`)
}) %>%
  bind_rows() %>%
  distinct(talent, `Video ID`, Title, `Content Type`, `Published At`) %>%
  mutate(talent = clean_talent_name(talent, underscores = TRUE))

write.csv(titles, out_csv, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
cat("Exported", nrow(titles), "title rows to", normalizePath(out_csv, winslash = "/", mustWork = FALSE), "\n")
