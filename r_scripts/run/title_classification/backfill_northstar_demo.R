# One-time backfill for the synthetic Northstar Story Lab demo classification export.
# This replaces existing Northstar demo rows and creates a timestamped CSV backup.

library(dplyr)
library(jsonlite)
library(readr)
library(stringr)

classification_path <- "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Title_classification/current/classification_export_current.csv"
analytics_path <- "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Northstar Story Lab Demo/raw_data/video_analytics/video_analytics_2026-03-25.csv"

classify_northstar_title <- function(title, content_type) {
  title_l <- str_to_lower(title)

  if (str_detect(title_l, "starfall|dungeon|pixel harbor|ranked run|first look|can we beat|community tries")) {
    topic <- "gaming"
    tags <- c("gameplay", "challenge", "community")
    primary_reference <- case_when(
      str_detect(title_l, "starfall") ~ "Starfall Frontier",
      str_detect(title_l, "dungeon") ~ "Dungeon Relay",
      str_detect(title_l, "pixel harbor") ~ "Pixel Harbor",
      TRUE ~ "Variety Game Night"
    )
    refs <- primary_reference
    flags <- c(interactive_entertainment = TRUE)
  } else if (str_detect(title_l, "cover|singing|starlit|midnight echo|silver carousel|summer static")) {
    topic <- "music"
    tags <- c("live_music", "cover", "performance")
    primary_reference <- "Northstar Studio Sessions"
    refs <- c("Northstar Studio Sessions")
    flags <- c(performance_artistry = TRUE)
  } else if (str_detect(title_l, "creator economy|creators|sponsor|revenue|analytics|strategy|algorithm")) {
    topic <- "creator_strategy"
    tags <- c("creator_economy", "strategy", "analytics")
    primary_reference <- "Creator Economy Lab"
    refs <- c("Creator Economy Lab")
    flags <- c(monetization = TRUE)
  } else if (str_detect(title_l, "community|member|milestone|recap")) {
    topic <- "community_updates"
    tags <- c("community", "milestone", "members")
    primary_reference <- "Northstar Community"
    refs <- c("Northstar Community")
    flags <- c(community_milestones = TRUE)
  } else if (str_detect(title_l, "behind the scenes|studio check-in|changed our approach|real story")) {
    topic <- "behind_the_scenes"
    tags <- c("studio_notes", "process", "storytelling")
    primary_reference <- "Northstar Story Lab"
    refs <- c("Northstar Story Lab")
    flags <- c(narrative_serialization = TRUE)
  } else {
    topic <- if (identical(content_type, "short")) "personality_short" else "free_chat_zatsu"
    tags <- c("commentary", "channel_update", "personality")
    primary_reference <- ""
    refs <- character()
    flags <- c(personality_conversation = TRUE)
  }

  if (identical(content_type, "short")) {
    tags <- unique(c(tags, "shorts"))
  }
  if (str_detect(title_l, "live|community tries|with")) {
    flags <- c(flags, collaborative_energy = TRUE)
  }

  all_flags <- c(
    collaborative_energy = FALSE,
    community_milestones = FALSE,
    interactive_entertainment = FALSE,
    meme_viral = FALSE,
    monetization = FALSE,
    narrative_serialization = FALSE,
    performance_artistry = FALSE,
    personality_conversation = FALSE
  )
  all_flags[names(flags)] <- flags

  list(
    topic = topic,
    tags = tags,
    primary_reference = primary_reference,
    referenced_entities = refs,
    flags = all_flags
  )
}

current <- readr::read_csv(classification_path, show_col_types = FALSE, progress = FALSE)
analytics <- readr::read_csv(analytics_path, show_col_types = FALSE, progress = FALSE)

northstar <- analytics %>%
  distinct(`Video ID`, .keep_all = TRUE) %>%
  transmute(
    video_id = as.character(`Video ID`),
    title_raw = as.character(Title),
    content_type = str_to_lower(as.character(`Content Type`)),
    published_at = format(as.POSIXct(`Published At`, tz = "UTC"), "%m-%d-%Y")
  ) %>%
  arrange(published_at, video_id)

classed <- lapply(seq_len(nrow(northstar)), function(i) {
  row <- northstar[i, ]
  cls <- classify_northstar_title(row$title_raw, row$content_type)
  classification <- c(
    list(
      topic = cls$topic,
      language = "en",
      tags = as.list(cls$tags),
      primary_reference = cls$primary_reference,
      referenced_entities = as.list(cls$referenced_entities)
    ),
    as.list(cls$flags)
  )

  tibble(
    video_id = row$video_id,
    talent_name = "Northstar Story Lab Demo",
    talent_profile = "northstar_story_lab_demo",
    model = "demo-synthetic",
    confidence = 0.95,
    title_raw = row$title_raw,
    content_type = row$content_type,
    published_at = row$published_at,
    topic = cls$topic,
    language = "en",
    tags = paste(cls$tags, collapse = ", "),
    primary_reference = cls$primary_reference,
    referenced_entities = paste(cls$referenced_entities, collapse = ", "),
    taxonomy_version = "demo_v1",
    prompt_version = "demo_v1",
    created_at = "06-15-2026 01:30:00",
    title_hash = paste0("northstar_", row$video_id),
    talent_id = "demo_northstar_story_lab",
    classification_json = as.character(jsonlite::toJSON(classification, auto_unbox = TRUE)),
    collaborative_energy = cls$flags[["collaborative_energy"]],
    community_milestones = cls$flags[["community_milestones"]],
    interactive_entertainment = cls$flags[["interactive_entertainment"]],
    meme_viral = cls$flags[["meme_viral"]],
    monetization = cls$flags[["monetization"]],
    narrative_serialization = cls$flags[["narrative_serialization"]],
    performance_artistry = cls$flags[["performance_artistry"]],
    personality_conversation = cls$flags[["personality_conversation"]]
  )
})

northstar_rows <- bind_rows(classed)

updated <- current %>%
  filter(talent_name != "Northstar Story Lab Demo") %>%
  bind_rows(northstar_rows) %>%
  select(all_of(names(current)))

backup_path <- file.path(
  dirname(classification_path),
  paste0("classification_export_current_before_northstar_demo_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
)
file.copy(classification_path, backup_path, overwrite = FALSE)
readr::write_csv(updated, classification_path)

cat("Wrote", nrow(northstar_rows), "Northstar demo classification rows\n")
cat("Backup:", backup_path, "\n")
print(northstar_rows %>% count(topic, sort = TRUE))
print(
  northstar_rows %>%
    tidyr::separate_rows(tags, sep = ",\\s*") %>%
    count(tags, sort = TRUE) %>%
    slice_head(n = 12)
)
