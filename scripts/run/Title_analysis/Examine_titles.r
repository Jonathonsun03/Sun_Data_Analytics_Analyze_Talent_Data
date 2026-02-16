library(purrr)
library(tidyverse)
library(dplyr)
library(rENA)
library(here)
library(scales)

source("scripts/lib/utils/staging_root.R")

list.files(here("scripts","lib","utils"), full.names = TRUE) %>%
    walk(source)
list.files(here("scripts", "lib", "import_data"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
list.files(here("scripts", "lib", "report_tables"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
list.files(here("scripts", "lib", "clean_data"), pattern = "[rR]$", full.names = TRUE) %>%
  keep(~ basename(.) != "CleanData.R") %>%
  walk(source)

# Importing Title data ----------------------
remove <- c("classification_json", "taxonomy_version", "prompt_version","created_at", "title_hash")

df <- fread(here("classification","output","title_classifications","classification_export_gpt-5-mini_v6_v6.csv")) %>%
    select(-all_of(remove))

df_clean <- df %>%
  rename_with(
    ~ paste0("code_", .x),
    collaborative_energy:personality_conversation
  ) %>%
  # 2) Standardize tags into a canonical form
  mutate(
    tags_list = str_split(tags %||% "", ",") %>%
      map(~ .x %>%
            str_trim() %>%
            str_to_lower() %>%
            discard(~ .x == "") %>%
            unique() %>%
            sort()),
    tags_clean = map_chr(tags_list, ~ paste(.x, collapse = ", "))
  )

# Analytics Data ---------------------
talent_paths <- select_talent("all")
files <- TalentFiles(talent_paths)

analytics_all <- purrr::map_dfr(
  seq_along(files),
  ~ video_analytics_prep(
      files = files,
      talent = basename(talent_paths[.x]),
      talent_index = .x
    ) %>%
      mutate(talent = basename(talent_paths[.x]))
)

glimpse(analytics_all)
# Bind Title Data 

# 1) Normalize analytics column names used for the join
analytics_join <- analytics_all %>%
  transmute(
    video_id = `Video ID`,
    channel_id = `Channel ID`,
    channel_name = `Channel Name`,
    title_yt = Title,
    published_at_yt = `Published At`,
    content_type_yt = `Content Type`,
    snapshot_date = date,
    views,
    estimatedMinutesWatched,
    averageViewDuration,
    averageViewPercentage,
    subscribersGained,
    subscribersLost,
    estimated_revenue = `Estimated Revenue`,
    cpm = CPM,
    duration_seconds = duration_seconds,
    duration_minutes = duration_minutes,
    duration_hours = duration_hours,
    publish_date,
    publish_year,
    publish_month,
    publish_month_num,
    publish_day,
    publish_wday,
    publish_wday_num,
    is_weekend,
    publish_hour,
    publish_min,
    publish_ampm,
    publish_hour_sin,
    publish_hour_cos,
    publish_week,
    publish_quarter
  ) %>%
  distinct(video_id, .keep_all = TRUE)

# 2) Join and remove redundant fields
df_full <- df_clean %>%
  left_join(analytics_join, by = "video_id") %>%
  mutate(
    title = coalesce(title_raw, title_yt),
    content_type = coalesce(content_type, content_type_yt),
    published_at = coalesce(published_at, published_at_yt)
  ) %>%
  select(
    -title_raw, -title_yt,
    -content_type_yt,
    -published_at_yt
  )

glimpse(df_full)

# Descriptive Plots ----------------------
## Histogram
log_breaks <- c(10, 30, 100, 300, 1e3, 3e3, 1e4, 3e4, 1e5)

df_full %>%
  ggplot(aes(x = views, fill = content_type)) +
  geom_histogram(position = "dodge", bins = 30) +
  facet_wrap(~ talent_name) +
  scale_x_log10(
    breaks = log_breaks,
    labels = label_comma()
  ) +
  labs(title = "Distribution of Views by Content Type", x = "Views", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
