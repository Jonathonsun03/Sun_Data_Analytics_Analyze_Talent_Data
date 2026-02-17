library(purrr)
library(tidyverse)
library(dplyr)
library(rENA)
library(here)
library(scales)
library(data.table)
library(moments)
library(corrplot)
library(car)
library(rlang)
library(stringr)

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
list.files(here("scripts", "lib", "ENA_prep"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)
list.files(here("scripts", "lib", "plots"), pattern = "\\.[rR]$", full.names = TRUE, recursive = TRUE) %>%
  walk(source)


# Importing Title data ----------------------
remove <- c("classification_json", "taxonomy_version", "prompt_version","created_at", "title_hash")

df <- fread(here("classification","output","title_classifications","classification_export_gpt-5-mini_v6_v6.csv")) %>%
    select(-all_of(remove))

df_clean <- df %>%
  rename_with(
    .fn   = ~ paste0("code_", .x),
    .cols = collaborative_energy:personality_conversation
  ) %>%
  mutate(
    tags_list = stringr::str_split(dplyr::coalesce(tags, ""), ",") %>%
      purrr::map(~ .x %>%
        stringr::str_trim() %>%
        stringr::str_to_lower() %>%
        purrr::discard(~ .x == "") %>%
        unique() %>%
        sort()
      ),
    tags_clean = purrr::map_chr(tags_list, ~ paste(.x, collapse = ", "))
  )

# Analytics Data ---------------------
talent_paths <- select_talent("all")
files <- TalentFiles(talent_paths)

analytics_all <- purrr::map_dfr(
  seq_along(files),
  ~ video_analytics_prep(
      files = files,
      talent = safe_basename(talent_paths[.x]),
      talent_index = .x
    ) %>%
      mutate(talent = safe_basename(talent_paths[.x]))
)

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

# Performance bins (shared z-score helper in scripts/lib/clean_data/analytics_core.R)
min_n <- 10
df_full <- add_view_performance_bins(
  df = df_full,
  views_col = "views",
  group_cols = c("talent_name", "content_type"),
  min_n = min_n,
  fallback = "global"
)

# Descriptive Plots ----------------------
## Histogram
log_breaks <- c(10, 30, 100, 300, 1e3, 3e3, 1e4, 3e4, 1e5)

plot_view_distribution_histogram(
  df = df_full,
  views_col = "views",
  fill_col = "content_type",
  facet_col = "talent_name",
  bins = 30,
  log_breaks = log_breaks
)

glimpse(df_full)
# ENA prep ----------------------
ena_cols <- ENA_setup(
  df = df_full,
  units_col = c("video_id", "talent_name", "view_perf_bin"),
  conversation_cols = "content_type",
  codes_cols = starts_with("code_"),
  metadata_cols = c("title", "content_type", "published_at", "views")
)

ena_accum <- ENA_accumulate_from_prep(ena_cols)
ENA_set <- ena.make.set(ena_accum)


ENA_mean_network <- ENA_lineweight(ENA_set)
ENA_mean_point <- ENA_points(ENA_set)

ENA_set$points

tbl <- prepare_ena_lineweight_tbl(
  ENA_set = ENA_set,
  unit_col = "ENA_UNIT"
)

# By talent
Talents <- unique(ENA_set$points$talent_name)

ENA_talents_points <- setNames(
  lapply(Talents, function(talent) ENA_points(ENA_set, filter_col = "talent_name", filter_val = talent)),
  Talents
)

ENA_talents_lineweights <- setNames(
  lapply(Talents, function(talent) ENA_lineweight(ENA_set, filter_col = "talent_name", filter_val = talent)),
  Talents
)

Base_plot <- ena.plot(ENA_set, title = "Talents Title Preferences") %>%
  ena.plot.network(network = ENA_mean_network, colors = "red")

ENA_set$points
Base_plot$plot

plot_talents <- add_ena_point_groups(Base_plot, ENA_talents_points)

plot_talents$plot

Talent_tbl <- setNames(
  lapply(Talents , function(talent) {
     prepare_ena_lineweight_tbl(
      ENA_set   = ENA_set,
      unit_col  = "talent_name",
      unit_val  = talent,
      add_z     = FALSE
    )
  }),
Talents
) %>% bind_rows(.id = "talent_name")

Talent_tbl <- add_weight_z_score(Talent_tbl, weight_col = "weight", z_col = "z_score")

# By Video Views
Classification <- unique(ENA_set$points$view_perf_bin)

ENA_view_classification_points <- setNames(
  lapply(Classification, function(classification) ENA_points(ENA_set, filter_col = "view_perf_bin", filter_val = classification)),
  Classification
)

ENA_view_classification_lineweights <- setNames(
  lapply(Classification, function(classification) ENA_lineweight(ENA_set, filter_col = "view_perf_bin", filter_val = classification)),
  Classification
)
  
Base_plot <- ena.plot(ENA_set, title = "Talents Title Preferences") %>%
  ena.plot.network(network = ENA_mean_network, colors = "red")

plot_view_classification <- add_ena_point_groups(Base_plot, ENA_view_classification_points,shape = "square")

plot_view_classification$plot

## Did not add Z score in each set, because I wanted to compare the line weights across sets
Views_tbl <- setNames(
  lapply(Classification, function(classification) {
    prepare_ena_lineweight_tbl(
      ENA_set   = ENA_set,
      unit_col  = "view_perf_bin",
      unit_val  = classification,
      add_z     = FALSE
    )
  }),
  Classification
) %>% bind_rows(.id = "view_perf_bin")

Views_tbl <- add_weight_z_score(Views_tbl, weight_col = "weight", z_col = "z_score")

# By VideoID

Classification <- unique(ENA_set$points$video_id)

ENA_video_id_points <- setNames(
  lapply(Classification, function(classification) ENA_points(ENA_set, filter_col = "video_id", filter_val = classification)),
  Classification
)

ENA_video_id_lineweights <- setNames(
  lapply(Classification, function(classification) ENA_lineweight(ENA_set, filter_col = "video_id", filter_val = classification)),
  Classification
)
  
Base_plot <- ena.plot(ENA_set, title = "Talents Title Preferences") %>%
  ena.plot.network(network = ENA_mean_network, colors = "red")

plot_video_id <- add_ena_point_groups(Base_plot, ENA_video_id_points,shape = "square")

plot_video_id$plot

## Did not add Z score in each set, because I wanted to compare the line weights across sets
VideoID_tbl <- setNames(
  lapply(Classification, function(classification) {
    prepare_ena_lineweight_tbl(
      ENA_set   = ENA_set,
      unit_col  = "video_id",
      unit_val  = classification,
      add_z     = FALSE
    )
  }),
  Classification
) %>% bind_rows(.id = "video_id")

VideoID_tbl <- add_weight_z_score(VideoID_tbl, weight_col = "weight", z_col = "z_score")

# Video Views and Talents 
Base_plot <- ena.plot(ENA_set, title = "Talents Title Preferences") %>%
  ena.plot.network(network = ENA_mean_network, colors = "red")
View_Talent_plot <- add_ena_point_groups(Base_plot, ENA_view_classification_points,shape = "square")
View_Talent_plot <- add_ena_point_groups(View_Talent_plot, ENA_talents_points, shape = "circle")

View_Talent_plot$plot

# Using ENA to build models
VideoID_tbl
VideoID_tbl_wide <- VideoID_tbl %>%
  mutate(Code_Pair = paste(Code_1, Code_2, sep = " & ")) %>%
  select(video_id, Code_Pair, weight, z_score) %>%
  pivot_wider(names_from = Code_Pair, values_from = c(weight, z_score))


df_model <- df_full %>%
  left_join(VideoID_tbl_wide, by = "video_id") %>%
  select(-all_of(starts_with("z_score"))) %>%
  mutate(log_views = log1p(views))

colnames(df_model)

## Checking all variables to each other

cor_mat <- df_model %>%
  select(
    log_views,
    duration_minutes,
    averageViewPercentage,
    subscribersGained,
    starts_with("weight_")
  ) %>%
  cor(use = "pairwise.complete.obs")

corrplot(cor_mat, method = "color", type = "upper")
cor_mat["log_views", ]

# Checking the predcitors specifically line weights
cor_predictors <- cor(select(df_model, starts_with("weight_"))) %>%
                as.data.frame()   
cor_df <- as.data.frame(cor_predictors)
cor_long <- cor_df %>%
  mutate(var1 = rownames(.)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
  filter(var1 != var2) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r))


## Actual model building --------------------
ena_vars  <- names(df_model)[grepl("^weight_", names(df_model))]
base_vars <- c(
  "duration_minutes",
  "averageViewPercentage",
  "subscribersGained",
  "publish_hour_sin",
  "publish_hour_cos",
  "is_weekend"
)

all_predictors <- c(base_vars, ena_vars)
rhs <- paste0("`", all_predictors, "`", collapse = " + ")

model_base <- lm(
  log_views ~ 
    duration_minutes +
    averageViewPercentage +
    subscribersGained +
    publish_hour_sin + 
    publish_hour_cos +
    is_weekend,
  data = df_model
)

form_ena <- as.formula(paste("log_views ~", rhs))
model_ena <- lm(form_ena, data = df_model)

anova(model_base, model_ena)
