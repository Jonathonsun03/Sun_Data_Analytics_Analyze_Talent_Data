library(here)
library(dplyr)
library(purrr)
library(stringr)

knitr::opts_knit$set(root.dir = here::here())
setwd(here::here())

source(here::here("r_scripts", "lib", "utils", "datalake_root.r"))
source(here::here(
  "r_scripts",
  "lib",
  "clean_data",
  "qualitative_data_prep",
  "qualitative_data_prep.r"
))
source(here::here(
  "r_scripts",
  "lib",
  "clean_data",
  "clean_subtitles",
  "Clean_subtitles.R"
))
source(here::here("r_scripts", "lib", "duckdb", "db_connect.R"))
source(here::here("r_scripts", "lib", "duckdb", "qualitative_schema.R"))
source(here::here("r_scripts", "lib", "duckdb", "qualitative_publish.R"))

env_or_default <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (nzchar(value)) value else default
}

env_flag <- function(name, default = FALSE) {
  value <- tolower(trimws(Sys.getenv(
    name,
    unset = if (isTRUE(default)) "true" else "false"
  )))
  value %in% c("1", "true", "yes")
}

talent_root <- get_datalake_root()
processed_root <- file.path(
  dirname(talent_root),
  "Processed",
  "Talent_Data"
)

db_path <- env_or_default(
  "QUALITATIVE_DB_PATH",
  talent_lakehouse_db_path(talent_root)
)
coded_data_dir <- env_or_default(
  "QUALITATIVE_CODED_DATA_DIR",
  file.path(
    processed_root,
    "saved_outputs",
    "chat_monetary_052526",
    "coded_data"
  )
)
codebook_path <- env_or_default(
  "QUALITATIVE_CODEBOOK_PATH",
  file.path(
    processed_root,
    "Qualitative Codebooks",
    "library",
    "selections",
    "chat_monetary_growth",
    "batch_002",
    "batch_002_codebook.csv"
  )
)
dataset_id <- env_or_default(
  "QUALITATIVE_DATASET_ID",
  "chat_monetary_growth_variance_30_video"
)
codebook_id <- env_or_default(
  "QUALITATIVE_CODEBOOK_ID",
  "chat_monetary_growth_v1"
)
codebook_name <- env_or_default(
  "QUALITATIVE_CODEBOOK_NAME",
  "Chat Monetary Growth"
)
codebook_version <- env_or_default(
  "QUALITATIVE_CODEBOOK_VERSION",
  "1"
)
wide_view_name <- env_or_default(
  "QUALITATIVE_WIDE_VIEW_NAME",
  "coding_chat_monetary_growth_v1"
)
dry_run <- env_flag("QUALITATIVE_PUBLISH_DRY_RUN")

message("DuckDB: ", db_path)
message("Coded data: ", coded_data_dir)
message("Codebook: ", codebook_path)
message("Dataset ID: ", dataset_id)
message("Codebook ID: ", codebook_id)
message("Dry run: ", dry_run)

con <- duckdb_connect(
  db_path = db_path,
  read_only = dry_run
)
on.exit(
  suppressWarnings(
    tryCatch(
      DBI::dbDisconnect(con, shutdown = TRUE),
      error = function(e) NULL
    )
  ),
  add = TRUE
)

result <- publish_qualitative_coding_dataset(
  con = con,
  coded_data_dir = coded_data_dir,
  codebook_path = codebook_path,
  dataset_id = dataset_id,
  codebook_id = codebook_id,
  codebook_name = codebook_name,
  codebook_version = codebook_version,
  wide_view_name = wide_view_name,
  dry_run = dry_run
)

print(result$summary)
if (!is.null(result$input_summary)) {
  print(result$input_summary)
}
if (!is.null(result$view_name)) {
  message("Published wide view: ", result$view_name)
}
