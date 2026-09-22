suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

source(here::here("r_scripts", "lib", "utils", "repo_env.R"))
source(here::here("r_scripts", "lib", "utils", "datalake_root.r"))
source(here::here("r_scripts", "lib", "duckdb", "db_connect.R"))
source(here::here("r_scripts", "lib", "duckdb", "viewer_activity_schema.R"))
source(here::here("r_scripts", "lib", "duckdb", "viewer_activity_publish.R"))

env_flag <- function(name, default = FALSE) {
  value <- tolower(trimws(Sys.getenv(
    name,
    unset = if (isTRUE(default)) "true" else "false"
  )))
  value %in% c("1", "true", "yes", "on")
}

load_repo_env(repo_root = here::here())
mapping_path <- here::here("config", "dashboard", "company_talents.csv")
if (!file.exists(mapping_path)) {
  stop("Company talent mapping not found: ", mapping_path, call. = FALSE)
}

company_memberships <- readr::read_csv(
  mapping_path,
  show_col_types = FALSE,
  progress = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)
mapping_checksum <- digest::digest(
  file = mapping_path,
  algo = "sha256",
  serialize = FALSE
)
dry_run <- env_flag("VIEWER_ACTIVITY_PUBLISH_DRY_RUN", default = TRUE)
db_path <- talent_lakehouse_db_path()

message("DuckDB: ", db_path)
message("Company mapping: ", mapping_path)
message("Dry run: ", dry_run)

con <- duckdb_connect(db_path = db_path, read_only = dry_run)
on.exit(
  suppressWarnings(
    tryCatch(
      DBI::dbDisconnect(con, shutdown = TRUE),
      error = function(e) NULL
    )
  ),
  add = TRUE
)

result <- publish_viewer_activity(
  con = con,
  company_memberships = company_memberships,
  company_mapping_checksum = mapping_checksum,
  dry_run = dry_run
)

print(result$source_summary)
if (!is.null(result$row_counts)) {
  print(result$row_counts)
}
if (isTRUE(result$published)) {
  message("Published viewer activity run: ", result$pipeline_run_id)
} else {
  message("Dry run completed; no database relations were changed.")
}
