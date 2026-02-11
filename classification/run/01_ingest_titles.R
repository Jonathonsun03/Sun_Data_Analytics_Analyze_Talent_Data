library(dplyr)
library(stringr)

if (!exists("Classification")) {
  stop("Classification data frame not found. Build it before running.")
}
if (!exists("Talent")) {
  stop("Talent variable not found. Set Talent before running.")
}

source("scripts/lib/utils/datalake_root.r")
source("scripts/lib/utils/talent_select.R")
source("scripts/lib/duckdb/db_connect.R")
source("scripts/lib/duckdb/db_schema.R")
source("scripts/lib/duckdb/ingest_videos.R")

con <- duckdb_connect()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

init_duckdb_schema(con)

talent_root <- select_talent(Talent)
talent_name <- safe_basename(talent_root)
talent_meta <- ensure_talent_id(con, talent_name)
talent_id <- talent_meta$talent_id[[1]]

to_ingest <- build_to_ingest(Classification, talent_id, talent_name)
upsert_videos(con, to_ingest)

message("Ingested: ", nrow(to_ingest))
