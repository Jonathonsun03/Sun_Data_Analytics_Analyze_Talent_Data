library(dplyr)
library(stringr)

if (!exists("Classification")) {
  stop("Classification data frame not found. Build it before running.")
}
if (!exists("Talent")) {
  stop("Talent variable not found. Set Talent before running.")
}

source("scripts/lib/utils/datalake_root.r")
source("scripts/lib/utils/staging_root.R")
source("scripts/lib/utils/talent_select.R")
source("scripts/lib/duckdb/db_connect.R")
source("scripts/lib/duckdb/db_schema.R")
source("scripts/lib/duckdb/ingest_videos.R")

con <- duckdb_connect()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

tryCatch(
  init_duckdb_schema(con),
  error = function(e) {
    if (!grepl("Invalid connection", conditionMessage(e), fixed = TRUE)) {
      stop(e)
    }
    warning("DuckDB connection became invalid during schema init; retrying once.")
    suppressWarnings(
      tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(disconnect_err) NULL)
    )
    con <<- duckdb_connect()
    init_duckdb_schema(con)
  }
)

talent_root <- tryCatch(
  select_talent(Talent),
  error = function(e) NA_character_
)
if (is.na(talent_root)) {
  warning("Talent not found in staging folders; using provided Talent value directly.")
  talent_name <- clean_talent_name(Talent, underscores = TRUE)
} else {
  talent_name <- safe_basename(talent_root)
}
talent_meta <- ensure_talent_id(con, talent_name)
talent_id <- talent_meta$talent_id[[1]]

to_ingest <- build_to_ingest(Classification, talent_id, talent_name)
upsert_videos(con, to_ingest)

message("Ingested: ", nrow(to_ingest))
