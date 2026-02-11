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
source("scripts/lib/duckdb/pending.R")

con <- duckdb_connect()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

init_duckdb_schema(con)

talent_root <- select_talent(Talent)
talent_name <- safe_basename(talent_root)
talent_meta <- ensure_talent_id(con, talent_name)
talent_id <- talent_meta$talent_id[[1]]

to_ingest <- build_to_ingest(Classification, talent_id, talent_name)

# Classification run controls
taxonomy_version <- "v1"
prompt_version <- "v1"
model <- Sys.getenv("OPENAI_MODEL", unset = "gpt-5-mini")

# Choose how to avoid rework:
# - "video_id": classify once per video per version tuple (default)
# - "title_hash": classify again if title changes
classification_key <- "video_id"

pending <- get_pending_titles(
  con,
  to_ingest,
  taxonomy_version,
  prompt_version,
  model,
  key = classification_key
)

message("Pending titles: ", nrow(pending))

# TODO: call GPT classifier here, then insert into classifications table.
