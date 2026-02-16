init_duckdb_schema <- function(con) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.")
  }

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS videos (
       video_id TEXT,
       talent_id TEXT,
       talent_name TEXT,
       title_raw TEXT,
       title_norm TEXT,
       title_hash TEXT,
       content_type TEXT,
       published_at TIMESTAMP,
       first_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
       last_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
       UNIQUE(video_id, talent_id)
     )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS classifications (
       title_hash TEXT,
       video_id TEXT,
       talent_id TEXT,
       taxonomy_version TEXT,
       prompt_version TEXT,
       model TEXT,
       talent_profile TEXT,
       classification_json TEXT,
       confidence DOUBLE,
       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
       UNIQUE(video_id, taxonomy_version, prompt_version, model)
     )"
  )

  DBI::dbExecute(
    con,
    "ALTER TABLE classifications
     ADD COLUMN IF NOT EXISTS talent_profile TEXT"
  )
}

ensure_classification_boolean_columns <- function(con, column_names) {
  if (length(column_names) == 0) {
    return(invisible(NULL))
  }

  for (nm in unique(column_names)) {
    if (!is.character(nm) || length(nm) != 1 || !nzchar(nm)) {
      next
    }
    if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", nm)) {
      stop("Unsafe classification column name: ", nm)
    }
    DBI::dbExecute(
      con,
      sprintf(
        "ALTER TABLE classifications ADD COLUMN IF NOT EXISTS \"%s\" BOOLEAN",
        nm
      )
    )
  }

  invisible(NULL)
}
