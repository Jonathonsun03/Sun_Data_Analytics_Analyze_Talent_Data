duckdb_get_db_path <- function(
    datalake_root = get_datalake_root(),
    filename = "classifications.duckdb"
) {
  file.path(datalake_root, filename)
}

duckdb_connect <- function(
    datalake_root = get_datalake_root(),
    db_path = NULL,
    read_only = FALSE
) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package `duckdb` is required.")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.")
  }

  if (is.null(db_path)) {
    db_path <- duckdb_get_db_path(datalake_root = datalake_root)
  }

  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    stop("DuckDB directory does not exist: ", db_dir)
  }

  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}
