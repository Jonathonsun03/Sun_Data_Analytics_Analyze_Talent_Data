duckdb_get_db_path <- function(
    datalake_root = get_datalake_root(),
    filename = "classifications.duckdb"
) {
  file.path(datalake_root, filename)
}

.duckdb_driver_cache <- new.env(parent = emptyenv())

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

  connect_once <- function() {
    key <- normalizePath(db_path, winslash = "/", mustWork = FALSE)
    drv <- duckdb::duckdb()
    assign(key, drv, envir = .duckdb_driver_cache)
    DBI::dbConnect(drv, dbdir = db_path, read_only = read_only)
  }

  con <- connect_once()
  is_healthy <- isTRUE(
    tryCatch({
      DBI::dbIsValid(con) && nrow(DBI::dbGetQuery(con, "SELECT 1 AS ok")) == 1
    }, error = function(e) FALSE)
  )
  if (is_healthy) {
    return(con)
  }

  # Retry once for transient invalid-handle cases observed in some environments.
  suppressWarnings(
    tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL)
  )
  con_retry <- connect_once()
  is_healthy_retry <- isTRUE(
    tryCatch({
      DBI::dbIsValid(con_retry) && nrow(DBI::dbGetQuery(con_retry, "SELECT 1 AS ok")) == 1
    }, error = function(e) FALSE)
  )
  if (!is_healthy_retry) {
    stop("DuckDB connection failed health check for: ", db_path)
  }

  con_retry
}
