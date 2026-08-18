suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(duckdb)
})

source(file.path("r_scripts", "lib", "dashboard", "data", "sources.R"))

assert_equal <- function(x, y, message) {
  if (!identical(x, y)) {
    stop(message, call. = FALSE)
  }
}

run_dashboard_catalog_test <- function() {
  database_path <- tempfile("dashboard-catalog-", fileext = ".duckdb")
  on.exit(unlink(database_path), add = TRUE)

  write_catalog_fixture <- function(video_date, snapshot_date) {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS catalog")
    DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS clean")
    DBI::dbExecute(
      con,
      paste(
        "CREATE TABLE IF NOT EXISTS catalog.talents (",
        "talent_code VARCHAR, talent_name VARCHAR, active BOOLEAN)"
      )
    )
    DBI::dbExecute(
      con,
      paste(
        "CREATE TABLE IF NOT EXISTS catalog.videos (",
        "talent_code VARCHAR, published_at TIMESTAMP)"
      )
    )
    DBI::dbExecute(
      con,
      paste(
        "CREATE TABLE IF NOT EXISTS clean.video_analytics_snapshots (",
        "talent_code VARCHAR, video_id VARCHAR, snapshot_date DATE)"
      )
    )
    DBI::dbExecute(con, "DELETE FROM catalog.talents")
    DBI::dbExecute(con, "DELETE FROM catalog.videos")
    DBI::dbExecute(con, "DELETE FROM clean.video_analytics_snapshots")
    DBI::dbExecute(
      con,
      "INSERT INTO catalog.talents VALUES ('AVA1', 'Avaritia_Hawthorne', TRUE)"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO catalog.videos VALUES ('AVA1', ?)",
      params = list(as.POSIXct(video_date, tz = "UTC"))
    )
    DBI::dbExecute(
      con,
      "INSERT INTO clean.video_analytics_snapshots VALUES ('AVA1', 'video-1', ?)",
      params = list(as.Date(snapshot_date))
    )
  }

  write_catalog_fixture("2026-08-10", "2026-08-10")
  initial <- dashboard_unified_talent_catalog(database_path)

  assert_equal(
    initial$latest_publish_date[[1]],
    as.Date("2026-08-10"),
    "The initial catalog should report the latest video publish date."
  )
  assert_equal(
    initial$earliest_analytics_snapshot_date[[1]],
    as.Date("2026-08-10"),
    "The initial catalog should report the earliest analytics snapshot date."
  )
  assert_equal(
    initial$latest_analytics_snapshot_date[[1]],
    as.Date("2026-08-10"),
    "The initial catalog should report the latest analytics snapshot date."
  )
  assert_equal(
    initial$latest_analytics_video_count[[1]],
    1L,
    "The initial catalog should report tracked videos in the latest snapshot."
  )

  write_catalog_fixture("2026-08-11", "2026-08-11")
  refreshed <- dashboard_unified_talent_catalog(database_path)

  assert_equal(
    refreshed$latest_publish_date[[1]],
    as.Date("2026-08-11"),
    "A new session catalog read should observe a newly published video."
  )
  assert_equal(
    refreshed$latest_analytics_snapshot_date[[1]],
    as.Date("2026-08-11"),
    "A new session catalog read should observe the latest analytics snapshot."
  )
  assert_equal(
    refreshed$latest_analytics_video_count[[1]],
    1L,
    "A refreshed catalog should report tracked videos in the latest snapshot."
  )
}

run_dashboard_catalog_test()

cat("dashboard catalog tests passed\n")
