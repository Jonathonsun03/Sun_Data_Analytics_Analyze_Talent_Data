suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(duckdb)
  library(tibble)
})

source(file.path("r_scripts", "lib", "duckdb", "viewer_activity_schema.R"))
source(file.path("r_scripts", "lib", "duckdb", "viewer_activity_publish.R"))

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_equal <- function(actual, expected, message) {
  comparison <- all.equal(actual, expected, check.attributes = FALSE)
  if (!isTRUE(comparison)) {
    stop(message, ": ", paste(comparison, collapse = " | "), call. = FALSE)
  }
}

fixture_path <- tempfile(fileext = ".duckdb")
on.exit(unlink(fixture_path), add = TRUE)
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = fixture_path)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbExecute(con, "CREATE SCHEMA catalog")
DBI::dbExecute(con, "CREATE SCHEMA clean")
DBI::dbExecute(con, "CREATE SCHEMA text")
DBI::dbExecute(
  con,
  "CREATE TABLE catalog.talents (
     talent_code VARCHAR, talent_name VARCHAR, active BOOLEAN
   )"
)
DBI::dbExecute(
  con,
  "CREATE TABLE catalog.videos (
     talent_code VARCHAR, channel_id VARCHAR, video_id VARCHAR,
     title VARCHAR, published_at TIMESTAMP, actual_start_at TIMESTAMP
   )"
)
DBI::dbExecute(
  con,
  "CREATE TABLE text.chat_messages (
     message_key VARCHAR, talent_code VARCHAR, channel_id VARCHAR,
     video_id VARCHAR, user_id VARCHAR, username VARCHAR,
     message_type VARCHAR, time_in_seconds DOUBLE
   )"
)
DBI::dbExecute(
  con,
  "CREATE TABLE clean.public_subscriber_snapshots (
     talent_code VARCHAR, subscriber_channel_id VARCHAR,
     subscribed_at TIMESTAMP, snapshot_date DATE, ingested_at TIMESTAMP
   )"
)

DBI::dbAppendTable(
  con,
  DBI::Id(schema = "catalog", table = "talents"),
  tibble::tribble(
    ~talent_code, ~talent_name, ~active,
    "T1", "Talent One", TRUE,
    "T2", "Talent Two", TRUE
  )
)
DBI::dbAppendTable(
  con,
  DBI::Id(schema = "catalog", table = "videos"),
  tibble::tribble(
    ~talent_code, ~channel_id, ~video_id, ~title, ~published_at, ~actual_start_at,
    "T1", "C1", "V1", "One", as.POSIXct("2026-01-01", tz = "UTC"), as.POSIXct("2026-01-01", tz = "UTC"),
    "T1", "C1", "V2", "Two", as.POSIXct("2026-01-03", tz = "UTC"), as.POSIXct("2026-01-03", tz = "UTC"),
    "T2", "C2", "V3", "Three", as.POSIXct("2026-01-02", tz = "UTC"), as.POSIXct("2026-01-02", tz = "UTC")
  )
)
DBI::dbAppendTable(
  con,
  DBI::Id(schema = "text", table = "chat_messages"),
  tibble::tribble(
    ~message_key, ~talent_code, ~channel_id, ~video_id, ~user_id,
    ~username, ~message_type, ~time_in_seconds,
    "M1", "T1", "C1", "V1", "U1", "Viewer One", "text_message", 10,
    "M2", "T1", "C1", "V1", "U1", "Viewer One", "membership_item", 20,
    "M3", "T1", "C1", "V1", "U2", "Viewer Two", "text_message", 30,
    "M4", "T1", "C1", "V2", "U1", "Viewer One Renamed", "text_message", 5,
    "M5", "T2", "C2", "V3", "U1", "Viewer One Renamed", "paid_message", 15,
    "M6", "T2", "C2", "V3", "U3", "Viewer Three", "text_message", 25
  )
)
DBI::dbAppendTable(
  con,
  DBI::Id(schema = "clean", table = "public_subscriber_snapshots"),
  tibble::tribble(
    ~talent_code, ~subscriber_channel_id, ~subscribed_at,
    ~snapshot_date, ~ingested_at,
    "T1", "U1", as.POSIXct("2025-12-01", tz = "UTC"),
    as.Date("2026-01-01"), as.POSIXct("2026-01-01", tz = "UTC"),
    "T1", "U1", as.POSIXct("2025-12-02", tz = "UTC"),
    as.Date("2026-01-05"), as.POSIXct("2026-01-05", tz = "UTC"),
    "T2", "U1", as.POSIXct("2025-11-01", tz = "UTC"),
    as.Date("2026-01-05"), as.POSIXct("2026-01-05", tz = "UTC")
  )
)

memberships <- tibble::tribble(
  ~company_code, ~company_name, ~talent_code,
  "COMPANY", "Example Company", "T1",
  "COMPANY", "Example Company", "T2"
)

dry_run <- publish_viewer_activity(
  con = con,
  company_memberships = memberships,
  company_mapping_checksum = "fixture-checksum",
  dry_run = TRUE
)
assert_true(!dry_run$published, "Dry run should not publish relations.")
assert_true(
  !DBI::dbExistsTable(con, DBI::Id(schema = "analysis", table = "viewer_profiles")),
  "Dry run created an analysis table."
)

published <- publish_viewer_activity(
  con = con,
  company_memberships = memberships,
  company_mapping_checksum = "fixture-checksum",
  dry_run = FALSE
)
assert_true(published$published, "Publication did not report success.")

counts <- stats::setNames(published$row_counts$rows, published$row_counts$relation)
assert_equal(unname(counts[["viewer_video_activity"]]), 5, "Wrong video activity rows.")
assert_equal(unname(counts[["viewer_profiles"]]), 4, "Wrong viewer profile rows.")
assert_equal(unname(counts[["global_viewer_profiles"]]), 3, "Wrong global profile rows.")
assert_equal(unname(counts[["company_viewer_profiles"]]), 3, "Wrong company profile rows.")

global_u1 <- DBI::dbGetQuery(
  con,
  "SELECT * FROM analysis.global_viewer_profiles WHERE user_id = 'U1'"
)
assert_equal(global_u1$talents_chatted_with, 2, "Global talent reach is incorrect.")
assert_equal(global_u1$videos_chatted_in, 3, "Global active-video count is incorrect.")
assert_equal(global_u1$total_messages, 4, "Global message count is incorrect.")
assert_equal(
  global_u1$public_subscription_talent_count,
  2,
  "Global public-subscription coverage is incorrect."
)
assert_true(global_u1$cross_talent_chatter, "Global cross-talent flag is incorrect.")

u1 <- DBI::dbGetQuery(
  con,
  "SELECT * FROM analysis.company_viewer_profiles
   WHERE company_code = 'COMPANY' AND user_id = 'U1'"
)
assert_equal(u1$talents_chatted_with, 2, "Company talent reach is incorrect.")
assert_equal(u1$videos_chatted_in, 3, "Company active-video count is incorrect.")
assert_equal(u1$total_messages, 4, "Company message count is incorrect.")
assert_equal(u1$public_subscription_talent_count, 2, "Subscription coverage is incorrect.")
assert_true(u1$cross_talent_chatter, "Cross-talent chatter flag is incorrect.")

t1_u1 <- DBI::dbGetQuery(
  con,
  "SELECT * FROM analysis.viewer_profiles
   WHERE talent_code = 'T1' AND user_id = 'U1'"
)
assert_equal(t1_u1$videos_chatted_in, 2, "Talent active-video count is incorrect.")
assert_equal(t1_u1$total_messages, 3, "Talent message count is incorrect.")
assert_true(t1_u1$subscription_date_changed, "Changed subscription date was not flagged.")
assert_equal(
  t1_u1$current_username,
  "Viewer One Renamed",
  "Latest observed username is incorrect."
)

run_status <- DBI::dbGetQuery(
  con,
  "SELECT status FROM ops.pipeline_runs WHERE pipeline_run_id = ?",
  params = list(published$pipeline_run_id)
)$status
assert_equal(run_status, "completed", "Pipeline run was not completed.")

message("Viewer activity publication tests passed.")
