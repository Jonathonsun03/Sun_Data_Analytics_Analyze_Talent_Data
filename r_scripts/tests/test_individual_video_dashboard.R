suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(duckdb)
  library(tibble)
})

source(file.path("r_scripts", "lib", "dashboard", "auth", "access.R"))
source(file.path("r_scripts", "lib", "dashboard", "data", "individual_video.R"))
source(file.path("r_scripts", "lib", "plots", "report", "bundle_e", "bundle_e_panel_prep.R"))
source(file.path("r_scripts", "lib", "dashboard", "metrics", "individual_video.R"))
source(file.path("r_scripts", "lib", "plots", "themes", "sun_data_brand_theme.R"))
source(file.path("r_scripts", "lib", "dashboard", "adapters", "individual_video.R"))

assert_true <- function(x, message) {
  if (!isTRUE(x)) {
    stop(message, call. = FALSE)
  }
}

assert_equal <- function(x, y, message, tolerance = 1e-12) {
  comparison <- all.equal(x, y, check.attributes = TRUE, tolerance = tolerance)
  if (!isTRUE(comparison)) {
    stop(message, ": ", paste(comparison, collapse = " | "), call. = FALSE)
  }
}

fixture_path <- tempfile(fileext = ".duckdb")
on.exit(unlink(fixture_path), add = TRUE)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = fixture_path)
DBI::dbExecute(con, "CREATE SCHEMA catalog")
DBI::dbExecute(con, "CREATE SCHEMA clean")
DBI::dbExecute(con, "CREATE SCHEMA text")
DBI::dbExecute(
  con,
  paste(
    "CREATE TABLE catalog.channels (",
    "talent_code VARCHAR, channel_id VARCHAR, channel_name VARCHAR)"
  )
)
DBI::dbExecute(
  con,
  paste(
    "CREATE TABLE text.subtitle_units (",
    "subtitle_unit_key VARCHAR, video_id VARCHAR, channel_id VARCHAR,",
    "talent_code VARCHAR, sequence_number BIGINT, subtitle_start VARCHAR,",
    "subtitle_text VARCHAR)"
  )
)
DBI::dbExecute(
  con,
  paste(
    "CREATE TABLE text.chat_messages (",
    "message_key VARCHAR, video_id VARCHAR, channel_id VARCHAR,",
    "talent_code VARCHAR, username VARCHAR, message VARCHAR,",
    "time_in_seconds DOUBLE, message_timestamp TIMESTAMP)"
  )
)
DBI::dbExecute(
  con,
  paste(
    "CREATE TABLE catalog.videos (",
    "talent_code VARCHAR, channel_id VARCHAR, video_id VARCHAR, title VARCHAR,",
    "published_at TIMESTAMP, content_type VARCHAR, duration_seconds DOUBLE)"
  )
)
DBI::dbExecute(
  con,
  paste(
    "CREATE TABLE clean.video_analytics_snapshots (",
    "talent_code VARCHAR, channel_id VARCHAR, video_id VARCHAR, snapshot_date DATE,",
    "views BIGINT, estimated_minutes_watched DOUBLE, average_view_duration DOUBLE,",
    "average_view_percentage DOUBLE, subscribers_gained BIGINT, subscribers_lost BIGINT)"
  )
)
DBI::dbExecute(
  con,
  paste(
    "CREATE TABLE clean.video_monetary_snapshots (",
    "talent_code VARCHAR, channel_id VARCHAR, video_id VARCHAR, snapshot_date DATE,",
    "estimated_revenue DOUBLE, cpm DOUBLE)"
  )
)

DBI::dbAppendTable(
  con,
  DBI::Id(schema = "catalog", table = "channels"),
  tibble::tribble(
    ~talent_code, ~channel_id, ~channel_name,
    "T1", "C1", "Talent One",
    "T2", "C2", "Talent Two"
  )
)
DBI::dbAppendTable(
  con,
  DBI::Id(schema = "catalog", table = "videos"),
  tibble::tribble(
    ~talent_code, ~channel_id, ~video_id, ~title, ~published_at, ~content_type, ~duration_seconds,
    "T1", "C1", "VIDEO_A", "Allowed Video", as.POSIXct("2026-01-01", tz = "UTC"), "video", 600,
    "T1", "C1", "VIDEO_C", "No Analytics Yet", as.POSIXct("2026-01-02", tz = "UTC"), "short", 30,
    "T2", "C2", "VIDEO_B", "Other Talent Video", as.POSIXct("2026-01-01", tz = "UTC"), "live", 3600
  )
)
analytics_fixture <- tibble::tibble(
  talent_code = c(rep("T1", 3), "T2"),
  channel_id = c(rep("C1", 3), "C2"),
  video_id = c(rep("VIDEO_A", 3), "VIDEO_B"),
  snapshot_date = as.Date(c("2026-01-01", "2026-01-11", "2026-01-16", "2026-01-16")),
  views = c(10, 40, 100, 999),
  estimated_minutes_watched = c(5, 20, 50, 400),
  average_view_duration = c(30, 35, 40, 45),
  average_view_percentage = c(40, 45, 50, 60),
  subscribers_gained = c(1, 5, 12, 50),
  subscribers_lost = c(0, 1, 2, 1)
)
DBI::dbAppendTable(
  con,
  DBI::Id(schema = "clean", table = "video_analytics_snapshots"),
  analytics_fixture
)
DBI::dbAppendTable(
  con,
  DBI::Id(schema = "clean", table = "video_monetary_snapshots"),
  analytics_fixture %>%
    dplyr::transmute(
      .data$talent_code,
      .data$channel_id,
      .data$video_id,
      .data$snapshot_date,
      estimated_revenue = c(1, 4, 10, 80),
      cpm = c(2, 2.5, 3, 4)
    )
)
DBI::dbAppendTable(
  con,
  DBI::Id(schema = "text", table = "subtitle_units"),
  tibble::tribble(
    ~subtitle_unit_key, ~video_id, ~channel_id, ~talent_code,
    ~sequence_number, ~subtitle_start, ~subtitle_text,
    "SUB_A_1", "VIDEO_A", "C1", "T1", 1L, "00:00:05.500", "Streamer opening",
    "SUB_A_2", "VIDEO_A", "C1", "T1", 2L, "00:00:20.000", "Streamer response",
    "SUB_B_1", "VIDEO_B", "C2", "T2", 1L, "00:00:01.000", "Other transcript"
  )
)
DBI::dbAppendTable(
  con,
  DBI::Id(schema = "text", table = "chat_messages"),
  tibble::tribble(
    ~message_key, ~video_id, ~channel_id, ~talent_code,
    ~username, ~message, ~time_in_seconds, ~message_timestamp,
    "CHAT_A_1", "VIDEO_A", "C1", "T1", "Viewer One", "Hello!", 10,
    as.POSIXct("2026-01-01 00:00:10", tz = "UTC"),
    "CHAT_A_2", "VIDEO_A", "C1", "T1", "Viewer Two", "Welcome!", 15,
    as.POSIXct("2026-01-01 00:00:15", tz = "UTC"),
    "CHAT_B_1", "VIDEO_B", "C2", "T2", "Other Viewer", "Other chat", 2,
    as.POSIXct("2026-01-01 00:00:02", tz = "UTC")
  )
)
DBI::dbDisconnect(con, shutdown = TRUE)

dashboard_resolve_database_path <- function(database_path = NULL) {
  normalizePath(database_path, winslash = "/", mustWork = TRUE)
}

access <- dashboard_access_context(
  list(
    HTTP_X_SDA_VERIFIED_EMAIL = "viewer@example.com",
    HTTP_X_SDA_ALLOWED_TALENT_CODES = "T1"
  ),
  c("T1", "T2")
)
assert_true(
  dashboard_talent_is_authorized("T1", access),
  "The permitted talent should pass the shared authorization check."
)
assert_true(
  !dashboard_talent_is_authorized("T2", access),
  "An unassigned talent should fail the shared authorization check."
)

catalog <- dashboard_individual_video_catalog(fixture_path, "T1")
assert_equal(
  catalog$video_id,
  c("VIDEO_A", "VIDEO_C"),
  "The catalog should include every permitted talent video and no others."
)
assert_equal(catalog$snapshot_count, c(3L, 0L), "Snapshot coverage is incorrect.")
assert_equal(
  unname(dashboard_individual_video_choices(catalog)),
  c("VIDEO_A", "VIDEO_C"),
  "The video choices should retain the authorized video IDs."
)
assert_equal(
  names(dashboard_individual_video_choices(catalog)),
  c("Allowed Video", "No Analytics Yet"),
  "The video choice labels should contain titles only."
)

assert_equal(
  dashboard_individual_video_timecode_seconds(c("01:02:03.500", "02:03", "7")),
  c(3723.5, 123, 7),
  "Video timecodes should convert to elapsed seconds."
)

transcript <- dashboard_load_individual_video_transcript(
  fixture_path,
  talent_code = "T1",
  video_id = "VIDEO_A"
)
assert_equal(
  transcript$source,
  c("subtitle", "chat", "chat", "subtitle"),
  "Streamer and chat transcript rows should be merged chronologically."
)
assert_equal(
  transcript$speaker,
  c("Talent One", "Viewer One", "Viewer Two", "Talent One"),
  "Transcript speakers should identify the streamer and chat participants."
)
assert_equal(
  transcript$seconds,
  c(5.5, 10, 15, 20),
  "Transcript timestamps are incorrect."
)
transcript_table <- dashboard_individual_video_transcript_table(transcript)
assert_equal(
  names(transcript_table),
  c("Speaker", "Timestamp", "Dialogue"),
  "The transcript table should expose only the requested columns."
)
assert_equal(
  transcript_table$Timestamp,
  c("00:00:05", "00:00:10", "00:00:15", "00:00:20"),
  "Transcript display timestamps are incorrect."
)

cross_talent_transcript <- dashboard_load_individual_video_transcript(
  fixture_path,
  talent_code = "T1",
  video_id = "VIDEO_B"
)
assert_equal(
  nrow(cross_talent_transcript),
  0L,
  "A transcript from another talent must not be returned."
)

cross_talent_history <- dashboard_load_individual_video_history(
  fixture_path,
  talent_code = "T1",
  video_id = "VIDEO_B"
)
assert_equal(
  nrow(cross_talent_history),
  0L,
  "A video ID from another talent must not return history."
)

history <- dashboard_load_individual_video_history(
  fixture_path,
  talent_code = "T1",
  video_id = "VIDEO_A"
)
prepared <- dashboard_individual_video_prepare(history)
assert_equal(prepared$video_age_days, c(0L, 10L, 15L), "Video age is incorrect.")
assert_equal(prepared$net_subscribers, c(1, 4, 10), "Net subscribers are incorrect.")
assert_equal(prepared$views_change, c(NA_real_, 30, 60), "View changes are incorrect.")

view_series <- dashboard_individual_video_metric_series(prepared, "views")
assert_equal(
  view_series$change_per_day,
  c(NA_real_, 3, 12),
  "Elapsed-day-normalized view changes are incorrect."
)
summary <- dashboard_individual_video_summary(prepared, "views")
assert_equal(summary$latest_value, 100, "Latest value is incorrect.")
assert_equal(summary$total_change, 90, "Selected-window change is incorrect.")
assert_equal(summary$average_change_per_day, 6, "Average daily change is incorrect.")
assert_equal(summary$snapshot_count, 3L, "Summary snapshot count is incorrect.")

metric_definitions <- dashboard_individual_video_metric_definitions()
assert_true(
  all(!is.na(metric_definitions$definition) & nzchar(metric_definitions$definition)),
  "Every selectable metric should have a definition."
)
average_duration_definition <- dashboard_individual_video_metric_definition(
  "average_view_duration"
)
assert_true(
  inherits(average_duration_definition, "shiny.tag"),
  "A selected metric definition should return an HTML tag."
)

snapshot_table <- dashboard_individual_video_snapshot_table(prepared, "views")
assert_equal(
  snapshot_table$`Snapshot Date`,
  rev(analytics_fixture$snapshot_date[1:3]),
  "The detail table should put the latest snapshot first."
)

scatter_plot <- dashboard_individual_video_scatter_plot(
  prepared,
  x_metric = "views",
  y_metric = "average_view_percentage"
)
assert_true(
  inherits(scatter_plot, "plotly"),
  "The plot builder should return a Plotly widget."
)
scatter_built <- plotly::plotly_build(scatter_plot)
assert_equal(
  length(scatter_built$x$frames),
  3L,
  "The plot builder should create one animation frame per paired snapshot."
)
assert_equal(
  vapply(scatter_built$x$frames, `[[`, character(1), "name"),
  format(analytics_fixture$snapshot_date[1:3], "%Y-%m-%d"),
  "Animation frames should follow snapshot order."
)

descriptive_statistics <- dashboard_individual_video_descriptive_statistics(
  prepared,
  c("views", "average_view_percentage")
)
view_statistics <- descriptive_statistics$variables %>%
  dplyr::filter(.data$metric == "views")
assert_equal(view_statistics$observations[[1]], 3L, "View observation count is incorrect.")
assert_equal(view_statistics$minimum[[1]], 10, "View minimum is incorrect.")
assert_equal(view_statistics$mean[[1]], 50, "View mean is incorrect.")
assert_equal(view_statistics$median[[1]], 40, "View median is incorrect.")
assert_equal(view_statistics$maximum[[1]], 100, "View maximum is incorrect.")
assert_equal(
  view_statistics$standard_deviation[[1]],
  stats::sd(c(10, 40, 100)),
  "View standard deviation is incorrect."
)
assert_equal(
  descriptive_statistics$paired_observations,
  3L,
  "Paired snapshot count is incorrect."
)
assert_equal(
  descriptive_statistics$pearson_correlation,
  stats::cor(c(10, 40, 100), c(40, 45, 50)),
  "Pearson correlation is incorrect."
)
statistics_panel <- dashboard_individual_video_statistics_panel(
  prepared,
  c("views", "average_view_percentage")
)
assert_true(
  inherits(statistics_panel, "shiny.tag"),
  "The descriptive-statistics panel should return an HTML tag."
)

cat("individual video dashboard tests passed\n")
