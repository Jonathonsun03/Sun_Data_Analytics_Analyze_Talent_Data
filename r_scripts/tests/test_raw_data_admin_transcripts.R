suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})
source("r_scripts/lib/duckdb/db_connect.R")
source("r_scripts/lib/dashboard/data/raw_data_admin.R")

legacy_details <- raw_data_admin_transcript_run_details(paste(
  "selected=1730; examined=50; new_started=50; published=0; current=0; failed=50;",
  "requested_blocks=0; reused_blocks=0; [1/1730] video - FAILED: example"
))
stopifnot(
  identical(
    legacy_details$counts,
    paste(
      "Videos awaiting processing at batch start: 1,730",
      "Videos attempted: 50",
      "Videos completed: 0",
      "Videos already current and skipped: 0",
      "Videos failed: 50",
      "Model blocks requested: 0",
      "Model blocks reused: 0",
      sep = " • "
    )
  ),
  identical(legacy_details$issues, "[1/1730] video - FAILED: example")
)
stopifnot(
  raw_data_admin_transcript_run_metric(
    "selected=1730; examined=50; published=0",
    c("attempted", "new_started", "examined")
  ) == 50,
  is.na(raw_data_admin_transcript_run_metric(NA_character_, "attempted"))
)

current_details <- raw_data_admin_transcript_run_details(paste(
  "backlog_at_start=1730; batch_limit=50; attempted=50; completed=0;",
  "skipped_current=0; failed=50; requested_blocks=0; reused_blocks=0"
))
stopifnot(
  grepl("Maximum videos this batch: 50", current_details$counts, fixed = TRUE),
  grepl("Videos attempted: 50", current_details$counts, fixed = TRUE),
  is.na(current_details$issues)
)

local({
  path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(path), add = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  DBI::dbExecute(con, "CREATE SCHEMA ops")
  DBI::dbExecute(con, "CREATE SCHEMA text")
  DBI::dbExecute(con, "CREATE SCHEMA catalog")
  DBI::dbExecute(con, paste(
    "CREATE TABLE ops.pipeline_runs (pipeline_run_id VARCHAR, pipeline_name VARCHAR,",
    "started_at TIMESTAMP, completed_at TIMESTAMP, status VARCHAR, error_summary VARCHAR)"
  ))
  DBI::dbExecute(con, "CREATE TABLE catalog.videos (video_id VARCHAR, talent_code VARCHAR, title VARCHAR)")
  DBI::dbExecute(con, "CREATE TABLE catalog.talents (talent_code VARCHAR, talent_name VARCHAR)")
  DBI::dbExecute(con, "INSERT INTO catalog.videos VALUES ('v1', 'T1', 'Proof <script>'), ('v2', 'T1', 'Failed video')")
  DBI::dbExecute(con, "INSERT INTO catalog.talents VALUES ('T1', 'Talent One')")
  DBI::dbDisconnect(con, shutdown = TRUE)
  stopifnot(nrow(raw_data_admin_transcript_runs(path)) == 0L)
  stopifnot(nrow(raw_data_admin_transcript_tracks(path, "missing")) == 0L)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  DBI::dbExecute(con, paste(
    "INSERT INTO ops.pipeline_runs VALUES",
    "('old', 'subtitle_sentence_backfill', '2026-09-10', '2026-09-10 00:02:00', 'completed', NULL),",
    "('new', 'subtitle_sentence_backfill', '2026-09-11', '2026-09-11 00:03:00', 'failed', 'failed=1'),",
    "('new-publication', 'subtitle_sentence_reconstruction', '2026-09-11 00:01:00',",
    "'2026-09-11 00:01:30', 'completed', NULL),",
    "('other', 'title_classification', '2026-09-12', NULL, 'running', NULL)"
  ))
  DBI::dbExecute(con, paste(
    "CREATE TABLE text.subtitle_sentence_units AS SELECT",
    "'key'::VARCHAR AS sentence_unit_key, 'new-publication'::VARCHAR AS pipeline_run_id,",
    "'v1'::VARCHAR AS video_id, 'T1'::VARCHAR AS talent_code,",
    "'en'::VARCHAR AS subtitle_language, 'auto'::VARCHAR AS subtitle_track_type,",
    "'full_track'::VARCHAR AS source_scope, 'v1'::VARCHAR AS pipeline_version,",
    "1 AS block_number, 2 AS sentence_number, 1.0 AS start_sec, 2.0 AS end_sec,",
    "'Second sentence.'::VARCHAR AS sentence_text,",
    "'block_approximate'::VARCHAR AS source_alignment_status, TRUE AS timestamps_approximate"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO text.subtitle_sentence_units SELECT 'first', pipeline_run_id, video_id, talent_code,",
    "subtitle_language, subtitle_track_type, source_scope, pipeline_version,",
    "1, 1, 0, 1, 'First sentence.', source_alignment_status, timestamps_approximate",
    "FROM text.subtitle_sentence_units"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO text.subtitle_sentence_units SELECT 'old', 'old', video_id, talent_code,",
    "subtitle_language, subtitle_track_type, source_scope, pipeline_version,",
    "1, 1, 0, 1, 'Old text.', source_alignment_status, timestamps_approximate",
    "FROM text.subtitle_sentence_units LIMIT 1"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO text.subtitle_sentence_units SELECT 'fr', 'new', video_id, talent_code,",
    "'fr', subtitle_track_type, source_scope, pipeline_version,",
    "1, 1, 0, 1, 'French track.', source_alignment_status, timestamps_approximate",
    "FROM text.subtitle_sentence_units LIMIT 1"
  ))
  DBI::dbExecute(con, paste(
    "CREATE TABLE ops.subtitle_reconstruction_blocks AS SELECT",
    "'new-publication'::VARCHAR AS pipeline_run_id, 'v2'::VARCHAR AS video_id,",
    "'T1'::VARCHAR AS talent_code,",
    "NULL::VARCHAR AS subtitle_language, 'auto'::VARCHAR AS subtitle_track_type,",
    "'full_track'::VARCHAR AS source_scope, 'v1'::VARCHAR AS pipeline_version,",
    "'failed'::VARCHAR AS status, 'Timeout'::VARCHAR AS error_summary"
  ))
  DBI::dbDisconnect(con, shutdown = TRUE)
  runs <- raw_data_admin_transcript_runs(path)
  stopifnot(identical(runs$pipeline_run_id, c("new", "old")), runs$duration_seconds[[1]] == 180)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  stopifnot(identical(
    raw_data_admin_transcript_related_run_ids(con, "new"),
    c("new", "new-publication")
  ))
  DBI::dbDisconnect(con, shutdown = TRUE)
  tracks <- raw_data_admin_transcript_tracks(path, "new")
  stopifnot(nrow(tracks) == 3L, tracks$failed_blocks[tracks$video_id == "v2"] == 1)
  stopifnot(tracks$block_errors[tracks$video_id == "v2"] == "Timeout")
  track <- tracks[tracks$video_id == "v1" & tracks$subtitle_language %in% "en", ]
  stopifnot(track$sentences == 2, track$title == "Proof <script>", track$talent_name == "Talent One")
  sentences <- raw_data_admin_transcript_text(path, "new", track)
  stopifnot(identical(sentences$sentence_text, c("First sentence.", "Second sentence.")))
  stopifnot(nrow(raw_data_admin_transcript_text(path, "missing", track)) == 0)
  stopifnot(nrow(raw_data_admin_transcript_text(path, "new", tracks[tracks$video_id == "v2", ])) == 0)
  stopifnot(nrow(raw_data_admin_transcript_tracks(path, "missing")) == 0)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  DBI::dbExecute(con, paste(
    "CREATE TABLE ops.subtitle_backfill_attempts (",
    "attempt_id VARCHAR, batch_pipeline_run_id VARCHAR, candidate_position BIGINT,",
    "video_id VARCHAR, talent_code VARCHAR, subtitle_language VARCHAR,",
    "subtitle_track_type VARCHAR, source_scope VARCHAR, pipeline_version VARCHAR,",
    "raw_rows BIGINT, started_at TIMESTAMP, completed_at TIMESTAMP, status VARCHAR,",
    "sentences BIGINT, blocks BIGINT, requested_blocks BIGINT, reused_blocks BIGINT,",
    "publication_pipeline_run_id VARCHAR, error_summary VARCHAR)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO ops.subtitle_backfill_attempts VALUES",
    "('a1', 'new', 1, 'v1', 'T1', 'en', 'auto', 'full_track', 'v1', 10,",
    "'2026-09-11 00:00:10', '2026-09-11 00:01:30', 'published', 2, 1, 1, 0,",
    "'new-publication', NULL),",
    "('a2', 'new', 2, 'v2', 'T1', NULL, 'auto', 'full_track', 'v1', 5,",
    "'2026-09-11 00:01:31', '2026-09-11 00:02:00', 'failed', NULL, NULL, NULL, NULL,",
    "NULL, 'Speaker service unavailable')"
  ))
  DBI::dbDisconnect(con, shutdown = TRUE)

  attempt_tracks <- raw_data_admin_transcript_tracks(path, "new")
  stopifnot(
    nrow(attempt_tracks) == 2L,
    identical(attempt_tracks$video_id, c("v1", "v2")),
    identical(attempt_tracks$result, c("Published", "Failed")),
    nrow(raw_data_admin_filter_transcript_tracks(attempt_tracks, "v2")) == 1L,
    nrow(raw_data_admin_filter_transcript_tracks(attempt_tracks, "talent one")) == 2L,
    nrow(raw_data_admin_filter_transcript_tracks(attempt_tracks, "unavailable")) == 1L
  )
})
cat("Transcript administration checks passed.\n")
