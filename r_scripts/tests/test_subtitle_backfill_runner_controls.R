source(file.path("r_scripts", "lib", "utils", "datalake_root.r"))
source(file.path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(file.path(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "clean_subtitles.R"
))
source(file.path(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "punctuation_client.R"
))
source(file.path(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "subtitle_units.R"
))
source(file.path("r_scripts", "lib", "duckdb", "subtitle_sentence_schema.R"))
source(file.path("r_scripts", "lib", "duckdb", "subtitle_sentence_publish.R"))
source(file.path("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_tracks.R"))
source(file.path("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_database.R"))
source(file.path("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_checkpoints.R"))
source(file.path("r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_inference.R"))
source(file.path(
  "r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_reconstruction.R"
))

assert_equal <- function(actual, expected, message) {
  comparison <- all.equal(actual, expected, check.attributes = FALSE)
  if (!isTRUE(comparison)) {
    stop(message, ": ", paste(comparison, collapse = " | "), call. = FALSE)
  }
}

test_root <- tempfile("subtitle-runner-controls-")
db_path <- file.path(test_root, "Data_lakehouse", "talent_lakehouse.duckdb")
dir.create(dirname(db_path), recursive = TRUE)
on.exit(unlink(test_root, recursive = TRUE), add = TRUE)

make_raw_track <- function(video_id) {
  tibble::tibble(
    subtitle_unit_key = paste0(video_id, "-1"),
    video_id = video_id,
    channel_id = "channel-1",
    talent_code = "TST1",
    sequence_number = 1L,
    subtitle_start = "00:00:00.000",
    subtitle_end = "00:00:02.000",
    subtitle_text = paste("hello from", video_id),
    subtitle_language = "en",
    subtitle_track_type = "manual",
    source_file_id = "source-1",
    source_path = paste0("/Original/", video_id, ".csv")
  )
}

video_ids <- c("a-current", "b-new", "c-current", "d-new")
raw_tracks <- lapply(video_ids, make_raw_track)
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
init_subtitle_sentence_schema(con)
DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS catalog")
DBI::dbWriteTable(
  con,
  DBI::Id(schema = "text", table = "subtitle_units"),
  dplyr::bind_rows(raw_tracks)
)
DBI::dbWriteTable(
  con,
  DBI::Id(schema = "catalog", table = "videos"),
  data.frame(video_id = video_ids, title = video_ids, content_type = "video")
)

build_track_result <- function(raw) {
  blocks <- build_punctuation_blocks(normalize_subtitle_units_for_reconstruction(raw))
  sentences <- sentence_units_from_block(
    blocks,
    paste0(blocks$model_input_text[[1]], "."),
    "fullstop-test"
  )
  list(blocks = blocks, sentences = sentences)
}

for (index in c(1L, 3L)) {
  built <- build_track_result(raw_tracks[[index]])
  publish_subtitle_reconstruction(
    con,
    raw_tracks[[index]],
    built$blocks,
    built$sentences,
    dry_run = FALSE
  )
}
pending_tracks <- list_subtitle_backfill_tracks(
  con,
  pipeline_version = "subtitle_sentence_v2",
  source_scope = "full_track",
  exclude_current = TRUE
)
assert_equal(
  pending_tracks$video_id,
  c("b-new", "d-new"),
  "Initial candidate query did not exclude current tracks"
)
DBI::dbDisconnect(con, shutdown = TRUE)

no_work_status <- system2(
  file.path(R.home("bin"), "Rscript"),
  c(
    "--vanilla",
    "r_scripts/run/Subtitle_clean/backfill_subtitle_sentences.R"
  ),
  env = c(
    "TALENT_LOAD_REPO_ENV=false",
    paste0("TALENT_DATALAKE_ROOT=", test_root),
    "SUBTITLE_BACKFILL_DRY_RUN=true",
    "SUBTITLE_BACKFILL_VIDEO_ID=a-current"
  ),
  stdout = FALSE,
  stderr = FALSE
)
assert_equal(no_work_status, 0L, "An empty filtered selection should succeed")

for (index in c(2L, 4L)) {
  raw <- raw_tracks[[index]]
  built <- build_track_result(raw)
  checkpoint <- subtitle_backfill_checkpoint_frame(
    raw,
    built$blocks,
    subtitle_sentence_source_checksum(raw),
    "local-test-run",
    "subtitle_sentence_v2",
    "full_track",
    "complete",
    1L,
    paste0(built$blocks$model_input_text[[1]], "."),
    "fullstop-test"
  )
  save_subtitle_backfill_checkpoints(
    checkpoint,
    subtitle_backfill_checkpoint_path(db_path, raw$video_id[[1]])
  )
}

runner_status <- system2(
  file.path(R.home("bin"), "Rscript"),
  c(
    "--vanilla",
    "r_scripts/run/Subtitle_clean/backfill_subtitle_sentences.R"
  ),
  env = c(
    "TALENT_LOAD_REPO_ENV=false",
    paste0("TALENT_DATALAKE_ROOT=", test_root),
    "SUBTITLE_BACKFILL_DRY_RUN=false",
    "SUBTITLE_BACKFILL_MAX_VIDEOS=0",
    "SUBTITLE_BACKFILL_MAX_NEW_VIDEOS=1",
    "SUBTITLE_SPEAKER_TURNS_ENABLED=false",
    "SUBTITLE_PUNCTUATION_URL=https://example.com/v1/punctuate"
  ),
  stdout = FALSE,
  stderr = FALSE
)
assert_equal(runner_status, 0L, "Controlled runner failed")

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
published <- DBI::dbGetQuery(
  con,
  paste(
    "SELECT video_id FROM text.subtitle_sentence_units",
    "WHERE video_id IN ('b-new', 'd-new') ORDER BY video_id"
  )
)$video_id
summary <- DBI::dbGetQuery(
  con,
  paste(
    "SELECT error_summary FROM ops.pipeline_runs",
    "WHERE pipeline_name = 'subtitle_sentence_backfill'",
    "ORDER BY started_at DESC LIMIT 1"
  )
)$error_summary[[1]]
attempts <- DBI::dbGetQuery(
  con,
  paste(
    "SELECT candidate_position, video_id, status, sentences, blocks,",
    "requested_blocks, reused_blocks, publication_pipeline_run_id, error_summary",
    "FROM ops.subtitle_backfill_attempts ORDER BY candidate_position"
  )
)
DBI::dbDisconnect(con, shutdown = TRUE)

assert_equal(published, "b-new", "--max-new-videos did not stop after one new video")
stopifnot(
  nrow(attempts) == 1L,
  attempts$candidate_position[[1]] == 1L,
  attempts$video_id[[1]] == "b-new",
  attempts$status[[1]] == "published",
  attempts$sentences[[1]] == 1L,
  attempts$blocks[[1]] == 1L,
  attempts$requested_blocks[[1]] == 0L,
  attempts$reused_blocks[[1]] == 1L,
  !is.na(attempts$publication_pipeline_run_id[[1]]),
  is.na(attempts$error_summary[[1]])
)
if (!grepl(
  "backlog_at_start=2; batch_limit=1; attempted=1; completed=1; skipped_current=0",
  summary,
  fixed = TRUE
)) {
  stop("Filtered candidate accounting was incorrect: ", summary, call. = FALSE)
}

cat("subtitle backfill runner control tests passed\n")
