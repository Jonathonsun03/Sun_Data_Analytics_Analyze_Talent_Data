source(file.path(
  "r_scripts", "lib", "utils", "datalake_root.r"
))
source(file.path(
  "r_scripts", "lib", "duckdb", "db_connect.R"
))
source(file.path(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "clean_subtitles.R"
))
source(file.path(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "punctuation_client.R"
))
source(file.path(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "subtitle_units.R"
))
source(file.path(
  "r_scripts", "lib", "duckdb", "subtitle_sentence_schema.R"
))
source(file.path(
  "r_scripts", "lib", "duckdb", "subtitle_sentence_publish.R"
))
source(file.path(
  "r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_tracks.R"
))
source(file.path(
  "r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_database.R"
))
source(file.path(
  "r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_checkpoints.R"
))
source(file.path(
  "r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_inference.R"
))
source(file.path(
  "r_scripts", "lib", "subtitle_backfill", "subtitle_backfill_reconstruction.R"
))

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_equal <- function(actual, expected, message) {
  comparison <- all.equal(actual, expected, check.attributes = FALSE)
  if (!isTRUE(comparison)) {
    stop(message, ": ", paste(comparison, collapse = " | "), call. = FALSE)
  }
}

raw_units <- tibble::tibble(
  subtitle_unit_key = paste0("subtitle-", 1:4),
  video_id = rep("backfill-video", 4),
  channel_id = rep("channel-1", 4),
  talent_code = rep("TST1", 4),
  sequence_number = 1:4,
  subtitle_start = c(
    "00:00:00.000", "00:00:01.000", "00:00:03.000", "00:00:04.000"
  ),
  subtitle_end = c(
    "00:00:02.000", "00:00:03.000", "00:00:04.000", "00:00:06.000"
  ),
  subtitle_text = c(
    "hello world",
    "hello world welcome back",
    "new topic today",
    "new topic today goodbye now"
  ),
  subtitle_language = rep("en", 4),
  subtitle_track_type = rep("manual", 4),
  source_file_id = rep("source-1", 4),
  source_path = rep("/Original/example.csv", 4)
)
normalized <- normalize_subtitle_units_for_reconstruction(raw_units)
blocks <- build_punctuation_blocks(
  normalized,
  target_words = 4L,
  max_words = 5L,
  talent_name = "Test talent"
)
assert_true(nrow(blocks) >= 2L, "Test fixture must produce multiple blocks")

checkpoint_dir <- tempfile("subtitle-backfill-checkpoints-")
Sys.setenv(SUBTITLE_BACKFILL_CHECKPOINT_DIR = checkpoint_dir)

database_path <- tempfile("subtitle-backfill-", fileext = ".duckdb")
initial_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path)
init_subtitle_sentence_schema(initial_con)
DBI::dbExecute(initial_con, "CREATE SCHEMA IF NOT EXISTS catalog")
DBI::dbWriteTable(initial_con, DBI::Id(schema = "text", table = "subtitle_units"), raw_units)
DBI::dbWriteTable(initial_con, DBI::Id(schema = "catalog", table = "videos"),
                 data.frame(video_id = "backfill-video", title = "Test", content_type = "video"))
DBI::dbDisconnect(initial_con, shutdown = TRUE)

mock_calls <- 0L
mock_punctuate <- function(text, url, timeout_sec, include_model) {
  mock_calls <<- mock_calls + 1L
  list(text = paste0(text, "."), model = "fullstop-test")
}

first_run_id <- start_subtitle_backfill_run(database_path)
first_result <- reconstruct_subtitle_track_with_checkpoints(
  db_path = database_path,
  raw_units = raw_units,
  blocks = blocks,
  pipeline_run_id = first_run_id,
  max_attempts = 2L,
  request_pause_sec = 0,
  punctuate_fn = mock_punctuate
)
finish_subtitle_backfill_run(database_path, first_run_id, "completed")

assert_equal(
  first_result$requested_blocks,
  nrow(blocks),
  "First run did not request every block"
)
assert_equal(mock_calls, nrow(blocks), "Mock request count was incorrect")

check_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path, read_only = TRUE)
checkpoint_count <- DBI::dbGetQuery(
  check_con,
  paste(
    "SELECT COUNT(*) AS n FROM ops.subtitle_reconstruction_blocks",
    "WHERE video_id = 'backfill-video' AND source_scope = 'full_track'",
    "AND status = 'complete'"
  )
)$n[[1]]
sentence_count <- DBI::dbGetQuery(
  check_con,
  paste(
    "SELECT COUNT(*) AS n FROM text.subtitle_sentence_units",
    "WHERE video_id = 'backfill-video' AND source_scope = 'full_track'"
  )
)$n[[1]]
DBI::dbDisconnect(check_con, shutdown = TRUE)
assert_equal(checkpoint_count, nrow(blocks), "Complete checkpoints were not saved")
assert_equal(sentence_count, nrow(blocks), "Final sentence rows were not published")

second_run_id <- start_subtitle_backfill_run(database_path)
second_result <- reconstruct_subtitle_track_with_checkpoints(
  db_path = database_path,
  raw_units = raw_units,
  blocks = blocks,
  pipeline_run_id = second_run_id,
  max_attempts = 2L,
  request_pause_sec = 0,
  punctuate_fn = function(...) stop("A complete checkpoint was requested again")
)
finish_subtitle_backfill_run(database_path, second_run_id, "completed")

assert_equal(second_result$requested_blocks, 0L, "Resume repeated model requests")
assert_equal(second_result$reused_blocks, nrow(blocks), "Resume did not reuse every block")

message("subtitle sentence checkpoint/resume tests passed")

# Inline speaker changes share a source caption interval. Multiple sentences
# per block must be merged chronologically without changing text or lineage.
overlap_raw <- raw_units[1L, , drop = FALSE]
overlap_raw$video_id <- "overlapping-speakers"
overlap_raw$subtitle_end <- "00:00:06.000"
overlap_raw$subtitle_text <- "Hello world welcome back >> New topic today goodbye now"
overlap_blocks <- build_punctuation_blocks(
  normalize_subtitle_units_for_reconstruction(overlap_raw)
)
overlap_punctuate <- function(text, ...) {
  list(
    text = if (startsWith(text, "Hello")) {
      "Hello world. Welcome back."
    } else {
      "New topic today. Goodbye now."
    },
    model = "fullstop-test"
  )
}
overlap_sentences <- reconstruct_sentence_units(
  overlap_blocks,
  punctuate_fn = overlap_punctuate
)
validate_subtitle_reconstruction(overlap_raw, overlap_blocks, overlap_sentences)
assert_true(!is.unsorted(overlap_sentences$start_sec), "Speaker sentences are not chronological")
assert_equal(overlap_sentences$block_number, c(1L, 2L, 1L, 2L), "Overlapping blocks were not merged by time")
source_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path)
DBI::dbWriteTable(source_con, DBI::Id(schema = "text", table = "subtitle_units"), overlap_raw, append = TRUE)
DBI::dbDisconnect(source_con, shutdown = TRUE)
overlap_run <- start_subtitle_backfill_run(database_path)
overlap_result <- reconstruct_subtitle_track_with_checkpoints(
  database_path, overlap_raw, overlap_blocks, overlap_run,
  punctuate_fn = overlap_punctuate
)
assert_equal(overlap_result$requested_blocks, 2L, "Speaker fixture did not publish fresh blocks")
resume_result <- reconstruct_subtitle_track_with_checkpoints(
  database_path, overlap_raw, overlap_blocks, overlap_run,
  punctuate_fn = function(...) stop("Cached speaker blocks must not call inference")
)
assert_equal(resume_result$reused_blocks, 2L, "Speaker checkpoints were not reused")
assert_equal(resume_result$requested_blocks, 0L, "Speaker resume issued inference requests")
finish_subtitle_backfill_run(database_path, overlap_run, "completed")
cat("overlapping speaker checkpoint publication tests passed\n")

# Each scenario uses real raw rows in a disposable canonical database.
make_test_track <- function(video_id) {
  raw <- raw_units
  raw$video_id <- video_id
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path)
  DBI::dbWriteTable(con, DBI::Id(schema = "text", table = "subtitle_units"), raw, append = TRUE)
  DBI::dbDisconnect(con, shutdown = TRUE)
  list(raw = raw, blocks = build_punctuation_blocks(
    normalize_subtitle_units_for_reconstruction(raw), target_words = 4L, max_words = 5L
  ))
}

run_test_track <- function(track, punctuate, ...) {
  reconstruct_subtitle_track_with_checkpoints(
    database_path, track$raw, track$blocks, "test-filesystem-checkpoints",
    checkpoints = data.frame(), punctuate_fn = punctuate, ...
  )
}

# A different R process can WRITE during every model call. This detects live
# connections/driver locks even if the inference helper never calls DBI itself.
external_write <- function() {
  code <- paste0(
    "con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ",
    deparse(database_path), "); ",
    "DBI::dbExecute(con, 'CREATE TABLE IF NOT EXISTS inference_concurrency_test (n INTEGER)'); ",
    "DBI::dbExecute(con, 'INSERT INTO inference_concurrency_test VALUES (1)'); ",
    "DBI::dbDisconnect(con, shutdown = TRUE)"
  )
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(code)), stdout = FALSE, stderr = FALSE
  )
  assert_equal(status, 0L, "Another writer was blocked during inference")
}

crash_track <- make_test_track("local-crash-resume")
crash_calls <- 0L
crash_result <- tryCatch(run_test_track(crash_track, function(text, ...) {
  external_write()
  crash_calls <<- crash_calls + 1L
  if (crash_calls == 2L) stop("Simulated interrupted model call")
  list(text = paste0(text, "."), model = "fullstop-test")
}, max_attempts = 1L), error = function(e) e)
assert_true(inherits(crash_result, "error"), "Interrupted inference unexpectedly succeeded")
local_path <- subtitle_backfill_checkpoint_path(database_path, "local-crash-resume")
assert_equal(nrow(readRDS(local_path)), 1L, "Successful block was not saved before interruption")
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path, read_only = TRUE)
assert_equal(DBI::dbGetQuery(con, paste(
  "SELECT COUNT(*) n FROM ops.subtitle_reconstruction_blocks",
  "WHERE video_id = 'local-crash-resume'"
))$n[[1]], 0, "Inference wrote checkpoints to DuckDB")
DBI::dbDisconnect(con, shutdown = TRUE)
resume_calls <- 0L
resumed <- run_test_track(crash_track, function(text, ...) {
  external_write()
  resume_calls <<- resume_calls + 1L
  list(text = paste0(text, "."), model = "fullstop-test")
})
assert_equal(resumed$reused_blocks, 1L, "Restart did not load the file checkpoint")
assert_equal(resume_calls, nrow(crash_track$blocks) - 1L, "Restart repeated a completed model call")

# Fixed retries happen only around the final database operation. Simulated
# lock acquisition failures must not re-enter inference or retry data errors.
local({
  original_connect <- duckdb_connect
  original_sleep <- Sys.sleep
  on.exit({
    assign("duckdb_connect", original_connect, envir = .GlobalEnv)
    assign("Sys.sleep", original_sleep, envir = .GlobalEnv)
  })
  retry_track <- make_test_track("locked-publication")
  attempts <- 0L
  model_calls <- 0L
  delays <- numeric()
  assign("duckdb_connect", function(..., read_only = FALSE) {
    attempts <<- attempts + 1L
    if (attempts <= 2L) stop("IO Error: Could not set lock on file: Conflicting lock is held")
    original_connect(..., read_only = read_only)
  }, envir = .GlobalEnv)
  assign("Sys.sleep", function(time) delays <<- c(delays, time), envir = .GlobalEnv)
  result <- run_test_track(retry_track, function(text, ...) {
    model_calls <<- model_calls + 1L
    list(text = paste0(text, "."), model = "fullstop-test")
  })
  assert_equal(attempts, 3L, "Final publication did not retry the lock")
  assert_equal(delays, c(5, 5), "Publication retry did not use a fixed delay")
  assert_equal(model_calls, nrow(retry_track$blocks), "Publication retry repeated inference")

  attempts <- 0L
  assign("duckdb_connect", function(...) {
    attempts <<- attempts + 1L
    stop("Invalid source schema")
  }, envir = .GlobalEnv)
  error <- tryCatch(run_test_track(retry_track, function(...) stop("Must reuse cache")), error = identity)
  assert_true(inherits(error, "error"), "Non-lock error unexpectedly succeeded")
  assert_equal(attempts, 1L, "Non-lock failure was retried")

  attempts <- 0L
  assign("duckdb_connect", function(...) {
    attempts <<- attempts + 1L
    stop("Could not set lock on file")
  }, envir = .GlobalEnv)
  error <- tryCatch(run_test_track(retry_track, function(...) stop("Must reuse cache")), error = identity)
  assert_equal(attempts, 12L, "Lock retry limit was not respected")
  assert_true(inherits(error, "error"), "Persistent lock unexpectedly succeeded")
  assign("duckdb_connect", original_connect, envir = .GlobalEnv)
  recovered <- run_test_track(retry_track, function(...) stop("Must reuse cache"))
  assert_equal(recovered$requested_blocks, 0L, "Recovery after a lock repeated inference")
})

changed_track <- make_test_track("source-changed")
changed <- FALSE
error <- tryCatch(run_test_track(changed_track, function(text, ...) {
  if (!changed) {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path)
    DBI::dbExecute(con, paste(
      "UPDATE text.subtitle_units SET subtitle_text = 'changed by ingestion'",
      "WHERE video_id = 'source-changed' AND sequence_number = 1"
    ))
    DBI::dbDisconnect(con, shutdown = TRUE)
    changed <<- TRUE
  }
  list(text = paste0(text, "."), model = "fullstop-test")
}), error = identity)
assert_true(inherits(error, "error") && grepl("Source track changed", conditionMessage(error)),
            "Changed source was not rejected")
assert_equal(nrow(readRDS(subtitle_backfill_checkpoint_path(database_path, "source-changed"))),
             nrow(changed_track$blocks), "Source conflict discarded completed inference")
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path, read_only = TRUE)
assert_equal(DBI::dbGetQuery(con, paste(
  "SELECT COUNT(*) n FROM text.subtitle_sentence_units WHERE video_id = 'source-changed'"
))$n[[1]], 0, "Stale source results were published")
DBI::dbDisconnect(con, shutdown = TRUE)
unlink(c(database_path, checkpoint_dir), recursive = TRUE)
Sys.unsetenv("SUBTITLE_BACKFILL_CHECKPOINT_DIR")
cat("filesystem checkpoint, concurrent writer, source checksum and publication retry tests passed\n")
