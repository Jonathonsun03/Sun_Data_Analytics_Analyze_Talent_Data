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
  subtitle_unit_key = c("subtitle-1", "subtitle-2"),
  video_id = c("video-1", "video-1"),
  channel_id = c("channel-1", "channel-1"),
  talent_code = c("TST1", "TST1"),
  sequence_number = c(1L, 2L),
  subtitle_start = c("00:00:00.000", "00:00:01.000"),
  subtitle_end = c("00:00:02.000", "00:00:03.000"),
  subtitle_text = c("hello world", "hello world welcome back"),
  subtitle_language = c("en", "en"),
  subtitle_track_type = c("manual", "manual"),
  source_file_id = c("source-1", "source-1"),
  source_path = c("/Original/example.csv", "/Original/example.csv")
)

normalized <- normalize_subtitle_units_for_reconstruction(raw_units)
blocks <- build_punctuation_blocks(
  normalized,
  target_words = 175L,
  max_words = 200L,
  talent_name = "Test talent"
)
assert_equal(nrow(blocks), 1L, "Expected one punctuation block")
assert_equal(
  blocks$source_subtitle_unit_keys[[1]],
  raw_units$subtitle_unit_key,
  "Punctuation block lost raw subtitle keys"
)

mock_punctuate <- function(
    text,
    url,
    timeout_sec,
    include_model) {
  list(text = "Hello world. Welcome back.", model = "fullstop-test")
}

sentences <- reconstruct_sentence_units(
  blocks,
  punctuate_fn = mock_punctuate
)
assert_equal(nrow(sentences), 2L, "Expected two reconstructed sentences")
assert_true(
  all(vapply(
    sentences$source_subtitle_unit_keys,
    identical,
    logical(1),
    raw_units$subtitle_unit_key
  )),
  "Sentence rows lost block-level raw subtitle lineage"
)

database_path <- tempfile("subtitle-sentence-publish-", fileext = ".duckdb")
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path)
on.exit({
  DBI::dbDisconnect(con, shutdown = TRUE)
  unlink(database_path)
}, add = TRUE)

dry_run <- publish_subtitle_reconstruction(
  con,
  raw_units = raw_units,
  blocks = blocks,
  sentence_units = sentences,
  source_scope = "notebook_sample",
  dry_run = TRUE
)
assert_true(!dry_run$published, "Dry run unexpectedly published rows")
assert_true(
  !DBI::dbExistsTable(
    con,
    DBI::Id(schema = "text", table = "subtitle_sentence_units")
  ),
  "Dry run unexpectedly created the derived table"
)

first_publish <- publish_subtitle_reconstruction(
  con,
  raw_units = raw_units,
  blocks = blocks,
  sentence_units = sentences,
  source_scope = "notebook_sample",
  dry_run = FALSE
)
assert_true(first_publish$published, "Publisher did not report success")
assert_equal(
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM text.subtitle_sentence_units")$n[[1]],
  2,
  "Publisher wrote the wrong sentence count"
)
assert_equal(
  DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM ops.subtitle_reconstruction_blocks"
  )$n[[1]],
  1,
  "Publisher wrote the wrong checkpoint count"
)

second_publish <- publish_subtitle_reconstruction(
  con,
  raw_units = raw_units,
  blocks = blocks,
  sentence_units = sentences,
  source_scope = "notebook_sample",
  dry_run = FALSE
)
assert_true(second_publish$published, "Idempotent republish did not succeed")
assert_equal(
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM text.subtitle_sentence_units")$n[[1]],
  2,
  "Idempotent republish duplicated sentence rows"
)
assert_equal(
  DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM ops.subtitle_reconstruction_blocks"
  )$n[[1]],
  1,
  "Idempotent republish duplicated checkpoint rows"
)
assert_equal(
  DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM ops.pipeline_runs WHERE status = 'completed'"
  )$n[[1]],
  2,
  "Completed pipeline runs were not recorded"
)

persisted <- load_published_subtitle_sentences(
  con,
  video_id = "video-1",
  source_scope = "notebook_sample"
)
assert_equal(
  persisted$sentence_text,
  sentences$text,
  "Read-back sentence text does not match generated text"
)
assert_true(
  all(persisted$source_scope == "notebook_sample"),
  "Notebook sample was not isolated from full-track output"
)

message("subtitle sentence DuckDB publication tests passed")
