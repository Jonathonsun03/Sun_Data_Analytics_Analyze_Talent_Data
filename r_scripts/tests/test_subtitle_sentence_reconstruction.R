source(file.path(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "punctuation_client.R"
))
source(file.path(
  "r_scripts", "lib", "clean_data", "clean_subtitles", "subtitle_units.R"
))

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_equal <- function(actual, expected, message, tolerance = 1e-10) {
  comparison <- all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE)
  if (!isTRUE(comparison)) {
    stop(message, ": ", paste(comparison, collapse = " | "), call. = FALSE)
  }
}

parsed <- parse_punctuation_response(
  '{"model":"fullstop-test","text":"Hello, world."}'
)
assert_equal(parsed$text, "Hello, world.", "API response text was not parsed")
assert_equal(parsed$model, "fullstop-test", "API response model was not retained")

missing_text_failed <- tryCatch(
  {
    parse_punctuation_response('{"model":"fullstop-test"}')
    FALSE
  },
  error = function(e) TRUE
)
assert_true(missing_text_failed, "API responses without text should fail")

words <- function(prefix, count) {
  paste(paste0(prefix, seq_len(count)), collapse = " ")
}

overlap_parts <- deduplicate_caption_overlaps(c(
  "you're going to",
  "going to really enjoy this",
  "this is still natural",
  "what the what is that"
))
assert_equal(
  overlap_parts,
  c(
    "you're going to",
    "really enjoy this",
    "is still natural",
    "what the what is that"
  ),
  "Exact suffix/prefix caption overlap was not removed conservatively"
)
assert_equal(
  deduplicate_caption_overlaps(c("keep these words", "entirely different words"))[[2]],
  "entirely different words",
  "Non-overlapping captions should remain intact"
)
assert_equal(
  deduplicate_caption_overlaps("what the what is that"),
  "what the what is that",
  "Repeated speech inside one caption should not be edited"
)

normalized_input <- normalize_punctuation_model_input(
  ">> yeah.. you're, don't! I've: kept 3.14"
)
assert_equal(
  normalized_input,
  "yeah you're don't I've kept 3.14",
  "Model input normalization changed words or retained restorable punctuation"
)
assert_true(
  !grepl(">>", normalized_input, fixed = TRUE),
  "Model input normalization retained a speaker marker"
)
assert_equal(
  normalize_fullstop_punctuation("Yeah... Really?? Wait,, what?!"),
  "Yeah. Really? Wait, what?!",
  "Duplicate punctuation normalization changed mixed punctuation or missed runs"
)

capitalized <- split_punctuated_sentences("i don't know. \"really?\" yes!")
assert_equal(
  capitalized,
  c("I don't know.", "\"Really?\"", "Yes!"),
  "Sentence beginnings were not capitalized"
)

captions <- tibble::tibble(
  VideoID = c(rep("video-a", 4), rep("video-b", 2)),
  start_sec = c(0, 10, 20, 30, 0, 10),
  stop_sec = c(9, 19, 29, 39, 9, 19),
  Text = c(
    words("a", 60), words("b", 60), words("c", 60), words("d", 60),
    words("e", 60), words("f", 60)
  ),
  subtitle_language = "en"
)

blocks <- build_punctuation_blocks(captions, target_words = 150, max_words = 200)
assert_equal(nrow(blocks), 3L, "Unexpected number of punctuation blocks")
assert_true(
  all(blocks$word_count <= 200L),
  "Ordinary caption rows should not create blocks above the maximum"
)
assert_equal(
  blocks$block_number[blocks$video_id == "video-a"],
  c(1L, 2L),
  "Block numbering should restart within each video"
)
assert_true(
  !any(grepl("a1", blocks$original_text[blocks$video_id == "video-b"], fixed = TRUE)),
  "A punctuation block crossed a video boundary"
)

marker_block <- build_punctuation_blocks(tibble::tibble(
  VideoID = "video-marker",
  start_sec = 0,
  stop_sec = 2,
  Text = ">> you're ready, aren't you?",
  subtitle_language = "en"
))
assert_equal(
  marker_block$model_input_text,
  "you're ready aren't you",
  "Block model input retained speaker markers or existing punctuation"
)

sentences <- split_punctuated_sentences(
  "This is one. Is this two? This is three! A final fragment"
)
assert_equal(length(sentences), 4L, "Sentence splitting did not preserve four units")
assert_equal(sentences[[2]], "Is this two?", "Question punctuation was not preserved")

timestamp_block <- tibble::tibble(
  video_id = "video-a",
  talent_name = "Test Talent",
  block_number = 1L,
  start_sec = 10,
  end_sec = 30,
  original_text = "one two three four five six",
  word_count = 6L,
  subtitle_language = "en"
)
timestamp_units <- sentence_units_from_block(
  timestamp_block,
  "One two. Three four five? Six!",
  "fullstop-test"
)
assert_equal(timestamp_units$start_sec[[1]], 10, "First approximate timestamp changed")
assert_equal(utils::tail(timestamp_units$end_sec, 1), 30, "Last approximate timestamp changed")
assert_true(
  all(diff(timestamp_units$start_sec) >= 0) && all(diff(timestamp_units$end_sec) >= 0),
  "Approximate timestamps are not ordered"
)
assert_true(
  all(timestamp_units$end_sec >= timestamp_units$start_sec),
  "A sentence ends before it starts"
)

language_blocks <- dplyr::bind_rows(
  timestamp_block,
  dplyr::mutate(
    timestamp_block,
    video_id = "video-fr",
    block_number = 1L,
    subtitle_language = "fr"
  )
)
call_count <- 0L
mock_punctuate <- function(text, ...) {
  call_count <<- call_count + 1L
  list(text = paste0(text, "."), model = "fullstop-test")
}
language_units <- reconstruct_sentence_units(
  language_blocks,
  allow_unknown_language = FALSE,
  punctuate_fn = mock_punctuate
)
assert_equal(call_count, 1L, "Non-English blocks should bypass the punctuation API")
assert_true(
  identical(unique(language_units$video_id), "video-a"),
  "Non-English blocks should not produce MVP sentence units"
)

output_dir <- tempfile("subtitle-sentence-test-")
dir.create(output_dir)
on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
cleaned_sentinel <- file.path(output_dir, "cleaned-caption.csv")
writeLines("cleaned caption remains", cleaned_sentinel)
output_path <- file.path(output_dir, "Sentence_Units", "video-a.parquet")
write_sentence_units_parquet(timestamp_units, output_path)

assert_true(file.exists(output_path), "Sentence Parquet was not created separately")
assert_equal(
  readLines(cleaned_sentinel),
  "cleaned caption remains",
  "Writing sentence output modified the cleaned caption source"
)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
quoted_output <- as.character(DBI::dbQuoteString(con, output_path))
written <- DBI::dbGetQuery(con, paste0("SELECT * FROM read_parquet(", quoted_output, ")"))
assert_equal(nrow(written), nrow(timestamp_units), "Parquet row count changed")
assert_true(
  all(c("video_id", "sentence_number", "text") %in% names(written)),
  "Sentence Parquet is missing required fields"
)

cat("subtitle sentence reconstruction tests passed\n")
