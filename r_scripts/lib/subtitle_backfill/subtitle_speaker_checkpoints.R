# Cache successful classifications beside the existing punctuation checkpoints.
subtitle_speaker_cached_classifier <- function(classify_fn, path, identity, force = FALSE) {
  force(classify_fn)
  identity_hash <- digest::digest(identity, algo = "sha256")
  cached <- if (!force && file.exists(path)) readRDS(path) else NULL
  results <- if (is.list(cached) && identical(cached$identity, identity_hash) &&
      is.list(cached$results)) cached$results else list()
  if (length(results) > 0L) message("  reusable speaker comparisons: ", length(results))
  function(previous, sentence_a, sentence_b, following) {
    key <- digest::digest(list(previous, sentence_a, sentence_b, following), algo = "sha256")
    if (!is.null(results[[key]])) return(results[[key]])
    result <- classify_fn(previous, sentence_a, sentence_b, following)
    if (length(result) != 1L || is.na(result) ||
        !result %in% c("same_speaker", "speaker_change", "uncertain")) {
      stop("Cannot checkpoint an invalid speaker classification.", call. = FALSE)
    }
    results[[key]] <<- result
    save_subtitle_backfill_checkpoints(list(identity = identity_hash, results = results), path)
    result
  }
}

subtitle_speaker_resume_classifier <- function(
    classify_fn, checkpoint_path, context, sentences, classifier_identity, force) {
  if (is.null(classifier_identity)) return(classify_fn)
  subtitle_speaker_cached_classifier(
    classify_fn,
    paste0(checkpoint_path, ".speakers.rds"),
    identity = list(
      source_checksum = context$source_checksum,
      language = context$subtitle_language,
      track_type = context$subtitle_track_type,
      scope = context$source_scope,
      pipeline = context$pipeline_version,
      sentences = sentences[, c("text", "speaker_turn_id", "speaker_change")],
      classifier = classifier_identity
    ),
    force = force
  )
}
