source(here::here("r_scripts", "lib", "subtitle_backfill", "subtitle_speaker_client.R"), local = TRUE)

infer_subtitle_speaker_turns <- function(
    sentences,
    classify_fn,
    stop_requested_fn = function() FALSE) {
  if (nrow(sentences) == 0L) {
    sentences$inferred_speaker_turn_id <- integer()
    return(sentences)
  }
  if (nrow(sentences) == 1L) {
    sentences$inferred_speaker_turn_id <- 1L
    return(sentences)
  }
  inferred <- integer(nrow(sentences))
  inferred[[1]] <- 1L
  comparison_count <- 0L
  for (i in seq.int(2L, nrow(sentences))) {
    if (isTRUE(stop_requested_fn())) {
      stop(subtitle_backfill_stop_condition())
    }
    explicit_change <- isTRUE(sentences$speaker_change[[i]]) ||
      !identical(sentences$speaker_turn_id[[i]], sentences$speaker_turn_id[[i - 1L]])
    changed <- explicit_change
    if (!explicit_change) {
      comparison_count <- comparison_count + 1L
      previous <- if (i > 2L) sentences$text[[i - 2L]] else ""
      following <- if (i < nrow(sentences)) sentences$text[[i + 1L]] else ""
      changed <- identical(classify_fn(
        previous,
        sentences$text[[i - 1L]],
        sentences$text[[i]],
        following
      ), "speaker_change")
      if (comparison_count == 1L || comparison_count %% 25L == 0L) {
        message("  speaker comparisons completed: ", comparison_count)
      }
    }
    inferred[[i]] <- inferred[[i - 1L]] + as.integer(changed)
  }
  if (comparison_count > 1L && comparison_count %% 25L != 0L) {
    message("  speaker comparisons completed: ", comparison_count)
  }
  sentences$inferred_speaker_turn_id <- inferred
  sentences
}
