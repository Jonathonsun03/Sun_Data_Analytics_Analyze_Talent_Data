# Per-video reconstruction, validation, and publication workflow.

reconstruct_subtitle_track_with_checkpoints <- function(
    db_path,
    raw_units,
    blocks,
    pipeline_run_id,
    pipeline_version = "subtitle_sentence_v2",
    source_scope = "full_track",
    punctuation_url = inference_punctuation_url(),
    timeout_sec = 120,
    max_attempts = 3L,
    request_pause_sec = 0,
    retry_failed = FALSE,
    force = FALSE,
    punctuate_fn = punctuate_text,
    speaker_change_fn = NULL,
    checkpoints = NULL,
    checkpoint_dir = NULL,
    before_publish = NULL) {
  if (nrow(blocks) == 0L) {
    stop("No punctuation blocks were produced.", call. = FALSE)
  }

  context <- list(
    video_id = subtitle_sentence_single_value(raw_units$video_id, "video_id"),
    subtitle_language = subtitle_sentence_single_value(
      raw_units$subtitle_language,
      "subtitle language"
    ),
    subtitle_track_type = subtitle_sentence_single_value(
      raw_units$subtitle_track_type,
      "subtitle track type"
    ),
    source_checksum = subtitle_sentence_source_checksum(raw_units),
    pipeline_run_id = pipeline_run_id,
    pipeline_version = pipeline_version,
    source_scope = source_scope
  )
  checkpoint_state <- subtitle_backfill_load_checkpoint_state(
    db_path,
    context,
    checkpoints,
    checkpoint_dir
  )
  inference <- subtitle_backfill_infer_blocks(
    raw_units = raw_units,
    blocks = blocks,
    context = context,
    checkpoint_state = checkpoint_state,
    punctuation_url = punctuation_url,
    timeout_sec = timeout_sec,
    max_attempts = max_attempts,
    request_pause_sec = request_pause_sec,
    retry_failed = retry_failed,
    force = force,
    punctuate_fn = punctuate_fn
  )

  sentences <- dplyr::bind_rows(inference$sentence_results) |>
    # Speaker blocks may overlap in time; retain stable order within each block.
    dplyr::arrange(
      .data$video_id,
      .data$start_sec,
      .data$block_number,
      .data$sentence_number
    ) |>
    dplyr::group_by(.data$video_id) |>
    dplyr::mutate(sentence_number = dplyr::row_number()) |>
    dplyr::ungroup()
  validate_subtitle_reconstruction(raw_units, blocks, sentences)
  if (!is.null(speaker_change_fn)) {
    sentences <- infer_subtitle_speaker_turns(sentences, speaker_change_fn)
  }

  if (!is.null(before_publish)) before_publish()
  publication <- subtitle_backfill_with_writer(db_path, function(con) {
    current_raw_units <- load_subtitle_track_for_backfill(
      con,
      context$video_id,
      context$subtitle_language,
      context$subtitle_track_type
    )
    current_checksum <- subtitle_sentence_source_checksum(current_raw_units)
    if (!identical(current_checksum, context$source_checksum)) {
      stop(
        paste(
          "Source track changed during inference; cached results were retained",
          "but not published."
        ),
        call. = FALSE
      )
    }
    publish_subtitle_reconstruction(
      con,
      raw_units = raw_units,
      blocks = blocks,
      sentence_units = sentences,
      pipeline_version = context$pipeline_version,
      source_scope = context$source_scope,
      dry_run = FALSE,
      checkpoint_attempts = inference$checkpoint_attempts
    )
  })

  list(
    video_id = context$video_id,
    source_checksum_sha256 = context$source_checksum,
    blocks = nrow(blocks),
    reused_blocks = inference$reused_blocks,
    requested_blocks = inference$requested_blocks,
    sentences = nrow(sentences),
    publication = publication
  )
}
