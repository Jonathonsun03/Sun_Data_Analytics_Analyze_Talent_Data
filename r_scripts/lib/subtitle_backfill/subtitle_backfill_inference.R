# Run or reuse punctuation inference for every block in one subtitle track.

subtitle_backfill_validate_response <- function(block, response) {
  if (is.null(response$text) || !nzchar(trimws(as.character(response$text)))) {
    stop("FullStop returned empty punctuated text.", call. = FALSE)
  }
  if (is.null(response$model) || is.na(response$model) ||
      !nzchar(trimws(as.character(response$model)))) {
    stop("FullStop did not return its model identity.", call. = FALSE)
  }

  sentence_rows <- sentence_units_from_block(
    block,
    punctuated_text = normalize_fullstop_punctuation(response$text),
    punctuation_model = response$model
  )
  if (nrow(sentence_rows) == 0L) {
    stop("FullStop response did not produce a substantive sentence.", call. = FALSE)
  }
  if (!identical(
    subtitle_sentence_extract_words(block$model_input_text),
    subtitle_sentence_extract_words(sentence_rows$text)
  )) {
    stop("FullStop changed the block word sequence.", call. = FALSE)
  }
  list(response = response, sentence_rows = sentence_rows)
}

subtitle_backfill_infer_blocks <- function(
    raw_units,
    blocks,
    context,
    checkpoint_state,
    punctuation_url,
    timeout_sec,
    max_attempts,
    request_pause_sec,
    retry_failed,
    force,
    punctuate_fn) {
  sentence_results <- vector("list", nrow(blocks))
  checkpoint_attempts <- integer()
  local_checkpoints <- checkpoint_state$local
  reused_blocks <- 0L
  requested_blocks <- 0L

  for (block_index in seq_len(nrow(blocks))) {
    block <- blocks[block_index, , drop = FALSE]
    block_checksum <- subtitle_sentence_hash(block$model_input_text[[1]])
    block_key <- subtitle_backfill_block_key(
      context$video_id,
      context$subtitle_language,
      context$subtitle_track_type,
      context$source_scope,
      context$source_checksum,
      context$pipeline_version,
      block$block_number[[1]],
      block_checksum
    )
    existing <- checkpoint_state$all[
      checkpoint_state$all$reconstruction_block_key == block_key,
      ,
      drop = FALSE
    ]

    if (!isTRUE(force) && nrow(existing) == 1L &&
        identical(existing$status[[1]], "complete")) {
      cached <- tryCatch(
        subtitle_backfill_validate_response(
          block,
          list(
            text = existing$punctuated_text[[1]],
            model = existing$punctuation_model[[1]]
          )
        ),
        error = function(error) NULL
      )
      if (!is.null(cached)) {
        sentence_results[[block_index]] <- cached$sentence_rows
        checkpoint_attempts[[block_key]] <- existing$attempt_count[[1]]
        reused_blocks <- reused_blocks + 1L
        message(
          "  block ",
          block_index,
          "/",
          nrow(blocks),
          " reused from checkpoint"
        )
        next
      }
    }

    prior_attempts <- if (nrow(existing) == 1L) {
      as.integer(existing$attempt_count[[1]])
    } else {
      0L
    }
    if (isTRUE(force) || (isTRUE(retry_failed) && nrow(existing) == 1L &&
        identical(existing$status[[1]], "failed"))) {
      prior_attempts <- 0L
    }
    if (prior_attempts >= max_attempts) {
      stop(
        "Block ",
        block$block_number[[1]],
        " exhausted ",
        max_attempts,
        " attempts. Use --retry-failed to try it again.",
        call. = FALSE
      )
    }

    block_complete <- FALSE
    for (attempt in seq.int(prior_attempts + 1L, max_attempts)) {
      message(
        "  block ",
        block_index,
        "/",
        nrow(blocks),
        " FullStop attempt ",
        attempt,
        "/",
        max_attempts
      )

      result <- tryCatch(
        {
          response <- punctuate_fn(
            text = block$model_input_text[[1]],
            url = punctuation_url,
            timeout_sec = timeout_sec,
            include_model = TRUE
          )
          subtitle_backfill_validate_response(block, response)
        },
        error = function(error) error
      )
      requested_blocks <- requested_blocks + 1L
      if (!inherits(result, "error")) {
        complete_row <- subtitle_backfill_checkpoint_frame(
          raw_units,
          block,
          context$source_checksum,
          context$pipeline_run_id,
          context$pipeline_version,
          context$source_scope,
          status = "complete",
          attempt_count = attempt,
          punctuated_text = normalize_fullstop_punctuation(result$response$text),
          punctuation_model = result$response$model
        )
        local_checkpoints <- dplyr::bind_rows(complete_row, local_checkpoints)
        local_checkpoints <- local_checkpoints[
          !duplicated(local_checkpoints$reconstruction_block_key),
          ,
          drop = FALSE
        ]
        save_subtitle_backfill_checkpoints(
          local_checkpoints,
          checkpoint_state$path
        )
        checkpoint_attempts[[block_key]] <- attempt
        sentence_results[[block_index]] <- result$sentence_rows
        block_complete <- TRUE
        if (request_pause_sec > 0) Sys.sleep(request_pause_sec)
        break
      }

      message("    failed: ", conditionMessage(result))
      if (attempt < max_attempts) Sys.sleep(min(60, 2^(attempt - 1L)))
    }
    if (!block_complete) {
      stop(
        "Block ",
        block$block_number[[1]],
        " failed after ",
        max_attempts,
        " attempts.",
        call. = FALSE
      )
    }
  }

  list(
    sentence_results = sentence_results,
    checkpoint_attempts = checkpoint_attempts,
    reused_blocks = reused_blocks,
    requested_blocks = requested_blocks
  )
}
