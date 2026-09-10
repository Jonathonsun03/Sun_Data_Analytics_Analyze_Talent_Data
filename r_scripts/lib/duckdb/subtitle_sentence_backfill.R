subtitle_backfill_scalar_text <- function(value) {
  value <- as.character(value[[1]])
  if (is.na(value) || !nzchar(trimws(value))) NA_character_ else trimws(value)
}

subtitle_backfill_relation_exists <- function(con, schema_name, relation_name) {
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT COUNT(*) = 1 AS available FROM information_schema.tables",
      "WHERE table_schema = ? AND table_name = ?"
    ),
    params = list(schema_name, relation_name)
  )$available[[1]]
}

list_subtitle_backfill_tracks <- function(
    con,
    talent_code = NULL,
    video_id = NULL) {
  conditions <- character()
  parameters <- list()
  if (!is.null(talent_code) && nzchar(trimws(as.character(talent_code)))) {
    conditions <- c(conditions, "subtitle.talent_code = ?")
    parameters <- c(parameters, list(trimws(as.character(talent_code))))
  }
  if (!is.null(video_id) && nzchar(trimws(as.character(video_id)))) {
    conditions <- c(conditions, "subtitle.video_id = ?")
    parameters <- c(parameters, list(trimws(as.character(video_id))))
  }
  where_sql <- if (length(conditions) == 0L) {
    ""
  } else {
    paste("WHERE", paste(conditions, collapse = " AND "))
  }

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT subtitle.video_id, subtitle.channel_id, subtitle.talent_code,",
      "subtitle.subtitle_language, subtitle.subtitle_track_type,",
      "video.content_type, video.title, COUNT(*) AS raw_rows,",
      "MIN(subtitle.sequence_number) AS sequence_start,",
      "MAX(subtitle.sequence_number) AS sequence_end",
      "FROM text.subtitle_units AS subtitle",
      "LEFT JOIN catalog.videos AS video USING (video_id)",
      where_sql,
      "GROUP BY subtitle.video_id, subtitle.channel_id, subtitle.talent_code,",
      "subtitle.subtitle_language, subtitle.subtitle_track_type,",
      "video.content_type, video.title",
      "ORDER BY raw_rows, subtitle.video_id"
    ),
    params = parameters
  )
}

load_subtitle_track_for_backfill <- function(
    con,
    video_id,
    subtitle_language = NA_character_,
    subtitle_track_type = NA_character_) {
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT subtitle.subtitle_unit_key, subtitle.video_id,",
      "subtitle.channel_id, subtitle.talent_code, subtitle.sequence_number,",
      "subtitle.subtitle_start, subtitle.subtitle_end, subtitle.subtitle_text,",
      "subtitle.subtitle_language, subtitle.subtitle_track_type,",
      "subtitle.source_file_id, subtitle.source_path, video.title,",
      "video.content_type",
      "FROM text.subtitle_units AS subtitle",
      "LEFT JOIN catalog.videos AS video USING (video_id)",
      "WHERE subtitle.video_id = ?",
      "AND COALESCE(subtitle.subtitle_language, '') =",
      "COALESCE(CAST(? AS VARCHAR), '')",
      "AND COALESCE(subtitle.subtitle_track_type, '') =",
      "COALESCE(CAST(? AS VARCHAR), '')",
      "ORDER BY subtitle.sequence_number"
    ),
    params = list(
      as.character(video_id),
      subtitle_backfill_scalar_text(subtitle_language),
      subtitle_backfill_scalar_text(subtitle_track_type)
    )
  )
}

subtitle_backfill_track_is_current <- function(
    con,
    raw_units,
    pipeline_version,
    source_scope = "full_track") {
  if (!subtitle_backfill_relation_exists(
    con,
    "text",
    "subtitle_sentence_units"
  )) {
    return(FALSE)
  }

  source_checksum <- subtitle_sentence_source_checksum(raw_units)
  video_id <- subtitle_sentence_single_value(raw_units$video_id, "video_id")
  subtitle_language <- subtitle_sentence_single_value(
    raw_units$subtitle_language,
    "subtitle language"
  )
  subtitle_track_type <- subtitle_sentence_single_value(
    raw_units$subtitle_track_type,
    "subtitle track type"
  )
  current <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT COUNT(*) AS sentence_rows FROM text.subtitle_sentence_units",
      "WHERE video_id = ?",
      "AND COALESCE(subtitle_language, '') = COALESCE(CAST(? AS VARCHAR), '')",
      "AND COALESCE(subtitle_track_type, '') = COALESCE(CAST(? AS VARCHAR), '')",
      "AND source_scope = ? AND source_checksum_sha256 = ?",
      "AND pipeline_version = ?"
    ),
    params = list(
      video_id,
      subtitle_language,
      subtitle_track_type,
      source_scope,
      source_checksum,
      pipeline_version
    )
  )
  current$sentence_rows[[1]] > 0L
}

subtitle_backfill_block_key <- function(
    video_id,
    subtitle_language,
    subtitle_track_type,
    source_scope,
    source_checksum,
    pipeline_version,
    block_number,
    block_checksum) {
  subtitle_sentence_hash(
    video_id,
    subtitle_language,
    subtitle_track_type,
    source_scope,
    source_checksum,
    pipeline_version,
    block_number,
    block_checksum
  )
}

subtitle_backfill_checkpoint_frame <- function(
    raw_units,
    block,
    source_checksum,
    pipeline_run_id,
    pipeline_version,
    source_scope,
    status,
    attempt_count,
    punctuated_text = NA_character_,
    punctuation_model = NA_character_,
    error_summary = NA_character_) {
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  video_id <- subtitle_sentence_single_value(raw_units$video_id, "video_id")
  talent_code <- subtitle_sentence_single_value(raw_units$talent_code, "talent code")
  subtitle_language <- subtitle_sentence_single_value(
    raw_units$subtitle_language,
    "subtitle language"
  )
  subtitle_track_type <- subtitle_sentence_single_value(
    raw_units$subtitle_track_type,
    "subtitle track type"
  )
  block_checksum <- subtitle_sentence_hash(block$model_input_text[[1]])

  tibble::tibble(
    reconstruction_block_key = subtitle_backfill_block_key(
      video_id,
      subtitle_language,
      subtitle_track_type,
      source_scope,
      source_checksum,
      pipeline_version,
      block$block_number[[1]],
      block_checksum
    ),
    video_id = video_id,
    talent_code = talent_code,
    subtitle_language = subtitle_language,
    subtitle_track_type = subtitle_track_type,
    source_scope = source_scope,
    source_sequence_start = min(raw_units$sequence_number, na.rm = TRUE),
    source_sequence_end = max(raw_units$sequence_number, na.rm = TRUE),
    block_number = as.integer(block$block_number[[1]]),
    start_sec = block$start_sec[[1]],
    end_sec = block$end_sec[[1]],
    model_input_text = block$model_input_text[[1]],
    punctuated_text = punctuated_text,
    source_subtitle_unit_keys = block$source_subtitle_unit_keys[1],
    source_checksum_sha256 = source_checksum,
    block_input_checksum_sha256 = block_checksum,
    punctuation_model = punctuation_model,
    pipeline_version = pipeline_version,
    status = status,
    attempt_count = as.integer(attempt_count),
    error_summary = error_summary,
    pipeline_run_id = pipeline_run_id,
    created_at = now,
    updated_at = now,
    completed_at = if (identical(status, "complete")) now else as.POSIXct(NA)
  )
}

subtitle_backfill_with_writer <- function(
    db_path,
    operation,
    connection_attempts = 12L,
    retry_delay_sec = 5) {
  last_error <- NULL
  for (attempt in seq_len(connection_attempts)) {
    con <- NULL
    result <- tryCatch(
      {
        con <- duckdb_connect(db_path = db_path, read_only = FALSE)
        value <- operation(con)
        DBI::dbDisconnect(con, shutdown = TRUE)
        con <- NULL
        list(ok = TRUE, value = value)
      },
      error = function(error) list(ok = FALSE, error = error)
    )
    if (!is.null(con)) {
      suppressWarnings(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE))
    }
    if (isTRUE(result$ok)) return(result$value)

    last_error <- result$error
    if (!grepl("Could not set lock|Conflicting lock|database is locked",
               conditionMessage(last_error), ignore.case = TRUE)) {
      stop(conditionMessage(last_error), call. = FALSE)
    }
    if (attempt < connection_attempts) {
      message(
        "DuckDB write attempt ",
        attempt,
        "/",
        connection_attempts,
        " failed: ",
        conditionMessage(last_error),
        "; retrying in ",
        retry_delay_sec,
        " seconds."
      )
      Sys.sleep(retry_delay_sec)
    }
  }
  stop(conditionMessage(last_error), call. = FALSE)
}

upsert_subtitle_backfill_checkpoint <- function(db_path, checkpoint_row) {
  subtitle_backfill_with_writer(db_path, function(con) {
    init_subtitle_sentence_schema(con)
    subtitle_sentence_register_frame(con, "subtitle_backfill_checkpoint", checkpoint_row)
    on.exit(
      try(duckdb::duckdb_unregister(con, "subtitle_backfill_checkpoint"), silent = TRUE),
      add = TRUE
    )
    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO ops.subtitle_reconstruction_blocks",
        "SELECT * FROM subtitle_backfill_checkpoint",
        "ON CONFLICT (reconstruction_block_key) DO UPDATE SET",
        "punctuated_text = excluded.punctuated_text,",
        "punctuation_model = excluded.punctuation_model,",
        "status = excluded.status,",
        "attempt_count = excluded.attempt_count,",
        "error_summary = excluded.error_summary,",
        "pipeline_run_id = excluded.pipeline_run_id,",
        "updated_at = excluded.updated_at,",
        "completed_at = excluded.completed_at"
      )
    )
    invisible(TRUE)
  })
}

load_subtitle_backfill_checkpoints <- function(
    con,
    video_id,
    subtitle_language,
    subtitle_track_type,
    source_checksum,
    pipeline_version,
    source_scope = "full_track") {
  if (!subtitle_backfill_relation_exists(
    con,
    "ops",
    "subtitle_reconstruction_blocks"
  )) {
    return(data.frame())
  }

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT reconstruction_block_key, block_number, punctuated_text,",
      "punctuation_model, status, attempt_count, error_summary",
      "FROM ops.subtitle_reconstruction_blocks WHERE video_id = ?",
      "AND COALESCE(subtitle_language, '') = COALESCE(CAST(? AS VARCHAR), '')",
      "AND COALESCE(subtitle_track_type, '') = COALESCE(CAST(? AS VARCHAR), '')",
      "AND source_scope = ? AND source_checksum_sha256 = ?",
      "AND pipeline_version = ? ORDER BY block_number"
    ),
    params = list(
      video_id,
      subtitle_language,
      subtitle_track_type,
      source_scope,
      source_checksum,
      pipeline_version
    )
  )
}

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

# One persistent RDS per video preserves native checkpoint types and lineage.
# Atomic replacement keeps the last complete file intact if a process is killed.
subtitle_backfill_checkpoint_path <- function(db_path, video_id, checkpoint_dir = NULL) {
  if (is.null(checkpoint_dir)) {
    checkpoint_dir <- Sys.getenv(
      "SUBTITLE_BACKFILL_CHECKPOINT_DIR",
      file.path(dirname(dirname(db_path)), "Processed", "subtitle_backfill_checkpoints")
    )
  }
  file.path(checkpoint_dir, paste0(subtitle_sentence_hash(video_id), ".rds"))
}

save_subtitle_backfill_checkpoints <- function(checkpoints, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary_path <- tempfile(".checkpoint-", tmpdir = dirname(path))
  on.exit(unlink(temporary_path), add = TRUE)
  saveRDS(checkpoints, temporary_path)
  if (!file.rename(temporary_path, path)) {
    stop("Could not replace video checkpoint file: ", path, call. = FALSE)
  }
  invisible(path)
}

reconstruct_subtitle_track_with_checkpoints <- function(
    db_path,
    raw_units,
    blocks,
    pipeline_run_id,
    pipeline_version = "subtitle_sentence_v1",
    source_scope = "full_track",
    punctuation_url = inference_punctuation_url(),
    timeout_sec = 120,
    max_attempts = 3L,
    request_pause_sec = 0,
    retry_failed = FALSE,
    force = FALSE,
    punctuate_fn = punctuate_text,
    checkpoints = NULL,
    checkpoint_dir = NULL) {
  if (nrow(blocks) == 0L) stop("No punctuation blocks were produced.", call. = FALSE)
  source_checksum <- subtitle_sentence_source_checksum(raw_units)
  video_id <- subtitle_sentence_single_value(raw_units$video_id, "video_id")
  subtitle_language <- subtitle_sentence_single_value(
    raw_units$subtitle_language,
    "subtitle language"
  )
  subtitle_track_type <- subtitle_sentence_single_value(
    raw_units$subtitle_track_type,
    "subtitle track type"
  )

  # The runner supplies checkpoints from the same read as the raw track.
  # Keep the optional read for existing direct callers of this helper.
  if (is.null(checkpoints)) {
    read_con <- duckdb_connect(db_path = db_path, read_only = TRUE)
    checkpoints <- tryCatch(
      load_subtitle_backfill_checkpoints(
        read_con, video_id, subtitle_language, subtitle_track_type,
        source_checksum, pipeline_version, source_scope
      ),
      finally = DBI::dbDisconnect(read_con, shutdown = TRUE)
    )
  }
  checkpoint_path <- subtitle_backfill_checkpoint_path(db_path, video_id, checkpoint_dir)
  local_checkpoints <- if (file.exists(checkpoint_path)) readRDS(checkpoint_path) else data.frame()
  if (nrow(local_checkpoints) > 0L) {
    subtitle_sentence_require_columns(
      local_checkpoints,
      c("reconstruction_block_key", "status", "attempt_count", "punctuated_text", "punctuation_model"),
      "Local checkpoints"
    )
  }
  checkpoints <- dplyr::bind_rows(local_checkpoints, checkpoints)
  if (nrow(checkpoints) > 0L) {
    checkpoints <- checkpoints[!duplicated(checkpoints$reconstruction_block_key), , drop = FALSE]
  }
  checkpoint_attempts <- integer()

  sentence_results <- vector("list", nrow(blocks))
  reused_blocks <- 0L
  requested_blocks <- 0L
  for (block_index in seq_len(nrow(blocks))) {
    block <- blocks[block_index, , drop = FALSE]
    block_checksum <- subtitle_sentence_hash(block$model_input_text[[1]])
    block_key <- subtitle_backfill_block_key(
      video_id,
      subtitle_language,
      subtitle_track_type,
      source_scope,
      source_checksum,
      pipeline_version,
      block$block_number[[1]],
      block_checksum
    )
    existing <- checkpoints[
      checkpoints$reconstruction_block_key == block_key,
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
          source_checksum,
          pipeline_run_id,
          pipeline_version,
          source_scope,
          status = "complete",
          attempt_count = attempt,
          punctuated_text = normalize_fullstop_punctuation(result$response$text),
          punctuation_model = result$response$model
        )
        local_checkpoints <- dplyr::bind_rows(complete_row, local_checkpoints)
        local_checkpoints <- local_checkpoints[
          !duplicated(local_checkpoints$reconstruction_block_key), , drop = FALSE
        ]
        save_subtitle_backfill_checkpoints(local_checkpoints, checkpoint_path)
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

  sentences <- dplyr::bind_rows(sentence_results) |>
    # Speaker blocks may overlap in time; retain stable order within each block.
    dplyr::arrange(.data$video_id, .data$start_sec, .data$block_number, .data$sentence_number) |>
    dplyr::group_by(.data$video_id) |>
    dplyr::mutate(sentence_number = dplyr::row_number()) |>
    dplyr::ungroup()
  validate_subtitle_reconstruction(raw_units, blocks, sentences)

  publication <- subtitle_backfill_with_writer(db_path, function(con) {
    current_raw_units <- load_subtitle_track_for_backfill(
      con, video_id, subtitle_language, subtitle_track_type
    )
    if (!identical(subtitle_sentence_source_checksum(current_raw_units), source_checksum)) {
      stop("Source track changed during inference; cached results were retained but not published.", call. = FALSE)
    }
    publish_subtitle_reconstruction(
      con,
      raw_units = raw_units,
      blocks = blocks,
      sentence_units = sentences,
      pipeline_version = pipeline_version,
      source_scope = source_scope,
      dry_run = FALSE,
      checkpoint_attempts = checkpoint_attempts
    )
  })

  list(
    video_id = video_id,
    source_checksum_sha256 = source_checksum,
    blocks = nrow(blocks),
    reused_blocks = reused_blocks,
    requested_blocks = requested_blocks,
    sentences = nrow(sentences),
    publication = publication
  )
}

start_subtitle_backfill_run <- function(db_path) {
  pipeline_run_id <- paste0(
    "subtitle_backfill_",
    format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
    "_",
    substr(subtitle_sentence_hash(Sys.getpid(), runif(1)), 1L, 12L)
  )
  subtitle_backfill_with_writer(db_path, function(con) {
    init_subtitle_sentence_schema(con)
    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO ops.pipeline_runs",
        "(pipeline_run_id, pipeline_name, started_at, status)",
        "VALUES (?, 'subtitle_sentence_backfill', ?, 'running')"
      ),
      params = list(pipeline_run_id, as.POSIXct(Sys.time(), tz = "UTC"))
    )
  })
  pipeline_run_id
}

finish_subtitle_backfill_run <- function(
    db_path,
    pipeline_run_id,
    status,
    error_summary = NA_character_) {
  subtitle_backfill_with_writer(db_path, function(con) {
    DBI::dbExecute(
      con,
      paste(
        "UPDATE ops.pipeline_runs SET completed_at = ?, status = ?,",
        "error_summary = ? WHERE pipeline_run_id = ?"
      ),
      params = list(
        as.POSIXct(Sys.time(), tz = "UTC"),
        status,
        error_summary,
        pipeline_run_id
      )
    )
  })
  invisible(TRUE)
}
