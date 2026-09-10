subtitle_sentence_require_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      label,
      " is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

subtitle_sentence_hash <- function(...) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package `digest` is required.", call. = FALSE)
  }
  values <- unlist(list(...), recursive = TRUE, use.names = FALSE)
  values <- ifelse(is.na(values), "<NA>", as.character(values))
  digest::digest(paste(values, collapse = "\u001f"), algo = "sha256", serialize = FALSE)
}

subtitle_sentence_single_value <- function(values, label) {
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(trimws(values))]
  if (length(values) > 1L) {
    stop("Expected one ", label, "; found: ", paste(values, collapse = ", "), call. = FALSE)
  }
  if (length(values) == 0L) NA_character_ else values[[1]]
}

load_subtitle_units_for_reconstruction <- function(
    con,
    video_id,
    start_sequence = NULL,
    row_limit = NULL) {
  video_id <- trimws(as.character(video_id))
  if (length(video_id) != 1L || is.na(video_id) || !nzchar(video_id)) {
    stop("video_id must be one non-empty value.", call. = FALSE)
  }

  conditions <- "subtitle.video_id = ?"
  query_parameters <- list(video_id)
  if (!is.null(start_sequence)) {
    start_sequence <- as.integer(start_sequence)
    if (is.na(start_sequence) || start_sequence < 1L) {
      stop("start_sequence must be a positive integer.", call. = FALSE)
    }
    conditions <- paste(conditions, "AND subtitle.sequence_number >= ?")
    query_parameters <- c(query_parameters, list(start_sequence))
  }

  limit_sql <- ""
  if (!is.null(row_limit)) {
    row_limit <- as.integer(row_limit)
    if (is.na(row_limit) || row_limit < 1L) {
      stop("row_limit must be a positive integer.", call. = FALSE)
    }
    limit_sql <- paste("LIMIT", row_limit)
  }

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      " subtitle.subtitle_unit_key, subtitle.video_id, subtitle.channel_id,",
      " subtitle.talent_code, subtitle.sequence_number,",
      " subtitle.subtitle_start, subtitle.subtitle_end, subtitle.subtitle_text,",
      " subtitle.subtitle_language, subtitle.subtitle_track_type,",
      " subtitle.source_file_id, subtitle.source_path, video.title,",
      " video.content_type",
      "FROM text.subtitle_units AS subtitle",
      "LEFT JOIN catalog.videos AS video USING (video_id)",
      "WHERE", conditions,
      "ORDER BY subtitle.sequence_number",
      limit_sql
    ),
    params = query_parameters
  )
}

normalize_subtitle_units_for_reconstruction <- function(raw_units) {
  subtitle_sentence_require_columns(
    raw_units,
    c(
      "subtitle_unit_key", "video_id", "sequence_number", "subtitle_start",
      "subtitle_end", "subtitle_text", "subtitle_language",
      "subtitle_track_type", "channel_id", "talent_code"
    ),
    "Raw subtitle units"
  )
  if (!exists("period_to_seconds", mode = "function")) {
    stop("Source clean_subtitles.R before normalizing subtitle units.", call. = FALSE)
  }

  raw_units |>
    dplyr::mutate(
      start_sec = period_to_seconds(.data$subtitle_start),
      end_sec = period_to_seconds(.data$subtitle_end),
      text = .data$subtitle_text |>
        stringr::str_replace_all(stringr::fixed("&gt;&gt;"), ">>") |>
        stringr::str_replace_all(stringr::fixed("&nbsp;"), " ") |>
        stringr::str_replace_all("[\\r\\n]+", " ") |>
        stringr::str_squish()
    ) |>
    dplyr::arrange(.data$video_id, .data$sequence_number)
}

subtitle_sentence_source_checksum <- function(raw_units) {
  subtitle_sentence_require_columns(
    raw_units,
    c(
      "subtitle_unit_key", "video_id", "sequence_number", "subtitle_start",
      "subtitle_end", "subtitle_text", "subtitle_language",
      "subtitle_track_type"
    ),
    "Raw subtitle units"
  )
  ordered <- raw_units[order(raw_units$video_id, raw_units$sequence_number), , drop = FALSE]
  rows <- vapply(seq_len(nrow(ordered)), function(i) {
    paste(
      ifelse(is.na(ordered$subtitle_unit_key[[i]]), "<NA>", ordered$subtitle_unit_key[[i]]),
      ifelse(is.na(ordered$video_id[[i]]), "<NA>", ordered$video_id[[i]]),
      ordered$sequence_number[[i]],
      ifelse(is.na(ordered$subtitle_start[[i]]), "<NA>", ordered$subtitle_start[[i]]),
      ifelse(is.na(ordered$subtitle_end[[i]]), "<NA>", ordered$subtitle_end[[i]]),
      ifelse(is.na(ordered$subtitle_text[[i]]), "<NA>", ordered$subtitle_text[[i]]),
      ifelse(is.na(ordered$subtitle_language[[i]]), "<NA>", ordered$subtitle_language[[i]]),
      ifelse(is.na(ordered$subtitle_track_type[[i]]), "<NA>", ordered$subtitle_track_type[[i]]),
      sep = "\u001f"
    )
  }, character(1), USE.NAMES = FALSE)
  subtitle_sentence_hash(rows)
}

subtitle_sentence_extract_words <- function(text) {
  tolower(unlist(
    stringr::str_extract_all(
      paste(as.character(text), collapse = " "),
      "[[:alnum:]]+(?:['’][[:alnum:]]+)*"
    ),
    use.names = FALSE
  ))
}

validate_subtitle_reconstruction <- function(raw_units, blocks, sentence_units) {
  subtitle_sentence_require_columns(
    blocks,
    c(
      "video_id", "block_number", "start_sec", "end_sec", "model_input_text",
      "source_subtitle_unit_keys"
    ),
    "Punctuation blocks"
  )
  subtitle_sentence_require_columns(
    sentence_units,
    c(
      "video_id", "block_number", "sentence_number", "start_sec", "end_sec",
      "text", "punctuation_model", "timestamps_approximate", "timestamp_method",
      "source_subtitle_unit_keys"
    ),
    "Sentence units"
  )
  if (nrow(blocks) == 0L || nrow(sentence_units) == 0L) {
    stop("Reconstruction must contain at least one block and sentence.", call. = FALSE)
  }
  if (!setequal(unique(blocks$block_number), unique(sentence_units$block_number))) {
    stop("Every punctuation block must produce at least one sentence.", call. = FALSE)
  }
  if (any(is.na(sentence_units$text) | !nzchar(trimws(sentence_units$text)))) {
    stop("Sentence text must be non-empty.", call. = FALSE)
  }
  if (any(!stringr::str_detect(sentence_units$text, "[[:alnum:]]"))) {
    stop("Sentence units cannot contain punctuation-only fragments.", call. = FALSE)
  }
  if (any(is.na(sentence_units$punctuation_model) |
      !nzchar(sentence_units$punctuation_model))) {
    stop("Every sentence must record its punctuation model.", call. = FALSE)
  }
  if (any(sentence_units$end_sec < sentence_units$start_sec, na.rm = TRUE)) {
    stop("Sentence timestamps cannot move backwards.", call. = FALSE)
  }
  if (is.unsorted(sentence_units$start_sec, na.rm = TRUE)) {
    stop("Sentence units must be chronological.", call. = FALSE)
  }

  raw_keys <- unique(as.character(raw_units$subtitle_unit_key))
  sentence_keys <- unique(unlist(sentence_units$source_subtitle_unit_keys, use.names = FALSE))
  if (length(sentence_keys) == 0L || any(!sentence_keys %in% raw_keys)) {
    stop("Every sentence must retain valid source subtitle keys.", call. = FALSE)
  }

  for (block_number in unique(blocks$block_number)) {
    block <- blocks[blocks$block_number == block_number, , drop = FALSE]
    sentences <- sentence_units[
      sentence_units$block_number == block_number,
      ,
      drop = FALSE
    ]
    if (!identical(
      subtitle_sentence_extract_words(block$model_input_text),
      subtitle_sentence_extract_words(sentences$text)
    )) {
      stop(
        "Punctuation changed the word sequence in block ",
        block_number,
        ".",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

subtitle_sentence_new_run_id <- function() {
  paste0(
    "subtitle_sentences_",
    format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
    "_",
    substr(subtitle_sentence_hash(Sys.getpid(), runif(1)), 1L, 12L)
  )
}

prepare_subtitle_reconstruction_records <- function(
    raw_units,
    blocks,
    sentence_units,
    pipeline_run_id,
    pipeline_version = "subtitle_sentence_v1",
    source_scope = "full_track") {
  validate_subtitle_reconstruction(raw_units, blocks, sentence_units)

  source_checksum <- subtitle_sentence_source_checksum(raw_units)
  video_id <- subtitle_sentence_single_value(raw_units$video_id, "video_id")
  channel_id <- subtitle_sentence_single_value(raw_units$channel_id, "channel_id")
  talent_code <- subtitle_sentence_single_value(raw_units$talent_code, "talent_code")
  subtitle_language <- subtitle_sentence_single_value(
    raw_units$subtitle_language,
    "subtitle language"
  )
  subtitle_track_type <- subtitle_sentence_single_value(
    raw_units$subtitle_track_type,
    "subtitle track type"
  )
  source_sequence_start <- min(raw_units$sequence_number, na.rm = TRUE)
  source_sequence_end <- max(raw_units$sequence_number, na.rm = TRUE)
  created_at <- as.POSIXct(Sys.time(), tz = "UTC")

  block_checksums <- vapply(
    blocks$model_input_text,
    subtitle_sentence_hash,
    character(1),
    USE.NAMES = FALSE
  )
  names(block_checksums) <- as.character(blocks$block_number)

  sentence_rows <- sentence_units |>
    dplyr::mutate(
      sentence_unit_key = vapply(seq_len(dplyr::n()), function(i) {
        subtitle_sentence_hash(
          video_id,
          subtitle_language,
          subtitle_track_type,
          source_scope,
          source_checksum,
          pipeline_version,
          .data$block_number[[i]],
          .data$sentence_number[[i]]
        )
      }, character(1)),
      channel_id = channel_id,
      talent_code = talent_code,
      subtitle_language = subtitle_language,
      subtitle_track_type = subtitle_track_type,
      source_scope = source_scope,
      source_sequence_start = source_sequence_start,
      source_sequence_end = source_sequence_end,
      sentence_text = .data$text,
      source_alignment_status = "block_approximate",
      source_checksum_sha256 = source_checksum,
      block_input_checksum_sha256 = unname(
        block_checksums[as.character(.data$block_number)]
      ),
      pipeline_version = pipeline_version,
      pipeline_run_id = pipeline_run_id,
      created_at = created_at
    ) |>
    dplyr::select(dplyr::all_of(c(
      "sentence_unit_key",
      "video_id",
      "channel_id",
      "talent_code",
      "subtitle_language",
      "subtitle_track_type",
      "source_scope",
      "source_sequence_start",
      "source_sequence_end",
      "block_number",
      "sentence_number",
      "speaker_turn_id",
      "speaker_change",
      "start_sec",
      "end_sec",
      "sentence_text",
      "source_subtitle_unit_keys",
      "source_alignment_status",
      "punctuation_model",
      "timestamps_approximate",
      "timestamp_method",
      "source_checksum_sha256",
      "block_input_checksum_sha256",
      "pipeline_version",
      "pipeline_run_id",
      "created_at"
    )))

  checkpoint_rows <- purrr::map_dfr(seq_len(nrow(blocks)), function(i) {
    block_number <- blocks$block_number[[i]]
    block_sentences <- sentence_units[
      sentence_units$block_number == block_number,
      ,
      drop = FALSE
    ]
    punctuation_model <- subtitle_sentence_single_value(
      block_sentences$punctuation_model,
      "punctuation model"
    )
    block_checksum <- block_checksums[[as.character(block_number)]]
    tibble::tibble(
      reconstruction_block_key = subtitle_sentence_hash(
        video_id,
        subtitle_language,
        subtitle_track_type,
        source_scope,
        source_checksum,
        pipeline_version,
        block_number,
        block_checksum
      ),
      video_id = video_id,
      talent_code = talent_code,
      subtitle_language = subtitle_language,
      subtitle_track_type = subtitle_track_type,
      source_scope = source_scope,
      source_sequence_start = source_sequence_start,
      source_sequence_end = source_sequence_end,
      block_number = as.integer(block_number),
      start_sec = blocks$start_sec[[i]],
      end_sec = blocks$end_sec[[i]],
      model_input_text = blocks$model_input_text[[i]],
      punctuated_text = paste(block_sentences$text, collapse = " "),
      source_subtitle_unit_keys = blocks$source_subtitle_unit_keys[i],
      source_checksum_sha256 = source_checksum,
      block_input_checksum_sha256 = block_checksum,
      punctuation_model = punctuation_model,
      pipeline_version = pipeline_version,
      status = "complete",
      attempt_count = 1L,
      error_summary = NA_character_,
      pipeline_run_id = pipeline_run_id,
      created_at = created_at,
      updated_at = created_at,
      completed_at = created_at
    )
  })

  list(
    source_checksum_sha256 = source_checksum,
    sentence_rows = sentence_rows,
    checkpoint_rows = checkpoint_rows
  )
}

subtitle_sentence_register_frame <- function(con, name, frame) {
  duckdb::duckdb_register(con, name, as.data.frame(frame))
  invisible(TRUE)
}

publish_subtitle_reconstruction <- function(
    con,
    raw_units,
    blocks,
    sentence_units,
    pipeline_version = "subtitle_sentence_v1",
    source_scope = "full_track",
    dry_run = TRUE,
    checkpoint_attempts = NULL) {
  pipeline_run_id <- subtitle_sentence_new_run_id()
  prepared <- prepare_subtitle_reconstruction_records(
    raw_units = raw_units,
    blocks = blocks,
    sentence_units = sentence_units,
    pipeline_run_id = pipeline_run_id,
    pipeline_version = pipeline_version,
    source_scope = source_scope
  )
  if (!is.null(checkpoint_attempts)) {
    attempts <- unname(checkpoint_attempts[prepared$checkpoint_rows$reconstruction_block_key])
    if (anyNA(attempts) || any(attempts < 1L)) {
      stop("Missing or invalid completed checkpoint attempt counts.", call. = FALSE)
    }
    prepared$checkpoint_rows$attempt_count <- as.integer(attempts)
  }
  if (isTRUE(dry_run)) {
    return(c(
      list(pipeline_run_id = pipeline_run_id, published = FALSE),
      prepared
    ))
  }

  init_subtitle_sentence_schema(con)
  started_at <- as.POSIXct(Sys.time(), tz = "UTC")
  DBI::dbExecute(
    con,
    "INSERT INTO ops.pipeline_runs (
       pipeline_run_id, pipeline_name, started_at, status
     ) VALUES (?, 'subtitle_sentence_reconstruction', ?, 'running')",
    params = list(pipeline_run_id, started_at)
  )

  sentence_rows <- prepared$sentence_rows
  checkpoint_rows <- prepared$checkpoint_rows
  video_id <- sentence_rows$video_id[[1]]
  subtitle_language <- sentence_rows$subtitle_language[[1]]
  subtitle_track_type <- sentence_rows$subtitle_track_type[[1]]

  transaction_open <- FALSE
  tryCatch(
    {
      DBI::dbBegin(con)
      transaction_open <- TRUE

      DBI::dbExecute(
        con,
        paste(
          "DELETE FROM text.subtitle_sentence_units",
          "WHERE video_id = ?",
          "AND COALESCE(subtitle_language, '') = COALESCE(CAST(? AS VARCHAR), '')",
          "AND COALESCE(subtitle_track_type, '') = COALESCE(CAST(? AS VARCHAR), '')",
          "AND source_scope = ?"
        ),
        params = list(
          video_id,
          subtitle_language,
          subtitle_track_type,
          source_scope
        )
      )

      subtitle_sentence_register_frame(con, "subtitle_sentence_publish_rows", sentence_rows)
      DBI::dbExecute(
        con,
        "INSERT INTO text.subtitle_sentence_units SELECT * FROM subtitle_sentence_publish_rows"
      )
      duckdb::duckdb_unregister(con, "subtitle_sentence_publish_rows")

      subtitle_sentence_register_frame(con, "subtitle_block_publish_rows", checkpoint_rows)
      DBI::dbExecute(
        con,
        "INSERT INTO ops.subtitle_reconstruction_blocks
         SELECT * FROM subtitle_block_publish_rows
         ON CONFLICT (reconstruction_block_key) DO UPDATE SET
           punctuated_text = excluded.punctuated_text,
           punctuation_model = excluded.punctuation_model,
           status = excluded.status,
           attempt_count = GREATEST(
             ops.subtitle_reconstruction_blocks.attempt_count,
             excluded.attempt_count
           ),
           error_summary = excluded.error_summary,
           pipeline_run_id = excluded.pipeline_run_id,
           updated_at = excluded.updated_at,
           completed_at = excluded.completed_at"
      )
      duckdb::duckdb_unregister(con, "subtitle_block_publish_rows")

      DBI::dbExecute(
        con,
        "UPDATE ops.pipeline_runs
         SET completed_at = ?, status = 'completed', error_summary = NULL
         WHERE pipeline_run_id = ?",
        params = list(as.POSIXct(Sys.time(), tz = "UTC"), pipeline_run_id)
      )
      DBI::dbCommit(con)
      transaction_open <- FALSE
    },
    error = function(error) {
      if (transaction_open) {
        try(DBI::dbRollback(con), silent = TRUE)
      }
      try(
        duckdb::duckdb_unregister(con, "subtitle_sentence_publish_rows"),
        silent = TRUE
      )
      try(
        duckdb::duckdb_unregister(con, "subtitle_block_publish_rows"),
        silent = TRUE
      )
      try(
        DBI::dbExecute(
          con,
          "UPDATE ops.pipeline_runs
           SET completed_at = ?, status = 'failed', error_summary = ?
           WHERE pipeline_run_id = ?",
          params = list(
            as.POSIXct(Sys.time(), tz = "UTC"),
            substr(conditionMessage(error), 1L, 1000L),
            pipeline_run_id
          )
        ),
        silent = TRUE
      )
      stop(conditionMessage(error), call. = FALSE)
    }
  )

  c(
    list(pipeline_run_id = pipeline_run_id, published = TRUE),
    prepared
  )
}

load_published_subtitle_sentences <- function(
    con,
    video_id,
    source_scope = "full_track") {
  if (!DBI::dbExistsTable(con, DBI::Id(schema = "text", table = "subtitle_sentence_units"))) {
    return(data.frame())
  }
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT * FROM text.subtitle_sentence_units",
      "WHERE video_id = ? AND source_scope = ?",
      "ORDER BY sentence_number"
    ),
    params = list(as.character(video_id), as.character(source_scope))
  )
}
