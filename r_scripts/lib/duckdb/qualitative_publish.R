qualitative_required_packages <- function() {
  required <- c("data.table", "DBI", "digest", "dplyr", "purrr", "stringr")
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    stop(
      "Missing required R package(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

qualitative_canonical_path <- function(path) {
  path <- as.character(path)
  if (length(path) > 1L) {
    return(vapply(path, qualitative_canonical_path, character(1)))
  }

  if (exists("resolve_datalake_path", mode = "function")) {
    path <- resolve_datalake_path(path)
  }
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (file.exists(path) || dir.exists(path)) {
    return(path)
  }

  if (exists("get_datalake_root", mode = "function")) {
    talent_root <- normalizePath(
      get_datalake_root(),
      winslash = "/",
      mustWork = FALSE
    )
    processed_root <- file.path(
      dirname(talent_root),
      "Processed",
      "Talent_Data"
    )
    path <- sub(
      "^.*?/DataLake/Sun_Data_Analytics/Talent_data",
      talent_root,
      path
    )
    path <- sub(
      "^.*?/DataLake/Sun_Data_Analytics/Processed/Talent_Data",
      processed_root,
      path
    )
  }

  normalizePath(path, winslash = "/", mustWork = FALSE)
}

qualitative_read_coded_exports <- function(coded_data_dir) {
  paths <- sort(list.files(
    coded_data_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  ))
  if (length(paths) == 0L) {
    stop("No coded CSV files found in: ", coded_data_dir, call. = FALSE)
  }

  out <- data.table::rbindlist(
    lapply(paths, function(path) {
      piece <- data.table::fread(path)
      piece[, coded_export_path := qualitative_canonical_path(path)]
      piece
    }),
    use.names = TRUE,
    fill = TRUE
  )

  required <- c(
    "target_unique_id",
    "request_custom_id",
    "csv_path",
    "source_file",
    "video_id",
    "row_number",
    "row_id",
    "sec",
    "timecode",
    "source",
    "speaker",
    "text",
    "response_status"
  )
  missing <- setdiff(required, names(out))
  if (length(missing) > 0L) {
    stop(
      "Coded exports are missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyDuplicated(out$row_id)) {
    stop("Coded exports contain duplicate row_id values.", call. = FALSE)
  }

  out
}

qualitative_file_identity <- function(path, source_type) {
  path <- qualitative_canonical_path(path)
  info <- file.info(path)
  if (is.na(info$size)) {
    stop("Source file does not exist: ", path, call. = FALSE)
  }

  data.frame(
    source_file_id = digest::digest(
      path,
      algo = "sha256",
      serialize = FALSE
    ),
    source_path = path,
    source_type = source_type,
    file_size = as.numeric(info$size),
    checksum_sha256 = digest::digest(
      file = path,
      algo = "sha256",
      serialize = FALSE
    ),
    modified_at = as.POSIXct(info$mtime, tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

qualitative_prepare_codebook <- function(
    codebook_path,
    codebook_id,
    codebook_name,
    codebook_version,
    wide_view_name,
    source_file_id
) {
  raw <- data.table::fread(codebook_path)
  required <- c(
    "Primary Code ID",
    "Primary Code",
    "Secondary Code ID",
    "Secondary Code",
    "Definition",
    "Examples from text"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0L) {
    stop(
      "Codebook is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  clean <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | !nzchar(x)] <- NA_character_
    x
  }

  primary_id <- clean(raw[["Primary Code ID"]])
  primary_name <- clean(raw[["Primary Code"]])
  secondary_id <- clean(raw[["Secondary Code ID"]])
  secondary_name <- clean(raw[["Secondary Code"]])
  code_id <- ifelse(is.na(secondary_id), primary_id, secondary_id)
  code_name <- ifelse(is.na(secondary_name), primary_name, secondary_name)
  code_column_name <- paste0("code_", code_id)

  invisible(vapply(
    code_column_name,
    qualitative_validate_identifier,
    character(1),
    label = "codebook column"
  ))

  data.frame(
    codebook_id = codebook_id,
    codebook_name = codebook_name,
    codebook_version = codebook_version,
    codebook_checksum = digest::digest(
      file = codebook_path,
      algo = "sha256",
      serialize = FALSE
    ),
    wide_view_name = wide_view_name,
    code_id = code_id,
    code_column_name = code_column_name,
    parent_code_id = ifelse(is.na(secondary_id), NA_character_, primary_id),
    primary_code_id = primary_id,
    primary_code_name = primary_name,
    secondary_code_id = secondary_id,
    secondary_code_name = secondary_name,
    code_name = code_name,
    definition = clean(raw[["Definition"]]),
    examples = clean(raw[["Examples from text"]]),
    display_order = seq_len(nrow(raw)),
    source_file_id = source_file_id,
    stringsAsFactors = FALSE
  )
}

qualitative_pipeline_run_id <- function(request_custom_id) {
  pieces <- strsplit(as.character(request_custom_id), "__", fixed = TRUE)
  vapply(pieces, function(x) {
    if (length(x) < 4L || !nzchar(x[[4]])) {
      stop(
        "Could not extract a pipeline run from request_custom_id.",
        call. = FALSE
      )
    }
    x[[4]]
  }, character(1))
}

qualitative_pipeline_timestamp <- function(pipeline_run_id) {
  stamp <- sub(
    "^.*_([0-9]{8}_[0-9]{6})$",
    "\\1",
    as.character(pipeline_run_id)
  )
  out <- as.POSIXct(stamp, format = "%Y%m%d_%H%M%S", tz = "UTC")
  out[stamp == pipeline_run_id] <- as.POSIXct(
    NA_real_,
    origin = "1970-01-01",
    tz = "UTC"
  )
  out
}

qualitative_actual_prepared_paths <- function(coded) {
  paths <- unique(as.character(coded$csv_path))
  resolved <- vapply(paths, qualitative_canonical_path, character(1))
  stats::setNames(resolved, paths)
}

qualitative_find_video_file <- function(root, subdirs, video_id, suffix) {
  directory <- do.call(file.path, as.list(c(root, subdirs)))
  matches <- list.files(
    directory,
    pattern = video_id,
    full.names = TRUE
  )
  matches <- matches[
    grepl(paste0(suffix, "$"), matches, ignore.case = TRUE)
  ]
  if (length(matches) != 1L) {
    stop(
      "Expected one ", suffix, " file for video ", video_id,
      " under ", directory, "; found ", length(matches), ".",
      call. = FALSE
    )
  }
  matches[[1]]
}

qualitative_align_chat_rows <- function(coded) {
  prepared_lookup <- qualitative_actual_prepared_paths(coded)
  coded[, prepared_path := unname(prepared_lookup[as.character(csv_path)])]
  coded[, talent_root := sub(
    "/qualitative coding/.*$",
    "",
    prepared_path,
    ignore.case = TRUE
  )]

  video_roots <- unique(coded[, .(video_id, talent_root)])
  raw <- data.table::rbindlist(
    lapply(seq_len(nrow(video_roots)), function(i) {
      video_id <- video_roots$video_id[[i]]
      root <- video_roots$talent_root[[i]]
      path <- qualitative_find_video_file(
        root,
        c("Chat", "Original"),
        video_id,
        "_chat\\.csv"
      )
      piece <- data.table::fread(
        path,
        select = c("video_id", "username", "message", "message_id")
      )
      piece[, source_order := seq_len(.N), by = video_id]
      piece
    }),
    use.names = TRUE,
    fill = TRUE
  )

  chat <- coded[source == "chat"]
  data.table::setorder(chat, video_id, row_number)
  chat[, source_order := seq_len(.N), by = video_id]

  expected <- chat[, .(coded_rows = .N), by = video_id]
  observed <- raw[, .(source_rows = .N), by = video_id]
  counts <- merge(expected, observed, by = "video_id", all = TRUE)
  if (any(counts$coded_rows != counts$source_rows)) {
    stop(
      "Chat source-row counts do not match the coded transcript rows.",
      call. = FALSE
    )
  }

  aligned <- merge(
    chat[, .(row_id, video_id, source_order, speaker, text)],
    raw[, .(
      video_id,
      source_order,
      source_username = username,
      source_text = message,
      source_record_key = message_id
    )],
    by = c("video_id", "source_order"),
    all.x = TRUE,
    sort = FALSE
  )

  speaker_matches <- as.character(aligned$speaker) ==
    as.character(aligned$source_username)
  if (any(!speaker_matches | is.na(speaker_matches))) {
    stop(
      "Chat source-order alignment produced speaker mismatches.",
      call. = FALSE
    )
  }
  if (any(is.na(aligned$source_record_key) |
      !nzchar(aligned$source_record_key))) {
    stop("Chat alignment produced missing message keys.", call. = FALSE)
  }

  exact_text <- as.character(aligned$text) ==
    as.character(aligned$source_text)
  aligned[, alignment_status := ifelse(
    exact_text,
    "exact_message_key",
    "source_order_verified"
  )]

  aligned[, .(row_id, source_record_key, alignment_status)]
}

qualitative_sql_string_list <- function(con, x) {
  paste(as.character(DBI::dbQuoteString(con, unique(x))), collapse = ", ")
}

qualitative_align_subtitle_rows <- function(coded, source_con) {
  if (!exists("CleanDuplicateTimestamps", mode = "function") ||
      !exists("period_to_seconds", mode = "function")) {
    stop(
      "Source clean_subtitles.R before aligning legacy subtitle rows.",
      call. = FALSE
    )
  }

  subtitle <- coded[source == "subtitle"]
  video_sql <- qualitative_sql_string_list(source_con, subtitle$video_id)
  raw <- DBI::dbGetQuery(
    source_con,
    paste0(
      "SELECT\n",
      "  subtitle_unit_key,\n",
      "  video_id,\n",
      "  subtitle_start,\n",
      "  subtitle_end,\n",
      "  subtitle_text\n",
      "FROM text.subtitle_units\n",
      "WHERE video_id IN (", video_sql, ")\n",
      "ORDER BY video_id, sequence_number"
    )
  )

  work <- tibble::tibble(
    subtitle_unit_key = raw$subtitle_unit_key,
    VideoID = raw$video_id,
    start_sec = period_to_seconds(raw$subtitle_start),
    stop_sec = period_to_seconds(raw$subtitle_end),
    Text = raw$subtitle_text
  )
  cleaned <- suppressWarnings(CleanDuplicateTimestamps(work))
  cleaned <- cleaned[
    !is.na(cleaned$Text) & nzchar(trimws(cleaned$Text)),
    ,
    drop = FALSE
  ]

  cleaned <- data.table::as.data.table(cleaned)
  data.table::setorder(cleaned, VideoID, start_sec, stop_sec)
  cleaned[, source_order := seq_len(.N), by = VideoID]

  data.table::setorder(subtitle, video_id, row_number)
  subtitle[, source_order := seq_len(.N), by = video_id]

  expected <- subtitle[, .(coded_rows = .N), by = video_id]
  observed <- cleaned[, .(source_rows = .N), by = VideoID]
  counts <- merge(
    expected,
    observed,
    by.x = "video_id",
    by.y = "VideoID",
    all = TRUE
  )
  if (any(counts$coded_rows != counts$source_rows)) {
    stop(
      "Replayed subtitle-row counts do not match the coded transcript rows.",
      call. = FALSE
    )
  }

  aligned <- merge(
    subtitle[, .(
      row_id,
      video_id,
      source_order,
      coded_seconds = as.numeric(sec),
      coded_text = as.character(text)
    )],
    cleaned[, .(
      video_id = VideoID,
      source_order,
      source_seconds = as.numeric(start_sec),
      source_text = as.character(Text),
      source_record_key = subtitle_unit_key
    )],
    by = c("video_id", "source_order"),
    all.x = TRUE,
    sort = FALSE
  )

  if (any(is.na(aligned$source_record_key) |
      !nzchar(aligned$source_record_key))) {
    stop("Subtitle alignment produced missing subtitle keys.", call. = FALSE)
  }

  exact_text <- aligned$coded_text == aligned$source_text
  exact_second <- floor(aligned$coded_seconds) ==
    floor(aligned$source_seconds)
  aligned[, alignment_status := ifelse(
    exact_text & exact_second,
    "exact_subtitle_key",
    "cleaning_replay_verified"
  )]

  aligned[, .(row_id, source_record_key, alignment_status)]
}

qualitative_prepare_transcripts <- function(
    coded,
    source_con,
    dataset_id,
    source_files
) {
  chat_alignment <- qualitative_align_chat_rows(coded)
  subtitle_alignment <- qualitative_align_subtitle_rows(coded, source_con)
  alignment <- data.table::rbindlist(
    list(chat_alignment, subtitle_alignment),
    use.names = TRUE
  )
  if (nrow(alignment) != nrow(coded) || anyDuplicated(alignment$row_id)) {
    stop("Transcript lineage alignment is incomplete.", call. = FALSE)
  }

  out <- merge(coded, alignment, by = "row_id", all.x = TRUE, sort = FALSE)
  video_sql <- qualitative_sql_string_list(source_con, out$video_id)
  catalog <- data.table::as.data.table(DBI::dbGetQuery(
    source_con,
    paste0(
      "SELECT video_id, talent_code\n",
      "FROM catalog.videos\n",
      "WHERE video_id IN (", video_sql, ")"
    )
  ))
  if (anyDuplicated(catalog$video_id)) {
    stop("Catalog contains duplicate selected video IDs.", call. = FALSE)
  }
  out <- merge(out, catalog, by = "video_id", all.x = TRUE, sort = FALSE)
  if (any(is.na(out$talent_code) | !nzchar(out$talent_code))) {
    stop("Some qualitative rows do not resolve to catalog talent codes.", call. = FALSE)
  }

  source_id_lookup <- stats::setNames(
    source_files$source_file_id,
    source_files$source_path
  )
  out[, source_file_id := unname(
    source_id_lookup[qualitative_canonical_path(coded_export_path)]
  )]
  if (any(is.na(out$source_file_id))) {
    stop("Some coded exports are missing source-file registration.", call. = FALSE)
  }

  out[, transcript_line_id := vapply(
    paste(dataset_id, row_id, sep = "::"),
    digest::digest,
    character(1),
    algo = "sha256",
    serialize = FALSE
  )]
  out[, text_sha256 := vapply(
    enc2utf8(as.character(text)),
    digest::digest,
    character(1),
    algo = "sha256",
    serialize = FALSE
  )]

  out[, .(
    transcript_line_id,
    dataset_id = dataset_id,
    video_id,
    talent_code,
    line_number = as.numeric(row_number),
    seconds = as.numeric(sec),
    timecode = as.character(timecode),
    source = as.character(source),
    speaker = as.character(speaker),
    text = as.character(text),
    source_record_key,
    alignment_status,
    source_file_id,
    source_file = as.character(source_file),
    legacy_row_id = as.character(row_id),
    legacy_target_unique_id = as.character(target_unique_id),
    text_sha256
  )]
}

qualitative_prepare_coding <- function(coded, transcripts, codebook) {
  code_columns <- codebook$code_column_name
  missing <- setdiff(code_columns, names(coded))
  if (length(missing) > 0L) {
    stop(
      "Coded exports are missing code columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  ids <- transcripts[, .(legacy_row_id, transcript_line_id)]
  out <- merge(
    coded,
    ids,
    by.x = "row_id",
    by.y = "legacy_row_id",
    all.x = TRUE,
    sort = FALSE
  )
  out[, pipeline_run_id := qualitative_pipeline_run_id(request_custom_id)]

  keep <- c(
    "transcript_line_id",
    "pipeline_run_id",
    "request_custom_id",
    "response_status",
    "confidence",
    "needs_review",
    "review_reason",
    "response_decision_count",
    "response_duplicate_status",
    "validation_error",
    code_columns
  )
  out <- out[, ..keep]
  out[, codebook_id := codebook$codebook_id[[1]]]

  for (column in code_columns) {
    values <- suppressWarnings(as.integer(out[[column]]))
    if (any(!is.na(values) & !(values %in% c(0L, 1L)))) {
      stop("Code column is not binary: ", column, call. = FALSE)
    }
    data.table::set(out, j = column, value = as.logical(values))
  }

  out
}

qualitative_register_source_files <- function(con, source_files) {
  for (i in seq_len(nrow(source_files))) {
    row <- source_files[i, , drop = FALSE]
    DBI::dbExecute(
      con,
      "INSERT INTO ops.source_files (
         source_file_id,
         source_path,
         source_type,
         file_size,
         checksum_sha256,
         modified_at,
         discovered_at,
         ingestion_status
       )
       VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, 'published')
       ON CONFLICT (source_path) DO UPDATE SET
         source_file_id = excluded.source_file_id,
         source_type = excluded.source_type,
         file_size = excluded.file_size,
         checksum_sha256 = excluded.checksum_sha256,
         modified_at = excluded.modified_at,
         ingestion_status = excluded.ingestion_status",
      params = list(
        row$source_file_id[[1]],
        row$source_path[[1]],
        row$source_type[[1]],
        row$file_size[[1]],
        row$checksum_sha256[[1]],
        row$modified_at[[1]]
      )
    )
  }
  invisible(NULL)
}

qualitative_register_pipeline_runs <- function(con, coding) {
  runs <- unique(as.character(coding$pipeline_run_id))
  for (run_id in runs) {
    started_at <- qualitative_pipeline_timestamp(run_id)
    if (is.na(started_at)) {
      started_at <- Sys.time()
    }
    DBI::dbExecute(
      con,
      "INSERT INTO ops.pipeline_runs (
         pipeline_run_id,
         pipeline_name,
         started_at,
         completed_at,
         status,
         error_summary
       )
       VALUES (?, 'qualitative_coding', ?, ?, 'completed', NULL)
       ON CONFLICT (pipeline_run_id) DO UPDATE SET
         pipeline_name = excluded.pipeline_name,
         completed_at = excluded.completed_at,
         status = excluded.status,
         error_summary = excluded.error_summary",
      params = list(run_id, started_at, started_at)
    )
  }
  invisible(NULL)
}

qualitative_upsert_codebook <- function(con, codebook) {
  DBI::dbWriteTable(
    con,
    "qualitative_codebook_stage",
    codebook,
    temporary = TRUE,
    overwrite = TRUE
  )
  DBI::dbExecute(
    con,
    "INSERT INTO qualitative.codebooks BY NAME
     SELECT * FROM qualitative_codebook_stage
     ON CONFLICT (codebook_id, code_id) DO UPDATE SET
       codebook_name = excluded.codebook_name,
       codebook_version = excluded.codebook_version,
       codebook_checksum = excluded.codebook_checksum,
       wide_view_name = excluded.wide_view_name,
       code_column_name = excluded.code_column_name,
       parent_code_id = excluded.parent_code_id,
       primary_code_id = excluded.primary_code_id,
       primary_code_name = excluded.primary_code_name,
       secondary_code_id = excluded.secondary_code_id,
       secondary_code_name = excluded.secondary_code_name,
       code_name = excluded.code_name,
       definition = excluded.definition,
       examples = excluded.examples,
       display_order = excluded.display_order,
       source_file_id = excluded.source_file_id"
  )
  invisible(NULL)
}

qualitative_upsert_transcripts <- function(con, transcripts) {
  DBI::dbWriteTable(
    con,
    "qualitative_transcript_stage",
    as.data.frame(transcripts),
    temporary = TRUE,
    overwrite = TRUE
  )
  DBI::dbExecute(
    con,
    "INSERT INTO qualitative.transcripts (
       transcript_line_id,
       dataset_id,
       video_id,
       talent_code,
       line_number,
       seconds,
       timecode,
       source,
       speaker,
       text,
       source_record_keys,
       alignment_status,
       source_file_id,
       source_file,
       legacy_row_id,
       legacy_target_unique_id,
       text_sha256
     )
     SELECT
       transcript_line_id,
       dataset_id,
       video_id,
       talent_code,
       line_number,
       seconds,
       timecode,
       source,
       speaker,
       text,
       CASE
         WHEN source_record_key IS NULL OR source_record_key = ''
           THEN []::VARCHAR[]
         ELSE [source_record_key]
       END,
       alignment_status,
       source_file_id,
       source_file,
       legacy_row_id,
       legacy_target_unique_id,
       text_sha256
     FROM qualitative_transcript_stage
     ON CONFLICT (transcript_line_id) DO UPDATE SET
       dataset_id = excluded.dataset_id,
       video_id = excluded.video_id,
       talent_code = excluded.talent_code,
       line_number = excluded.line_number,
       seconds = excluded.seconds,
       timecode = excluded.timecode,
       source = excluded.source,
       speaker = excluded.speaker,
       text = excluded.text,
       source_record_keys = excluded.source_record_keys,
       alignment_status = excluded.alignment_status,
       source_file_id = excluded.source_file_id,
       source_file = excluded.source_file,
       legacy_row_id = excluded.legacy_row_id,
       legacy_target_unique_id = excluded.legacy_target_unique_id,
       text_sha256 = excluded.text_sha256"
  )
  invisible(NULL)
}

qualitative_upsert_coding <- function(con, coding, codebook) {
  DBI::dbWriteTable(
    con,
    "qualitative_coding_stage",
    as.data.frame(coding),
    temporary = TRUE,
    overwrite = TRUE
  )

  code_ids <- as.character(DBI::dbQuoteString(con, codebook$code_id))
  code_columns <- as.character(
    DBI::dbQuoteIdentifier(con, codebook$code_column_name)
  )
  map_sql <- paste0(
    "map([",
    paste(code_ids, collapse = ", "),
    "], [",
    paste0("CAST(", code_columns, " AS BOOLEAN)", collapse = ", "),
    "])"
  )

  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO qualitative.coding (\n",
      "  transcript_line_id,\n",
      "  pipeline_run_id,\n",
      "  codebook_id,\n",
      "  request_custom_id,\n",
      "  response_status,\n",
      "  confidence,\n",
      "  needs_review,\n",
      "  review_reason,\n",
      "  response_decision_count,\n",
      "  response_duplicate_status,\n",
      "  validation_error,\n",
      "  code_values\n",
      ")\n",
      "SELECT\n",
      "  transcript_line_id,\n",
      "  pipeline_run_id,\n",
      "  codebook_id,\n",
      "  request_custom_id,\n",
      "  response_status,\n",
      "  confidence,\n",
      "  needs_review,\n",
      "  review_reason,\n",
      "  response_decision_count,\n",
      "  response_duplicate_status,\n",
      "  validation_error,\n  ",
      map_sql,
      "\nFROM qualitative_coding_stage\n",
      paste0(
        "ON CONFLICT (transcript_line_id, pipeline_run_id, codebook_id) ",
        "DO UPDATE SET\n"
      ),
      "  request_custom_id = excluded.request_custom_id,\n",
      "  response_status = excluded.response_status,\n",
      "  confidence = excluded.confidence,\n",
      "  needs_review = excluded.needs_review,\n",
      "  review_reason = excluded.review_reason,\n",
      "  response_decision_count = excluded.response_decision_count,\n",
      "  response_duplicate_status = excluded.response_duplicate_status,\n",
      "  validation_error = excluded.validation_error,\n",
      "  code_values = excluded.code_values"
    )
  )
  invisible(NULL)
}

qualitative_publish_summary <- function(con, dataset_id, codebook_id) {
  DBI::dbGetQuery(
    con,
    "SELECT
       count(*) AS coding_rows,
       count(DISTINCT t.transcript_line_id) AS transcript_rows,
       count(DISTINCT t.video_id) AS video_count,
       count(DISTINCT t.talent_code) AS talent_count,
       sum(c.response_status = 'coded') AS coded_rows,
       sum(c.response_status = 'unknown_code') AS unknown_code_rows,
       sum(c.response_status = 'missing_response') AS missing_response_rows
     FROM qualitative.coding c
     JOIN qualitative.transcripts t USING (transcript_line_id)
     WHERE t.dataset_id = ?
       AND c.codebook_id = ?",
    params = list(dataset_id, codebook_id)
  )
}

publish_qualitative_coding_dataset <- function(
    con,
    coded_data_dir,
    codebook_path,
    dataset_id,
    codebook_id,
    codebook_name,
    codebook_version,
    wide_view_name,
    dry_run = FALSE
) {
  qualitative_required_packages()
  qualitative_validate_identifier(dataset_id, label = "dataset ID")
  qualitative_validate_identifier(codebook_id, label = "codebook ID")
  qualitative_validate_identifier(wide_view_name, label = "wide-view name")

  coded_data_dir <- qualitative_canonical_path(coded_data_dir)
  codebook_path <- qualitative_canonical_path(codebook_path)
  coded <- qualitative_read_coded_exports(coded_data_dir)

  coded_paths <- sort(unique(coded$coded_export_path))
  source_files <- do.call(
    rbind,
    c(
      lapply(
        coded_paths,
        qualitative_file_identity,
        source_type = "qualitative_coded_export"
      ),
      list(qualitative_file_identity(
        codebook_path,
        source_type = "qualitative_codebook"
      ))
    )
  )
  row.names(source_files) <- NULL

  codebook_source_id <- source_files$source_file_id[
    source_files$source_path == codebook_path
  ][[1]]
  codebook <- qualitative_prepare_codebook(
    codebook_path = codebook_path,
    codebook_id = codebook_id,
    codebook_name = codebook_name,
    codebook_version = codebook_version,
    wide_view_name = wide_view_name,
    source_file_id = codebook_source_id
  )
  transcripts <- qualitative_prepare_transcripts(
    coded = coded,
    source_con = con,
    dataset_id = dataset_id,
    source_files = source_files
  )
  coding <- qualitative_prepare_coding(coded, transcripts, codebook)

  input_summary <- data.frame(
    transcript_rows = nrow(transcripts),
    coding_rows = nrow(coding),
    video_count = data.table::uniqueN(transcripts$video_id),
    talent_count = data.table::uniqueN(transcripts$talent_code),
    code_count = nrow(codebook),
    exact_alignment_rows = sum(grepl("^exact_", transcripts$alignment_status)),
    verified_alignment_rows = sum(
      !grepl("^exact_", transcripts$alignment_status)
    )
  )
  if (isTRUE(dry_run)) {
    return(list(
      summary = input_summary,
      transcripts = transcripts,
      coding = coding,
      codebook = codebook
    ))
  }

  DBI::dbBegin(con)
  committed <- FALSE
  on.exit({
    if (!committed) {
      try(DBI::dbRollback(con), silent = TRUE)
    }
  }, add = TRUE)

  init_qualitative_schema(con)
  qualitative_register_source_files(con, source_files)
  qualitative_register_pipeline_runs(con, coding)
  qualitative_upsert_codebook(con, codebook)
  qualitative_upsert_transcripts(con, transcripts)
  qualitative_upsert_coding(con, coding, codebook)
  create_qualitative_coding_view(con, codebook_id)

  DBI::dbCommit(con)
  committed <- TRUE

  list(
    summary = qualitative_publish_summary(con, dataset_id, codebook_id),
    input_summary = input_summary,
    view_name = paste0("qualitative.", wide_view_name)
  )
}
