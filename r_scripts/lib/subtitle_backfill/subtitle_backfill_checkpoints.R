# Database and filesystem checkpoint helpers for resumable subtitle backfills.

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

subtitle_backfill_load_checkpoint_state <- function(
    db_path,
    context,
    checkpoints = NULL,
    checkpoint_dir = NULL) {
  # Direct callers may omit the checkpoints normally loaded by the batch runner.
  if (is.null(checkpoints)) {
    read_con <- duckdb_connect(db_path = db_path, read_only = TRUE)
    checkpoints <- tryCatch(
      load_subtitle_backfill_checkpoints(
        read_con,
        context$video_id,
        context$subtitle_language,
        context$subtitle_track_type,
        context$source_checksum,
        context$pipeline_version,
        context$source_scope
      ),
      finally = DBI::dbDisconnect(read_con, shutdown = TRUE)
    )
  }

  path <- subtitle_backfill_checkpoint_path(
    db_path,
    context$video_id,
    checkpoint_dir
  )
  local <- if (file.exists(path)) readRDS(path) else data.frame()
  if (nrow(local) > 0L) {
    subtitle_sentence_require_columns(
      local,
      c(
        "reconstruction_block_key", "status", "attempt_count",
        "punctuated_text", "punctuation_model"
      ),
      "Local checkpoints"
    )
  }

  combined <- dplyr::bind_rows(local, checkpoints)
  if (nrow(combined) > 0L) {
    combined <- combined[
      !duplicated(combined$reconstruction_block_key),
      ,
      drop = FALSE
    ]
  }
  list(all = combined, local = local, path = path)
}
