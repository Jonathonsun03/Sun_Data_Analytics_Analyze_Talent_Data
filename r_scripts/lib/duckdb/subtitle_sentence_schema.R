init_subtitle_sentence_schema <- function(con) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.", call. = FALSE)
  }

  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS text")
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS ops")

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS ops.pipeline_runs (
       pipeline_run_id VARCHAR PRIMARY KEY,
       pipeline_name VARCHAR NOT NULL,
       started_at TIMESTAMP NOT NULL,
       completed_at TIMESTAMP,
       status VARCHAR NOT NULL,
       error_summary VARCHAR
     )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS text.subtitle_sentence_units (
       sentence_unit_key VARCHAR PRIMARY KEY,
       video_id VARCHAR NOT NULL,
       channel_id VARCHAR,
       talent_code VARCHAR,
       subtitle_language VARCHAR,
       subtitle_track_type VARCHAR,
       source_scope VARCHAR NOT NULL,
       source_sequence_start BIGINT,
       source_sequence_end BIGINT,
       block_number BIGINT NOT NULL,
       sentence_number BIGINT NOT NULL,
       speaker_turn_id BIGINT,
       speaker_change BOOLEAN NOT NULL,
       start_sec DOUBLE,
       end_sec DOUBLE,
       sentence_text VARCHAR NOT NULL,
       source_subtitle_unit_keys VARCHAR[] NOT NULL,
       source_alignment_status VARCHAR NOT NULL,
       punctuation_model VARCHAR NOT NULL,
       timestamps_approximate BOOLEAN NOT NULL,
       timestamp_method VARCHAR NOT NULL,
       source_checksum_sha256 VARCHAR NOT NULL,
       block_input_checksum_sha256 VARCHAR NOT NULL,
       pipeline_version VARCHAR NOT NULL,
       pipeline_run_id VARCHAR NOT NULL,
       created_at TIMESTAMP NOT NULL
     )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS ops.subtitle_reconstruction_blocks (
       reconstruction_block_key VARCHAR PRIMARY KEY,
       video_id VARCHAR NOT NULL,
       talent_code VARCHAR,
       subtitle_language VARCHAR,
       subtitle_track_type VARCHAR,
       source_scope VARCHAR NOT NULL,
       source_sequence_start BIGINT,
       source_sequence_end BIGINT,
       block_number BIGINT NOT NULL,
       start_sec DOUBLE,
       end_sec DOUBLE,
       model_input_text VARCHAR NOT NULL,
       punctuated_text VARCHAR,
       source_subtitle_unit_keys VARCHAR[] NOT NULL,
       source_checksum_sha256 VARCHAR NOT NULL,
       block_input_checksum_sha256 VARCHAR NOT NULL,
       punctuation_model VARCHAR,
       pipeline_version VARCHAR NOT NULL,
       status VARCHAR NOT NULL CHECK (
         status IN ('pending', 'running', 'complete', 'failed')
       ),
       attempt_count BIGINT NOT NULL,
       error_summary VARCHAR,
       pipeline_run_id VARCHAR NOT NULL,
       created_at TIMESTAMP NOT NULL,
       updated_at TIMESTAMP NOT NULL,
       completed_at TIMESTAMP
     )"
  )

  invisible(TRUE)
}
