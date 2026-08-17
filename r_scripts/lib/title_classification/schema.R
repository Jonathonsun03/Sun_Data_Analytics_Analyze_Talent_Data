title_classification_title_hash_sql <- function(table_alias = "video") {
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", table_alias)) {
    stop("Unsafe table alias for title hash SQL: ", table_alias)
  }
  sprintf(
    paste0(
      "COALESCE(NULLIF(TRIM(%1$s.title_hash), ''), ",
      "LEFT(SHA256(%1$s.talent_code || '||' || LOWER(TRIM(",
      "REGEXP_REPLACE(COALESCE(%1$s.title, ''), '[[:space:]]+', ' ', 'g')",
      "))), 16))"
    ),
    table_alias
  )
}

ensure_title_classification_schema <- function(con) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.")
  }

  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS catalog")
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS classification")
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS ops")

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS classification.title_versions (
       title_version_id VARCHAR PRIMARY KEY,
       taxonomy_version VARCHAR NOT NULL,
       prompt_version VARCHAR NOT NULL,
       system_prompt VARCHAR NOT NULL,
       instructions VARCHAR NOT NULL,
       content_type_rules VARCHAR,
       definitions_json VARCHAR NOT NULL,
       user_template VARCHAR,
       output_schema_json VARCHAR NOT NULL,
       checksum_sha256 VARCHAR NOT NULL,
       active BOOLEAN NOT NULL DEFAULT FALSE,
       version_status VARCHAR NOT NULL DEFAULT 'complete',
       source_revision VARCHAR,
       created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
       updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
     )"
  )
  DBI::dbExecute(
    con,
    "ALTER TABLE classification.title_versions
     ADD COLUMN IF NOT EXISTS version_status VARCHAR DEFAULT 'complete'"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS catalog.talent_profiles (
       profile_id VARCHAR PRIMARY KEY,
       talent_code VARCHAR,
       profile_version VARCHAR NOT NULL,
       display_name VARCHAR NOT NULL,
       profile_json VARCHAR NOT NULL,
       checksum_sha256 VARCHAR NOT NULL,
       active BOOLEAN NOT NULL DEFAULT TRUE,
       source_pipeline_run_id VARCHAR,
       created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
       updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
     )"
  )

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
    "CREATE TABLE IF NOT EXISTS ops.source_files (
       source_file_id VARCHAR PRIMARY KEY,
       source_path VARCHAR UNIQUE NOT NULL,
       source_type VARCHAR NOT NULL,
       file_size BIGINT,
       checksum_sha256 VARCHAR,
       modified_at TIMESTAMP,
       discovered_at TIMESTAMP NOT NULL,
       ingestion_status VARCHAR NOT NULL
     )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS classification.title_classification_results (
       video_id VARCHAR,
       talent_code VARCHAR,
       channel_id VARCHAR,
       legacy_talent_id VARCHAR,
       title_hash VARCHAR,
       taxonomy_version VARCHAR,
       prompt_version VARCHAR,
       model VARCHAR,
       talent_profile VARCHAR,
       classification_json VARCHAR,
       confidence DOUBLE,
       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
     )"
  )

  for (column_sql in c(
    "title_version_id VARCHAR",
    "profile_id VARCHAR",
    "pipeline_run_id VARCHAR"
  )) {
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE classification.title_classification_results ",
        "ADD COLUMN IF NOT EXISTS ",
        column_sql
      )
    )
  }

  DBI::dbExecute(
    con,
    "UPDATE classification.title_classification_results
     SET title_version_id = 'title-' || prompt_version
     WHERE title_version_id IS NULL
       AND prompt_version IS NOT NULL"
  )

  DBI::dbExecute(
    con,
    "INSERT INTO classification.title_versions (
       title_version_id, taxonomy_version, prompt_version,
       system_prompt, instructions, content_type_rules,
       definitions_json, user_template, output_schema_json,
       checksum_sha256, active, version_status, source_revision
     )
     SELECT
       result.title_version_id,
       MIN(result.taxonomy_version),
       MIN(result.prompt_version),
       '', '', '', '[]', '', '{}',
       'legacy-metadata:' || result.title_version_id,
       FALSE,
       'metadata_only',
       'migrated_from_legacy_results'
     FROM classification.title_classification_results AS result
     WHERE result.title_version_id IS NOT NULL
       AND NOT EXISTS (
         SELECT 1
         FROM classification.title_versions AS version
         WHERE version.title_version_id = result.title_version_id
       )
     GROUP BY result.title_version_id"
  )

  DBI::dbExecute(
    con,
    "UPDATE classification.title_classification_results AS result
     SET profile_id = NULL
     WHERE result.profile_id IS NOT NULL
       AND NOT EXISTS (
         SELECT 1
         FROM catalog.talent_profiles AS profile
         WHERE profile.profile_id = result.profile_id
           AND profile.profile_version = result.prompt_version
       )"
  )

  DBI::dbExecute(
    con,
    "UPDATE classification.title_classification_results AS result
     SET profile_id = profile.profile_id
     FROM catalog.talent_profiles AS profile
     WHERE result.profile_id IS NULL
       AND profile.active
       AND profile.talent_code = result.talent_code
       AND profile.profile_version = result.prompt_version"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS classification.title_classification_scheduled_state (
       state_key VARCHAR PRIMARY KEY,
       run_dir VARCHAR,
       manifest_path VARCHAR,
       batch_id VARCHAR,
       input_file_id VARCHAR,
       output_file_id VARCHAR,
       error_file_id VARCHAR,
       status VARCHAR,
       created_at VARCHAR,
       submitted_at VARCHAR,
       last_checked_at VARCHAR,
       applied_at VARCHAR,
       request_count BIGINT,
       pending_rows BIGINT,
       artifacts_json VARCHAR,
       state_json VARCHAR,
       updated_at VARCHAR
     )"
  )

  effective_hash_sql <- title_classification_title_hash_sql("video")
  DBI::dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE VIEW classification.title_classification_status AS
     WITH active_version AS (
       SELECT title_version_id
       FROM classification.title_versions
       WHERE active
       ORDER BY updated_at DESC
       LIMIT 1
     ), current_videos AS (
       SELECT video.*, ", effective_hash_sql, " AS effective_title_hash
       FROM catalog.videos AS video
       WHERE video.is_available
     )
     SELECT
       video.video_id,
       video.talent_code,
       video.channel_id,
       video.title,
       video.effective_title_hash AS title_hash,
       video.published_at,
       version.title_version_id,
       EXISTS (
         SELECT 1
         FROM classification.title_classification_results AS result
         WHERE result.video_id = video.video_id
           AND result.title_version_id = version.title_version_id
       ) AS has_version_result,
       EXISTS (
         SELECT 1
         FROM classification.title_classification_results AS result
         WHERE result.video_id = video.video_id
           AND result.title_hash = video.effective_title_hash
           AND result.title_version_id = version.title_version_id
       ) AS is_classified,
       CASE
         WHEN EXISTS (
           SELECT 1
           FROM classification.title_classification_results AS result
           WHERE result.video_id = video.video_id
             AND result.title_hash = video.effective_title_hash
             AND result.title_version_id = version.title_version_id
         ) THEN 'classified'
         WHEN EXISTS (
           SELECT 1
           FROM classification.title_classification_results AS result
           WHERE result.video_id = video.video_id
             AND result.title_version_id = version.title_version_id
         ) THEN 'changed_title'
         ELSE 'new_or_unclassified'
       END AS classification_status
     FROM current_videos AS video
     CROSS JOIN active_version AS version"
    )
  )

  invisible(TRUE)
}

ensure_title_result_boolean_columns <- function(con, column_names) {
  for (column_name in unique(column_names)) {
    if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", column_name)) {
      stop("Unsafe classification column name: ", column_name)
    }
    DBI::dbExecute(
      con,
      sprintf(
        paste(
          "ALTER TABLE classification.title_classification_results",
          "ADD COLUMN IF NOT EXISTS \"%s\" BOOLEAN"
        ),
        column_name
      )
    )
  }
  invisible(TRUE)
}
