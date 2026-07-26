qualitative_validate_identifier <- function(x, label = "identifier") {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[A-Za-z][A-Za-z0-9_]*$", x)) {
    stop("Unsafe ", label, ": ", paste(x, collapse = ", "), call. = FALSE)
  }
  x
}

qualitative_coding_table_sql <- function(table_name = "coding") {
  table_name <- qualitative_validate_identifier(
    table_name,
    label = "qualitative coding table"
  )
  paste0(
    "CREATE TABLE qualitative.", table_name, " (\n",
    "  transcript_line_id VARCHAR NOT NULL,\n",
    "  pipeline_run_id VARCHAR NOT NULL,\n",
    "  codebook_id VARCHAR NOT NULL,\n",
    "  request_custom_id VARCHAR,\n",
    "  response_status VARCHAR NOT NULL,\n",
    "  confidence VARCHAR,\n",
    "  needs_review BOOLEAN,\n",
    "  review_reason VARCHAR,\n",
    "  response_decision_count BIGINT,\n",
    "  response_duplicate_status VARCHAR,\n",
    "  validation_error VARCHAR,\n",
    "  code_values MAP(VARCHAR, BOOLEAN) NOT NULL,\n",
    "  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\n",
    paste0(
      "  PRIMARY KEY(transcript_line_id, pipeline_run_id, codebook_id)\n"
    ),
    ")"
  )
}

qualitative_migrate_coding_primary_key <- function(con) {
  primary_key <- DBI::dbGetQuery(
    con,
    "SELECT
       array_to_string(constraint_column_names, ',') AS key_columns
     FROM duckdb_constraints()
     WHERE schema_name = 'qualitative'
       AND table_name = 'coding'
       AND constraint_type = 'PRIMARY KEY'"
  )
  if (nrow(primary_key) != 1L) {
    stop(
      "qualitative.coding must have exactly one primary key.",
      call. = FALSE
    )
  }

  key_columns <- primary_key$key_columns[[1]]
  expected <- "transcript_line_id,pipeline_run_id,codebook_id"
  if (identical(key_columns, expected)) {
    return(invisible(FALSE))
  }
  legacy <- "transcript_line_id,pipeline_run_id"
  if (!identical(key_columns, legacy)) {
    stop(
      "Unexpected qualitative.coding primary key: ",
      key_columns,
      call. = FALSE
    )
  }

  view_metadata <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT codebook_id, wide_view_name
     FROM qualitative.codebooks
     ORDER BY codebook_id"
  )
  for (view_name in unique(view_metadata$wide_view_name)) {
    view_name <- qualitative_validate_identifier(
      view_name,
      label = "qualitative wide-view name"
    )
    DBI::dbExecute(
      con,
      paste0(
        "DROP VIEW IF EXISTS qualitative.",
        as.character(DBI::dbQuoteIdentifier(con, view_name))
      )
    )
  }

  DBI::dbExecute(
    con,
    qualitative_coding_table_sql("coding_primary_key_migration")
  )
  DBI::dbExecute(
    con,
    "INSERT INTO qualitative.coding_primary_key_migration
     SELECT * FROM qualitative.coding"
  )
  DBI::dbExecute(con, "DROP TABLE qualitative.coding")
  DBI::dbExecute(
    con,
    paste(
      "ALTER TABLE qualitative.coding_primary_key_migration",
      "RENAME TO coding"
    )
  )

  for (codebook_id in view_metadata$codebook_id) {
    create_qualitative_coding_view(con, codebook_id)
  }
  invisible(TRUE)
}

init_qualitative_schema <- function(con) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.", call. = FALSE)
  }

  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS qualitative")

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS qualitative.transcripts (
       transcript_line_id VARCHAR PRIMARY KEY,
       dataset_id VARCHAR NOT NULL,
       video_id VARCHAR NOT NULL,
       talent_code VARCHAR NOT NULL,
       line_number BIGINT NOT NULL,
       seconds DOUBLE,
       timecode VARCHAR,
       source VARCHAR NOT NULL,
       speaker VARCHAR,
       text VARCHAR NOT NULL,
       source_record_keys VARCHAR[],
       alignment_status VARCHAR NOT NULL,
       source_file_id VARCHAR,
       source_file VARCHAR,
       legacy_row_id VARCHAR,
       legacy_target_unique_id VARCHAR,
       text_sha256 VARCHAR NOT NULL,
       created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
       UNIQUE(dataset_id, legacy_row_id)
     )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS qualitative.codebooks (
       codebook_id VARCHAR NOT NULL,
       codebook_name VARCHAR NOT NULL,
       codebook_version VARCHAR NOT NULL,
       codebook_checksum VARCHAR NOT NULL,
       wide_view_name VARCHAR NOT NULL,
       code_id VARCHAR NOT NULL,
       code_column_name VARCHAR NOT NULL,
       parent_code_id VARCHAR,
       primary_code_id VARCHAR,
       primary_code_name VARCHAR,
       secondary_code_id VARCHAR,
       secondary_code_name VARCHAR,
       code_name VARCHAR NOT NULL,
       definition VARCHAR,
       examples VARCHAR,
       display_order INTEGER NOT NULL,
       source_file_id VARCHAR,
       created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
       PRIMARY KEY(codebook_id, code_id),
       UNIQUE(codebook_id, code_column_name)
     )"
  )

  DBI::dbExecute(
    con,
    sub(
      "^CREATE TABLE ",
      "CREATE TABLE IF NOT EXISTS ",
      qualitative_coding_table_sql()
    )
  )
  qualitative_migrate_coding_primary_key(con)

  invisible(con)
}

create_qualitative_coding_view <- function(con, codebook_id) {
  qualitative_validate_identifier(
    sub("^coding_", "", codebook_id),
    label = "codebook identifier"
  )

  codebook <- DBI::dbGetQuery(
    con,
    "SELECT
       code_id,
       code_column_name,
       wide_view_name,
       display_order
     FROM qualitative.codebooks
     WHERE codebook_id = ?
     ORDER BY display_order, code_id",
    params = list(codebook_id)
  )

  if (nrow(codebook) == 0L) {
    stop("Unknown qualitative codebook: ", codebook_id, call. = FALSE)
  }

  view_names <- unique(codebook$wide_view_name)
  if (length(view_names) != 1L) {
    stop(
      "Codebook `", codebook_id, "` has inconsistent wide-view names.",
      call. = FALSE
    )
  }

  view_name <- qualitative_validate_identifier(
    view_names[[1]],
    label = "qualitative wide-view name"
  )
  invisible(vapply(
    codebook$code_column_name,
    qualitative_validate_identifier,
    character(1),
    label = "qualitative code column"
  ))

  code_select <- vapply(seq_len(nrow(codebook)), function(i) {
    code_id <- as.character(DBI::dbQuoteString(con, codebook$code_id[[i]]))
    column_name <- as.character(
      DBI::dbQuoteIdentifier(con, codebook$code_column_name[[i]])
    )
    paste0("code_values[", code_id, "] AS ", column_name)
  }, character(1))

  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, "qualitative"))
  view_sql <- as.character(DBI::dbQuoteIdentifier(con, view_name))
  codebook_sql <- as.character(DBI::dbQuoteString(con, codebook_id))

  DBI::dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE VIEW ", schema_sql, ".", view_sql, " AS\n",
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
      paste(code_select, collapse = ",\n  "),
      "\nFROM qualitative.coding\n",
      "WHERE codebook_id = ", codebook_sql
    )
  )

  invisible(paste0("qualitative.", view_name))
}
