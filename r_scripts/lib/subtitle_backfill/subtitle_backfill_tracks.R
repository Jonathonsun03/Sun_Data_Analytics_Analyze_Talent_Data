# Track discovery, loading, and current-result checks for subtitle backfills.

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

subtitle_backfill_column_exists <- function(con, schema_name, table_name, column_name) {
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT COUNT(*) = 1 AS available FROM information_schema.columns",
      "WHERE table_schema = ? AND table_name = ? AND column_name = ?"
    ),
    params = list(schema_name, table_name, column_name)
  )$available[[1]]
}

list_subtitle_backfill_tracks <- function(
    con,
    talent_code = NULL,
    video_id = NULL,
    pipeline_version = "subtitle_sentence_v2",
    source_scope = "full_track",
    exclude_current = FALSE) {
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
  source_where_sql <- if (length(conditions) == 0L) {
    ""
  } else {
    paste("WHERE", paste(conditions, collapse = " AND "))
  }
  can_exclude_current <- isTRUE(exclude_current) &&
    subtitle_backfill_relation_exists(con, "text", "subtitle_sentence_units") &&
    subtitle_backfill_column_exists(
      con,
      "text",
      "subtitle_sentence_units",
      "inferred_speaker_turn_id"
    )
  current_filter_sql <- if (can_exclude_current) {
    parameters <- c(parameters, list(source_scope, pipeline_version))
    paste(
      "WHERE NOT EXISTS (",
      "SELECT 1 FROM text.subtitle_sentence_units AS sentence",
      "WHERE sentence.video_id = track.video_id",
      "AND COALESCE(sentence.subtitle_language, '') =",
      "COALESCE(track.subtitle_language, '')",
      "AND COALESCE(sentence.subtitle_track_type, '') =",
      "COALESCE(track.subtitle_track_type, '')",
      "AND sentence.source_scope = ?",
      "AND sentence.source_checksum_sha256 = track.source_checksum_sha256",
      "AND sentence.pipeline_version = ?",
      "AND sentence.inferred_speaker_turn_id IS NOT NULL",
      ")"
    )
  } else {
    ""
  }

  row_checksum_sql <- paste(
    "COALESCE(CAST(subtitle.subtitle_unit_key AS VARCHAR), '<NA>')",
    "|| chr(31) || COALESCE(CAST(subtitle.video_id AS VARCHAR), '<NA>')",
    "|| chr(31) || COALESCE(CAST(subtitle.sequence_number AS VARCHAR), 'NA')",
    "|| chr(31) || COALESCE(CAST(subtitle.subtitle_start AS VARCHAR), '<NA>')",
    "|| chr(31) || COALESCE(CAST(subtitle.subtitle_end AS VARCHAR), '<NA>')",
    "|| chr(31) || COALESCE(CAST(subtitle.subtitle_text AS VARCHAR), '<NA>')",
    "|| chr(31) || COALESCE(CAST(subtitle.subtitle_language AS VARCHAR), '<NA>')",
    "|| chr(31) || COALESCE(CAST(subtitle.subtitle_track_type AS VARCHAR), '<NA>')"
  )

  DBI::dbGetQuery(
    con,
    paste(
      "WITH track AS (",
      "SELECT subtitle.video_id, subtitle.channel_id, subtitle.talent_code,",
      "subtitle.subtitle_language, subtitle.subtitle_track_type,",
      "video.content_type, video.title, COUNT(*) AS raw_rows,",
      "MIN(subtitle.sequence_number) AS sequence_start,",
      "MAX(subtitle.sequence_number) AS sequence_end,",
      "sha256(string_agg(", row_checksum_sql, ", chr(31)",
      "ORDER BY subtitle.video_id, subtitle.sequence_number",
      ")) AS source_checksum_sha256",
      "FROM text.subtitle_units AS subtitle",
      "LEFT JOIN catalog.videos AS video USING (video_id)",
      source_where_sql,
      "GROUP BY subtitle.video_id, subtitle.channel_id, subtitle.talent_code,",
      "subtitle.subtitle_language, subtitle.subtitle_track_type,",
      "video.content_type, video.title",
      ")",
      "SELECT video_id, channel_id, talent_code, subtitle_language,",
      "subtitle_track_type, content_type, title, raw_rows, sequence_start,",
      "sequence_end FROM track",
      current_filter_sql,
      "ORDER BY raw_rows, video_id"
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
  if (!subtitle_backfill_column_exists(
    con,
    "text",
    "subtitle_sentence_units",
    "inferred_speaker_turn_id"
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
      "AND pipeline_version = ?",
      "AND inferred_speaker_turn_id IS NOT NULL"
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
