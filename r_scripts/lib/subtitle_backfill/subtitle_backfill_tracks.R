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

