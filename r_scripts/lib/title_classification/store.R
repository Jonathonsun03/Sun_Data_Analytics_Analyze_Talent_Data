title_classification_connect <- function(db_path = NULL, read_only = FALSE) {
  if (is.null(db_path) || !nzchar(db_path)) {
    db_path <- talent_lakehouse_db_path()
  }
  duckdb_connect(db_path = db_path, read_only = read_only)
}

title_classification_pipeline_run_id <- function(prefix = "title_classification") {
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
  suffix <- substr(digest::digest(paste(stamp, Sys.getpid(), runif(1))), 1, 12)
  paste(prefix, stamp, suffix, sep = "_")
}

register_title_classification_run <- function(
    con,
    pipeline_run_id,
    status = "running",
    pipeline_name = "title_classification"
) {
  DBI::dbExecute(
    con,
    "INSERT INTO ops.pipeline_runs (
       pipeline_run_id, pipeline_name, started_at, status
     ) VALUES (?, ?, CURRENT_TIMESTAMP, ?)
     ON CONFLICT (pipeline_run_id) DO UPDATE SET status = excluded.status",
    params = list(pipeline_run_id, pipeline_name, status)
  )
}

complete_title_classification_run <- function(
    con,
    pipeline_run_id,
    status = "completed",
    error_summary = NULL
) {
  if (is.null(error_summary)) error_summary <- NA_character_
  DBI::dbExecute(
    con,
    "UPDATE ops.pipeline_runs
     SET completed_at = CURRENT_TIMESTAMP,
         status = ?,
         error_summary = ?
     WHERE pipeline_run_id = ?",
    params = list(status, error_summary, pipeline_run_id)
  )
}

register_title_classification_source_file <- function(
    con,
    source_path,
    source_type,
    ingestion_status = "ingested"
) {
  if (!file.exists(source_path)) {
    stop("Source file does not exist: ", source_path)
  }
  normalized_path <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  info <- file.info(normalized_path)
  checksum <- digest::digest(file = normalized_path, algo = "sha256")
  source_file_id <- paste0("source_", substr(digest::digest(normalized_path), 1, 24))
  DBI::dbExecute(
    con,
    "INSERT INTO ops.source_files (
       source_file_id, source_path, source_type, file_size,
       checksum_sha256, modified_at, discovered_at, ingestion_status
     ) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?)
     ON CONFLICT (source_path) DO UPDATE SET
       source_type = excluded.source_type,
       file_size = excluded.file_size,
       checksum_sha256 = excluded.checksum_sha256,
       modified_at = excluded.modified_at,
       ingestion_status = excluded.ingestion_status",
    params = list(
      source_file_id,
      normalized_path,
      source_type,
      as.numeric(info$size[[1]]),
      checksum,
      as.POSIXct(info$mtime[[1]], tz = "UTC"),
      ingestion_status
    )
  )
  invisible(source_file_id)
}

publish_title_version <- function(
    con,
    title_version_id,
    taxonomy_version,
    prompt_version,
    system_prompt,
    instructions,
    content_type_rules,
    definitions,
    user_template,
    output_schema,
    active = TRUE,
    source_revision = NA_character_
) {
  definitions_json <- as.character(jsonlite::toJSON(
    definitions,
    auto_unbox = TRUE,
    null = "null"
  ))
  output_schema_json <- as.character(jsonlite::toJSON(
    output_schema,
    auto_unbox = TRUE,
    null = "null"
  ))
  checksum <- digest::digest(
    paste(
      system_prompt,
      instructions,
      content_type_rules,
      definitions_json,
      user_template,
      output_schema_json,
      sep = "\n---\n"
    ),
    algo = "sha256"
  )

  DBI::dbWithTransaction(con, {
    if (isTRUE(active)) {
      DBI::dbExecute(con, "UPDATE classification.title_versions SET active = FALSE WHERE active")
    }
    DBI::dbExecute(
      con,
      "INSERT INTO classification.title_versions (
         title_version_id, taxonomy_version, prompt_version,
         system_prompt, instructions, content_type_rules,
         definitions_json, user_template, output_schema_json,
         checksum_sha256, active, version_status, source_revision
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'complete', ?)
       ON CONFLICT (title_version_id) DO UPDATE SET
         taxonomy_version = excluded.taxonomy_version,
         prompt_version = excluded.prompt_version,
         system_prompt = excluded.system_prompt,
         instructions = excluded.instructions,
         content_type_rules = excluded.content_type_rules,
         definitions_json = excluded.definitions_json,
         user_template = excluded.user_template,
         output_schema_json = excluded.output_schema_json,
         checksum_sha256 = excluded.checksum_sha256,
         active = excluded.active,
         version_status = excluded.version_status,
         source_revision = excluded.source_revision,
         updated_at = now()",
      params = list(
        title_version_id,
        taxonomy_version,
        prompt_version,
        system_prompt,
        instructions,
        content_type_rules,
        definitions_json,
        user_template,
        output_schema_json,
        checksum,
        active,
        source_revision
      )
    )
  })

  invisible(list(title_version_id = title_version_id, checksum_sha256 = checksum))
}

publish_talent_profile <- function(
    con,
    profile_id,
    talent_code = NA_character_,
    profile_version,
    display_name,
    profile,
    active = TRUE,
    source_pipeline_run_id = NA_character_
) {
  profile_json <- as.character(jsonlite::toJSON(
    profile,
    auto_unbox = TRUE,
    null = "null"
  ))
  checksum <- digest::digest(profile_json, algo = "sha256")
  normalized_talent_code <- if (is.na(talent_code) || !nzchar(talent_code)) NA_character_ else talent_code

  DBI::dbWithTransaction(con, {
    if (isTRUE(active)) {
      if (is.na(normalized_talent_code)) {
        DBI::dbExecute(
          con,
          "UPDATE catalog.talent_profiles SET active = FALSE WHERE talent_code IS NULL"
        )
      } else {
        DBI::dbExecute(
          con,
          "UPDATE catalog.talent_profiles SET active = FALSE WHERE talent_code = ?",
          params = list(normalized_talent_code)
        )
      }
    }
    DBI::dbExecute(
      con,
      "INSERT INTO catalog.talent_profiles (
         profile_id, talent_code, profile_version, display_name,
         profile_json, checksum_sha256, active, source_pipeline_run_id
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT (profile_id) DO UPDATE SET
         talent_code = excluded.talent_code,
         profile_version = excluded.profile_version,
         display_name = excluded.display_name,
         profile_json = excluded.profile_json,
         checksum_sha256 = excluded.checksum_sha256,
         active = excluded.active,
         source_pipeline_run_id = excluded.source_pipeline_run_id,
         updated_at = now()",
      params = list(
        profile_id,
        normalized_talent_code,
        profile_version,
        display_name,
        profile_json,
        checksum,
        active,
        source_pipeline_run_id
      )
    )
  })

  invisible(list(profile_id = profile_id, checksum_sha256 = checksum))
}

load_active_title_version <- function(con, title_version_id = NULL) {
  if (is.null(title_version_id) || !nzchar(title_version_id)) {
    rows <- DBI::dbGetQuery(
      con,
      "SELECT *
       FROM classification.title_versions
       WHERE active
       ORDER BY updated_at DESC
       LIMIT 1"
    )
  } else {
    rows <- DBI::dbGetQuery(
      con,
      "SELECT *
       FROM classification.title_versions
       WHERE title_version_id = ?
       LIMIT 1",
      params = list(title_version_id)
    )
  }
  if (nrow(rows) != 1L) {
    stop("No title-classification version is available.")
  }
  rows[1, , drop = FALSE]
}

load_active_talent_profile <- function(con, talent_code) {
  rows <- DBI::dbGetQuery(
    con,
    "SELECT *
     FROM catalog.talent_profiles
     WHERE active
       AND (talent_code = ? OR talent_code IS NULL)
     ORDER BY CASE WHEN talent_code = ? THEN 0 ELSE 1 END, updated_at DESC
     LIMIT 1",
    params = list(talent_code, talent_code)
  )
  if (nrow(rows) != 1L) {
    stop("No active talent profile or default profile exists for: ", talent_code)
  }
  rows[1, , drop = FALSE]
}

load_title_prompt_bundle <- function(con, talent_code, title_version_id = NULL) {
  version <- load_active_title_version(con, title_version_id)
  profile <- load_active_talent_profile(con, talent_code)
  definitions <- jsonlite::fromJSON(version$definitions_json[[1]], simplifyVector = FALSE)
  schema <- jsonlite::fromJSON(version$output_schema_json[[1]], simplifyVector = FALSE)
  profile_data <- jsonlite::fromJSON(profile$profile_json[[1]], simplifyVector = FALSE)
  title_context <- profile_data$contexts$title_classification
  overlay_text <- title_context$overlay_text
  if (is.null(overlay_text) || !nzchar(overlay_text)) {
    overlay_text <- "No talent-specific title guidance is available. Apply the shared definitions conservatively."
  }

  user_prompt_template <- compile_user_prompt_template(
    instructions_text = version$instructions[[1]],
    definitions = definitions,
    overlay_text = overlay_text,
    content_type_rules_text = version$content_type_rules[[1]]
  )

  list(
    title_version_id = version$title_version_id[[1]],
    taxonomy_version = version$taxonomy_version[[1]],
    prompt_version = version$prompt_version[[1]],
    checksum_sha256 = version$checksum_sha256[[1]],
    profile_id = profile$profile_id[[1]],
    profile_name = profile$display_name[[1]],
    profile_version = profile$profile_version[[1]],
    system_prompt = version$system_prompt[[1]],
    user_prompt_template = user_prompt_template,
    definitions = definitions,
    definition_fields = unique(vapply(definitions, `[[`, character(1), "field_name")),
    schema = schema,
    schema_text = as.character(jsonlite::toJSON(
      schema,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    ))
  )
}

list_pending_title_classifications <- function(
    con,
    title_version_id = NULL,
    talent_code = NULL,
    force_reclassify = FALSE,
    limit = 0L
) {
  version <- load_active_title_version(con, title_version_id)
  effective_hash_sql <- title_classification_title_hash_sql("video")
  where_parts <- c("video.is_available")
  params <- list()
  if (!is.null(talent_code) && nzchar(talent_code)) {
    where_parts <- c(
      where_parts,
      "(LOWER(video.talent_code) = LOWER(?) OR LOWER(talent.talent_name) = LOWER(?))"
    )
    params <- c(params, list(talent_code, talent_code))
  }
  if (!isTRUE(force_reclassify)) {
    where_parts <- c(
      where_parts,
      paste0(
        "NOT EXISTS (",
        " SELECT 1 FROM classification.title_classification_results AS result",
        " WHERE result.video_id = video.video_id",
        " AND result.title_hash = ", effective_hash_sql,
        " AND result.title_version_id = ?",
        ")"
      )
    )
    params <- c(params, list(version$title_version_id[[1]]))
  }

  limit_sql <- ""
  if (!is.na(limit) && limit > 0L) {
    limit_sql <- sprintf(" LIMIT %d", as.integer(limit))
  }
  sql <- paste0(
    "SELECT video.video_id, video.talent_code, video.channel_id,",
    " talent.talent_name, talent.legacy_talent_id,",
    " video.title AS title_raw, ", effective_hash_sql, " AS title_hash,",
    " video.content_type, video.published_at",
    " FROM catalog.videos AS video",
    " LEFT JOIN catalog.talents AS talent USING (talent_code)",
    " WHERE ", paste(where_parts, collapse = " AND "),
    " ORDER BY video.talent_code, video.published_at, video.video_id",
    limit_sql
  )
  DBI::dbGetQuery(con, sql, params = params)
}

insert_title_classification_results <- function(
    con,
    rows,
    prompt_bundle,
    model,
    pipeline_run_id
) {
  definition_fields <- prompt_bundle$definition_fields
  ensure_title_result_boolean_columns(con, definition_fields)
  base_columns <- c(
    "video_id", "talent_code", "channel_id", "legacy_talent_id", "title_hash",
    "title_version_id", "taxonomy_version", "prompt_version", "model",
    "talent_profile", "profile_id", "pipeline_run_id",
    "classification_json", "confidence"
  )
  all_columns <- c(base_columns, definition_fields)
  column_sql <- paste(sprintf("\"%s\"", all_columns), collapse = ", ")
  placeholder_sql <- paste(rep("?", length(all_columns)), collapse = ", ")
  inserted <- 0L

  for (i in seq_len(nrow(rows))) {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT EXISTS (
         SELECT 1
         FROM classification.title_classification_results
         WHERE video_id = ? AND title_hash = ? AND title_version_id = ?
       ) AS already_exists",
      params = list(
        rows$video_id[[i]],
        rows$title_hash[[i]],
        prompt_bundle$title_version_id
      )
    )$already_exists[[1]]
    if (isTRUE(exists)) {
      next
    }

    values <- list(
      rows$video_id[[i]], rows$talent_code[[i]], rows$channel_id[[i]],
      rows$legacy_talent_id[[i]], rows$title_hash[[i]],
      prompt_bundle$title_version_id, prompt_bundle$taxonomy_version,
      prompt_bundle$prompt_version, model, prompt_bundle$profile_name,
      prompt_bundle$profile_id, pipeline_run_id,
      rows$classification_json[[i]], rows$confidence[[i]]
    )
    for (field_name in definition_fields) {
      values[[length(values) + 1L]] <- rows[[field_name]][[i]]
    }
    inserted <- inserted + DBI::dbExecute(
      con,
      paste0(
        "INSERT INTO classification.title_classification_results (",
        column_sql,
        ") VALUES (",
        placeholder_sql,
        ")"
      ),
      params = values
    )
  }
  inserted
}

delete_title_classification_results <- function(con, video_ids, title_version_id) {
  video_ids <- unique(as.character(video_ids))
  video_ids <- video_ids[nzchar(video_ids)]
  if (length(video_ids) == 0L) {
    return(0L)
  }
  DBI::dbWriteTable(
    con,
    "title_classification_delete_ids",
    data.frame(video_id = video_ids),
    temporary = TRUE,
    overwrite = TRUE
  )
  DBI::dbExecute(
    con,
    "DELETE FROM classification.title_classification_results
     WHERE title_version_id = ?
       AND video_id IN (SELECT video_id FROM title_classification_delete_ids)",
    params = list(title_version_id)
  )
}
