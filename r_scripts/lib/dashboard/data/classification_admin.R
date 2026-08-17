classification_admin_scalar <- function(value, default = NA_character_) {
  if (is.null(value) || length(value) == 0L) return(default)
  value <- value[[1]]
  if (is.null(value) || length(value) == 0L) return(default)
  as.character(value)
}

classification_admin_integer <- function(value, default = NA_integer_) {
  scalar <- classification_admin_scalar(value, default = NA_character_)
  parsed <- suppressWarnings(as.integer(scalar))
  if (is.na(parsed)) default else parsed
}

classification_admin_parse_time <- function(value) {
  if (inherits(value, "POSIXt")) return(as.POSIXct(value, tz = "UTC"))
  value <- as.character(value)
  value[!nzchar(value) | is.na(value)] <- NA_character_
  value <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", value)
  parsed <- as.POSIXct(
    value,
    format = "%Y-%m-%dT%H:%M:%OS%z",
    tz = "UTC"
  )
  missing <- is.na(parsed) & !is.na(value)
  if (any(missing)) {
    parsed[missing] <- as.POSIXct(value[missing], tz = "UTC")
  }
  parsed
}

classification_admin_read_json <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) NULL
  )
}

classification_admin_resolve_run_root <- function(run_root = NULL) {
  if (!is.null(run_root) && nzchar(run_root)) {
    return(normalizePath(run_root, winslash = "/", mustWork = FALSE))
  }
  configured <- Sys.getenv("TITLE_CLASSIFICATION_BATCH_RUN_ROOT", unset = "")
  if (nzchar(configured)) {
    return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  }
  talent_root <- Sys.getenv("TALENT_DATALAKE_ROOT", unset = "")
  if (!nzchar(talent_root)) {
    talent_root <- Sys.getenv("TALENT_DATA_ROOT", unset = "")
  }
  if (!nzchar(talent_root)) {
    stop(
      "Set TALENT_DATALAKE_ROOT or TITLE_CLASSIFICATION_BATCH_RUN_ROOT.",
      call. = FALSE
    )
  }
  talent_root <- sub("/+$", "", talent_root)
  analytics_root <- if (basename(talent_root) == "Talent_data") {
    dirname(talent_root)
  } else {
    talent_root
  }
  normalizePath(
    file.path(
      analytics_root,
      "Processed",
      "Logs",
      "classification",
      "title_classification",
      "batch_runs"
    ),
    winslash = "/",
    mustWork = FALSE
  )
}

classification_admin_empty_candidates <- function() {
  data.frame(
    run_id = character(),
    pipeline_run_id = character(),
    video_id = character(),
    talent_code = character(),
    talent_name = character(),
    title_raw = character(),
    title_hash = character(),
    content_type = character(),
    published_at = character(),
    stringsAsFactors = FALSE
  )
}

classification_admin_scan_runs <- function(run_root) {
  if (!dir.exists(run_root)) {
    return(list(
      manifests = data.frame(),
      candidates = classification_admin_empty_candidates(),
      artifacts = data.frame()
    ))
  }
  manifest_paths <- list.files(
    run_root,
    pattern = "^manifest[.]json$",
    recursive = TRUE,
    full.names = TRUE
  )
  manifest_rows <- list()
  candidate_rows <- list()
  artifact_rows <- list()

  for (manifest_path in manifest_paths) {
    manifest <- classification_admin_read_json(manifest_path)
    if (is.null(manifest)) next
    run_dir <- dirname(manifest_path)
    run_id <- classification_admin_scalar(manifest$run_id, basename(run_dir))
    pipeline_run_id <- classification_admin_scalar(manifest$pipeline_run_id)
    artifacts <- manifest$artifacts
    if (is.null(artifacts) || !is.list(artifacts)) artifacts <- list()
    candidate_path <- classification_admin_scalar(
      artifacts$candidate_rows_csv,
      file.path(run_dir, "candidate_rows.csv")
    )
    candidates <- if (file.exists(candidate_path)) {
      tryCatch(
        read.csv(
          candidate_path,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          fileEncoding = "UTF-8"
        ),
        error = function(error) data.frame()
      )
    } else {
      data.frame()
    }
    if (nrow(candidates) > 0L) {
      candidates$run_id <- run_id
      candidates$pipeline_run_id <- pipeline_run_id
      keep_columns <- c(
        "run_id", "pipeline_run_id", "video_id", "talent_code",
        "talent_name", "title_raw", "title_hash", "content_type",
        "published_at"
      )
      for (column_name in setdiff(keep_columns, names(candidates))) {
        candidates[[column_name]] <- NA_character_
      }
      candidate_rows[[length(candidate_rows) + 1L]] <- candidates[
        , keep_columns, drop = FALSE
      ]
    }

    apply_summary <- classification_admin_read_json(
      file.path(run_dir, "apply_summary.json")
    )
    talent_codes <- if (nrow(candidates) > 0L) {
      sort(unique(candidates$talent_code[nzchar(candidates$talent_code)]))
    } else {
      character()
    }
    manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
      run_id = run_id,
      pipeline_run_id = pipeline_run_id,
      batch_id = classification_admin_scalar(manifest$batch_id),
      status = classification_admin_scalar(
        manifest$batch_status,
        classification_admin_scalar(manifest$status, "created")
      ),
      model = classification_admin_scalar(manifest$model),
      title_version_id = classification_admin_scalar(manifest$title_version_id),
      title_count = classification_admin_integer(
        manifest$pending_rows,
        nrow(candidates)
      ),
      request_count = classification_admin_integer(manifest$request_count, 0L),
      talent_count = length(talent_codes),
      talents = paste(talent_codes, collapse = ", "),
      created_at = classification_admin_parse_time(
        classification_admin_scalar(manifest$created_at)
      ),
      submitted_at = classification_admin_parse_time(
        classification_admin_scalar(manifest$submitted_at)
      ),
      last_checked_at = classification_admin_parse_time(
        classification_admin_scalar(manifest$last_checked_at)
      ),
      applied_at = classification_admin_parse_time(
        classification_admin_scalar(apply_summary$applied_at)
      ),
      inserted_rows = classification_admin_integer(
        apply_summary$inserted_rows,
        0L
      ),
      failed_count = classification_admin_integer(
        apply_summary$failed_count,
        0L
      ),
      run_dir = normalizePath(run_dir, winslash = "/", mustWork = FALSE),
      manifest_path = normalizePath(
        manifest_path,
        winslash = "/",
        mustWork = FALSE
      ),
      stringsAsFactors = FALSE
    )

    run_files <- list.files(
      run_dir,
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = FALSE,
      all.files = TRUE,
      no.. = TRUE
    )
    if (length(run_files) > 0L) {
      file_info <- file.info(run_files)
      relative_path <- substring(run_files, nchar(run_dir) + 2L)
      artifact_rows[[length(artifact_rows) + 1L]] <- data.frame(
        run_id = run_id,
        artifact = relative_path,
        extension = tools::file_ext(relative_path),
        bytes = as.numeric(file_info$size),
        modified_at = as.POSIXct(file_info$mtime, tz = "UTC"),
        path = normalizePath(run_files, winslash = "/", mustWork = FALSE),
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    manifests = dplyr::bind_rows(manifest_rows),
    candidates = if (length(candidate_rows) == 0L) {
      classification_admin_empty_candidates()
    } else {
      dplyr::bind_rows(candidate_rows)
    },
    artifacts = dplyr::bind_rows(artifact_rows)
  )
}

classification_admin_parse_results <- function(results) {
  if (nrow(results) == 0L) {
    results$topic <- character()
    results$language <- character()
    results$tags <- character()
    results$primary_reference <- character()
    results$referenced_entities <- character()
    results$tag_values <- I(list())
    return(results)
  }
  parsed <- lapply(results$classification_json, function(value) {
    tryCatch(
      jsonlite::fromJSON(value, simplifyVector = FALSE),
      error = function(error) list()
    )
  })
  scalar_field <- function(field_name) {
    vapply(parsed, function(item) {
      classification_admin_scalar(item[[field_name]])
    }, character(1))
  }
  list_field <- function(field_name) {
    lapply(parsed, function(item) {
      value <- item[[field_name]]
      if (is.null(value)) return(character())
      value <- trimws(as.character(unlist(value, use.names = FALSE)))
      unique(value[nzchar(value)])
    })
  }
  tag_values <- list_field("tags")
  reference_values <- list_field("referenced_entities")
  results$topic <- scalar_field("topic")
  results$language <- scalar_field("language")
  results$tags <- vapply(tag_values, paste, character(1), collapse = ", ")
  results$primary_reference <- scalar_field("primary_reference")
  results$referenced_entities <- vapply(
    reference_values,
    paste,
    character(1),
    collapse = ", "
  )
  results$tag_values <- I(tag_values)
  results
}

classification_admin_tag_rows <- function(results) {
  empty_tags <- data.frame(
    pipeline_run_id = character(),
    video_id = character(),
    talent_code = character(),
    talent_name = character(),
    title_version_id = character(),
    tag = character(),
    stringsAsFactors = FALSE
  )
  if (nrow(results) == 0L) return(empty_tags)
  rows <- lapply(seq_len(nrow(results)), function(index) {
    tags <- results$tag_values[[index]]
    if (length(tags) == 0L) return(NULL)
    data.frame(
      pipeline_run_id = results$pipeline_run_id[[index]],
      video_id = results$video_id[[index]],
      talent_code = results$talent_code[[index]],
      talent_name = results$talent_name[[index]],
      title_version_id = results$title_version_id[[index]],
      tag = tags,
      stringsAsFactors = FALSE
    )
  })
  bound <- dplyr::bind_rows(rows)
  if (nrow(bound) == 0L) empty_tags else bound
}

classification_admin_load_snapshot <- function(
    database_path = NULL,
    run_root = NULL
) {
  if (is.null(database_path) || !nzchar(database_path)) {
    database_path <- talent_lakehouse_db_path()
  }
  database_path <- normalizePath(
    database_path,
    winslash = "/",
    mustWork = TRUE
  )
  run_root <- classification_admin_resolve_run_root(run_root)
  scanned <- classification_admin_scan_runs(run_root)

  con <- duckdb_connect(db_path = database_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  pipeline_runs <- DBI::dbGetQuery(
    con,
    "SELECT pipeline_run_id, pipeline_name, started_at, completed_at,
            status AS pipeline_status, error_summary
     FROM ops.pipeline_runs
     WHERE pipeline_name = 'title_classification'
     ORDER BY started_at DESC"
  )
  scheduled_state <- DBI::dbGetQuery(
    con,
    "SELECT state_key, run_dir, batch_id, status, request_count, pending_rows,
            submitted_at, last_checked_at, applied_at, updated_at
     FROM classification.title_classification_scheduled_state"
  )
  current_status <- DBI::dbGetQuery(
    con,
    "SELECT status.video_id, status.talent_code, talent.talent_name,
            status.title, status.title_hash, status.published_at,
            status.title_version_id, status.classification_status,
            status.is_classified
     FROM classification.title_classification_status AS status
     LEFT JOIN catalog.talents AS talent USING (talent_code)"
  )
  results <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT result.pipeline_run_id, result.video_id, result.talent_code,",
      "talent.talent_name, video.title, result.title_hash,",
      "result.title_version_id, result.taxonomy_version, result.prompt_version,",
      "result.model, result.profile_id, result.talent_profile,",
      "result.confidence, result.created_at, result.classification_json,",
      "result.collaborative_energy, result.community_milestones,",
      "result.interactive_entertainment, result.meme_viral,",
      "result.monetization, result.narrative_serialization,",
      "result.performance_artistry, result.personality_conversation,",
      "status.classification_status, status.is_classified",
      "FROM classification.title_classification_results AS result",
      "LEFT JOIN catalog.videos AS video USING (video_id)",
      "LEFT JOIN catalog.talents AS talent",
      "ON talent.talent_code = result.talent_code",
      "LEFT JOIN classification.title_classification_status AS status",
      "ON status.video_id = result.video_id",
      "AND status.title_version_id = result.title_version_id"
    )
  )
  results <- classification_admin_parse_results(results)
  tags <- classification_admin_tag_rows(results)

  manifests <- scanned$manifests
  if (nrow(manifests) > 0L) {
    result_summary <- results |>
      dplyr::filter(!is.na(.data$pipeline_run_id)) |>
      dplyr::group_by(.data$pipeline_run_id) |>
      dplyr::summarise(
        result_count = dplyr::n(),
        result_talent_count = dplyr::n_distinct(.data$talent_code),
        .groups = "drop"
      )
    tag_summary <- tags |>
      dplyr::filter(!is.na(.data$pipeline_run_id)) |>
      dplyr::count(.data$pipeline_run_id, .data$tag, name = "uses") |>
      dplyr::group_by(.data$pipeline_run_id) |>
      dplyr::arrange(dplyr::desc(.data$uses), .data$tag, .by_group = TRUE) |>
      dplyr::summarise(
        tag_count = dplyr::n(),
        top_tags = paste0(
          utils::head(paste0(.data$tag, " (", .data$uses, ")"), 10L),
          collapse = ", "
        ),
        .groups = "drop"
      )
    manifests <- manifests |>
      dplyr::left_join(pipeline_runs, by = "pipeline_run_id") |>
      dplyr::left_join(result_summary, by = "pipeline_run_id") |>
      dplyr::left_join(tag_summary, by = "pipeline_run_id") |>
      dplyr::mutate(
        status = dplyr::coalesce(.data$pipeline_status, .data$status),
        result_count = dplyr::coalesce(.data$result_count, 0L),
        result_talent_count = dplyr::coalesce(.data$result_talent_count, 0L),
        tag_count = dplyr::coalesce(.data$tag_count, 0L),
        top_tags = dplyr::coalesce(.data$top_tags, ""),
        artifact_count = vapply(
          .data$run_id,
          function(value) sum(scanned$artifacts$run_id == value),
          integer(1)
        )
      ) |>
      dplyr::arrange(dplyr::desc(.data$created_at))
  }

  legacy_history <- results |>
    dplyr::filter(is.na(.data$pipeline_run_id)) |>
    dplyr::group_by(.data$title_version_id, .data$model) |>
    dplyr::summarise(
      result_count = dplyr::n(),
      talent_count = dplyr::n_distinct(.data$talent_code),
      first_result_at = suppressWarnings(min(.data$created_at, na.rm = TRUE)),
      last_result_at = suppressWarnings(max(.data$created_at, na.rm = TRUE)),
      tags = paste(
        sort(unique(unlist(.data$tag_values, use.names = FALSE))),
        collapse = ", "
      ),
      .groups = "drop"
    )
  legacy_history$first_result_at[!is.finite(legacy_history$first_result_at)] <- NA
  legacy_history$last_result_at[!is.finite(legacy_history$last_result_at)] <- NA

  list(
    database_path = database_path,
    database_modified_at = as.POSIXct(file.info(database_path)$mtime, tz = "UTC"),
    run_root = run_root,
    refreshed_at = Sys.time(),
    runs = manifests,
    candidates = scanned$candidates,
    artifacts = scanned$artifacts,
    pipeline_runs = pipeline_runs,
    scheduled_state = scheduled_state,
    current_status = current_status,
    results = results,
    tags = tags,
    legacy_history = legacy_history
  )
}
