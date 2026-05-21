source("r_scripts/lib/ENA_prep/shared/ena_precoding_paths.R")

if (!exists("select_talent", mode = "function") ||
    !exists("talent_slugify", mode = "function")) {
  ena_precoding_source_repo_file("r_scripts", "lib", "utils", "talent_select.R")
}

resolve_datalake_path <- function(path, talent_data_root = NULL) {
  normalized_path <- gsub("\\\\", "/", path)
  normalized_path <- normalizePath(normalized_path, winslash = "/", mustWork = FALSE)
  talent_data_root <- ena_precoding_talent_data_root(talent_data_root)
  processed_root <- ena_precoding_processed_root(talent_data_root)

  normalized_path <- sub(
    "^([A-Za-z]:)?/DataLake/Sun_Data_Analytics/Talent_data",
    talent_data_root,
    normalized_path
  )

  sub(
    "^([A-Za-z]:)?/DataLake/Sun_Data_Analytics/Processed/Talent_Data",
    processed_root,
    normalized_path
  )
}

list_qualitative_prep_files <- function(talent_data_root = NULL,
                                        recursive = TRUE,
                                        full.names = TRUE) {
  qualitative_prep_path <- ena_precoding_qualitative_prep_root(talent_data_root)
  list.files(
    qualitative_prep_path,
    recursive = recursive,
    full.names = full.names
  )
}

qualitative_codebook_root <- function(talent_data_root = NULL) {
  file.path(
    ena_precoding_processed_root(talent_data_root),
    "Qualitative Codebook"
  )
}

list_qualitative_codebook_snapshots <- function(talent_data_root = NULL,
                                                pattern = "^personality_qualitative_code_log_.*\\.(csv|md|json)$") {
  snapshots_root <- file.path(qualitative_codebook_root(talent_data_root), "snapshots")
  files <- list.files(
    snapshots_root,
    pattern = pattern,
    full.names = TRUE
  )

  if (length(files) == 0L) {
    return(data.frame(
      file_name = character(),
      snapshot_path = character(),
      modified_time = as.POSIXct(character()),
      file_size_bytes = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(files)
  out <- data.frame(
    file_name = basename(files),
    snapshot_path = normalizePath(files, winslash = "/", mustWork = FALSE),
    modified_time = as.POSIXct(info$mtime, tz = "UTC"),
    file_size_bytes = as.numeric(info$size),
    stringsAsFactors = FALSE
  )
  out[order(out$modified_time, decreasing = TRUE), , drop = FALSE]
}

resolve_qualitative_codebook <- function(version = c("current", "latest_snapshot", "snapshot"),
                                         snapshot = NULL,
                                         talent_data_root = NULL,
                                         must_exist = TRUE) {
  version <- match.arg(version)
  codebook_root <- qualitative_codebook_root(talent_data_root)

  path <- switch(
    version,
    current = ena_precoding_codebook_path(talent_data_root = talent_data_root),
    latest_snapshot = {
      snapshots <- list_qualitative_codebook_snapshots(talent_data_root)
      if (nrow(snapshots) == 0L) {
        file.path(codebook_root, "snapshots")
      } else {
        snapshots$snapshot_path[[1]]
      }
    },
    snapshot = {
      if (is.null(snapshot) || !nzchar(trimws(snapshot))) {
        stop("`snapshot` is required when version = 'snapshot'.", call. = FALSE)
      }
      if (grepl("[/\\\\]", snapshot)) {
        resolve_datalake_path(snapshot, talent_data_root)
      } else {
        file.path(codebook_root, "snapshots", snapshot)
      }
    }
  )

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (isTRUE(must_exist) && !file.exists(path)) {
    stop("Qualitative codebook path does not exist: ", path, call. = FALSE)
  }
  path
}

resolve_talent_text_playback <- function(talent_query,
                                         talent_data_root = NULL,
                                         must_exist = TRUE) {
  talent_data_root <- ena_precoding_talent_data_root(talent_data_root)
  talent_paths <- as.character(select_talent(talent_query, root = talent_data_root))

  rows <- lapply(talent_paths, function(talent_path) {
    talent_name <- safe_basename(talent_path)
    text_playback_path <- file.path(talent_path, "text_playback")

    data.frame(
      talent_name = talent_name,
      talent_slug = talent_slugify(talent_name),
      talent_path = normalizePath(talent_path, winslash = "/", mustWork = FALSE),
      text_playback_path = normalizePath(text_playback_path, winslash = "/", mustWork = FALSE),
      text_playback_exists = dir.exists(text_playback_path),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL

  missing <- out[!out$text_playback_exists, , drop = FALSE]
  if (isTRUE(must_exist) && nrow(missing) > 0L) {
    stop(
      "Missing text_playback folder for: ",
      paste(missing$talent_name, collapse = ", "),
      call. = FALSE
    )
  }

  out
}

resolve_talent_qualitative_prep_dir <- function(talent_query,
                                                talent_data_root = NULL,
                                                create = TRUE,
                                                coding_folder = "monetary conversation codes") {
  talents <- resolve_talent_text_playback(
    talent_query = talent_query,
    talent_data_root = talent_data_root,
    must_exist = TRUE
  )

  if (is.null(coding_folder) || !nzchar(trimws(coding_folder))) {
    stop("`coding_folder` must be a non-empty folder name.", call. = FALSE)
  }

  talents$qualitative_coding_root <- normalizePath(
    file.path(talents$talent_path, "qualitative coding"),
    winslash = "/",
    mustWork = FALSE
  )

  talents$qualitative_prep_dir <- normalizePath(
    file.path(talents$qualitative_coding_root, coding_folder),
    winslash = "/",
    mustWork = FALSE
  )

  if (isTRUE(create)) {
    created <- vapply(
      talents$qualitative_prep_dir,
      dir.create,
      logical(1),
      recursive = TRUE,
      showWarnings = FALSE
    )
    exists_now <- dir.exists(talents$qualitative_prep_dir)
    if (any(!exists_now)) {
      stop(
        "Could not create qualitative prep folder(s): ",
        paste(talents$qualitative_prep_dir[!exists_now], collapse = ", "),
        call. = FALSE
      )
    }
    talents$qualitative_prep_created <- created
  } else {
    talents$qualitative_prep_created <- FALSE
  }

  talents$qualitative_prep_exists <- dir.exists(talents$qualitative_prep_dir)
  talents
}
