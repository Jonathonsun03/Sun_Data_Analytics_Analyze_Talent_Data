gdrive_talent_raw_data_types <- function() {
  c("public_subs", "video_analytics", "video_demographics", "video_monetary")
}

gdrive_talent_bundle_types <- function() {
  c("bundle_a", "bundle_b", "bundle_c", "bundle_e", "bundle_z")
}

gdrive_talent_is_bundle_type <- function(data_type) {
  tolower(gdrive_talent_chr(data_type)) %in% gdrive_talent_bundle_types()
}

gdrive_talent_file_date <- function(path) {
  base <- basename(path)
  match <- regmatches(base, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", base))
  if (length(match) == 0 || !nzchar(match)) {
    return(as.Date(NA))
  }
  as.Date(match)
}

gdrive_talent_list_files <- function(root) {
  if (is.na(root) || !dir.exists(root)) return(character())
  list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
}

gdrive_talent_report_dirs <- function(source_dir, include_archive = FALSE) {
  source_dir <- normalizePath(source_dir, winslash = "/", mustWork = FALSE)
  current_dirs <- c(
    file.path(source_dir, "report", "current"),
    file.path(source_dir, "current")
  )
  current_dirs <- current_dirs[dir.exists(current_dirs)]
  dirs <- if (length(current_dirs) > 0) {
    current_dirs
  } else {
    source_dir
  }
  if (isTRUE(include_archive)) {
    dirs <- c(
      dirs,
      file.path(source_dir, "report", "archive"),
      file.path(source_dir, "archive")
    )
  }
  unique(dirs[dir.exists(dirs)])
}

gdrive_talent_list_report_files <- function(source_dir, include_archive = FALSE) {
  dirs <- gdrive_talent_report_dirs(source_dir, include_archive = include_archive)
  if (length(dirs) == 0) return(character())
  files <- unique(unlist(lapply(dirs, function(dir) {
    list.files(dir, full.names = TRUE, recursive = FALSE, all.files = FALSE, no.. = TRUE)
  }), use.names = FALSE))
  if (is.null(files)) return(character())
  files <- as.character(files)
  files <- files[file.exists(files) & !dir.exists(files)]
  files[grepl("\\.(html?|pdf|docx)$", files, ignore.case = TRUE)]
}

gdrive_talent_select_latest_report_file <- function(files, window_days = NA_real_) {
  if (length(files) == 0) return(character())
  if (!is.na(window_days) && window_days > 0) {
    window_pattern <- paste0("window_", as.integer(window_days), "d")
    window_files <- files[grepl(window_pattern, basename(files), ignore.case = TRUE)]
    if (length(window_files) > 0) {
      files <- window_files
    }
  }

  current_files <- files[grepl("/(report/)?current/", files, ignore.case = TRUE)]
  if (length(current_files) > 0) {
    files <- current_files
  }

  info <- file.info(files)
  files[which.max(info$mtime)]
}

gdrive_talent_drive_report_folder <- function(folder_name) {
  file.path(gdrive_talent_chr(folder_name), "report", "current")
}

gdrive_talent_select_delivery_files <- function(
  source_dir,
  data_type,
  raw_days = 14
) {
  files <- gdrive_talent_list_files(source_dir)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (length(files) == 0) return(character())

  data_type <- tolower(gdrive_talent_chr(data_type))

  if (data_type %in% gdrive_talent_raw_data_types()) {
    file_dates <- vapply(files, gdrive_talent_file_date, as.Date(NA))
    dated <- !is.na(file_dates)
    cutoff <- Sys.Date() - raw_days
    selected <- files[dated & file_dates >= cutoff]
    undated <- files[!dated]
    return(sort(unique(c(selected, undated))))
  }

  if (gdrive_talent_is_bundle_type(data_type)) {
    reports <- gdrive_talent_list_report_files(source_dir, include_archive = FALSE)
    if (length(reports) == 0) {
      reports <- gdrive_talent_list_report_files(source_dir, include_archive = TRUE)
    }
    sort(unique(reports))
  } else {
    sort(files)
  }
}

gdrive_talent_build_delivery_file_plan <- function(
  structure_plan,
  raw_days = 14
) {
  structure_plan <- as.data.frame(structure_plan, stringsAsFactors = FALSE)
  if (nrow(structure_plan) == 0) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_display_name",
      "talent_id",
      "data_type",
      "local_file",
      "drive_folder",
      "drive_file_name"
    )))
  }

  rows <- vector("list", 0)
  idx <- 0L

  if (!"upload_enabled" %in% names(structure_plan)) {
    structure_plan$upload_enabled <- TRUE
  }
  structure_plan <- structure_plan[gdrive_talent_bool(structure_plan$upload_enabled), , drop = FALSE]
  if (nrow(structure_plan) == 0) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_display_name",
      "talent_id",
      "data_type",
      "local_file",
      "drive_folder",
      "drive_file_name"
    )))
  }

  unique_sources <- unique(structure_plan[, c(
    "delivery_group_display_name",
    "talent_id",
    "data_type",
    "source_local_path",
    "source_relative_path",
    "folder_name"
  ), drop = FALSE])

  for (i in seq_len(nrow(unique_sources))) {
    src <- unique_sources[i, , drop = FALSE]
    files <- gdrive_talent_select_delivery_files(
      source_dir = src$source_local_path[[1]],
      data_type = src$data_type[[1]],
      raw_days = raw_days
    )

    if (length(files) == 0) next

    for (file in files) {
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        delivery_group_display_name = src$delivery_group_display_name[[1]],
        talent_id = src$talent_id[[1]],
        data_type = src$data_type[[1]],
        local_file = normalizePath(file, winslash = "/", mustWork = FALSE),
        drive_folder = if (gdrive_talent_is_bundle_type(src$data_type[[1]])) {
          src$source_relative_path[[1]]
        } else {
          src$folder_name[[1]]
        },
        drive_file_name = gdrive_talent_safe_file_name(basename(file)),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_display_name",
      "talent_id",
      "data_type",
      "local_file",
      "drive_folder",
      "drive_file_name"
    )))
  }

  out <- unique(do.call(rbind, rows))
  rownames(out) <- NULL
  out
}

gdrive_talent_should_overwrite_delivery_file <- function(data_type) {
  !tolower(gdrive_talent_chr(data_type)) %in% c("text_playback")
}

gdrive_talent_execute_delivery_file_plan <- function(
  file_plan,
  root_folder_id,
  overwrite = TRUE,
  dry_run = TRUE
) {
  file_plan <- as.data.frame(file_plan, stringsAsFactors = FALSE)
  if (nrow(file_plan) == 0) {
    return(gdrive_talent_empty_df(c(
      "action",
      "local_file",
      "drive_file_name",
      "status"
    )))
  }

  rows <- vector("list", nrow(file_plan))

  for (i in seq_len(nrow(file_plan))) {
    row <- file_plan[i, , drop = FALSE]
    destination <- gdrive_talent_resolve_destination(
      root_folder_id = root_folder_id,
      delivery_group_display_name = row$delivery_group_display_name[[1]],
      talent_id = row$talent_id[[1]],
      drive_subdir = row$drive_folder[[1]],
      dry_run = dry_run
    )

    row_overwrite <- isTRUE(overwrite) &&
      gdrive_talent_should_overwrite_delivery_file(row$data_type[[1]])

    rows[[i]] <- gdrive_talent_upload_file(
      local_file = row$local_file[[1]],
      destination_folder = destination,
      drive_file_name = row$drive_file_name[[1]],
      overwrite = row_overwrite,
      dry_run = dry_run
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
