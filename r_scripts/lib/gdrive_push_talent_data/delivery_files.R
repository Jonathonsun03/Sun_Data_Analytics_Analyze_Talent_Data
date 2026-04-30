gdrive_talent_raw_data_types <- function() {
  c("public_subs", "video_analytics", "video_demographics", "video_monetary")
}

gdrive_talent_bundle_types <- function() {
  c("bundle_a", "bundle_b", "bundle_c", "bundle_e", "bundle_z")
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

  if (data_type %in% gdrive_talent_bundle_types()) {
    reports <- files[tolower(tools::file_ext(files)) %in% c("html", "pdf")]
    if (length(reports) == 0) return(character())
    info <- file.info(reports)
    reports[which.max(info$mtime)]
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

  unique_sources <- unique(structure_plan[, c(
    "delivery_group_display_name",
    "talent_id",
    "data_type",
    "source_local_path",
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
        drive_folder = src$folder_name[[1]],
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

    rows[[i]] <- gdrive_talent_upload_file(
      local_file = row$local_file[[1]],
      destination_folder = destination,
      drive_file_name = row$drive_file_name[[1]],
      overwrite = overwrite,
      dry_run = dry_run
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
