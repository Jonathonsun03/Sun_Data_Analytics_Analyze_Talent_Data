gdrive_talent_execute_upload_plan <- function(
  upload_plan,
  root_folder_id,
  overwrite = TRUE,
  dry_run = TRUE
) {
  upload_plan <- as.data.frame(upload_plan, stringsAsFactors = FALSE)
  if (nrow(upload_plan) == 0) {
    return(gdrive_talent_empty_df(c(
      "action",
      "local_file",
      "drive_file_name",
      "destination_folder_id",
      "status"
    )))
  }

  if (!"delivery_group_display_name" %in% names(upload_plan)) {
    upload_plan$delivery_group_display_name <- upload_plan$client_display_name
  }

  upload_plan <- upload_plan[!duplicated(upload_plan[, c(
    "delivery_group_display_name",
    "talent_id",
    "drive_subdir",
    "local_file",
    "drive_file_name"
  ), drop = FALSE]), , drop = FALSE]

  rows <- vector("list", nrow(upload_plan))

  for (i in seq_len(nrow(upload_plan))) {
    row <- upload_plan[i, , drop = FALSE]
    delivery_group_display_name <- if ("delivery_group_display_name" %in% names(row)) {
      row$delivery_group_display_name[[1]]
    } else {
      row$client_display_name[[1]]
    }

    destination <- gdrive_talent_resolve_destination(
      root_folder_id = root_folder_id,
      delivery_group_display_name = delivery_group_display_name,
      talent_id = row$talent_id[[1]],
      drive_subdir = row$drive_subdir[[1]],
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

gdrive_talent_execute_stale_file_plan <- function(
  stale_plan,
  root_folder_id,
  mode = c("archive", "trash"),
  archive_folder_name = "_revoked_or_stale",
  dry_run = TRUE
) {
  mode <- match.arg(mode)
  stale_plan <- as.data.frame(stale_plan, stringsAsFactors = FALSE)

  if (nrow(stale_plan) == 0) {
    return(gdrive_talent_empty_df(c(
      "action",
      "drive_file_name",
      "drive_file_id",
      "status"
    )))
  }

  rows <- vector("list", nrow(stale_plan))
  archive_date <- format(Sys.Date(), "%Y-%m-%d")

  if (!isTRUE(dry_run)) {
    gdrive_talent_assert_packages("googledrive")
  }

  for (i in seq_len(nrow(stale_plan))) {
    row <- stale_plan[i, , drop = FALSE]
    status <- "dry_run"
    action <- mode

    if (!isTRUE(dry_run)) {
      file <- googledrive::as_id(row$drive_file_id[[1]])

      if (identical(mode, "trash")) {
        googledrive::drive_rm(file)
        status <- "trashed"
      } else {
        talent_folder <- gdrive_talent_get_or_create_folder(
          row$talent_id[[1]],
          parent = gdrive_talent_get_or_create_folder(
            if ("delivery_group_display_name" %in% names(row)) row$delivery_group_display_name[[1]] else row$client_display_name[[1]],
            parent = root_folder_id,
            dry_run = FALSE
          ),
          dry_run = FALSE
        )
        archive_root <- gdrive_talent_get_or_create_folder(
          archive_folder_name,
          parent = talent_folder,
          dry_run = FALSE
        )
        archive_date_folder <- gdrive_talent_get_or_create_folder(
          archive_date,
          parent = archive_root,
          dry_run = FALSE
        )
        googledrive::drive_mv(file = file, path = archive_date_folder)
        status <- "archived"
      }
    }

    rows[[i]] <- data.frame(
      action = action,
      drive_file_name = row$drive_file_name[[1]],
      drive_file_id = row$drive_file_id[[1]],
      status = status,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

gdrive_talent_expected_current_report_index <- function(file_plan) {
  file_plan <- as.data.frame(file_plan, stringsAsFactors = FALSE)
  if (nrow(file_plan) == 0 || !"drive_folder" %in% names(file_plan)) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_display_name",
      "talent_id",
      "legacy_drive_subdir",
      "current_drive_subdir",
      "drive_file_name"
    )))
  }

  if (!"delivery_group_display_name" %in% names(file_plan)) {
    file_plan$delivery_group_display_name <- file_plan$client_display_name
  }

  report_rows <- file_plan[
    grepl("/report/current$", file_plan$drive_folder, ignore.case = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(report_rows) == 0) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_display_name",
      "talent_id",
      "legacy_drive_subdir",
      "current_drive_subdir",
      "drive_file_name"
    )))
  }

  out <- data.frame(
    delivery_group_display_name = report_rows$delivery_group_display_name,
    talent_id = report_rows$talent_id,
    legacy_drive_subdir = sub("/report/current$", "", report_rows$drive_folder, ignore.case = TRUE),
    current_drive_subdir = report_rows$drive_folder,
    drive_file_name = report_rows$drive_file_name,
    stringsAsFactors = FALSE
  )
  unique(out)
}

gdrive_talent_build_report_file_migration_plan <- function(
  file_plan,
  root_folder_id
) {
  gdrive_talent_assert_packages("googledrive")

  expected <- gdrive_talent_expected_current_report_index(file_plan)
  if (nrow(expected) == 0) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_display_name",
      "talent_id",
      "source_drive_subdir",
      "target_drive_subdir",
      "drive_file_name",
      "drive_file_id",
      "reason"
    )))
  }

  groups <- unique(expected[, c(
    "delivery_group_display_name",
    "talent_id",
    "legacy_drive_subdir",
    "current_drive_subdir"
  ), drop = FALSE])

  rows <- vector("list", 0)
  idx <- 0L

  for (i in seq_len(nrow(groups))) {
    grp <- groups[i, , drop = FALSE]
    delivery_group_folder <- gdrive_talent_find_child_folder(
      grp$delivery_group_display_name[[1]],
      parent = root_folder_id
    )
    if (is.null(delivery_group_folder)) next

    talent_folder <- gdrive_talent_find_child_folder(
      grp$talent_id[[1]],
      parent = delivery_group_folder
    )
    if (is.null(talent_folder)) next

    source_folder <- gdrive_talent_find_child_folder(
      grp$legacy_drive_subdir[[1]],
      parent = talent_folder
    )
    if (is.null(source_folder)) next

    actual <- googledrive::drive_ls(source_folder)
    if (nrow(actual) == 0) next

    actual <- actual[!is.na(actual$name), , drop = FALSE]
    report_files <- actual[
      grepl("\\.(html?|pdf|docx)$", actual$name, ignore.case = TRUE),
      ,
      drop = FALSE
    ]
    if (nrow(report_files) == 0) next

    current_names <- expected$drive_file_name[
      expected$delivery_group_display_name == grp$delivery_group_display_name[[1]] &
        expected$talent_id == grp$talent_id[[1]] &
        expected$legacy_drive_subdir == grp$legacy_drive_subdir[[1]]
    ]

    for (j in seq_len(nrow(report_files))) {
      target <- if (report_files$name[[j]] %in% current_names) {
        grp$current_drive_subdir[[1]]
      } else {
        file.path(grp$legacy_drive_subdir[[1]], "report", "archive")
      }
      reason <- if (report_files$name[[j]] %in% current_names) {
        "matches_current_report"
      } else {
        "legacy_or_noncurrent_report"
      }

      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        delivery_group_display_name = grp$delivery_group_display_name[[1]],
        talent_id = grp$talent_id[[1]],
        source_drive_subdir = grp$legacy_drive_subdir[[1]],
        target_drive_subdir = target,
        drive_file_name = report_files$name[[j]],
        drive_file_id = as.character(report_files$id[[j]]),
        reason = reason,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_display_name",
      "talent_id",
      "source_drive_subdir",
      "target_drive_subdir",
      "drive_file_name",
      "drive_file_id",
      "reason"
    )))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

gdrive_talent_execute_report_file_migration_plan <- function(
  migration_plan,
  root_folder_id,
  dry_run = TRUE
) {
  migration_plan <- as.data.frame(migration_plan, stringsAsFactors = FALSE)
  if (nrow(migration_plan) == 0) {
    return(gdrive_talent_empty_df(c(
      "action",
      "drive_file_name",
      "target_drive_subdir",
      "drive_file_id",
      "status"
    )))
  }

  rows <- vector("list", nrow(migration_plan))

  for (i in seq_len(nrow(migration_plan))) {
    row <- migration_plan[i, , drop = FALSE]
    status <- "dry_run"

    if (!isTRUE(dry_run)) {
      destination <- gdrive_talent_resolve_destination(
        root_folder_id = root_folder_id,
        delivery_group_display_name = row$delivery_group_display_name[[1]],
        talent_id = row$talent_id[[1]],
        drive_subdir = row$target_drive_subdir[[1]],
        dry_run = FALSE
      )
      googledrive::drive_mv(
        file = googledrive::as_id(row$drive_file_id[[1]]),
        path = destination
      )
      status <- "moved"
    }

    rows[[i]] <- data.frame(
      action = "move_report_file",
      drive_file_name = row$drive_file_name[[1]],
      target_drive_subdir = row$target_drive_subdir[[1]],
      drive_file_id = row$drive_file_id[[1]],
      status = status,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
