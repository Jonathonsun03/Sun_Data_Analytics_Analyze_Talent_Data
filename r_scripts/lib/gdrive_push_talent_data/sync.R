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
