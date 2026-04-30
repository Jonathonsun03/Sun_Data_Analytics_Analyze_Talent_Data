gdrive_talent_as_id <- function(id_or_dribble) {
  gdrive_talent_assert_packages("googledrive")

  if (inherits(id_or_dribble, "dribble")) {
    return(id_or_dribble)
  }

  googledrive::as_id(id_or_dribble)
}

gdrive_talent_drive_ls <- function(parent, type = NULL) {
  gdrive_talent_assert_packages("googledrive")
  args <- list(path = gdrive_talent_as_id(parent))
  if (!is.null(type)) args$type <- type
  do.call(googledrive::drive_ls, args)
}

gdrive_talent_get_or_create_folder <- function(name, parent, dry_run = FALSE) {
  name <- gdrive_talent_safe_name(name)

  if (isTRUE(dry_run)) {
    return(data.frame(
      name = name,
      id = NA_character_,
      drive_resource = I(list(NULL)),
      stringsAsFactors = FALSE
    ))
  }

  gdrive_talent_assert_packages("googledrive")
  parent <- gdrive_talent_as_id(parent)

  existing <- googledrive::drive_ls(parent, type = "folder")
  existing <- existing[existing$name == name, , drop = FALSE]

  if (nrow(existing) > 0) {
    return(existing[1, , drop = FALSE])
  }

  googledrive::drive_mkdir(name = name, path = parent)
}

gdrive_talent_resolve_destination <- function(
  root_folder_id,
  delivery_group_display_name = NULL,
  talent_id,
  drive_subdir,
  dry_run = FALSE,
  client_display_name = NULL
) {
  if (is.null(delivery_group_display_name) || !nzchar(delivery_group_display_name)) {
    delivery_group_display_name <- client_display_name
  }

  delivery_group_folder <- gdrive_talent_get_or_create_folder(
    delivery_group_display_name,
    parent = root_folder_id,
    dry_run = dry_run
  )
  talent_folder <- gdrive_talent_get_or_create_folder(
    talent_id,
    parent = delivery_group_folder,
    dry_run = dry_run
  )
  data_folder <- gdrive_talent_get_or_create_folder(
    drive_subdir,
    parent = talent_folder,
    dry_run = dry_run
  )

  data_folder
}

gdrive_talent_upload_file <- function(
  local_file,
  destination_folder,
  drive_file_name = basename(local_file),
  overwrite = TRUE,
  dry_run = TRUE
) {
  if (isTRUE(dry_run)) {
    drive_file_name <- gdrive_talent_safe_file_name(drive_file_name)
    return(data.frame(
      action = "upload",
      local_file = local_file,
      drive_file_name = drive_file_name,
      destination_folder_id = NA_character_,
      status = "dry_run",
      stringsAsFactors = FALSE
    ))
  }

  gdrive_talent_assert_packages("googledrive")
  drive_file_name <- gdrive_talent_safe_file_name(drive_file_name)

  if (isTRUE(overwrite)) {
    existing <- googledrive::drive_ls(destination_folder)
    existing <- existing[existing$name == drive_file_name, , drop = FALSE]
    if (nrow(existing) > 0) {
      googledrive::drive_rm(existing)
    }
  }

  uploaded <- googledrive::drive_upload(
    media = local_file,
    path = destination_folder,
    name = drive_file_name,
    overwrite = FALSE
  )

  data.frame(
    action = "upload",
    local_file = local_file,
    drive_file_name = drive_file_name,
    destination_folder_id = uploaded$id[[1]],
    status = "uploaded",
    stringsAsFactors = FALSE
  )
}
