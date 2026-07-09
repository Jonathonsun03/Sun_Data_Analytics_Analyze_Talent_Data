gdrive_talent_build_upload_plan <- function(
  permissions,
  manifest,
  permission_cols = gdrive_talent_default_permission_cols(),
  active_only = FALSE,
  warn_missing = TRUE
) {
  normalized <- gdrive_talent_normalize_permissions(
    permissions,
    permission_cols = permission_cols,
    active_only = active_only
  )
  allowed <- gdrive_talent_allowed_permissions(
    normalized,
    permission_cols = permission_cols
  )
  resolved <- gdrive_talent_resolve_local_files(
    allowed,
    manifest,
    warn_missing = warn_missing
  )

  list(
    permissions = normalized,
    allowed = allowed,
    upload_plan = resolved$upload_plan,
    missing = resolved$missing
  )
}

gdrive_talent_expected_remote_index <- function(upload_plan) {
  upload_plan <- as.data.frame(upload_plan, stringsAsFactors = FALSE)
  if (!"delivery_group_display_name" %in% names(upload_plan)) {
    upload_plan$delivery_group_display_name <- upload_plan$client_display_name
  }

  required <- c("delivery_group_display_name", "talent_id", "drive_subdir", "drive_file_name")
  missing <- setdiff(required, names(upload_plan))
  if (length(missing) > 0) {
    stop(
      "Upload plan is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  unique(upload_plan[, required, drop = FALSE])
}

gdrive_talent_build_stale_file_plan <- function(
  upload_plan,
  root_folder_id
) {
  gdrive_talent_assert_packages("googledrive")

  expected <- gdrive_talent_expected_remote_index(upload_plan)
  groups <- unique(expected[, c("delivery_group_display_name", "talent_id", "drive_subdir"), drop = FALSE])
  rows <- vector("list", 0)
  idx <- 0L

  for (i in seq_len(nrow(groups))) {
    grp <- groups[i, , drop = FALSE]
    folder <- gdrive_talent_resolve_destination(
      root_folder_id = root_folder_id,
      delivery_group_display_name = grp$delivery_group_display_name[[1]],
      talent_id = grp$talent_id[[1]],
      drive_subdir = grp$drive_subdir[[1]],
      dry_run = FALSE
    )

    actual <- googledrive::drive_ls(folder)
    if (nrow(actual) == 0) next

    expected_names <- expected$drive_file_name[
      expected$delivery_group_display_name == grp$delivery_group_display_name[[1]] &
        expected$talent_id == grp$talent_id[[1]] &
        expected$drive_subdir == grp$drive_subdir[[1]]
    ]

    stale <- actual[!(actual$name %in% expected_names), , drop = FALSE]
    if (nrow(stale) == 0) next

    for (j in seq_len(nrow(stale))) {
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        delivery_group_display_name = grp$delivery_group_display_name[[1]],
        talent_id = grp$talent_id[[1]],
        drive_subdir = grp$drive_subdir[[1]],
        drive_file_name = stale$name[[j]],
        drive_file_id = stale$id[[j]],
        action = "stale",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_display_name",
      "talent_id",
      "drive_subdir",
      "drive_file_name",
      "drive_file_id",
      "action"
    )))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
