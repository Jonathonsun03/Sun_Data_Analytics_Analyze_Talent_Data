gdrive_talent_build_share_plan <- function(permissions) {
  permissions <- as.data.frame(permissions, stringsAsFactors = FALSE)
  if (nrow(permissions) == 0) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_id",
      "delivery_group_display_name",
      "client_id",
      "client_display_name",
      "client_email"
    )))
  }

  required <- c(
    "delivery_group_id",
    "delivery_group_display_name",
    "client_id",
    "client_display_name"
  )
  missing <- setdiff(required, names(permissions))
  if (length(missing) > 0) {
    stop(
      "Permissions are missing required share column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"client_email" %in% names(permissions)) {
    permissions$client_email <- ""
  }

  permissions$client_email <- gdrive_talent_chr(permissions$client_email)
  plan <- permissions[nzchar(permissions$client_email), c(
    "delivery_group_id",
    "delivery_group_display_name",
    "client_id",
    "client_display_name",
    "client_email"
  ), drop = FALSE]

  if (nrow(plan) == 0) {
    return(plan)
  }

  plan <- unique(plan)
  rownames(plan) <- NULL
  plan
}

gdrive_talent_execute_share_plan <- function(
  share_plan,
  root_folder_id,
  role = "reader",
  dry_run = TRUE
) {
  share_plan <- as.data.frame(share_plan, stringsAsFactors = FALSE)
  if (nrow(share_plan) == 0) {
    return(gdrive_talent_empty_df(c(
      "delivery_group_display_name",
      "client_email",
      "role",
      "status"
    )))
  }

  rows <- vector("list", nrow(share_plan))

  for (i in seq_len(nrow(share_plan))) {
    row <- share_plan[i, , drop = FALSE]
    status <- "dry_run"

    if (!isTRUE(dry_run)) {
      gdrive_talent_assert_packages("googledrive")
      folder <- gdrive_talent_get_or_create_folder(
        name = row$delivery_group_display_name[[1]],
        parent = root_folder_id,
        dry_run = FALSE
      )
      googledrive::drive_share(
        file = folder,
        role = role,
        type = "user",
        emailAddress = row$client_email[[1]]
      )
      status <- "shared"
    }

    rows[[i]] <- data.frame(
      delivery_group_display_name = row$delivery_group_display_name[[1]],
      client_email = row$client_email[[1]],
      role = role,
      status = status,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
