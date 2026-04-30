gdrive_talent_default_permission_cols <- function() {
  c(
    "public_subs",
    "video_analytics",
    "video_demographics",
    "video_monetary",
    "bundle_a",
    "bundle_b",
    "bundle_c",
    "bundle_e",
    "bundle_z",
    "text_playback"
  )
}

gdrive_talent_read_permissions <- function(
  sheet_id,
  sheet = NULL,
  permission_cols = gdrive_talent_default_permission_cols(),
  active_only = FALSE
) {
  gdrive_talent_assert_packages("googlesheets4")

  raw <- googlesheets4::read_sheet(
    ss = gdrive_talent_normalize_sheet_id(sheet_id),
    sheet = sheet
  )
  gdrive_talent_normalize_permissions(
    raw,
    permission_cols = permission_cols,
    active_only = active_only
  )
}

gdrive_talent_normalize_sheet_id <- function(sheet_id) {
  sheet_id <- gdrive_talent_chr(sheet_id)

  url_match <- regexec("/spreadsheets/d/([A-Za-z0-9_-]+)", sheet_id)
  url_parts <- regmatches(sheet_id, url_match)[[1]]
  if (length(url_parts) >= 2) {
    return(url_parts[[2]])
  }

  partial_match <- regexec("^([A-Za-z0-9_-]+)/", sheet_id)
  partial_parts <- regmatches(sheet_id, partial_match)[[1]]
  if (length(partial_parts) >= 2) {
    return(partial_parts[[2]])
  }

  sheet_id
}

gdrive_talent_normalize_permissions <- function(
  permissions,
  permission_cols = gdrive_talent_default_permission_cols(),
  active_only = FALSE
) {
  required <- c("client_id", "client_display_name", "talent_id")
  missing <- setdiff(required, names(permissions))
  if (length(missing) > 0) {
    stop(
      "Permissions data is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  permissions <- as.data.frame(permissions, stringsAsFactors = FALSE)

  permissions$client_id <- gdrive_talent_chr(permissions$client_id)
  permissions$client_display_name <- gdrive_talent_chr(permissions$client_display_name)
  permissions$talent_id <- gdrive_talent_chr(permissions$talent_id)

  if ("client_email" %in% names(permissions)) {
    permissions$client_email <- gdrive_talent_chr(permissions$client_email)
  }
  if ("delivery_group_id" %in% names(permissions)) {
    permissions$delivery_group_id <- gdrive_talent_chr(permissions$delivery_group_id)
  } else {
    permissions$delivery_group_id <- permissions$client_id
  }
  if ("delivery_group_display_name" %in% names(permissions)) {
    permissions$delivery_group_display_name <- gdrive_talent_chr(permissions$delivery_group_display_name)
  } else {
    permissions$delivery_group_display_name <- permissions$client_display_name
  }

  missing_talent_id <- !nzchar(permissions$talent_id) & nzchar(permissions$delivery_group_id)
  permissions$talent_id[missing_talent_id] <- permissions$delivery_group_id[missing_talent_id]

  present_permission_cols <- intersect(permission_cols, names(permissions))
  for (col in present_permission_cols) {
    permissions[[col]] <- gdrive_talent_bool(permissions[[col]])
  }

  if ("active" %in% names(permissions)) {
    permissions$active <- gdrive_talent_bool(permissions$active)
    if (isTRUE(active_only)) {
      permissions <- permissions[permissions$active, , drop = FALSE]
    }
  }

  permissions <- permissions[
    nzchar(permissions$client_id) &
      nzchar(permissions$client_display_name) &
      nzchar(permissions$delivery_group_id) &
      nzchar(permissions$delivery_group_display_name) &
      nzchar(permissions$talent_id),
    ,
    drop = FALSE
  ]

  rownames(permissions) <- NULL
  permissions
}

gdrive_talent_allowed_permissions <- function(
  permissions,
  permission_cols = gdrive_talent_default_permission_cols()
) {
  permissions <- gdrive_talent_normalize_permissions(
    permissions,
    permission_cols = permission_cols,
    active_only = FALSE
  )

  permission_cols <- intersect(permission_cols, names(permissions))
  if (length(permission_cols) == 0) {
    return(gdrive_talent_empty_df(c(
      "client_id",
      "client_display_name",
      "client_email",
      "delivery_group_id",
      "delivery_group_display_name",
      "talent_id",
      "data_type"
    )))
  }

  rows <- vector("list", 0)
  idx <- 0L

  for (i in seq_len(nrow(permissions))) {
    allowed_flags <- unlist(
      permissions[i, permission_cols, drop = FALSE],
      use.names = FALSE
    )
    allowed_cols <- permission_cols[as.logical(allowed_flags)]
    if (length(allowed_cols) == 0) next

    for (data_type in allowed_cols) {
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        client_id = permissions$client_id[[i]],
        client_display_name = permissions$client_display_name[[i]],
        client_email = if ("client_email" %in% names(permissions)) permissions$client_email[[i]] else "",
        delivery_group_id = permissions$delivery_group_id[[i]],
        delivery_group_display_name = permissions$delivery_group_display_name[[i]],
        talent_id = permissions$talent_id[[i]],
        data_type = data_type,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(gdrive_talent_empty_df(c(
      "client_id",
      "client_display_name",
      "client_email",
      "delivery_group_id",
      "delivery_group_display_name",
      "talent_id",
      "data_type"
    )))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
