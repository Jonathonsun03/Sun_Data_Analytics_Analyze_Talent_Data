gdrive_talent_manifest <- function(entries) {
  entries <- as.data.frame(entries, stringsAsFactors = FALSE)

  required <- c("data_type", "local_path_pattern")
  missing <- setdiff(required, names(entries))
  if (length(missing) > 0) {
    stop(
      "Manifest is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  entries$data_type <- gdrive_talent_chr(entries$data_type)
  entries$local_path_pattern <- gdrive_talent_chr(entries$local_path_pattern)

  if (!"drive_subdir" %in% names(entries)) {
    entries$drive_subdir <- entries$data_type
  }
  entries$drive_subdir <- gdrive_talent_safe_name(entries$drive_subdir)

  entries <- entries[nzchar(entries$data_type) & nzchar(entries$local_path_pattern), , drop = FALSE]
  rownames(entries) <- NULL
  entries
}

gdrive_talent_resolve_local_files <- function(
  allowed_permissions,
  manifest,
  warn_missing = TRUE
) {
  manifest <- gdrive_talent_manifest(manifest)
  allowed_permissions <- as.data.frame(allowed_permissions, stringsAsFactors = FALSE)

  required <- c("client_id", "client_display_name", "talent_id", "data_type")
  missing <- setdiff(required, names(allowed_permissions))
  if (length(missing) > 0) {
    stop(
      "Allowed permissions are missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  rows <- vector("list", 0)
  missing_rows <- vector("list", 0)
  row_idx <- 0L
  missing_idx <- 0L

  for (i in seq_len(nrow(allowed_permissions))) {
    perm <- allowed_permissions[i, , drop = FALSE]
    entry <- manifest[manifest$data_type == perm$data_type[[1]], , drop = FALSE]

    if (nrow(entry) == 0) {
      missing_idx <- missing_idx + 1L
      missing_rows[[missing_idx]] <- data.frame(
        client_id = perm$client_id[[1]],
        delivery_group_id = if ("delivery_group_id" %in% names(perm)) perm$delivery_group_id[[1]] else perm$client_id[[1]],
        delivery_group_display_name = if ("delivery_group_display_name" %in% names(perm)) perm$delivery_group_display_name[[1]] else perm$client_display_name[[1]],
        talent_id = perm$talent_id[[1]],
        data_type = perm$data_type[[1]],
        reason = "data_type_not_in_manifest",
        pattern = "",
        stringsAsFactors = FALSE
      )
      next
    }

    for (j in seq_len(nrow(entry))) {
      values <- as.list(perm)
      values$client_slug <- gdrive_talent_slug(perm$client_id[[1]])
      values$delivery_group_slug <- gdrive_talent_slug(
        if ("delivery_group_id" %in% names(perm)) perm$delivery_group_id[[1]] else perm$client_id[[1]]
      )
      values$talent_slug <- gdrive_talent_slug(perm$talent_id[[1]])
      values$data_type_slug <- gdrive_talent_slug(perm$data_type[[1]])

      pattern <- gdrive_talent_replace_tokens(entry$local_path_pattern[[j]], values)
      files <- Sys.glob(pattern)
      files <- files[file.exists(files) & !dir.exists(files)]

      if (length(files) == 0) {
        missing_idx <- missing_idx + 1L
        missing_rows[[missing_idx]] <- data.frame(
          client_id = perm$client_id[[1]],
          delivery_group_id = if ("delivery_group_id" %in% names(perm)) perm$delivery_group_id[[1]] else perm$client_id[[1]],
          delivery_group_display_name = if ("delivery_group_display_name" %in% names(perm)) perm$delivery_group_display_name[[1]] else perm$client_display_name[[1]],
          talent_id = perm$talent_id[[1]],
          data_type = perm$data_type[[1]],
          reason = "no_local_files_matched",
          pattern = pattern,
          stringsAsFactors = FALSE
        )
        next
      }

      for (local_file in files) {
        row_idx <- row_idx + 1L
        rows[[row_idx]] <- data.frame(
          client_id = perm$client_id[[1]],
          client_display_name = perm$client_display_name[[1]],
          client_email = if ("client_email" %in% names(perm)) perm$client_email[[1]] else "",
          delivery_group_id = if ("delivery_group_id" %in% names(perm)) perm$delivery_group_id[[1]] else perm$client_id[[1]],
          delivery_group_display_name = if ("delivery_group_display_name" %in% names(perm)) perm$delivery_group_display_name[[1]] else perm$client_display_name[[1]],
          talent_id = perm$talent_id[[1]],
          data_type = perm$data_type[[1]],
          drive_subdir = entry$drive_subdir[[j]],
          local_file = normalizePath(local_file, winslash = "/", mustWork = FALSE),
          drive_file_name = basename(local_file),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  upload_plan <- if (length(rows) > 0) {
    do.call(rbind, rows)
  } else {
    gdrive_talent_empty_df(c(
      "client_id",
      "client_display_name",
      "client_email",
      "delivery_group_id",
      "delivery_group_display_name",
      "talent_id",
      "data_type",
      "drive_subdir",
      "local_file",
      "drive_file_name"
    ))
  }

  missing_plan <- if (length(missing_rows) > 0) {
    do.call(rbind, missing_rows)
  } else {
    gdrive_talent_empty_df(c(
      "client_id",
      "delivery_group_id",
      "delivery_group_display_name",
      "talent_id",
      "data_type",
      "reason",
      "pattern"
    ))
  }

  if (isTRUE(warn_missing) && nrow(missing_plan) > 0) {
    warning(
      nrow(missing_plan),
      " permission/manifest row(s) did not resolve to local files.",
      call. = FALSE
    )
  }

  if (nrow(upload_plan) > 0) {
    upload_plan <- unique(upload_plan)
  }

  rownames(upload_plan) <- NULL
  rownames(missing_plan) <- NULL

  list(upload_plan = upload_plan, missing = missing_plan)
}
