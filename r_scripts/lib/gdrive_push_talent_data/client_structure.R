gdrive_talent_build_client_structure_plan <- function(
  permissions,
  datalake_root,
  permission_cols = gdrive_talent_default_permission_cols(),
  active_only = FALSE,
  local_max_depth = Inf,
  data_search_depth = Inf
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

  rows <- vector("list", 0)
  missing <- vector("list", 0)
  row_idx <- 0L
  missing_idx <- 0L

  for (i in seq_len(nrow(allowed))) {
    row <- allowed[i, , drop = FALSE]
    talent_dir <- gdrive_talent_resolve_local_talent_dir(
      datalake_root = datalake_root,
      talent_id = row$talent_id[[1]]
    )

    if (is.na(talent_dir)) {
      missing_idx <- missing_idx + 1L
      missing[[missing_idx]] <- data.frame(
        delivery_group_id = row$delivery_group_id[[1]],
        delivery_group_display_name = row$delivery_group_display_name[[1]],
        client_display_name = row$client_display_name[[1]],
        talent_id = row$talent_id[[1]],
        data_type = row$data_type[[1]],
        reason = "talent_folder_not_found",
        local_path = "",
        stringsAsFactors = FALSE
      )
      next
    }

    data_dir <- gdrive_talent_resolve_local_data_dir(
      talent_dir = talent_dir,
      data_type = row$data_type[[1]],
      search_depth = data_search_depth
    )

    if (is.na(data_dir)) {
      missing_idx <- missing_idx + 1L
      missing[[missing_idx]] <- data.frame(
        delivery_group_id = row$delivery_group_id[[1]],
        delivery_group_display_name = row$delivery_group_display_name[[1]],
        client_display_name = row$client_display_name[[1]],
        talent_id = row$talent_id[[1]],
        data_type = row$data_type[[1]],
        reason = "data_type_folder_not_found",
        local_path = talent_dir,
        stringsAsFactors = FALSE
      )
      next
    }

    row_idx <- row_idx + 1L
    rows[[row_idx]] <- data.frame(
      client_id = row$client_id[[1]],
      client_display_name = row$client_display_name[[1]],
      client_email = if ("client_email" %in% names(row)) row$client_email[[1]] else "",
      delivery_group_id = row$delivery_group_id[[1]],
      delivery_group_display_name = row$delivery_group_display_name[[1]],
      talent_id = row$talent_id[[1]],
      data_type = row$data_type[[1]],
      source_local_path = data_dir,
      source_relative_path = row$data_type[[1]],
      folder_name = row$data_type[[1]],
      parent_path = "",
      depth = 1L,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) > 0) {
    rows <- rows[!duplicated(vapply(
      rows,
      function(x) paste(
        x$delivery_group_id,
        x$talent_id,
        x$data_type,
        x$source_relative_path,
        sep = "\r"
      ),
      character(1)
    ))]
  }

  structure_plan <- if (length(rows) > 0) {
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
      "source_local_path",
      "source_relative_path",
      "folder_name",
      "parent_path",
      "depth"
    ))
  }

  missing_plan <- if (length(missing) > 0) {
    do.call(rbind, missing)
  } else {
    gdrive_talent_empty_df(c(
      "client_display_name",
      "delivery_group_id",
      "delivery_group_display_name",
      "talent_id",
      "data_type",
      "reason",
      "local_path"
    ))
  }

  rownames(structure_plan) <- NULL
  rownames(missing_plan) <- NULL

  list(
    permissions = normalized,
    allowed = allowed,
    structure_plan = structure_plan,
    missing = missing_plan
  )
}

gdrive_talent_execute_client_structure_plan <- function(
  structure_plan,
  client_drive_root_folder_id,
  dry_run = TRUE
) {
  structure_plan <- as.data.frame(structure_plan, stringsAsFactors = FALSE)
  if (nrow(structure_plan) == 0) {
    return(gdrive_talent_empty_df(c(
      "client_display_name",
      "delivery_group_display_name",
      "talent_id",
      "source_relative_path",
      "folder_name",
      "status"
    )))
  }

  structure_plan <- structure_plan[order(
    structure_plan$client_display_name,
    structure_plan$delivery_group_display_name,
    structure_plan$talent_id,
    structure_plan$depth,
    structure_plan$source_relative_path
  ), , drop = FALSE]

  folders <- list()
  folders[[""]] <- client_drive_root_folder_id
  rows <- vector("list", 0)
  idx <- 0L

  for (i in seq_len(nrow(structure_plan))) {
    row <- structure_plan[i, , drop = FALSE]

    delivery_group_key <- paste("delivery_group", row$delivery_group_id[[1]], sep = "::")
    if (is.null(folders[[delivery_group_key]])) {
      folders[[delivery_group_key]] <- gdrive_talent_get_or_create_folder(
        name = row$delivery_group_display_name[[1]],
        parent = client_drive_root_folder_id,
        dry_run = dry_run
      )
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        client_display_name = row$client_display_name[[1]],
        delivery_group_display_name = row$delivery_group_display_name[[1]],
        talent_id = "",
        source_relative_path = "",
        folder_name = row$delivery_group_display_name[[1]],
        status = if (isTRUE(dry_run)) "dry_run" else "created_or_existing",
        stringsAsFactors = FALSE
      )
    }

    talent_key <- paste(delivery_group_key, "talent", row$talent_id[[1]], sep = "::")
    if (is.null(folders[[talent_key]])) {
      folders[[talent_key]] <- gdrive_talent_get_or_create_folder(
        name = row$talent_id[[1]],
        parent = folders[[delivery_group_key]],
        dry_run = dry_run
      )
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        client_display_name = row$client_display_name[[1]],
        delivery_group_display_name = row$delivery_group_display_name[[1]],
        talent_id = row$talent_id[[1]],
        source_relative_path = "",
        folder_name = row$talent_id[[1]],
        status = if (isTRUE(dry_run)) "dry_run" else "created_or_existing",
        stringsAsFactors = FALSE
      )
    }

    parent_key <- if (nzchar(row$parent_path[[1]])) {
      paste(talent_key, row$parent_path[[1]], sep = "::")
    } else {
      talent_key
    }

    parent <- folders[[parent_key]]
    if (is.null(parent)) {
      stop(
        "Could not resolve destination parent for folder: ",
        row$source_relative_path[[1]],
        call. = FALSE
      )
    }

    folder_key <- paste(talent_key, row$source_relative_path[[1]], sep = "::")
    if (!is.null(folders[[folder_key]])) next

    folders[[folder_key]] <- gdrive_talent_get_or_create_folder(
      name = row$folder_name[[1]],
      parent = parent,
      dry_run = dry_run
    )

    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      client_display_name = row$client_display_name[[1]],
      delivery_group_display_name = row$delivery_group_display_name[[1]],
      talent_id = row$talent_id[[1]],
      source_relative_path = row$source_relative_path[[1]],
      folder_name = row$folder_name[[1]],
      status = if (isTRUE(dry_run)) "dry_run" else "created_or_existing",
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    return(gdrive_talent_empty_df(c(
      "client_display_name",
      "delivery_group_display_name",
      "talent_id",
      "source_relative_path",
      "folder_name",
      "status"
    )))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
