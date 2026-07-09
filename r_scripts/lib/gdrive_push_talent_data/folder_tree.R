gdrive_talent_read_folder_tree <- function(
  source_root_folder_id,
  include_root = FALSE,
  max_depth = Inf
) {
  gdrive_talent_assert_packages("googledrive")

  source_root <- gdrive_talent_as_id(source_root_folder_id)
  rows <- vector("list", 0)
  visited <- character()
  idx <- 0L

  walk <- function(parent, parent_path, depth) {
    if (depth > max_depth) return(invisible(NULL))

    folders <- googledrive::drive_ls(parent, type = "folder")
    if (nrow(folders) == 0) return(invisible(NULL))

    for (i in seq_len(nrow(folders))) {
      folder_id <- as.character(folders$id[[i]])
      if (folder_id %in% visited) next
      visited <<- c(visited, folder_id)

      folder_name <- gdrive_talent_safe_name(folders$name[[i]])
      folder_path <- if (nzchar(parent_path)) {
        file.path(parent_path, folder_name)
      } else {
        folder_name
      }
      folder_path <- gsub("\\\\", "/", folder_path)

      idx <<- idx + 1L
      rows[[idx]] <<- data.frame(
        path = folder_path,
        name = folder_name,
        id = folder_id,
        parent_path = parent_path,
        depth = depth,
        stringsAsFactors = FALSE
      )

      walk(folders[i, , drop = FALSE], folder_path, depth + 1L)
    }

    invisible(NULL)
  }

  if (isTRUE(include_root)) {
    root_meta <- googledrive::drive_get(source_root)
    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      path = gdrive_talent_safe_name(root_meta$name[[1]]),
      name = gdrive_talent_safe_name(root_meta$name[[1]]),
      id = as.character(root_meta$id[[1]]),
      parent_path = "",
      depth = 0L,
      stringsAsFactors = FALSE
    )
    visited <- c(visited, as.character(root_meta$id[[1]]))
    walk(source_root, rows[[idx]]$path[[1]], 1L)
  } else {
    walk(source_root, "", 1L)
  }

  if (length(rows) == 0) {
    return(gdrive_talent_empty_df(c("path", "name", "id", "parent_path", "depth")))
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$depth, out$path), , drop = FALSE]
  rownames(out) <- NULL
  out
}

gdrive_talent_validate_folder_tree <- function(folder_tree) {
  folder_tree <- as.data.frame(folder_tree, stringsAsFactors = FALSE)
  required <- c("path", "name", "parent_path", "depth")
  missing <- setdiff(required, names(folder_tree))
  if (length(missing) > 0) {
    stop(
      "Folder tree is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  duplicate_paths <- folder_tree$path[duplicated(folder_tree$path)]
  if (length(duplicate_paths) > 0) {
    stop(
      "Folder tree has duplicate paths. Rename duplicate sibling folders before cloning: ",
      paste(unique(duplicate_paths), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

gdrive_talent_clone_folder_tree <- function(
  folder_tree,
  destination_root_folder_id,
  dry_run = TRUE
) {
  gdrive_talent_validate_folder_tree(folder_tree)
  folder_tree <- as.data.frame(folder_tree, stringsAsFactors = FALSE)
  folder_tree <- folder_tree[order(folder_tree$depth, folder_tree$path), , drop = FALSE]

  created <- list()
  created[[""]] <- destination_root_folder_id
  rows <- vector("list", nrow(folder_tree))

  for (i in seq_len(nrow(folder_tree))) {
    row <- folder_tree[i, , drop = FALSE]
    parent_path <- row$parent_path[[1]]
    parent <- created[[parent_path]]

    if (is.null(parent)) {
      stop(
        "Could not resolve destination parent for source path: ",
        row$path[[1]],
        call. = FALSE
      )
    }

    dest <- gdrive_talent_get_or_create_folder(
      name = row$name[[1]],
      parent = parent,
      dry_run = dry_run
    )
    created[[row$path[[1]]]] <- dest

    rows[[i]] <- data.frame(
      source_path = row$path[[1]],
      folder_name = row$name[[1]],
      destination_parent_path = parent_path,
      destination_folder_id = if ("id" %in% names(dest)) as.character(dest$id[[1]]) else NA_character_,
      status = if (isTRUE(dry_run)) "dry_run" else "created_or_existing",
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    return(gdrive_talent_empty_df(c(
      "source_path",
      "folder_name",
      "destination_parent_path",
      "destination_folder_id",
      "status"
    )))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

gdrive_talent_clone_drive_structure <- function(
  source_root_folder_id,
  destination_root_folder_id,
  include_root = FALSE,
  max_depth = Inf,
  dry_run = TRUE
) {
  tree <- gdrive_talent_read_folder_tree(
    source_root_folder_id = source_root_folder_id,
    include_root = include_root,
    max_depth = max_depth
  )
  result <- gdrive_talent_clone_folder_tree(
    folder_tree = tree,
    destination_root_folder_id = destination_root_folder_id,
    dry_run = dry_run
  )

  list(tree = tree, result = result)
}
