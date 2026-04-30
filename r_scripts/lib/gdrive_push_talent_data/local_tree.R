gdrive_talent_local_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "", x)
  x
}

gdrive_talent_relative_path <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  root_prefix <- paste0(gsub("/+$", "", root), "/")
  if (startsWith(path, root_prefix)) {
    return(substring(path, nchar(root_prefix) + 1L))
  }
  if (identical(path, root)) {
    return("")
  }
  path
}

gdrive_talent_read_local_folder_tree <- function(
  source_root,
  include_root = TRUE,
  max_depth = Inf
) {
  if (!dir.exists(source_root)) {
    stop("Local source folder does not exist: ", source_root, call. = FALSE)
  }

  source_root <- normalizePath(source_root, winslash = "/", mustWork = TRUE)
  dirs <- list.dirs(source_root, recursive = TRUE, full.names = TRUE)
  if (!isTRUE(include_root)) {
    dirs <- setdiff(dirs, source_root)
  }

  rows <- vector("list", 0)
  idx <- 0L

  for (dir in dirs) {
    rel <- if (identical(dir, source_root)) {
      basename(source_root)
    } else {
      rel_from_root <- gdrive_talent_relative_path(dir, source_root)
      if (isTRUE(include_root)) file.path(basename(source_root), rel_from_root) else rel_from_root
    }
    rel <- gsub("\\\\", "/", rel)
    depth <- if (!nzchar(rel)) 0L else length(strsplit(rel, "/", fixed = TRUE)[[1]])

    if (depth > max_depth) next

    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      path = rel,
      name = basename(dir),
      local_path = dir,
      parent_path = if (grepl("/", rel, fixed = TRUE)) dirname(rel) else "",
      depth = depth,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    return(gdrive_talent_empty_df(c("path", "name", "local_path", "parent_path", "depth")))
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$depth, out$path), , drop = FALSE]
  rownames(out) <- NULL
  out
}

gdrive_talent_resolve_local_talent_dir <- function(datalake_root, talent_id) {
  if (!dir.exists(datalake_root)) {
    stop("DataLake root does not exist: ", datalake_root, call. = FALSE)
  }

  candidates <- list.dirs(datalake_root, recursive = FALSE, full.names = TRUE)
  if (length(candidates) == 0) return(NA_character_)

  talent_key <- gdrive_talent_local_key(talent_id)
  candidate_names <- basename(candidates)
  candidate_keys <- gdrive_talent_local_key(candidate_names)

  aliases <- gdrive_talent_default_talent_aliases()
  alias_value <- aliases[[talent_key]]
  if (!is.null(alias_value)) {
    alias_key <- gdrive_talent_local_key(alias_value)
    alias_match <- which(candidate_keys == alias_key)
    if (length(alias_match) > 0) return(candidates[[alias_match[[1]]]])
  }

  exact <- which(candidate_names == talent_id)
  if (length(exact) > 0) return(candidates[[exact[[1]]]])

  key_match <- which(candidate_keys == talent_key)
  if (length(key_match) > 0) return(candidates[[key_match[[1]]]])

  reverse_contains <- vapply(
    candidate_keys,
    function(candidate_key) grepl(candidate_key, talent_key, fixed = TRUE),
    logical(1)
  )
  contains_match <- which(grepl(talent_key, candidate_keys, fixed = TRUE) | reverse_contains)
  if (length(contains_match) > 0) return(candidates[[contains_match[[1]]]])

  NA_character_
}

gdrive_talent_default_talent_aliases <- function() {
  list(
    teri = "Terberri Solaris Ch"
  )
}

gdrive_talent_resolve_local_data_dir <- function(
  talent_dir,
  data_type,
  search_depth = Inf
) {
  if (is.na(talent_dir) || !dir.exists(talent_dir)) return(NA_character_)

  for (rel_path in gdrive_talent_data_type_aliases(data_type)) {
    candidate <- file.path(talent_dir, rel_path)
    if (dir.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  dirs <- list.dirs(talent_dir, recursive = TRUE, full.names = TRUE)
  dirs <- setdiff(dirs, talent_dir)
  if (length(dirs) == 0) return(NA_character_)

  rel <- vapply(dirs, gdrive_talent_relative_path, character(1), root = talent_dir)
  depth <- vapply(strsplit(rel, "/", fixed = TRUE), length, integer(1))
  keep <- depth <= search_depth
  dirs <- dirs[keep]
  rel <- rel[keep]
  depth <- depth[keep]

  data_key <- gdrive_talent_local_key(data_type)
  base_keys <- gdrive_talent_local_key(basename(dirs))

  matches <- which(base_keys == data_key)
  if (length(matches) == 0) {
    rel_keys <- gdrive_talent_local_key(rel)
    matches <- which(grepl(data_key, rel_keys, fixed = TRUE))
  }

  if (length(matches) == 0) return(NA_character_)

  matches <- matches[order(depth[matches], rel[matches])]
  dirs[[matches[[1]]]]
}

gdrive_talent_data_type_aliases <- function(data_type) {
  data_type <- tolower(gdrive_talent_chr(data_type))

  aliases <- list(
    public_subs = c("raw_data/public_subscribers", "raw_data/subs_daily"),
    video_analytics = "raw_data/video_analytics",
    video_demographics = "raw_data/video_demographics",
    video_monetary = "raw_data/video_monetary",
    text_playback = "text_playback",
    chat_logs = "Chat",
    bundle_a = c("reports/bundle_A", "reports/bundle_a"),
    bundle_b = c("reports/bundle_B", "reports/bundle_b"),
    bundle_c = c("reports/bundle_C", "reports/bundle_c"),
    bundle_e = c("reports/bundle_E", "reports/bundle_e"),
    bundle_z = c("reports/bundle_Z", "reports/bundle_z")
  )

  if (!is.null(aliases[[data_type]])) {
    return(aliases[[data_type]])
  }

  character()
}
