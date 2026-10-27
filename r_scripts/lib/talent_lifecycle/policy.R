# Read the same per-talent control records as the Python collectors.

talent_control_records <- function() {
  root <- trimws(Sys.getenv("TALENT_CONTROL_ROOT", unset = ""))
  if (!nzchar(root)) return(list())
  if (!dir.exists(root)) stop("Talent control directory is unavailable: ", root)
  mount <- trimws(Sys.getenv("TALENT_CONTROL_MOUNT", unset = ""))
  if (nzchar(mount)) {
    mount <- normalizePath(mount, mustWork = TRUE)
    mounts <- vapply(strsplit(readLines("/proc/self/mountinfo"), " ", fixed = TRUE),
                     function(fields) fields[[5L]], character(1))
    if (!mount %in% mounts ||
        !startsWith(paste0(normalizePath(root, mustWork = TRUE), "/"), paste0(mount, "/"))) {
      stop("Talent control mount is unavailable or does not contain the control directory")
    }
  }
  if (file.access(root, 4L) != 0L) stop("Talent control directory is unreadable")
  paths <- list.files(root, pattern = "\\.json$", full.names = TRUE)
  lapply(paths, function(path) {
    record <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    code <- tools::file_path_sans_ext(basename(path))
    flag_valid <- function(value) is.logical(value) && length(value) == 1L && !is.na(value)
    strings_valid <- function(value) {
      is.list(value) && is.null(names(value)) &&
        all(vapply(value, function(x) is.character(x) && length(x) == 1L, logical(1)))
    }
    if (!grepl("^[A-Za-z][A-Za-z0-9_-]{0,79}$", code) ||
        !identical(record$talent_code, code) ||
        !identical(record$version, 1L) ||
        !flag_valid(record$retrieval_enabled) || !flag_valid(record$processing_enabled) ||
        !strings_valid(record$channel_ids) || !strings_valid(record$aliases) ||
        !is.numeric(record$revision) || length(record$revision) != 1L ||
        is.na(record$revision) || record$revision < 1 || record$revision %% 1 != 0) {
      stop("Invalid talent control record: ", path, call. = FALSE)
    }
    record
  })
}

talent_work_allowed <- function(identifiers, activity = "processing") {
  if (!activity %in% c("processing", "retrieval")) stop("Invalid talent activity")
  identifiers <- tolower(trimws(as.character(identifiers)))
  for (record in talent_control_records()) {
    identities <- tolower(trimws(unlist(c(
      record$talent_code, record$channel_ids, record$aliases
    ))))
    if (any(identifiers %in% identities) && !record[[paste0(activity, "_enabled")]]) {
      return(FALSE)
    }
  }
  TRUE
}

talent_assert_processing <- function(identifiers) {
  if (!talent_work_allowed(identifiers)) {
    stop(errorCondition(
      paste("Talent processing paused:", paste(identifiers, collapse = ", ")),
      class = "talent_processing_paused"
    ))
  }
  invisible(TRUE)
}

talent_processing_filter <- function(rows, column = "talent_code") {
  if (nrow(rows) == 0L) return(rows)
  keep <- vapply(as.character(rows[[column]]), talent_work_allowed, logical(1))
  rows[keep, , drop = FALSE]
}

talent_paused_codes <- function() {
  vapply(Filter(function(record) !record$processing_enabled, talent_control_records()),
         function(record) record$talent_code, character(1))
}
