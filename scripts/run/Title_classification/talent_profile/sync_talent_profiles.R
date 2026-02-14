library(jsonlite)

tp_get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

tp_repo_root <- normalizePath(file.path(tp_get_script_dir(), "..", "..", "..", ".."), winslash = "/", mustWork = FALSE)
tp_repo_path <- function(...) normalizePath(file.path(tp_repo_root, ...), winslash = "/", mustWork = FALSE)

args <- commandArgs(trailingOnly = TRUE)

config_path <- tp_repo_path("classification", "config", "talent_profiles.json")
talents_dir <- tp_repo_path("classification", "prompts", "talents")

if (length(args) > 0) {
  for (i in seq(1, length(args), by = 2)) {
    key <- args[[i]]
    val <- if (i + 1 <= length(args)) args[[i + 1]] else NA_character_
    if (identical(key, "--config") && !is.na(val)) {
      config_path <- val
    }
    if (identical(key, "--talents-dir") && !is.na(val)) {
      talents_dir <- val
    }
  }
}

if (!file.exists(config_path)) {
  stop("Missing config file: ", config_path)
}
if (!dir.exists(talents_dir)) {
  stop("Missing talents directory: ", talents_dir)
}

cfg <- fromJSON(config_path, simplifyVector = FALSE)
if (is.null(cfg$default_profile) || !nzchar(cfg$default_profile)) {
  cfg$default_profile <- "default"
}

dirs <- list.dirs(talents_dir, full.names = FALSE, recursive = FALSE)
dirs <- sort(unique(dirs[nzchar(dirs)]))

profiles <- list()
for (d in dirs) {
  overlay_path <- file.path("prompts", "talents", d, "overlay.txt")
  if (file.exists(file.path(talents_dir, d, "overlay.txt"))) {
    profiles[[d]] <- list(overlay = overlay_path)
  }
}

if (is.null(profiles[[cfg$default_profile]])) {
  stop(
    "Default profile folder missing or has no overlay: ",
    cfg$default_profile,
    " (expected under ",
    talents_dir,
    ")"
  )
}

matchers <- list()
profile_names <- sort(names(profiles))
for (nm in profile_names) {
  if (identical(nm, cfg$default_profile)) {
    next
  }
  matchers[[length(matchers) + 1L]] <- list(
    pattern = nm,
    profile = nm
  )
}

cfg$profiles <- profiles
cfg$matchers <- matchers

writeLines(toJSON(cfg, pretty = TRUE, auto_unbox = TRUE), con = config_path, useBytes = TRUE)
cat("Synced talent profiles from folder names: ", talents_dir, "\n", sep = "")
cat("Updated config: ", config_path, "\n", sep = "")
