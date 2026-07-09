#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x
}

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) ""
)
if (!nzchar(script_path)) {
  script_path <- normalizePath("r_scripts/run/gdrive_push/clone_folder_structure.R", winslash = "/", mustWork = FALSE)
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = FALSE)

source(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data", "load_all.R"))
gdrive_talent_load_all(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data"))

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript r_scripts/run/gdrive_push/clone_folder_structure.R \\",
      "    [--config r_scripts/run/gdrive_push/config/config.yml] \\",
      "    [--source-folder SOURCE_FOLDER_ID_OR_URL] \\",
      "    [--destination-folder DESTINATION_FOLDER_ID_OR_URL] [--include-root] [--max-depth N] [--execute]",
      "",
      "Default config path: r_scripts/run/gdrive_push/config/config.yml",
      "Default behavior is dry-run. Add --execute to create missing folders.",
      "",
      sep = "\n"
    )
  )
}

parse_args <- function(args) {
  default_config <- gdrive_talent_default_config_path(repo_root)
  out <- list(
    config = Sys.getenv("TALENT_GDRIVE_PUSH_CONFIG", unset = default_config),
    source_folder = Sys.getenv("TALENT_GDRIVE_SOURCE_FOLDER", unset = ""),
    destination_folder = Sys.getenv("TALENT_GDRIVE_DESTINATION_FOLDER", unset = ""),
    include_root = NA,
    max_depth = NA,
    execute = FALSE,
    help = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "-h")) {
      out$help <- TRUE
    } else if (arg == "--include-root") {
      out$include_root <- TRUE
    } else if (arg == "--execute") {
      out$execute <- TRUE
    } else if (arg %in% c("--config", "--source-folder", "--destination-folder", "--max-depth")) {
      if (i == length(args)) stop("Missing value after ", arg, call. = FALSE)
      key <- gsub("-", "_", sub("^--", "", arg))
      out[[key]] <- args[[i + 1L]]
      i <- i + 1L
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1L
  }

  config <- gdrive_talent_read_config(out$config)
  if (!nzchar(out$source_folder)) {
    out$source_folder <- gdrive_talent_config_value(config, "source_folder", "")
  }
  if (!nzchar(out$destination_folder)) {
    out$destination_folder <- gdrive_talent_config_value(config, "destination_folder", "")
  }
  if (is.na(out$include_root)) {
    out$include_root <- isTRUE(gdrive_talent_config_value(config, "include_root", FALSE))
  }
  if (is.na(out$max_depth)) {
    out$max_depth <- gdrive_talent_config_value(config, "max_depth", Inf)
    if (is.null(out$max_depth)) out$max_depth <- Inf
  }

  if (!identical(out$max_depth, Inf)) {
    out$max_depth <- suppressWarnings(as.integer(out$max_depth))
    if (is.na(out$max_depth) || out$max_depth < 1L) {
      stop("--max-depth must be a positive integer.", call. = FALSE)
    }
  }

  out
}

write_preview <- function(x, label, n = 40L) {
  cat("\n", label, "\n", sep = "")
  if (is.null(x) || nrow(x) == 0) {
    cat("  <none>\n")
    return(invisible(NULL))
  }
  print(utils::head(x, n), row.names = FALSE)
  if (nrow(x) > n) {
    cat("  ... ", nrow(x) - n, " more row(s)\n", sep = "")
  }
  invisible(NULL)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(args$help)) {
  usage()
  quit(status = 0)
}

if (!nzchar(args$source_folder)) {
  stop("Missing --source-folder or TALENT_GDRIVE_SOURCE_FOLDER.", call. = FALSE)
}
if (!nzchar(args$destination_folder)) {
  stop("Missing --destination-folder or TALENT_GDRIVE_DESTINATION_FOLDER.", call. = FALSE)
}

cat("Reading source Drive folder tree...\n")
cat("Mode: ", if (isTRUE(args$execute)) "execute" else "dry-run", "\n", sep = "")

clone <- gdrive_talent_clone_drive_structure(
  source_root_folder_id = args$source_folder,
  destination_root_folder_id = args$destination_folder,
  include_root = args$include_root,
  max_depth = args$max_depth,
  dry_run = !isTRUE(args$execute)
)

write_preview(clone$tree, "Source folder tree:")
write_preview(clone$result, "Folder clone result:")

cat("\nDone.\n")
