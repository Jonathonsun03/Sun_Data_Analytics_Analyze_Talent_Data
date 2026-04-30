#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x
}

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) ""
)
if (!nzchar(script_path)) {
  script_path <- normalizePath("r_scripts/run/gdrive_push/sync_permissions_to_drive.R", winslash = "/", mustWork = FALSE)
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = FALSE)

source(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data", "load_all.R"))
gdrive_talent_load_all(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data"))

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript r_scripts/run/gdrive_push/sync_permissions_to_drive.R \\",
      "    --sheet SHEET_ID_OR_URL \\",
      "    --drive-folder DRIVE_FOLDER_ID_OR_URL \\",
      "    --manifest PATH_TO_MANIFEST_CSV [--sheet-tab TAB_NAME] [--execute] [--plan-stale] [--apply-stale] [--stale-mode archive|trash]",
      "",
      "Manifest CSV columns:",
      "  data_type,local_path_pattern,drive_subdir",
      "",
      "Examples:",
      "  # Plan uploads only; does not upload files.",
      "  Rscript r_scripts/run/gdrive_push/sync_permissions_to_drive.R \\",
      "    --sheet \"$TALENT_GDRIVE_PERMISSIONS_SHEET\" \\",
      "    --drive-folder \"1yHtl7fp5cLUh7mSpaK0ux08PJzcPJfHT\" \\",
      "    --manifest /mnt/datalake/.../gdrive_push_manifest.csv",
      "",
      "  # Upload approved files.",
      "  Rscript r_scripts/run/gdrive_push/sync_permissions_to_drive.R \\",
      "    --sheet \"$TALENT_GDRIVE_PERMISSIONS_SHEET\" \\",
      "    --drive-folder \"1yHtl7fp5cLUh7mSpaK0ux08PJzcPJfHT\" \\",
      "    --manifest /mnt/datalake/.../gdrive_push_manifest.csv \\",
      "    --execute",
      "",
      sep = "\n"
    )
  )
}

parse_args <- function(args) {
  out <- list(
    sheet = Sys.getenv("TALENT_GDRIVE_PERMISSIONS_SHEET", unset = ""),
    sheet_tab = Sys.getenv("TALENT_GDRIVE_PERMISSIONS_SHEET_TAB", unset = ""),
    drive_folder = Sys.getenv("TALENT_GDRIVE_ROOT_FOLDER", unset = "1yHtl7fp5cLUh7mSpaK0ux08PJzcPJfHT"),
    manifest = Sys.getenv("TALENT_GDRIVE_PUSH_MANIFEST", unset = ""),
    execute = FALSE,
    plan_stale = FALSE,
    apply_stale = FALSE,
    stale_mode = "archive",
    active_only = FALSE,
    help = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg %in% c("--help", "-h")) {
      out$help <- TRUE
    } else if (arg == "--execute") {
      out$execute <- TRUE
    } else if (arg == "--plan-stale") {
      out$plan_stale <- TRUE
    } else if (arg == "--apply-stale") {
      out$apply_stale <- TRUE
      out$plan_stale <- TRUE
    } else if (arg == "--active-only") {
      out$active_only <- TRUE
    } else if (arg %in% c("--sheet", "--sheet-tab", "--drive-folder", "--manifest", "--stale-mode")) {
      if (i == length(args)) {
        stop("Missing value after ", arg, call. = FALSE)
      }
      value <- args[[i + 1L]]
      key <- sub("^--", "", arg)
      key <- gsub("-", "_", key)
      out[[key]] <- value
      i <- i + 1L
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }

    i <- i + 1L
  }

  out
}

read_manifest_csv <- function(path) {
  if (!nzchar(path)) {
    stop("Missing --manifest. Provide a CSV with data_type, local_path_pattern, and drive_subdir.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Manifest CSV does not exist: ", path, call. = FALSE)
  }

  gdrive_talent_manifest(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
}

write_preview <- function(x, label, n = 20L) {
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

if (!nzchar(args$sheet)) {
  stop("Missing --sheet or TALENT_GDRIVE_PERMISSIONS_SHEET.", call. = FALSE)
}
if (!nzchar(args$drive_folder)) {
  stop("Missing --drive-folder or TALENT_GDRIVE_ROOT_FOLDER.", call. = FALSE)
}
if (!args$stale_mode %in% c("archive", "trash")) {
  stop("--stale-mode must be either archive or trash.", call. = FALSE)
}

cat("Reading permissions sheet...\n")
permissions <- gdrive_talent_read_permissions(
  sheet_id = args$sheet,
  sheet = if (nzchar(args$sheet_tab)) args$sheet_tab else NULL,
  active_only = args$active_only
)

cat("Reading local manifest...\n")
manifest <- read_manifest_csv(args$manifest)

cat("Building upload plan...\n")
plan <- gdrive_talent_build_upload_plan(
  permissions = permissions,
  manifest = manifest,
  active_only = args$active_only
)

write_preview(plan$allowed, "Allowed permission rows:")
write_preview(plan$upload_plan, "Resolved upload plan:")
write_preview(plan$missing, "Missing local files / unmapped data types:")

cat("\nUpload mode: ", if (isTRUE(args$execute)) "execute" else "dry-run", "\n", sep = "")
upload_result <- gdrive_talent_execute_upload_plan(
  upload_plan = plan$upload_plan,
  root_folder_id = args$drive_folder,
  dry_run = !isTRUE(args$execute)
)
write_preview(upload_result, "Upload result:")

if (isTRUE(args$plan_stale)) {
  cat("\nBuilding stale-file plan from Google Drive...\n")
  stale <- gdrive_talent_build_stale_file_plan(
    upload_plan = plan$upload_plan,
    root_folder_id = args$drive_folder
  )
  write_preview(stale, "Stale remote files:")

  if (isTRUE(args$apply_stale)) {
    cat("\nApplying stale-file action: ", args$stale_mode, "\n", sep = "")
    stale_result <- gdrive_talent_execute_stale_file_plan(
      stale_plan = stale,
      root_folder_id = args$drive_folder,
      mode = args$stale_mode,
      dry_run = FALSE
    )
    write_preview(stale_result, "Stale-file result:")
  } else {
    cat("\nStale-file action: dry-run only. Add --apply-stale to archive/trash stale files.\n")
  }
}

cat("\nDone.\n")
