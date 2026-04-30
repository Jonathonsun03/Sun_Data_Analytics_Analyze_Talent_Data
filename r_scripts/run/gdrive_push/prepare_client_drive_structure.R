#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x
}

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) ""
)
if (!nzchar(script_path)) {
  script_path <- normalizePath("r_scripts/run/gdrive_push/prepare_client_drive_structure.R", winslash = "/", mustWork = FALSE)
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = FALSE)

source(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data", "load_all.R"))
gdrive_talent_load_all(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data"))

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript r_scripts/run/gdrive_push/prepare_client_drive_structure.R \\",
      "    [--config r_scripts/run/gdrive_push/config/config.yml] \\",
      "    [--sheet SHEET_ID_OR_URL] \\",
      "    [--client-drive-root DRIVE_FOLDER_ID_OR_URL] \\",
      "    [--datalake-root /mnt/datalake/...] [--execute]",
      "",
      "Creates empty Google Drive folders as:",
      "  delivery_group / talent / allowed_data_folder_structure",
      "",
      "Use --upload-files with --execute to upload selected delivery files.",
      "Use --share-folders with --execute to share each delivery group folder with client_email recipients.",
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
    sheet = Sys.getenv("TALENT_GDRIVE_PERMISSIONS_SHEET", unset = ""),
    sheet_tab = Sys.getenv("TALENT_GDRIVE_PERMISSIONS_SHEET_TAB", unset = ""),
    client_drive_root = Sys.getenv("TALENT_GDRIVE_CLIENT_ROOT", unset = ""),
    datalake_root = Sys.getenv("TALENT_DATALAKE_ROOT", unset = ""),
    google_email = Sys.getenv("TALENT_GDRIVE_EMAIL", unset = ""),
    google_auth_cache = Sys.getenv("TALENT_GDRIVE_AUTH_CACHE", unset = ""),
    local_max_depth = NA,
    raw_days = NA,
    data_search_depth = NA,
    active_only = NA,
    share_role = "",
    execute = FALSE,
    upload_files = FALSE,
    share_folders = FALSE,
    help = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "-h")) {
      out$help <- TRUE
    } else if (arg == "--execute") {
      out$execute <- TRUE
    } else if (arg == "--upload-files") {
      out$upload_files <- TRUE
    } else if (arg == "--share-folders") {
      out$share_folders <- TRUE
    } else if (arg == "--active-only") {
      out$active_only <- TRUE
    } else if (arg %in% c(
      "--config",
      "--sheet",
      "--sheet-tab",
      "--client-drive-root",
      "--datalake-root",
      "--google-email",
      "--google-auth-cache",
      "--local-max-depth",
      "--raw-days",
      "--data-search-depth",
      "--share-role"
    )) {
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
  if (!nzchar(out$sheet)) {
    out$sheet <- gdrive_talent_config_value(config, "permissions_sheet", "")
  }
  if (!nzchar(out$sheet_tab)) {
    out$sheet_tab <- gdrive_talent_config_value(config, "permissions_sheet_tab", "")
  }
  if (!nzchar(out$client_drive_root)) {
    out$client_drive_root <- gdrive_talent_config_value(config, "client_drive_root", "")
  }
  if (!nzchar(out$datalake_root)) {
    out$datalake_root <- gdrive_talent_config_value(config, "datalake_root", "")
  }
  if (!nzchar(out$google_email)) {
    out$google_email <- gdrive_talent_config_value(config, "google_email", "")
  }
  if (!nzchar(out$google_auth_cache)) {
    out$google_auth_cache <- gdrive_talent_config_value(config, "google_auth_cache", "")
  }
  if (nzchar(out$google_auth_cache)) {
    out$google_auth_cache <- path.expand(out$google_auth_cache)
  }
  if (is.na(out$local_max_depth)) {
    out$local_max_depth <- gdrive_talent_config_value(config, "local_max_depth", Inf)
    if (is.null(out$local_max_depth)) out$local_max_depth <- Inf
  }
  if (is.na(out$data_search_depth)) {
    out$data_search_depth <- gdrive_talent_config_value(config, "data_search_depth", Inf)
    if (is.null(out$data_search_depth)) out$data_search_depth <- Inf
  }
  if (is.na(out$raw_days)) {
    out$raw_days <- gdrive_talent_config_value(config, "raw_days", 14)
    if (is.null(out$raw_days)) out$raw_days <- 14
  }
  if (is.na(out$active_only)) {
    out$active_only <- isTRUE(gdrive_talent_config_value(config, "active_only", FALSE))
  }
  if (!nzchar(out$share_role)) {
    out$share_role <- gdrive_talent_config_value(config, "share_role", "reader")
  }

  for (key in c("local_max_depth", "data_search_depth")) {
    if (!identical(out[[key]], Inf)) {
      out[[key]] <- suppressWarnings(as.integer(out[[key]]))
      if (is.na(out[[key]]) || out[[key]] < 1L) {
        stop("--", gsub("_", "-", key), " must be a positive integer.", call. = FALSE)
      }
    }
  }
  out$raw_days <- suppressWarnings(as.integer(out$raw_days))
  if (is.na(out$raw_days) || out$raw_days < 1L) {
    stop("--raw-days must be a positive integer.", call. = FALSE)
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

if (!nzchar(args$sheet)) {
  stop("Missing --sheet or permissions_sheet in config.", call. = FALSE)
}
if (!nzchar(args$client_drive_root)) {
  stop("Missing --client-drive-root or client_drive_root in config.", call. = FALSE)
}
if (!nzchar(args$datalake_root)) {
  stop("Missing --datalake-root or datalake_root in config.", call. = FALSE)
}

if (nzchar(args$google_email)) {
  cat("Using cached Google auth for: ", args$google_email, "\n", sep = "")
  options(
    gargle_oauth_client_type = "web",
    gargle_oob_default = TRUE
  )
  auth_cache <- if (nzchar(args$google_auth_cache)) args$google_auth_cache else gargle::gargle_oauth_cache()
  googledrive::drive_auth(email = args$google_email, cache = auth_cache)
  googlesheets4::gs4_auth(email = args$google_email, cache = auth_cache)
}

cat("Reading permissions sheet...\n")
permissions <- gdrive_talent_read_permissions(
  sheet_id = args$sheet,
  sheet = if (nzchar(args$sheet_tab)) args$sheet_tab else NULL,
  active_only = args$active_only
)

cat("Building client Drive folder structure plan from DataLake...\n")
plan <- gdrive_talent_build_client_structure_plan(
  permissions = permissions,
  datalake_root = args$datalake_root,
  active_only = args$active_only,
  local_max_depth = args$local_max_depth,
  data_search_depth = args$data_search_depth
)

write_preview(plan$allowed, "Allowed permissions:")
write_preview(plan$structure_plan, "Folder structure plan:")
write_preview(plan$missing, "Missing local DataLake folders:")

file_plan <- gdrive_talent_build_delivery_file_plan(
  structure_plan = plan$structure_plan,
  raw_days = args$raw_days
)
write_preview(file_plan, "Selected delivery files:")

share_plan <- gdrive_talent_build_share_plan(plan$permissions)
write_preview(share_plan, "Delivery group sharing plan:")

cat("\nMode: ", if (isTRUE(args$execute)) "execute" else "dry-run", "\n", sep = "")
result <- gdrive_talent_execute_client_structure_plan(
  structure_plan = plan$structure_plan,
  client_drive_root_folder_id = args$client_drive_root,
  dry_run = !isTRUE(args$execute)
)
write_preview(result, "Drive folder creation result:")

if (isTRUE(args$upload_files)) {
  cat("\nFile upload mode: ", if (isTRUE(args$execute)) "execute" else "dry-run", "\n", sep = "")
  upload_result <- gdrive_talent_execute_delivery_file_plan(
    file_plan = file_plan,
    root_folder_id = args$client_drive_root,
    dry_run = !isTRUE(args$execute)
  )
  write_preview(upload_result, "Delivery file upload result:")
}

if (isTRUE(args$share_folders)) {
  cat("\nFolder sharing mode: ", if (isTRUE(args$execute)) "execute" else "dry-run", "\n", sep = "")
  share_result <- gdrive_talent_execute_share_plan(
    share_plan = share_plan,
    root_folder_id = args$client_drive_root,
    role = args$share_role,
    dry_run = !isTRUE(args$execute)
  )
  write_preview(share_result, "Delivery group sharing result:")
}

cat("\nDone.\n")
