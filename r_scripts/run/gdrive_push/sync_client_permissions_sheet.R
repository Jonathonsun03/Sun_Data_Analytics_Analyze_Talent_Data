#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x
}

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) ""
)
if (!nzchar(script_path)) {
  script_path <- normalizePath("r_scripts/run/gdrive_push/sync_client_permissions_sheet.R", winslash = "/", mustWork = FALSE)
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = FALSE)

source(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data", "load_all.R"))
gdrive_talent_load_all(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data"))

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript r_scripts/run/gdrive_push/sync_client_permissions_sheet.R \\",
      "    [--config r_scripts/run/gdrive_push/config/config.yml] \\",
      "    [--sheet SHEET_ID_OR_URL] [--datalake-root /mnt/datalake/...] \\",
      "    [--skip-talents] [--skip-reports] [--skip-schedule]",
      "",
      "Updates generated/control tabs in the client permissions spreadsheet:",
      "  1. talents",
      "  2. reports",
      "  3. talent_report_schedule",
      "",
      "The schedule tab is updated last because it depends on named ranges from",
      "the talents and reports tabs.",
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
    datalake_root = Sys.getenv("TALENT_DATALAKE_ROOT", unset = ""),
    google_email = Sys.getenv("TALENT_GDRIVE_EMAIL", unset = ""),
    google_auth_cache = Sys.getenv("TALENT_GDRIVE_AUTH_CACHE", unset = ""),
    talents_tab = "talents",
    reports_tab = "reports",
    schedule_tab = "talent_report_schedule",
    skip_talents = FALSE,
    skip_reports = FALSE,
    skip_schedule = FALSE,
    help = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "-h")) {
      out$help <- TRUE
    } else if (arg == "--skip-talents") {
      out$skip_talents <- TRUE
    } else if (arg == "--skip-reports") {
      out$skip_reports <- TRUE
    } else if (arg == "--skip-schedule") {
      out$skip_schedule <- TRUE
    } else if (arg %in% c(
      "--config",
      "--sheet",
      "--datalake-root",
      "--google-email",
      "--google-auth-cache",
      "--talents-tab",
      "--reports-tab",
      "--schedule-tab"
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

  out
}

write_summary <- function(table, label) {
  cat("\n", label, "\n", sep = "")
  if (is.null(table) || nrow(table) == 0) {
    cat("  <no rows>\n")
    return(invisible(NULL))
  }
  cat("  rows: ", nrow(table), "\n", sep = "")
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
  googlesheets4::gs4_auth(email = args$google_email, cache = auth_cache)
}

cat("Syncing client permissions spreadsheet: ", args$sheet, "\n", sep = "")

if (!isTRUE(args$skip_talents)) {
  talents <- gdrive_talent_write_talents_sheet(
    sheet_id = args$sheet,
    worksheet = args$talents_tab,
    datalake_root = args$datalake_root,
    auth = FALSE
  )
  write_summary(talents, "Talents tab synced:")
}

if (!isTRUE(args$skip_reports)) {
  reports <- gdrive_talent_write_reports_sheet(
    sheet_id = args$sheet,
    worksheet = args$reports_tab,
    datalake_root = args$datalake_root,
    auth = FALSE
  )
  write_summary(reports, "Reports tab synced:")
}

if (!isTRUE(args$skip_schedule)) {
  schedule <- gdrive_talent_write_report_schedule_sheet(
    sheet_id = args$sheet,
    worksheet = args$schedule_tab,
    datalake_root = args$datalake_root,
    auth = FALSE
  )
  write_summary(schedule, "Talent report schedule tab synced:")
}

cat("\nDone.\n")
