#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && !nzchar(x))) y else x
}

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) ""
)
if (!nzchar(script_path)) {
  script_path <- normalizePath(
    "r_scripts/run/run_scheduled_reports.R",
    winslash = "/",
    mustWork = FALSE
  )
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = FALSE)

source(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data", "load_all.R"))
gdrive_talent_load_all(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data"))

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript r_scripts/run/run_scheduled_reports.R \\",
      "    [--config r_scripts/run/gdrive_push/config/config.yml] [--sheet SHEET_ID_OR_URL] \\",
      "    [--as-of-date YYYY-MM-DD] [--dry-run] [--no-interpretation]",
      "",
      "Reads the client permissions spreadsheet schedule and dispatches due report rows.",
      "",
      "Schedule mapping:",
      "  talent_report_schedule.active                 -> row must be TRUE",
      "  talent_report_schedule.report_id              -> dispatches to a supported report runner",
      "  supported report_id values                    -> bundle_a, bundle_e, bundle_f",
      "  talent_report_schedule.canonical_talent_name  -> mapped via talents.datalake_folder_name",
      "  talent_report_schedule.window_days            -> passed to the report runner; use lifetime for all data",
      "  talent_report_schedule.report_params          -> optional JSON bundle parameters",
      "  talent_report_schedule.next_run/status        -> due-date filtering",
      "",
      "Options:",
      "  --config PATH                  Config YAML path",
      "  --sheet SHEET_ID_OR_URL         Override permissions_sheet",
      "  --google-email EMAIL            Override google_email",
      "  --google-auth-cache PATH        Override google_auth_cache",
      "  --schedule-tab TAB              Schedule worksheet (default: talent_report_schedule)",
      "  --talents-tab TAB               Talents worksheet (default: talents)",
      "  --as-of-date YYYY-MM-DD         Date used for due filtering (default: today)",
      "  --report-id ID                  Only run matching report_id rows",
      "  --talent NAME                   Only run matching canonical or datalake talent rows",
      "  --window-days N|lifetime        Only run matching window_days rows",
      "  --bundle-a-runner PATH          Bundle A full-pipeline runner path",
      "  --bundle-e-runner PATH          Bundle E full-pipeline runner path",
      "  --bundle-f-runner PATH          Bundle F full-pipeline runner path",
      "  --input-source staging|datalake Passed to report runners",
      "  --datalake-root PATH            Passed to report runners",
      "  --staging-root PATH             Passed to report runners",
      "  --limit N                       Run at most N due rows",
      "  --dry-run                       Print commands without running reports",
      "  --refresh-schedule-only         Refresh computed schedule columns without running reports",
      "  --skip-schedule-update          Do not refresh computed schedule columns after runs",
      "  --no-interpretation             Skip interpretation and editorial rewrite",
      "  --skip-interpretation           Pass --skip-interpretation through",
      "  --skip-editorial-rewrite        Pass --skip-editorial-rewrite through",
      "  -h, --help                      Show this help",
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
    google_email = Sys.getenv("TALENT_GDRIVE_EMAIL", unset = ""),
    google_auth_cache = Sys.getenv("TALENT_GDRIVE_AUTH_CACHE", unset = ""),
    schedule_tab = "talent_report_schedule",
    talents_tab = "talents",
    as_of_date = Sys.Date(),
    report_id = "",
    talent = "",
    window_days = "",
    bundle_a_runner = "bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh",
    bundle_e_runner = "bin/linux/render_reports/bundle_E/run_bundle_E_full_pipeline.sh",
    bundle_f_runner = "bin/linux/render_reports/bundle_F/run_bundle_F_full_pipeline.sh",
    input_source = "datalake",
    datalake_root = "",
    staging_root = "",
    limit = NA_integer_,
    dry_run = FALSE,
    refresh_schedule_only = FALSE,
    skip_schedule_update = FALSE,
    no_interpretation = FALSE,
    skip_interpretation = FALSE,
    skip_editorial_rewrite = FALSE,
    help = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "-h")) {
      out$help <- TRUE
    } else if (arg == "--dry-run") {
      out$dry_run <- TRUE
    } else if (arg == "--refresh-schedule-only") {
      out$refresh_schedule_only <- TRUE
    } else if (arg == "--skip-schedule-update") {
      out$skip_schedule_update <- TRUE
    } else if (arg == "--no-interpretation") {
      out$no_interpretation <- TRUE
    } else if (arg == "--skip-interpretation") {
      out$skip_interpretation <- TRUE
    } else if (arg == "--skip-editorial-rewrite") {
      out$skip_editorial_rewrite <- TRUE
    } else if (arg %in% c(
      "--config",
      "--sheet",
      "--google-email",
      "--google-auth-cache",
      "--schedule-tab",
      "--talents-tab",
      "--as-of-date",
      "--report-id",
      "--talent",
      "--window-days",
      "--bundle-a-runner",
      "--bundle-e-runner",
      "--bundle-f-runner",
      "--input-source",
      "--datalake-root",
      "--staging-root",
      "--limit"
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
  if (!nzchar(out$google_email)) {
    out$google_email <- gdrive_talent_config_value(config, "google_email", "")
  }
  if (!nzchar(out$google_auth_cache)) {
    out$google_auth_cache <- gdrive_talent_config_value(config, "google_auth_cache", "")
  }
  if (!nzchar(out$datalake_root)) {
    out$datalake_root <- gdrive_talent_config_value(config, "datalake_root", "")
  }

  out$as_of_date <- as.Date(out$as_of_date)
  if (is.na(out$as_of_date)) {
    stop("--as-of-date must be YYYY-MM-DD.", call. = FALSE)
  }

  out$limit <- suppressWarnings(as.integer(out$limit))
  if (!is.na(out$limit) && out$limit <= 0L) {
    stop("--limit must be a positive integer.", call. = FALSE)
  }

  out$report_id <- gdrive_talent_schedule_normalize_report_id(out$report_id)
  out$talent <- gdrive_talent_chr(out$talent)
  out$window_days <- gdrive_talent_window_days_key(out$window_days, blank_as_lifetime = FALSE)
  if (is.na(out$window_days)) {
    stop("--window-days must be a positive integer or 'lifetime'.", call. = FALSE)
  }

  out$input_source <- tolower(out$input_source)
  if (!out$input_source %in% c("staging", "datalake")) {
    stop("--input-source must be either staging or datalake.", call. = FALSE)
  }

  if (nzchar(out$google_auth_cache)) {
    out$google_auth_cache <- path.expand(out$google_auth_cache)
  }

  if (!grepl("^/", out$bundle_a_runner)) {
    out$bundle_a_runner <- file.path(repo_root, out$bundle_a_runner)
  }
  out$bundle_a_runner <- normalizePath(out$bundle_a_runner, winslash = "/", mustWork = FALSE)
  if (!grepl("^/", out$bundle_e_runner)) {
    out$bundle_e_runner <- file.path(repo_root, out$bundle_e_runner)
  }
  out$bundle_e_runner <- normalizePath(out$bundle_e_runner, winslash = "/", mustWork = FALSE)
  if (!grepl("^/", out$bundle_f_runner)) {
    out$bundle_f_runner <- file.path(repo_root, out$bundle_f_runner)
  }
  out$bundle_f_runner <- normalizePath(out$bundle_f_runner, winslash = "/", mustWork = FALSE)

  out
}

sheet_key <- function(x) {
  gdrive_talent_sheet_key(gdrive_talent_chr(x))
}

read_sheet_chars <- function(sheet_id, worksheet) {
  gdrive_talent_assert_packages("googlesheets4")
  out <- googlesheets4::read_sheet(
    ss = sheet_id,
    sheet = worksheet,
    col_types = "c"
  )
  as.data.frame(out, stringsAsFactors = FALSE)
}

build_due_rows <- function(schedule, talents, args) {
  schedule <- gdrive_talent_schedule_normalize_existing(schedule)
  if (nrow(schedule) == 0) {
    return(schedule[FALSE, , drop = FALSE])
  }

  talent_lookup <- gdrive_talent_schedule_talent_lookup(talents)
  schedule$report_id <- gdrive_talent_schedule_normalize_report_id(
    schedule$report_id,
    schedule$report_name
  )
  schedule$next_run_date <- gdrive_talent_schedule_date(schedule$next_run)
  schedule$status_key <- tolower(gdrive_talent_chr(schedule$schedule_status))
  schedule$window_days_key <- gdrive_talent_window_days_key(schedule$window_days)

  due <- schedule$active &
    nzchar(schedule$report_id) &
    !is.na(schedule$window_days_key) &
    (
      (!is.na(schedule$next_run_date) & schedule$next_run_date <= args$as_of_date) |
        schedule$status_key %in% c("due today", "overdue", "never run")
    )

  if (nzchar(args$report_id)) {
    due <- due & schedule$report_id == args$report_id
  }
  if (nzchar(args$window_days)) {
    due <- due & schedule$window_days_key == args$window_days
  }

  out <- schedule[due, , drop = FALSE]
  if (nrow(out) == 0) {
    return(out)
  }

  out$talent_key <- sheet_key(out$canonical_talent_name)
  out$datalake_folder_name <- ""

  for (i in seq_len(nrow(out))) {
    hit <- which(talent_lookup$key == out$talent_key[[i]])
    if (length(hit) > 0) {
      out$datalake_folder_name[[i]] <- talent_lookup$datalake_folder_name[[hit[[1]]]]
    }
  }

  if (nzchar(args$talent)) {
    target_key <- sheet_key(args$talent)
    keep <- out$talent_key == target_key | sheet_key(out$datalake_folder_name) == target_key
    out <- out[keep, , drop = FALSE]
  }

  out
}

identical_or <- function(x, value) {
  !is.na(x) & x == value
}

quote_command <- function(cmd, args) {
  paste(shQuote(c(cmd, args)), collapse = " ")
}

runner_for_report <- function(report_id, args) {
  switch(
    report_id,
    bundle_a = args$bundle_a_runner,
    bundle_e = args$bundle_e_runner,
    bundle_f = args$bundle_f_runner,
    ""
  )
}

run_one <- function(row, args) {
  report_id <- row$report_id[[1]]
  runner <- runner_for_report(report_id, args)
  if (!nzchar(runner)) {
    cat(
      "\n[schedule] Skipping unsupported report_id: ",
      report_id,
      " for ",
      row$canonical_talent_name[[1]],
      "\n",
      sep = ""
    )
    return(NA_integer_)
  }
  if (!file.exists(runner)) {
    stop("Missing runner for ", report_id, ": ", runner, call. = FALSE)
  }

  talent <- row$datalake_folder_name[[1]]
  if (!nzchar(talent)) {
    stop(
      "Could not map schedule talent to a datalake folder: ",
      row$canonical_talent_name[[1]],
      call. = FALSE
    )
  }

  runner_args <- c("--talent", talent)
  window_days_key <- gdrive_talent_window_days_key(row$window_days[[1]])
  if (is.na(window_days_key)) {
    stop("Unsupported window_days value: ", row$window_days[[1]], call. = FALSE)
  }
  if (!identical(window_days_key, "lifetime")) {
    runner_args <- c(runner_args, "--window-days", window_days_key)
  }
  runner_args <- c(runner_args, "--input-source", args$input_source)
  if (nzchar(args$datalake_root)) {
    runner_args <- c(runner_args, "--datalake-root", args$datalake_root)
  }
  if (nzchar(args$staging_root)) {
    runner_args <- c(runner_args, "--staging-root", args$staging_root)
  }
  runner_args <- c(runner_args, schedule_report_params_args(report_id, row$report_params[[1]]))

  skip_interpretation <- isTRUE(args$skip_interpretation) || isTRUE(args$no_interpretation)
  skip_editorial <- isTRUE(args$skip_editorial_rewrite) || isTRUE(args$no_interpretation)
  if (skip_interpretation) {
    runner_args <- c(runner_args, "--skip-interpretation")
  }
  if (skip_editorial) {
    runner_args <- c(runner_args, "--skip-editorial-rewrite")
  }
  if (isTRUE(args$no_interpretation)) {
    runner_args <- c(runner_args, "--no-interpretations")
  }

  cat("\n[schedule] ", report_id, ": ", row$canonical_talent_name[[1]], " -> ", talent, "\n", sep = "")
  window_label <- if (nzchar(row$window_days[[1]])) row$window_days[[1]] else "lifetime"
  cat("[schedule] next_run=", row$next_run[[1]], " window_days=", window_label, "\n", sep = "")
  cat("[schedule] Command: ", quote_command(runner, runner_args), "\n", sep = "")

  if (isTRUE(args$dry_run)) {
    return(0L)
  }

  status <- system2(runner, args = shQuote(runner_args))
  if (is.null(status)) 0L else as.integer(status)
}

schedule_report_params_args <- function(report_id, report_params) {
  report_params <- gdrive_talent_chr(report_params)
  if (!nzchar(report_params)) {
    return(character())
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to parse talent_report_schedule.report_params.", call. = FALSE)
  }

  params <- tryCatch(
    jsonlite::fromJSON(report_params, simplifyVector = FALSE),
    error = function(e) {
      stop("Invalid JSON in report_params for ", report_id, ": ", e$message, call. = FALSE)
    }
  )
  if (!is.list(params)) {
    stop("report_params must be a JSON object for ", report_id, ".", call. = FALSE)
  }

  if (!identical(report_id, "bundle_f")) {
    return(character())
  }

  allowed <- c("content_type", "content_types", "reference_day")
  unknown <- setdiff(names(params), allowed)
  if (length(unknown) > 0) {
    stop(
      "Unsupported Bundle F report_params key(s): ",
      paste(unknown, collapse = ", "),
      ". Allowed keys: ",
      paste(allowed, collapse = ", "),
      call. = FALSE
    )
  }

  out <- character()
  if (!is.null(params$content_type) && nzchar(gdrive_talent_chr(params$content_type))) {
    out <- c(out, "--content-type", gdrive_talent_chr(params$content_type))
  }
  if (!is.null(params$content_types)) {
    content_types <- unlist(params$content_types, use.names = FALSE)
    content_types <- gdrive_talent_chr(content_types)
    content_types <- content_types[nzchar(content_types)]
    if (length(content_types) > 0) {
      out <- c(out, "--content-types", paste(content_types, collapse = ","))
    }
  }
  if (!is.null(params$reference_day) && nzchar(gdrive_talent_chr(params$reference_day))) {
    out <- c(out, "--reference-day", gdrive_talent_chr(params$reference_day))
  }

  out
}

refresh_schedule <- function(args) {
  cat("\n[schedule] Refreshing computed schedule columns after report run.\n")
  gdrive_talent_write_report_schedule_sheet(
    sheet_id = args$sheet,
    worksheet = args$schedule_tab,
    repo_root = repo_root,
    datalake_root = args$datalake_root,
    preserve_manual_columns = TRUE,
    apply_validation = FALSE,
    refresh_dropdown_named_ranges = FALSE,
    auth = FALSE
  )
  invisible(TRUE)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(args$help)) {
  usage()
  quit(status = 0)
}

if (!nzchar(args$sheet)) {
  stop("Missing --sheet or permissions_sheet in config.", call. = FALSE)
}
if (!file.exists(args$bundle_a_runner)) {
  stop("Missing Bundle A runner: ", args$bundle_a_runner, call. = FALSE)
}
if (!file.exists(args$bundle_e_runner)) {
  stop("Missing Bundle E runner: ", args$bundle_e_runner, call. = FALSE)
}
if (!file.exists(args$bundle_f_runner)) {
  stop("Missing Bundle F runner: ", args$bundle_f_runner, call. = FALSE)
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

cat("Reading report schedule from: ", args$sheet, "\n", sep = "")
cat("As-of date: ", as.character(args$as_of_date), "\n", sep = "")

if (isTRUE(args$refresh_schedule_only)) {
  refresh_schedule(args)
  cat("\nSchedule refresh complete.\n")
  quit(status = 0)
}

schedule <- read_sheet_chars(args$sheet, args$schedule_tab)
talents <- read_sheet_chars(args$sheet, args$talents_tab)
due_rows <- build_due_rows(schedule, talents, args)

if (!is.na(args$limit) && nrow(due_rows) > args$limit) {
  due_rows <- due_rows[seq_len(args$limit), , drop = FALSE]
}

cat("Due report rows: ", nrow(due_rows), "\n", sep = "")
if (nrow(due_rows) == 0) {
  quit(status = 0)
}

status <- 0L
success_count <- 0L
for (i in seq_len(nrow(due_rows))) {
  rc <- tryCatch(
    run_one(due_rows[i, , drop = FALSE], args),
    error = function(e) {
      message("[schedule] Error: ", e$message)
      1L
    }
  )
  if (is.na(rc)) {
    next
  } else if (!identical(rc, 0L)) {
    status <- 1L
  } else {
    success_count <- success_count + 1L
  }
}

if (!isTRUE(args$dry_run) && !isTRUE(args$skip_schedule_update) && success_count > 0L) {
  tryCatch(
    refresh_schedule(args),
    error = function(e) {
      message("[schedule] Error refreshing schedule columns: ", e$message)
      status <<- 1L
    }
  )
}

if (status == 0L) {
  cat("\nScheduled report run complete.\n")
} else {
  cat("\nScheduled report run completed with failures.\n")
}

quit(status = status)
