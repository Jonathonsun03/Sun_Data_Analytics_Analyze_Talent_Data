gdrive_talent_schedule_empty <- function() {
  gdrive_talent_empty_df(c(
    "active",
    "canonical_talent_name",
    "report_id",
    "report_name",
    "cadence_days",
    "window_days",
    "last_run",
    "next_run",
    "schedule_status",
    "latest_report_path",
    "latest_report_file",
    "run_source",
    "updated_at",
    "notes"
  ))
}

gdrive_talent_schedule_column <- function(x, candidates, default = "") {
  hit <- intersect(candidates, names(x))
  if (length(hit) == 0) {
    return(rep(default, nrow(x)))
  }

  gdrive_talent_chr(x[[hit[[1]]]])
}

gdrive_talent_schedule_number <- function(x, candidates, default = NA_real_) {
  raw <- gdrive_talent_schedule_column(x, candidates, default = "")
  out <- suppressWarnings(as.numeric(raw))
  out[is.na(out) & nzchar(raw)] <- default
  out
}

gdrive_talent_schedule_date <- function(x) {
  x <- gdrive_talent_chr(x)
  parsed <- suppressWarnings(as.Date(x))
  missing <- is.na(parsed) & nzchar(x)
  if (any(missing)) {
    parsed[missing] <- suppressWarnings(as.Date(as.POSIXct(x[missing], tz = Sys.timezone())))
  }
  parsed
}

gdrive_talent_schedule_normalize_report_id <- function(report_id, report_name = "") {
  report_id <- gdrive_talent_chr(report_id)
  report_name <- gdrive_talent_chr(report_name)

  out <- ifelse(nzchar(report_id), report_id, report_name)
  out <- tolower(out)
  out <- gsub("^bundle\\s+", "bundle_", out)
  out <- gsub("[^a-z0-9]+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  out
}

gdrive_talent_schedule_normalize_existing <- function(existing) {
  if (is.null(existing) || nrow(existing) == 0) {
    return(gdrive_talent_schedule_empty())
  }

  existing <- as.data.frame(existing, stringsAsFactors = FALSE)

  canonical_talent_name <- gdrive_talent_schedule_column(
    existing,
    c("canonical_talent_name", "talent_name", "talent", "Talent", "Talent Name")
  )
  report_id <- gdrive_talent_schedule_column(
    existing,
    c("report_id", "report", "bundle", "Report", "Bundle")
  )
  report_name <- gdrive_talent_schedule_column(
    existing,
    c("report_name", "report_display_name", "Report Name")
  )
  report_id <- gdrive_talent_schedule_normalize_report_id(report_id, report_name)

  out <- data.frame(
    active = gdrive_talent_bool(gdrive_talent_schedule_column(existing, c("active", "Active"), default = "TRUE")),
    canonical_talent_name = canonical_talent_name,
    report_id = report_id,
    report_name = ifelse(nzchar(report_name), report_name, gdrive_talent_report_title(report_id)),
    cadence_days = gdrive_talent_schedule_number(
      existing,
      c("cadence_days", "report_cadence_days", "frequency_days", "Frequency Days"),
      default = NA_real_
    ),
    window_days = gdrive_talent_schedule_number(
      existing,
      c("window_days", "report_window_days", "Window Days"),
      default = NA_real_
    ),
    last_run = gdrive_talent_schedule_column(existing, c("last_run", "Last Run")),
    next_run = gdrive_talent_schedule_column(existing, c("next_run", "Next Run")),
    schedule_status = gdrive_talent_schedule_column(existing, c("schedule_status", "Schedule Status")),
    latest_report_path = gdrive_talent_schedule_column(existing, c("latest_report_path", "Latest Report Path")),
    latest_report_file = gdrive_talent_schedule_column(existing, c("latest_report_file", "Latest Report File")),
    run_source = gdrive_talent_schedule_column(existing, c("run_source", "Run Source")),
    updated_at = gdrive_talent_schedule_column(existing, c("updated_at", "Updated At")),
    notes = gdrive_talent_schedule_column(existing, c("notes", "Notes")),
    stringsAsFactors = FALSE
  )

  keep <- nzchar(out$canonical_talent_name) | nzchar(out$report_id) | nzchar(out$notes)
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

gdrive_talent_schedule_talent_lookup <- function(talents_table) {
  talents_table <- as.data.frame(talents_table, stringsAsFactors = FALSE)
  keys <- unique(c(
    gdrive_talent_sheet_key(talents_table$canonical_talent_name),
    gdrive_talent_sheet_key(talents_table$datalake_folder_name),
    gdrive_talent_sheet_key(talents_table$talent_slug)
  ))

  rows <- vector("list", 0)
  idx <- 0L
  for (i in seq_len(nrow(talents_table))) {
    row_keys <- unique(c(
      gdrive_talent_sheet_key(talents_table$canonical_talent_name[[i]]),
      gdrive_talent_sheet_key(talents_table$datalake_folder_name[[i]]),
      gdrive_talent_sheet_key(talents_table$talent_slug[[i]])
    ))
    row_keys <- row_keys[nzchar(row_keys)]
    for (key in row_keys) {
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        key = key,
        canonical_talent_name = talents_table$canonical_talent_name[[i]],
        datalake_folder_name = talents_table$datalake_folder_name[[i]],
        datalake_path = talents_table$datalake_path[[i]],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(gdrive_talent_empty_df(c("key", "canonical_talent_name", "datalake_folder_name", "datalake_path")))
  }

  out <- do.call(rbind, rows)
  out <- out[!duplicated(out$key), , drop = FALSE]
  rownames(out) <- NULL
  out
}

gdrive_talent_schedule_report_lookup <- function(reports_table) {
  reports_table <- as.data.frame(reports_table, stringsAsFactors = FALSE)
  reports_table$key <- gdrive_talent_schedule_normalize_report_id(
    reports_table$report_id,
    reports_table$report_name
  )
  reports_table[!duplicated(reports_table$key), , drop = FALSE]
}

gdrive_talent_find_latest_scheduled_report <- function(
  talent_path,
  report_id,
  window_days = NA_real_
) {
  if (!nzchar(talent_path) || !dir.exists(talent_path) || !nzchar(report_id)) {
    return(list(
      last_run = as.Date(NA),
      latest_report_path = "",
      latest_report_file = "",
      run_source = "not_found"
    ))
  }

  suffix <- sub("^bundle_", "", report_id)
  report_dirs <- c(
    file.path(talent_path, "reports", paste0("bundle_", suffix)),
    file.path(talent_path, "reports", paste0("bundle_", toupper(suffix)))
  )
  files <- unique(unlist(lapply(report_dirs, function(dir) {
    Sys.glob(file.path(dir, "*"))
  }), use.names = FALSE))
  files <- files[file.exists(files)]
  files <- files[grepl("\\.(html?|pdf|docx)$", files, ignore.case = TRUE)]

  if (length(files) == 0) {
    return(list(
      last_run = as.Date(NA),
      latest_report_path = "",
      latest_report_file = "",
      run_source = "not_found"
    ))
  }

  if (!is.na(window_days) && window_days > 0) {
    window_pattern <- paste0("window_", as.integer(window_days), "d")
    window_files <- files[grepl(window_pattern, basename(files), ignore.case = TRUE)]
    if (length(window_files) > 0) {
      files <- window_files
    }
  }

  info <- file.info(files)
  latest_idx <- which.max(info$mtime)
  latest_path <- normalizePath(files[[latest_idx]], winslash = "/", mustWork = FALSE)

  list(
    last_run = as.Date(info$mtime[[latest_idx]]),
    latest_report_path = latest_path,
    latest_report_file = basename(latest_path),
    run_source = "datalake_rendered_report_mtime"
  )
}

gdrive_talent_schedule_status <- function(active, last_run, next_run, today = Sys.Date()) {
  if (!isTRUE(active)) {
    return("inactive")
  }
  if (is.na(last_run)) {
    return("never run")
  }
  if (is.na(next_run)) {
    return("scheduled")
  }
  if (next_run < today) {
    return("overdue")
  }
  if (next_run == today) {
    return("due today")
  }
  "scheduled"
}

gdrive_talent_build_report_schedule_table <- function(
  existing_schedule,
  repo_root = gdrive_talent_repo_root(),
  datalake_root = NULL,
  default_cadence_days = 90,
  today = Sys.Date(),
  sort_rows = FALSE
) {
  gdrive_talent_source_repo_helpers(repo_root)
  if (is.null(datalake_root) || !nzchar(datalake_root)) {
    datalake_root <- get_datalake_root()
  }

  schedule <- gdrive_talent_schedule_normalize_existing(existing_schedule)
  if (nrow(schedule) == 0) {
    return(gdrive_talent_schedule_empty())
  }

  talents <- gdrive_talent_build_talents_table(
    datalake_root = datalake_root,
    repo_root = repo_root
  )
  reports <- gdrive_talent_build_reports_table(
    repo_root = repo_root,
    datalake_root = datalake_root,
    include_render_counts = FALSE
  )
  talent_lookup <- gdrive_talent_schedule_talent_lookup(talents)
  report_lookup <- gdrive_talent_schedule_report_lookup(reports)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")

  rows <- vector("list", nrow(schedule))
  for (i in seq_len(nrow(schedule))) {
    row <- schedule[i, , drop = FALSE]
    talent_key <- gdrive_talent_sheet_key(row$canonical_talent_name[[1]])
    talent_idx <- which(talent_lookup$key == talent_key)
    talent <- if (length(talent_idx) > 0) talent_lookup[talent_idx[[1]], , drop = FALSE] else NULL

    report_key <- gdrive_talent_schedule_normalize_report_id(row$report_id[[1]], row$report_name[[1]])
    report_idx <- which(report_lookup$key == report_key)
    report <- if (length(report_idx) > 0) report_lookup[report_idx[[1]], , drop = FALSE] else NULL

    cadence_days <- row$cadence_days[[1]]
    if (is.na(cadence_days) || cadence_days <= 0) {
      cadence_days <- default_cadence_days
    }

    window_days <- row$window_days[[1]]
    latest <- gdrive_talent_find_latest_scheduled_report(
      talent_path = if (is.null(talent)) "" else talent$datalake_path[[1]],
      report_id = report_key,
      window_days = window_days
    )

    manual_last_run <- gdrive_talent_schedule_date(row$last_run[[1]])
    last_run <- latest$last_run
    run_source <- latest$run_source
    if (is.na(last_run) && !is.na(manual_last_run)) {
      last_run <- manual_last_run
      run_source <- "manual_last_run"
    }

    next_run <- if (is.na(last_run)) as.Date(NA) else last_run + as.integer(cadence_days)
    status <- gdrive_talent_schedule_status(row$active[[1]], last_run, next_run, today = today)

    rows[[i]] <- data.frame(
      active = row$active[[1]],
      canonical_talent_name = if (is.null(talent)) row$canonical_talent_name[[1]] else talent$canonical_talent_name[[1]],
      report_id = report_key,
      report_name = if (is.null(report)) row$report_name[[1]] else report$report_name[[1]],
      cadence_days = cadence_days,
      window_days = if (is.na(window_days)) NA_real_ else window_days,
      last_run = if (is.na(last_run)) "" else as.character(last_run),
      next_run = if (is.na(next_run)) "" else as.character(next_run),
      schedule_status = status,
      latest_report_path = latest$latest_report_path,
      latest_report_file = latest$latest_report_file,
      run_source = run_source,
      updated_at = now,
      notes = row$notes[[1]],
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  if (isTRUE(sort_rows)) {
    out <- out[order(!out$active, out$next_run, out$canonical_talent_name, out$report_id), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

gdrive_talent_schedule_computed_columns <- function() {
  c(
    "last_run",
    "next_run",
    "schedule_status",
    "latest_report_path",
    "latest_report_file",
    "run_source",
    "updated_at",
    "notes"
  )
}

gdrive_talent_apply_schedule_validation <- function(
  sheet_id,
  worksheet = "talent_report_schedule",
  max_rows = 1000,
  talent_named_range = "Canonical_talent_name",
  report_named_range = "Report_id"
) {
  gdrive_talent_assert_packages("googlesheets4")

  normalized_sheet_id <- gdrive_talent_normalize_sheet_id(sheet_id)
  spreadsheet <- googlesheets4::gs4_get(normalized_sheet_id)
  worksheet_id <- gdrive_talent_sheet_id_for_name(spreadsheet$sheets, worksheet)

  data_range <- function(start_col, end_col) {
    list(
      sheetId = worksheet_id,
      startRowIndex = 1L,
      endRowIndex = as.integer(max_rows),
      startColumnIndex = as.integer(start_col),
      endColumnIndex = as.integer(end_col)
    )
  }

  dropdown_rule <- function(named_range) {
    list(
      condition = list(
        type = "ONE_OF_RANGE",
        values = list(list(userEnteredValue = paste0("=", named_range)))
      ),
      strict = TRUE,
      showCustomUi = TRUE
    )
  }

  requests <- list(
    list(
      setDataValidation = list(
        range = data_range(0L, 1L),
        rule = list(
          condition = list(type = "BOOLEAN"),
          strict = TRUE,
          showCustomUi = TRUE
        )
      )
    ),
    list(
      setDataValidation = list(
        range = data_range(1L, 2L),
        rule = dropdown_rule(talent_named_range)
      )
    ),
    list(
      setDataValidation = list(
        range = data_range(2L, 3L),
        rule = dropdown_rule(report_named_range)
      )
    )
  )

  req <- googlesheets4:::request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = googlesheets4::as_sheets_id(normalized_sheet_id),
      requests = requests
    )
  )
  resp_raw <- googlesheets4:::request_make(req)
  gargle::response_process(resp_raw)

  invisible(normalized_sheet_id)
}

gdrive_talent_refresh_schedule_named_ranges <- function(
  sheet_id,
  repo_root = gdrive_talent_repo_root(),
  datalake_root = NULL,
  talents_worksheet = "talents",
  reports_worksheet = "reports"
) {
  normalized_sheet_id <- gdrive_talent_normalize_sheet_id(sheet_id)

  tryCatch({
    talents <- gdrive_talent_build_talents_table(
      datalake_root = datalake_root,
      repo_root = repo_root
    )
    gdrive_talent_upsert_named_range(
      sheet_id = normalized_sheet_id,
      worksheet = talents_worksheet,
      named_range = "Canonical_talent_name",
      column_name = "canonical_talent_name",
      data = talents,
      include_header = FALSE
    )
  }, error = function(e) {
    warning("Could not refresh Canonical_talent_name named range: ", e$message, call. = FALSE)
  })

  tryCatch({
    reports <- gdrive_talent_build_reports_table(
      repo_root = repo_root,
      datalake_root = datalake_root,
      include_render_counts = FALSE
    )
    gdrive_talent_upsert_named_range(
      sheet_id = normalized_sheet_id,
      worksheet = reports_worksheet,
      named_range = "Report_id",
      column_name = "report_id",
      data = reports,
      include_header = FALSE
    )
  }, error = function(e) {
    warning("Could not refresh Report_id named range: ", e$message, call. = FALSE)
  })

  invisible(normalized_sheet_id)
}

gdrive_talent_write_report_schedule_sheet <- function(
  sheet_id,
  worksheet = "talent_report_schedule",
  repo_root = gdrive_talent_repo_root(),
  datalake_root = NULL,
  default_cadence_days = 90,
  computed_range_start = "G1",
  preserve_manual_columns = TRUE,
  apply_validation = TRUE,
  refresh_dropdown_named_ranges = TRUE,
  validation_rows = 1000,
  auth = FALSE,
  google_email = Sys.getenv("TALENT_GDRIVE_EMAIL", unset = ""),
  auth_cache = Sys.getenv("TALENT_GDRIVE_AUTH_CACHE", unset = "~/.cache/gargle")
) {
  gdrive_talent_assert_packages("googlesheets4")
  gdrive_talent_source_repo_helpers(repo_root)

  if (isTRUE(auth)) {
    gdrive_talent_auth_sheets(
      google_email = google_email,
      auth_cache = auth_cache,
      repo_root = repo_root
    )
  }

  normalized_sheet_id <- gdrive_talent_normalize_sheet_id(sheet_id)
  existing <- tryCatch(
    googlesheets4::read_sheet(
      ss = normalized_sheet_id,
      sheet = worksheet,
      col_types = "c"
    ),
    error = function(e) NULL
  )

  table <- gdrive_talent_build_report_schedule_table(
    existing_schedule = existing,
    repo_root = repo_root,
    datalake_root = datalake_root,
    default_cadence_days = default_cadence_days,
    sort_rows = !isTRUE(preserve_manual_columns)
  )

  if (isTRUE(preserve_manual_columns) && !is.null(existing) && nrow(existing) > 0) {
    computed_cols <- gdrive_talent_schedule_computed_columns()
    computed <- table[, computed_cols, drop = FALSE]

    googlesheets4::range_clear(
      ss = normalized_sheet_id,
      sheet = worksheet,
      range = "G:N",
      reformat = FALSE
    )
    googlesheets4::range_write(
      ss = normalized_sheet_id,
      data = computed,
      sheet = worksheet,
      range = computed_range_start,
      col_names = TRUE,
      reformat = FALSE
    )
  } else {
    googlesheets4::sheet_write(
      data = table,
      ss = normalized_sheet_id,
      sheet = worksheet
    )
  }

  if (isTRUE(apply_validation)) {
    if (isTRUE(refresh_dropdown_named_ranges)) {
      gdrive_talent_refresh_schedule_named_ranges(
        sheet_id = normalized_sheet_id,
        repo_root = repo_root,
        datalake_root = datalake_root
      )
    }
    gdrive_talent_apply_schedule_validation(
      sheet_id = normalized_sheet_id,
      worksheet = worksheet,
      max_rows = validation_rows
    )
  }

  invisible(table)
}
