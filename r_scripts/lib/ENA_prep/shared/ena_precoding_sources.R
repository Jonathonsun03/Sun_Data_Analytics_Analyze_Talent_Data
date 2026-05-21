if (!exists("ena_precoding_talent_data_root", mode = "function")) {
  source(file.path("r_scripts", "lib", "ENA_prep", "shared", "ena_precoding_paths.R"))
}
if (!exists("select_talent", mode = "function") ||
    !exists("list_talents", mode = "function") ||
    !exists("talent_slugify", mode = "function")) {
  ena_precoding_source_repo_file("r_scripts", "lib", "utils", "staging_root.R")
  ena_precoding_source_repo_file("r_scripts", "lib", "utils", "talent_select.R")
}

ENA_PRECODING_EXPECTED_REPLAY_COLUMNS <- c(
  "video_id",
  "sec",
  "source",
  "speaker",
  "text",
  "message_type",
  "paid_amount_text",
  "paid_amount_value",
  "paid_currency",
  "timecode",
  "replay_line"
)

ena_precoding_extract_video_id <- function(path) {
  base <- basename(path)
  m <- regexpr("([A-Za-z0-9_-]{11})(?:\\.csv)?$", base, perl = TRUE)
  if (m[[1]] > 0) {
    out <- regmatches(base, m)
    return(sub("\\.csv$", "", out))
  }
  ""
}

ena_precoding_count_csv_rows <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  con <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  n <- 0L
  repeat {
    chunk <- readLines(con, n = 10000L, warn = FALSE)
    if (length(chunk) == 0L) break
    n <- n + length(chunk)
  }
  max(0L, n - 1L)
}

ena_precoding_read_csv_header <- function(path) {
  tryCatch(
    names(utils::read.csv(path, nrows = 0, check.names = FALSE)),
    error = function(e) character()
  )
}

ena_precoding_load_talent_id_lookup <- function(talent_data_root = NULL) {
  db_path <- ena_precoding_classifications_db_path(talent_data_root)
  empty <- data.frame(
    talent_name = character(),
    talent_slug = character(),
    talent_id = character(),
    stringsAsFactors = FALSE
  )
  if (!file.exists(db_path) ||
      !requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) {
    return(empty)
  }
  con <- tryCatch(
    DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE),
    error = function(e) NULL
  )
  if (is.null(con)) return(empty)
  on.exit(tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL), add = TRUE)
  if (!DBI::dbExistsTable(con, "talents")) return(empty)
  out <- tryCatch(
    DBI::dbGetQuery(con, "SELECT talent_name, talent_slug, talent_id FROM talents"),
    error = function(e) empty
  )
  as.data.frame(out, stringsAsFactors = FALSE)
}

ena_precoding_resolve_talent_id <- function(talent_name, lookup) {
  slug <- talent_slugify(talent_name)
  if (is.data.frame(lookup) && nrow(lookup) > 0) {
    exact <- lookup[lookup$talent_name == talent_name, , drop = FALSE]
    if (nrow(exact) >= 1) return(exact$talent_id[[1]])
    by_slug <- lookup[lookup$talent_slug == slug, , drop = FALSE]
    if (nrow(by_slug) >= 1) return(by_slug$talent_id[[1]])
  }
  paste0("talent_", slug)
}

ena_precoding_discover_sources <- function(talent_query = "all", text_playback_root = NULL) {
  talent_root <- ena_precoding_talent_data_root(text_playback_root)
  selected_paths <- as.character(select_talent(talent_query, root = talent_root))
  selected_paths <- selected_paths[dir.exists(selected_paths)]
  lookup <- ena_precoding_load_talent_id_lookup(talent_root)

  rows <- lapply(selected_paths, function(talent_path) {
    talent_name <- safe_basename(talent_path)
    talent_slug <- talent_slugify(talent_name)
    talent_id <- ena_precoding_resolve_talent_id(talent_name, lookup)
    text_dir <- file.path(talent_path, "text_playback")
    files <- if (dir.exists(text_dir)) {
      list.files(text_dir, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
    } else {
      character()
    }
    if (length(files) == 0L) {
      return(data.frame(
        talent_name = talent_name,
        talent_id = talent_id,
        talent_slug = talent_slug,
        video_id = NA_character_,
        source_path = NA_character_,
        file_name = NA_character_,
        file_size_bytes = NA_real_,
        modified_time = as.POSIXct(NA),
        row_count = NA_integer_,
        columns_present = "",
        columns_missing = paste(ENA_PRECODING_EXPECTED_REPLAY_COLUMNS, collapse = "|"),
        source_status = "missing_text_playback_files",
        stringsAsFactors = FALSE
      ))
    }
    do.call(rbind, lapply(files, function(path) {
      cols <- ena_precoding_read_csv_header(path)
      missing <- setdiff(ENA_PRECODING_EXPECTED_REPLAY_COLUMNS, cols)
      info <- file.info(path)
      data.frame(
        talent_name = talent_name,
        talent_id = talent_id,
        talent_slug = talent_slug,
        video_id = ena_precoding_extract_video_id(path),
        source_path = normalizePath(path, winslash = "/", mustWork = FALSE),
        file_name = basename(path),
        file_size_bytes = as.numeric(info$size),
        modified_time = as.POSIXct(info$mtime, tz = "UTC"),
        row_count = ena_precoding_count_csv_rows(path),
        columns_present = paste(cols, collapse = "|"),
        columns_missing = paste(missing, collapse = "|"),
        source_status = if (length(missing) == 0L) "ok" else "missing_expected_columns",
        stringsAsFactors = FALSE
      )
    }))
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

ena_precoding_source_validation <- function(source_inventory) {
  if (!is.data.frame(source_inventory) || nrow(source_inventory) == 0L) {
    return(data.frame(
      talent_name = character(),
      source_path = character(),
      check = character(),
      status = character(),
      detail = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(seq_len(nrow(source_inventory)), function(i) {
    row <- source_inventory[i, , drop = FALSE]
    data.frame(
      talent_name = row$talent_name,
      source_path = row$source_path,
      check = "expected_text_playback_columns",
      status = if (identical(row$source_status, "ok")) "ok" else "warning",
      detail = if (nzchar(row$columns_missing)) row$columns_missing else "all expected columns present",
      stringsAsFactors = FALSE
    )
  }))
}

