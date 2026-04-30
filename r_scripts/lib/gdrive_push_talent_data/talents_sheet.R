gdrive_talent_repo_root <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)

  repeat {
    if (file.exists(file.path(cur, "Sun_Data_Analytics_Analyze_Talent_Data.Rproj"))) {
      return(cur)
    }

    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }

  normalizePath(start, winslash = "/", mustWork = FALSE)
}

gdrive_talent_source_repo_helpers <- function(repo_root = gdrive_talent_repo_root()) {
  helper_paths <- c(
    file.path(repo_root, "r_scripts", "lib", "utils", "datalake_root.r"),
    file.path(repo_root, "r_scripts", "lib", "utils", "staging_root.R"),
    file.path(repo_root, "r_scripts", "lib", "utils", "talent_select.R"),
    file.path(repo_root, "r_scripts", "lib", "duckdb", "db_connect.R"),
    file.path(repo_root, "r_scripts", "lib", "stream_classification", "talent_rules.R")
  )

  for (path in helper_paths[file.exists(helper_paths)]) {
    source(path)
  }

  invisible(helper_paths)
}

gdrive_talent_sheet_key <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("(?i)<\\s*u\\+[0-9a-f]{4,6}\\s*>", " ", x, perl = TRUE)
  x <- gsub("(?i)<\\s*[0-9a-f]{2}\\s*>", " ", x, perl = TRUE)
  x <- gsub("[\\[\\]【】<>]", " ", x)

  ascii <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  fallback <- iconv(x, to = "ASCII//TRANSLIT", sub = " ")
  x <- ifelse(is.na(ascii), fallback, ascii)
  x <- ifelse(is.na(x), "", x)

  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

gdrive_talent_read_duckdb_talents <- function(datalake_root, db_path = NULL) {
  if (!exists("duckdb_connect", mode = "function")) {
    stop("duckdb_connect() is not loaded.", call. = FALSE)
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.", call. = FALSE)
  }

  con <- duckdb_connect(
    datalake_root = datalake_root,
    db_path = db_path,
    read_only = TRUE
  )
  on.exit(
    suppressWarnings(tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL)),
    add = TRUE
  )

  if (!"talents" %in% DBI::dbListTables(con)) {
    return(gdrive_talent_empty_df(c(
      "talent_num",
      "canonical_talent_name",
      "talent_slug",
      "talent_id",
      "talent_created_at"
    )))
  }

  out <- DBI::dbGetQuery(
    con,
    "SELECT
       talent_num,
       talent_name AS canonical_talent_name,
       talent_slug,
       talent_id,
       created_at AS talent_created_at
     FROM talents
     ORDER BY talent_num, talent_name"
  )

  as.data.frame(out, stringsAsFactors = FALSE)
}

gdrive_talent_profile_matcher_pattern <- function(profile_name, cfg) {
  matchers <- cfg$matchers
  if (is.null(matchers) || length(matchers) == 0) {
    return("")
  }

  patterns <- vapply(
    matchers,
    function(matcher) {
      profile <- matcher$profile
      pattern <- matcher$pattern
      if (is.null(profile) || is.null(pattern) || !identical(profile, profile_name)) {
        return(NA_character_)
      }
      as.character(pattern)
    },
    character(1)
  )

  paste(stats::na.omit(patterns), collapse = "; ")
}

gdrive_talent_profile_overlay <- function(profile_name, cfg) {
  profile <- cfg$profiles[[profile_name]]
  if (is.null(profile) || is.null(profile$overlay)) {
    return("")
  }
  as.character(profile$overlay)
}

gdrive_talent_build_talents_table <- function(
  datalake_root = NULL,
  db_path = NULL,
  repo_root = gdrive_talent_repo_root(),
  classification_config_path = NULL,
  include_excluded = FALSE
) {
  gdrive_talent_source_repo_helpers(repo_root)

  if (is.null(datalake_root) || !nzchar(datalake_root)) {
    datalake_root <- get_datalake_root()
  }
  if (is.null(classification_config_path) || !nzchar(classification_config_path)) {
    classification_config_path <- file.path(repo_root, "classification", "config", "talent_profiles.json")
  }

  if (!dir.exists(datalake_root)) {
    stop("DataLake talent root does not exist: ", datalake_root, call. = FALSE)
  }

  talents <- list_talents(
    root = datalake_root,
    include_excluded = include_excluded
  )
  talents <- as.data.frame(talents, stringsAsFactors = FALSE)
  names(talents)[names(talents) == "name"] <- "datalake_folder_name"
  names(talents)[names(talents) == "path"] <- "datalake_path"
  talents$datalake_key <- gdrive_talent_sheet_key(talents$datalake_folder_name)

  db_talents <- gdrive_talent_read_duckdb_talents(
    datalake_root = datalake_root,
    db_path = db_path
  )
  if (nrow(db_talents) > 0) {
    db_talents$db_key <- gdrive_talent_sheet_key(db_talents$talent_slug)
    db_talents$name_key <- gdrive_talent_sheet_key(db_talents$canonical_talent_name)
  } else {
    db_talents$db_key <- character()
    db_talents$name_key <- character()
  }

  cfg <- load_talent_profiles(classification_config_path)

  rows <- vector("list", nrow(talents))
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")

  for (i in seq_len(nrow(talents))) {
    key <- talents$datalake_key[[i]]
    db_idx <- which(db_talents$db_key == key | db_talents$name_key == key)
    db_row <- if (length(db_idx) > 0) db_talents[db_idx[[1]], , drop = FALSE] else NULL

    selected <- select_talent_profile(
      talent_name = talents$datalake_folder_name[[i]],
      cfg = cfg,
      classification_root = file.path(repo_root, "classification")
    )

    profile_name <- selected$profile_name
    profile_overlay <- gdrive_talent_profile_overlay(profile_name, cfg)

    rows[[i]] <- data.frame(
      active = TRUE,
      talent_num = if (is.null(db_row)) NA_integer_ else db_row$talent_num[[1]],
      talent_id = if (is.null(db_row)) "" else db_row$talent_id[[1]],
      talent_slug = if (is.null(db_row)) key else db_row$talent_slug[[1]],
      canonical_talent_name = if (is.null(db_row)) "" else db_row$canonical_talent_name[[1]],
      datalake_folder_name = talents$datalake_folder_name[[i]],
      datalake_path = talents$datalake_path[[i]],
      datalake_key = key,
      classification_profile = profile_name,
      classification_match_pattern = gdrive_talent_profile_matcher_pattern(profile_name, cfg),
      classification_overlay = profile_overlay,
      mapping_source = if (is.null(db_row)) "datalake_only" else "datalake_duckdb_classification_config",
      aliases = "",
      notes = "",
      updated_at = now,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$active, out$talent_num, out$datalake_folder_name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

gdrive_talent_existing_talent_key <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  talent_id <- if ("talent_id" %in% names(x)) gdrive_talent_chr(x$talent_id) else rep("", nrow(x))
  datalake_key <- if ("datalake_key" %in% names(x)) gdrive_talent_chr(x$datalake_key) else rep("", nrow(x))
  talent_slug <- if ("talent_slug" %in% names(x)) gdrive_talent_chr(x$talent_slug) else rep("", nrow(x))

  fallback <- ifelse(nzchar(datalake_key), datalake_key, gdrive_talent_sheet_key(talent_slug))
  ifelse(nzchar(talent_id), paste0("talent_id:", talent_id), paste0("key:", fallback))
}

gdrive_talent_merge_existing_talents <- function(
  table,
  existing,
  preserve_cols = c("active", "aliases", "notes")
) {
  if (is.null(existing) || nrow(existing) == 0 || length(preserve_cols) == 0) {
    return(table)
  }

  existing <- as.data.frame(existing, stringsAsFactors = FALSE)
  preserve_cols <- intersect(preserve_cols, intersect(names(table), names(existing)))
  if (length(preserve_cols) == 0) {
    return(table)
  }

  table_key <- gdrive_talent_existing_talent_key(table)
  existing_key <- gdrive_talent_existing_talent_key(existing)

  for (col in preserve_cols) {
    for (i in seq_len(nrow(table))) {
      match_idx <- which(existing_key == table_key[[i]])
      if (length(match_idx) == 0) next

      value <- existing[[col]][[match_idx[[1]]]]
      if (is.null(value) || length(value) == 0 || is.na(value)) next

      if (identical(col, "active")) {
        table[[col]][[i]] <- gdrive_talent_bool(value)
      } else if (nzchar(trimws(as.character(value)))) {
        table[[col]][[i]] <- as.character(value)
      }
    }
  }

  table
}

gdrive_talent_column_to_a1 <- function(index) {
  index <- as.integer(index)
  if (length(index) != 1 || is.na(index) || index < 1) {
    stop("Column index must be a positive integer.", call. = FALSE)
  }

  out <- character()
  while (index > 0) {
    index <- index - 1L
    out <- c(LETTERS[(index %% 26L) + 1L], out)
    index <- index %/% 26L
  }

  paste(out, collapse = "")
}

gdrive_talent_sheet_id_for_name <- function(sheet_meta, worksheet) {
  hit <- which(sheet_meta$name == worksheet)
  if (length(hit) == 0) {
    stop("Worksheet not found after write: ", worksheet, call. = FALSE)
  }

  sheet_meta$id[[hit[[1]]]]
}

gdrive_talent_existing_named_range_id <- function(spreadsheet_meta, named_range) {
  ranges <- spreadsheet_meta$named_ranges
  if (is.null(ranges) || nrow(ranges) == 0 || !"name" %in% names(ranges)) {
    return("")
  }

  hit <- which(ranges$name == named_range)
  if (length(hit) == 0) {
    return("")
  }

  ranges$id[[hit[[1]]]]
}

gdrive_talent_upsert_named_range <- function(
  sheet_id,
  worksheet,
  named_range = "Canonical_talent_name",
  column_name = "canonical_talent_name",
  data,
  include_header = FALSE
) {
  gdrive_talent_assert_packages("googlesheets4")

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  col_idx <- match(column_name, names(data))
  if (is.na(col_idx)) {
    stop("Column not found for named range: ", column_name, call. = FALSE)
  }

  normalized_sheet_id <- gdrive_talent_normalize_sheet_id(sheet_id)
  spreadsheet <- googlesheets4::gs4_get(normalized_sheet_id)
  worksheet_id <- gdrive_talent_sheet_id_for_name(spreadsheet$sheets, worksheet)
  existing_range_id <- gdrive_talent_existing_named_range_id(spreadsheet, named_range)

  start_row <- if (isTRUE(include_header)) 0L else 1L
  end_row <- max(start_row + 1L, nrow(data) + 1L)
  start_col <- as.integer(col_idx - 1L)
  end_col <- as.integer(col_idx)

  requests <- list()
  if (nzchar(existing_range_id)) {
    requests[[length(requests) + 1L]] <- list(
      deleteNamedRange = list(
        namedRangeId = existing_range_id
      )
    )
  }
  requests[[length(requests) + 1L]] <- list(
    addNamedRange = list(
      namedRange = list(
        name = named_range,
        range = list(
          sheetId = worksheet_id,
          startRowIndex = start_row,
          endRowIndex = end_row,
          startColumnIndex = start_col,
          endColumnIndex = end_col
        )
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

  first_row <- if (isTRUE(include_header)) 1L else 2L
  last_row <- max(first_row, nrow(data) + 1L)
  col_a1 <- gdrive_talent_column_to_a1(col_idx)

  invisible(list(
    name = named_range,
    range = paste0(worksheet, "!", col_a1, first_row, ":", col_a1, last_row)
  ))
}

gdrive_talent_auth_sheets <- function(
  google_email = Sys.getenv("TALENT_GDRIVE_EMAIL", unset = ""),
  auth_cache = Sys.getenv("TALENT_GDRIVE_AUTH_CACHE", unset = "~/.cache/gargle"),
  config_path = NULL,
  repo_root = gdrive_talent_repo_root()
) {
  gdrive_talent_assert_packages(c("googlesheets4", "yaml"))

  if (is.null(config_path) || !nzchar(config_path)) {
    config_path <- gdrive_talent_default_config_path(repo_root)
  }
  config <- gdrive_talent_read_config(config_path)

  google_email <- gdrive_talent_config_value(config, "google_email", google_email)
  auth_cache <- path.expand(gdrive_talent_config_value(config, "google_auth_cache", auth_cache))

  if (!nzchar(google_email)) {
    stop("Missing google_email in config or TALENT_GDRIVE_EMAIL.", call. = FALSE)
  }

  dir.create(auth_cache, recursive = TRUE, showWarnings = FALSE)
  options(
    gargle_oob_default = TRUE,
    gargle_oauth_client_type = "web"
  )

  googlesheets4::gs4_auth(
    email = google_email,
    cache = auth_cache,
    use_oob = TRUE
  )

  invisible(googlesheets4::gs4_user())
}

gdrive_talent_write_talents_sheet <- function(
  sheet_id,
  worksheet = "talents",
  datalake_root = NULL,
  db_path = NULL,
  repo_root = gdrive_talent_repo_root(),
  classification_config_path = NULL,
  include_excluded = FALSE,
  preserve_manual_columns = TRUE,
  manual_columns = c("active", "aliases", "notes"),
  create_named_range = TRUE,
  named_range = "Canonical_talent_name",
  named_range_column = "canonical_talent_name",
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

  table <- gdrive_talent_build_talents_table(
    datalake_root = datalake_root,
    db_path = db_path,
    repo_root = repo_root,
    classification_config_path = classification_config_path,
    include_excluded = include_excluded
  )

  normalized_sheet_id <- gdrive_talent_normalize_sheet_id(sheet_id)
  if (isTRUE(preserve_manual_columns)) {
    existing <- tryCatch(
      googlesheets4::read_sheet(
        ss = normalized_sheet_id,
        sheet = worksheet,
        col_types = "c"
      ),
      error = function(e) NULL
    )
    table <- gdrive_talent_merge_existing_talents(
      table = table,
      existing = existing,
      preserve_cols = manual_columns
    )
  }

  googlesheets4::sheet_write(
    data = table,
    ss = normalized_sheet_id,
    sheet = worksheet
  )

  if (isTRUE(create_named_range)) {
    gdrive_talent_upsert_named_range(
      sheet_id = normalized_sheet_id,
      worksheet = worksheet,
      named_range = named_range,
      column_name = named_range_column,
      data = table,
      include_header = FALSE
    )
  }

  invisible(table)
}
