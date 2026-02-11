library(dplyr)
library(stringr)

normalize_talent_path <- function(x) {
  # 1. Universally decode <U+XXXX> Unicode placeholders
  x <- str_replace_all(x, "<U\\+([0-9a-fA-F]+)>", function(m) {
    hex <- str_remove_all(m, "<U\\+|>")
    unname(sapply(hex, function(h) intToUtf8(strtoi(h, 16L))))
  })
  
  # 2. Universally decode <xx><yy> raw byte sequences (e.g., <e3><80><90>)
  x <- str_replace_all(x, "(?:<[0-9a-fA-F]{2}>)+", function(m) {
    unname(sapply(m, function(seq) {
      # Extract the hex values, convert to raw bytes, then to a UTF-8 string
      hex_bytes <- str_extract_all(seq, "[0-9a-fA-F]{2}")[[1]]
      raw_bytes <- as.raw(strtoi(hex_bytes, 16L))
      res <- rawToChar(raw_bytes)
      Encoding(res) <- "UTF-8"
      res
    }))
  })
  
  x <- enc2utf8(x)
  
  # Avoid hard failures on non-native encodings; normalize when safe.
  normalized <- suppressWarnings(
    tryCatch(
      normalizePath(x, winslash = "/", mustWork = FALSE),
      error = function(e) x
    )
  )
  normalized
}

safe_basename <- function(x) {
  # Updated regex to handle both forward (/) and backward (\) slashes 
  # just in case normalizePath leaves Windows-style slashes behind.
  str_extract(x, "[^/\\\\]+$") %>% enc2utf8()
}

list_talents <- function(root = get_staging_root()) {
  has_use_bytes <- "useBytes" %in% names(formals(list.dirs))
  paths <- if (has_use_bytes) {
    list.dirs(root, full.names = TRUE, recursive = FALSE, useBytes = TRUE)
  } else {
    list.dirs(root, full.names = TRUE, recursive = FALSE)
  }
  
  paths <- normalize_talent_path(paths)
  
  tibble(
    name = safe_basename(paths),
    path = paths
  ) %>%
    arrange(name)
}

select_talent <- function(query, root = get_staging_root(), ignore_case = TRUE) {
  talents <- list_talents(root)
  
  if (is.numeric(query)) {
    if (length(query) != 1 || query < 1 || query > nrow(talents)) {
      stop("Numeric query out of range.")
    }
    return(talents$path[[query]])
  }

  q <- enc2utf8(as.character(query))
  
  name_match <- if (ignore_case) {
    str_to_lower(talents$name) == str_to_lower(q)
  } else {
    talents$name == q
  }

  if (any(name_match)) {
    return(talents$path[[which(name_match)[1]]])
  }

  partial_match <- if (ignore_case) {
    str_detect(str_to_lower(talents$name), str_to_lower(q))
  } else {
    str_detect(talents$name, q)
  }

  hits <- talents[partial_match, , drop = FALSE]
  if (nrow(hits) == 1) return(hits$path[[1]])
  if (nrow(hits) == 0) stop("No talent matched query: ", q)

  stop(
    "Multiple talents matched query: ",
    q,
    ". Matches: ",
    paste(hits$name, collapse = ", ")
  )
}

talent_slugify <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_+|_+$", "")
  if (!nzchar(x)) "talent" else x
}

ensure_talent_id <- function(con, talent_name, table = "talents") {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required for ensure_talent_id().")
  }

  if (!DBI::dbExistsTable(con, table)) {
    DBI::dbExecute(
      con,
      sprintf(
        "CREATE TABLE %s (
           talent_num INTEGER PRIMARY KEY,
           talent_name TEXT UNIQUE,
           talent_slug TEXT,
           talent_id TEXT UNIQUE,
           created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
         )",
        table
      )
    )
  }

  existing <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT talent_num, talent_slug, talent_id FROM %s WHERE talent_name = ? LIMIT 1",
      table
    ),
    params = list(talent_name)
  )
  if (nrow(existing) == 1) {
    return(existing)
  }

  next_num <- DBI::dbGetQuery(
    con,
    sprintf("SELECT COALESCE(MAX(talent_num), 0) + 1 AS next_num FROM %s", table)
  )$next_num[[1]]

  talent_slug <- talent_slugify(talent_name)
  talent_id <- sprintf("%04d_%s", next_num, talent_slug)

  DBI::dbExecute(
    con,
    sprintf(
      "INSERT INTO %s (talent_num, talent_name, talent_slug, talent_id) VALUES (?, ?, ?, ?)",
      table
    ),
    params = list(next_num, talent_name, talent_slug, talent_id)
  )

  data.frame(
    talent_num = next_num,
    talent_slug = talent_slug,
    talent_id = talent_id,
    stringsAsFactors = FALSE
  )
}
