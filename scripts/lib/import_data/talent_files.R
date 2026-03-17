library(tools)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# Keep this for the parent paths, but we will handle the file lists differently
fix_talent_paths <- function(x) {
  x <- str_replace_all(x, "<U\\+([0-9a-fA-F]+)>", function(m) {
    hex <- str_remove_all(m, "<U\\+|>")
    unname(sapply(hex, function(h) intToUtf8(strtoi(h, 16L))))
  })

  x <- str_replace_all(x, "(?:<[0-9a-fA-F]{2}>)+", function(m) {
    unname(sapply(m, function(seq) {
      hex_bytes <- str_extract_all(seq, "[0-9a-fA-F]{2}")[[1]]
      raw_bytes <- as.raw(strtoi(hex_bytes, 16L))
      res <- rawToChar(raw_bytes)
      Encoding(res) <- "UTF-8"
      res
    }))
  })

  enc2utf8(x)
}

with_utf8_locale <- function(expr) {
  old_locale <- Sys.getlocale("LC_CTYPE")
  if (!is.na(old_locale) && !str_detect(old_locale, "UTF-8")) {
    locale_set <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
    if (is.na(locale_set) || locale_set == "") {
      suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
    }
    on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old_locale)), add = TRUE)
  }
  force(expr)
}

list_csv_files_utf8 <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    return(character(0))
  }

  has_use_bytes <- "useBytes" %in% names(formals(list.files))
  files <- if (has_use_bytes) {
    list.files(dir_path, full.names = FALSE, recursive = FALSE, useBytes = TRUE)
  } else {
    list.files(dir_path, full.names = FALSE, recursive = FALSE)
  }

  files <- files[str_detect(files, "(?i)\\.csv$")]
  if (length(files) == 0) {
    return(character(0))
  }

  full_paths <- file.path(dir_path, files)
  Encoding(full_paths) <- "UTF-8"
  full_paths
}

list_talent_snapshot_files <- function(talent_path) {
  talent_path <- fix_talent_paths(talent_path)

  root_files <- list_csv_files_utf8(talent_path)

  raw_data_root <- file.path(talent_path, "raw_data")
  raw_data_root <- fix_talent_paths(raw_data_root)
  raw_files <- character(0)
  if (dir.exists(raw_data_root)) {
    raw_top_files <- list_csv_files_utf8(raw_data_root)

    has_use_bytes <- "useBytes" %in% names(formals(list.files))
    raw_children <- if (has_use_bytes) {
      list.files(raw_data_root, full.names = TRUE, recursive = FALSE, useBytes = TRUE)
    } else {
      list.files(raw_data_root, full.names = TRUE, recursive = FALSE)
    }
    raw_children <- fix_talent_paths(raw_children)
    raw_type_dirs <- raw_children[dir.exists(raw_children)]
    raw_type_files <- unlist(map(raw_type_dirs, list_csv_files_utf8), use.names = FALSE)

    raw_files <- c(raw_top_files, raw_type_files)
  }

  unique(c(root_files, raw_files))
}

infer_snapshot_type_key <- function(path) {
  normalized <- fix_talent_paths(path)
  normalized <- str_replace_all(normalized, "\\\\", "/")

  raw_match <- str_match(normalized, "/raw_data/([^/]+)/[^/]+$")
  if (!is.na(raw_match[, 2]) && nzchar(raw_match[, 2])) {
    return(enc2utf8(raw_match[, 2]))
  }

  base <- basename(normalized)
  base <- str_remove(base, "(?i)\\.csv$")
  base <- str_replace(base, "_\\d{4}-\\d{2}-\\d{2}.*$", "")
  enc2utf8(base)
}

TalentFiles <- function(Paths) {
  if (is.character(Paths)) {
    # 1. Decode the parent directory path first
    Paths <- fix_talent_paths(Paths)

    Paths <- map(Paths, function(p) {
      with_utf8_locale({
        if (!dir.exists(p)) {
          warning("Path does not exist: ", p)
          return(character(0))
        }

        # Collect legacy root-level snapshot CSVs plus the new raw_data/<type> layout.
        full_paths <- list_talent_snapshot_files(p)
        if (length(full_paths) == 0) return(character(0))
        Encoding(full_paths) <- "UTF-8"

        full_paths
      })
    })
  } else if (is.list(Paths)) {
    Paths <- map(Paths, function(p) {
      if (length(p) == 0) return(character(0))
      p <- fix_talent_paths(p)
      Encoding(p) <- "UTF-8"
      p
    })
  } else {
    stop("`Paths` must be a character vector of directories or a list of file paths.")
  }

  map(Paths, function(folder_files) {
    if (length(folder_files) == 0) return(list())

    # Read each CSV and attach date inferred from filename
    dfs <- map(folder_files, function(f) {
      with_utf8_locale({
        df <- tryCatch(
          # Some columns (e.g., paid chat amounts) can be sparse and appear after
          # the default readr guess window; scan full file to avoid logical NA coercion.
          read_csv(
            f,
            show_col_types = FALSE,
            locale = locale(encoding = "UTF-8"),
            guess_max = Inf
          ),
          error = function(e) {
            warning("Could not read file: ", f, "\nError: ", e$message)
            NULL
          }
        )

        if (!is.null(df)) {
          date_str <- str_extract(basename(f), "\\d{4}-\\d{2}-\\d{2}")
          df$date <- as.Date(date_str)
        }
        df
      })
    })

    keep_idx <- !map_lgl(dfs, is.null)
    dfs <- dfs[keep_idx]
    files_kept <- folder_files[keep_idx]

    if (length(dfs) == 0) return(list())

    safe_basename <- function(x) {
      str_extract(x, "[^/\\\\]+$") %>% enc2utf8()
    }

    type_keys <- map_chr(files_kept, function(f) {
      key <- infer_snapshot_type_key(f)
      if (nzchar(key)) {
        return(key)
      }
      base <- safe_basename(f)
      str_remove(str_replace(base, "_\\d{4}-\\d{2}-\\d{2}.*$", ""), "(?i)\\.csv$")
    })

    split_indices <- split(seq_along(dfs), type_keys)
    map(split_indices, function(idxs) {
      dplyr::bind_rows(dfs[idxs])
    })
  })
}
