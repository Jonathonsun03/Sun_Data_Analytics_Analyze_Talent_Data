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

        # 2. Get the files. Do NOT use full.names = TRUE yet.
        # This prevents R from gluing the mangled parent path to the clean file names.
        has_use_bytes <- "useBytes" %in% names(formals(list.files))
        files <- if (has_use_bytes) {
          list.files(p, full.names = FALSE, useBytes = TRUE)
        } else {
          list.files(p, full.names = FALSE)
        }

        if (length(files) == 0) return(character(0))

        # 3. Filter for CSVs based purely on the filename
        files <- files[str_detect(files, "(?i)\\.csv$")]
        if (length(files) == 0) return(character(0))

        # 4. Manually construct the full paths and force UTF-8 encoding
        full_paths <- file.path(p, files)
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
      base <- safe_basename(f)
      str_replace(base, "_\\d{4}-\\d{2}-\\d{2}.*$", "")
    })

    split_indices <- split(seq_along(dfs), type_keys)
    map(split_indices, function(idxs) {
      dplyr::bind_rows(dfs[idxs])
    })
  })
}
