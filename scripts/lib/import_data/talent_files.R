library(tools)

TalentFiles <- function(Paths) {
  if (is.character(Paths)) {
    Paths <- map(
      Paths,
      ~ list.files(.x, full.names = TRUE) %>%
        keep(~ !isTRUE(file.info(.x)$isdir)) %>%
        keep(~ str_detect(.x, "\\.csv$"))
    )
  }

  map(Paths, function(folder_files) {
    # Fix known escaped UTF-8 byte sequences for full-width brackets.
    folder_files <- str_replace_all(
      folder_files,
      c("<e3><80><90>" = "\u3010", "<e3><80><91>" = "\u3011")
    )
    folder_files <- enc2utf8(folder_files)
    folder_files <- normalizePath(folder_files, winslash = "/", mustWork = FALSE)
    exists <- file.exists(folder_files)
    if (any(!exists)) {
      warning(
        "Missing paths (check mount/encoding): ",
        paste(folder_files[!exists], collapse = ", ")
      )
      folder_files <- folder_files[exists]
    }

    # 1) Read each CSV and attach date inferred from filename
    dfs <- map(folder_files, function(f) {
      df <- read_csv(f, show_col_types = FALSE)
      date_str <- str_extract(basename(f), "\\d{4}-\\d{2}-\\d{2}")
      df$date <- as.Date(date_str)  # NA if no date found
      df
    })
    names(dfs) <- basename(folder_files)

    # 2) Derive type from filename (strip extension + trailing date)
    file_bases <- file_path_sans_ext(names(dfs))
    types <- str_remove(file_bases, "_\\d{4}-\\d{2}-\\d{2}$")

    # 3) Split dfs list into sublists by type
    split(dfs, types)
  })
}
