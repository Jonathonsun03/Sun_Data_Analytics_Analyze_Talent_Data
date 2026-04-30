gdrive_talent_assert_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required package(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

gdrive_talent_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

gdrive_talent_bool <- function(x) {
  if (is.logical(x)) {
    x[is.na(x)] <- FALSE
    return(x)
  }

  if (is.numeric(x)) {
    return(!is.na(x) & x != 0)
  }

  x <- tolower(trimws(as.character(x)))
  x %in% c("true", "t", "yes", "y", "1", "checked", "x")
}

gdrive_talent_safe_name <- function(x) {
  x <- gdrive_talent_chr(x)
  x <- gsub("[/\\\\:*?\"<>|]+", "_", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

gdrive_talent_safe_file_name <- function(x, max_chars = 180) {
  x <- gdrive_talent_chr(x)
  ext <- tools::file_ext(x)
  stem <- if (nzchar(ext)) sub(paste0("\\.", ext, "$"), "", x) else x

  stem <- gsub("[`'\"“”‘’]+", "", stem)
  stem <- gsub("[\\[\\]()]+", "_", stem)
  stem <- gsub("[{}]+", "_", stem, fixed = FALSE)
  stem <- gsub("[【】]+", "_", stem)
  stem <- gsub("[/\\\\:*?<>|#%&$!@+=;,]+", "_", stem)
  stem <- gsub("[[:cntrl:]]+", "_", stem)
  stem <- gsub("[^A-Za-z0-9._ -]+", "_", stem)
  stem <- gsub("[ _.-]+", "_", stem)
  stem <- gsub("^_+|_+$", "", stem)
  if (!nzchar(stem)) stem <- "file"

  max_stem <- max_chars
  if (nzchar(ext)) {
    max_stem <- max(1L, max_chars - nchar(ext) - 1L)
  }
  if (nchar(stem) > max_stem) {
    stem <- substr(stem, 1L, max_stem)
    stem <- gsub("_+$", "", stem)
  }

  if (nzchar(ext)) paste0(stem, ".", ext) else stem
}

gdrive_talent_slug <- function(x) {
  x <- tolower(gdrive_talent_safe_name(x))
  x <- gsub("[^a-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

gdrive_talent_replace_tokens <- function(pattern, values) {
  out <- pattern
  for (nm in names(values)) {
    out <- gsub(
      paste0("\\{", nm, "\\}"),
      as.character(values[[nm]]),
      out,
      fixed = FALSE
    )
  }
  out
}

gdrive_talent_empty_df <- function(cols) {
  out <- as.data.frame(
    stats::setNames(rep(list(character()), length(cols)), cols),
    stringsAsFactors = FALSE
  )
  out
}
