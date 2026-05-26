source_dir <- function(..., pattern = "[rR]$", recursive = FALSE) {
  files <- list.files(
    here::here(...),
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive
  )

  files <- sort(files)
  invisible(lapply(files, source))
}
