chatgpt_load_all <- function(exclude_dirs = c("examples")) {
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package `here` is required to load ChatGPT modules.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package `purrr` is required to load ChatGPT modules.")
  }
  base_dir <- here::here("scripts", "lib", "ChatGPT")
  files <- list.files(
    base_dir,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.R$"
  )

  if (length(exclude_dirs) > 0) {
    exclude_paths <- file.path(base_dir, exclude_dirs)
    files <- files[!vapply(
      files,
      function(f) any(startsWith(f, exclude_paths)),
      logical(1)
    )]
  }

  purrr::walk(files, source)
  invisible(TRUE)
}
