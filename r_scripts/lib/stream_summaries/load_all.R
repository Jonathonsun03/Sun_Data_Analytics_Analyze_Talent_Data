ss_load_all <- function(root = NULL) {
  if (is.null(root) || !nzchar(root)) {
    root <- file.path("r_scripts", "lib", "stream_summaries")
  }
  files <- list.files(root, pattern = "\\.R$", full.names = TRUE)
  files <- files[basename(files) != "load_all.R"]
  for (f in sort(files)) {
    source(f)
  }
  invisible(files)
}
