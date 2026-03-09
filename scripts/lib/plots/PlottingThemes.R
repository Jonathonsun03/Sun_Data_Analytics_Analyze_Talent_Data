this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) ""
)
plot_themes_dir <- if (nzchar(this_file)) {
  file.path(dirname(this_file), "themes")
} else {
  file.path("scripts", "lib", "plots", "themes")
}

theme_files <- list.files(plot_themes_dir, pattern = "\\.[rR]$", full.names = TRUE)

for (theme_file in sort(theme_files)) {
  source(theme_file)
}
