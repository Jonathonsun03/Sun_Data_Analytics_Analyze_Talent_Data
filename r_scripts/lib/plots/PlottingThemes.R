this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) ""
)

theme_dir_candidates <- unique(c(
  if (nzchar(this_file)) file.path(dirname(this_file), "themes") else NULL,
  file.path("r_scripts", "lib", "plots", "themes"),
  file.path(getwd(), "r_scripts", "lib", "plots", "themes"),
  tryCatch(here::here("r_scripts", "lib", "plots", "themes"), error = function(e) NULL)
))

pick_theme_dir <- function(candidates) {
  for (cand in candidates) {
    if (is.null(cand) || !nzchar(cand)) {
      next
    }
    cand_norm <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    if (!dir.exists(cand_norm)) {
      next
    }
    has_r <- length(list.files(cand_norm, pattern = "\\.[rR]$", full.names = FALSE)) > 0
    if (has_r) {
      return(cand_norm)
    }
  }
  ""
}

plot_themes_dir <- pick_theme_dir(theme_dir_candidates)
if (!nzchar(plot_themes_dir)) {
  stop(
    "Could not locate plot themes directory. Tried: ",
    paste(theme_dir_candidates, collapse = ", ")
  )
}

theme_files <- list.files(plot_themes_dir, pattern = "\\.[rR]$", full.names = TRUE)

for (theme_file in sort(theme_files)) {
  source(theme_file)
}
