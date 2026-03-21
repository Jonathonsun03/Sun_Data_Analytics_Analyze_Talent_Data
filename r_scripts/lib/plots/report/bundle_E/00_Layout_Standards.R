bundle_e_load_bundle_a_helpers <- function() {
  if (exists("bundle_a_wrap_text", mode = "function") &&
      exists("bundle_a_talent_subtitle", mode = "function")) {
    return(invisible(TRUE))
  }

  helper_candidates <- c(
    file.path("r_scripts", "lib", "plots", "report", "bundle_A", "00_Bundle_A_Helpers.R"),
    here::here("r_scripts", "lib", "plots", "report", "bundle_A", "00_Bundle_A_Helpers.R")
  )

  for (path in unique(helper_candidates)) {
    if (file.exists(path)) {
      source(path)
      if (exists("bundle_a_wrap_text", mode = "function") &&
          exists("bundle_a_talent_subtitle", mode = "function")) {
        return(invisible(TRUE))
      }
    }
  }

  stop("Could not load Bundle A helper functions for Bundle E layout standards.")
}

bundle_e_load_bundle_a_helpers()

bundle_e_empty_plot <- function(title, subtitle = NULL, label = "No data available for current filters.") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = label, size = 4.4) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    theme_void() +
    ggplot2::labs(title = title, subtitle = subtitle)
}

bundle_e_title_text <- function(title_suffix, width = 58) {
  bundle_a_wrap_text(title_suffix, width = width)
}

bundle_e_subtitle <- function(talent, detail = NULL, width = 110) {
  bundle_a_talent_subtitle(talent, detail = detail, width = width)
}
