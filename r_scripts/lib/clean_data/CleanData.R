.clean_data_dir <- function() {
  get_source_ofile <- function() {
    frames <- sys.frames()
    for (i in rev(seq_along(frames))) {
      of <- frames[[i]]$ofile
      if (!is.null(of) && nzchar(of)) {
        return(of)
      }
    }
    NULL
  }

  this_file <- tryCatch(get_source_ofile(), error = function(e) NULL)
  if (is.null(this_file)) {
    stop(
      "CleanData.R must be sourced with a full path, e.g. ",
      "source(here::here('r_scripts','lib','clean_data','CleanData.R'))."
    )
  }
  dirname(normalizePath(this_file))
}

clean_data_dir <- .clean_data_dir()
utils_dir <- normalizePath(file.path(clean_data_dir, "..", "utils"), mustWork = FALSE)
bundle_e_module_dir <- normalizePath(
  file.path(clean_data_dir, "..", "plots", "report", "bundle_e"),
  mustWork = FALSE
)

source_bundle_e_module <- function(filename) {
  candidates <- c(
    file.path(clean_data_dir, filename),
    file.path(bundle_e_module_dir, filename)
  )

  for (path in unique(candidates)) {
    if (file.exists(path)) {
      source(path)
      return(invisible(TRUE))
    }
  }

  stop("Could not locate required Bundle E module: ", filename)
}

content_type_utils <- file.path(utils_dir, "content_type_utils.R")
if (file.exists(content_type_utils)) {
  source(content_type_utils)
}

source(file.path(clean_data_dir, "analytics_core.R"))
source(file.path(clean_data_dir, "analytics_join.R"))
source(file.path(clean_data_dir, "clean_columns.R"))
source(file.path(clean_data_dir, "video_prep.R"))
source(file.path(clean_data_dir, "title_classification_join.R"))
source_bundle_e_module("bundle_e_panel_prep.R")
source_bundle_e_module("bundle_e_summary_metrics.R")
source_bundle_e_module("bundle_e_short_window_metrics.R")

rm(.clean_data_dir, clean_data_dir, utils_dir, bundle_e_module_dir, source_bundle_e_module, content_type_utils)
