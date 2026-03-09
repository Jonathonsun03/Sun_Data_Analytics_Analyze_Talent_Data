.clean_data_dir <- function() {
  this_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(this_file)) {
    stop(
      "CleanData.R must be sourced with a full path, e.g. ",
      "source(here::here('scripts','lib','clean_data','CleanData.R'))."
    )
  }
  dirname(normalizePath(this_file))
}

clean_data_dir <- .clean_data_dir()
utils_dir <- normalizePath(file.path(clean_data_dir, "..", "utils"), mustWork = FALSE)

content_type_utils <- file.path(utils_dir, "content_type_utils.R")
if (file.exists(content_type_utils)) {
  source(content_type_utils)
}

source(file.path(clean_data_dir, "analytics_core.R"))
source(file.path(clean_data_dir, "analytics_join.R"))
source(file.path(clean_data_dir, "clean_columns.R"))
source(file.path(clean_data_dir, "video_prep.R"))
source(file.path(clean_data_dir, "title_classification_join.R"))

rm(.clean_data_dir, clean_data_dir, utils_dir, content_type_utils)
