gdrive_talent_default_config_path <- function(repo_root = ".") {
  file.path(repo_root, "r_scripts", "run", "gdrive_push", "config", "config.yml")
}

gdrive_talent_read_config <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(list())
  }

  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("yaml", "yml")) {
    gdrive_talent_assert_packages("yaml")
    return(yaml::read_yaml(path))
  }

  if (identical(ext, "json")) {
    gdrive_talent_assert_packages("jsonlite")
    return(jsonlite::fromJSON(path, simplifyVector = FALSE))
  }

  stop("Unsupported config file extension: ", ext, call. = FALSE)
}

gdrive_talent_config_value <- function(config, key, default = "") {
  if (!is.null(config[[key]]) && length(config[[key]]) > 0) {
    value <- config[[key]]
    if (is.null(value) || (length(value) == 1 && is.na(value))) {
      return(default)
    }
    return(value)
  }

  default
}
