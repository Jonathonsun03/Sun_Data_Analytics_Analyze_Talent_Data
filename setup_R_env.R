#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- cmd_args[grepl("^--file=", cmd_args)]

if (length(file_arg) > 0) {
  script_path <- sub("^--file=", "", file_arg[1])
  script_dir <- dirname(normalizePath(script_path))
  setwd(script_dir)
}

cat("\n")
cat("==========================================\n")
cat(" Analyze Talent Data - R Env Setup\n")
cat("==========================================\n")
cat("Repo root:", getwd(), "\n\n")

fallback_packages <- c(
  "renv",
  "here",
  "tidyverse",
  "DBI",
  "DT",
  "bslib",
  "dplyr",
  "duckdb",
  "ggplot2",
  "ggrepel",
  "htmltools",
  "jsonlite",
  "knitr",
  "lubridate",
  "plotly",
  "purrr",
  "readr",
  "rENA",
  "rlang",
  "scales",
  "shiny",
  "stringr",
  "tidyr",
  "tidyselect"
)

ensure_cran_repo <- function() {
  repos <- getOption("repos")
  if (
    is.null(repos) ||
      !("CRAN" %in% names(repos)) ||
      is.na(repos[["CRAN"]]) ||
      repos[["CRAN"]] == "@CRAN@"
  ) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
}

ensure_renv <- function() {
  if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installing renv...\n")
    ensure_cran_repo()
    install.packages("renv")
  }
}

initialize_renv_if_needed <- function() {
  if (file.exists(file.path("renv", "activate.R"))) {
    cat("renv project infrastructure already exists.\n")
    return(invisible(TRUE))
  }

  cat("Initializing renv...\n")
  renv::init(project = getwd(), bare = TRUE)
}

install_if_missing <- function(pkgs) {
  lib <- renv::paths$library()
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE, lib.loc = lib)]
  if (length(missing) == 0) {
    cat("All required R packages are already installed.\n")
    return(invisible(TRUE))
  }

  cat("Installing missing packages:\n")
  cat("  ", paste(missing, collapse = ", "), "\n\n")
  ensure_cran_repo()
  renv::install(missing, dependencies = c("Depends", "Imports"))
}

lockfile_exists <- file.exists("renv.lock")

ensure_cran_repo()
ensure_renv()
initialize_renv_if_needed()

if (lockfile_exists) {
  cat("Restoring packages from renv.lock...\n")
  renv::restore(project = getwd(), prompt = FALSE)
} else {
  cat("No renv.lock found; installing the fallback package set...\n")
  install_if_missing(fallback_packages)
  cat("Creating renv.lock...\n")
  renv::snapshot(project = getwd(), prompt = FALSE)
}

cat("\n")
cat("==========================================\n")
cat(" DONE - R Environment Ready\n")
cat("==========================================\n")
cat("R version:\n")
print(R.version.string)
cat("\n")
