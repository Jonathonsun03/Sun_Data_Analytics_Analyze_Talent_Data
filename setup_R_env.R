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
cat(" VtuberNewsScraper - R Env Setup\n")
cat("==========================================\n")
cat("Repo root:", getwd(), "\n\n")

packages <- c(
  "renv",
  "here",
  "tidyverse",
  "dplyr",
  "purrr",
  "jsonlite",
  "rENA",
  "ggplot2",
  "ggrepel",
  "stringr",
  "tidyr",
  "rlang",
  "tidyselect",
  "lubridate"
)

ensure_renv <- function() {
  if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installing renv...\n")
    ensure_cran_repo()
    install.packages("renv")
  }
}

init_or_activate_renv <- function() {
  if (dir.exists("renv")) {
    cat("renv already exists. Activating...\n")
    renv::activate()
  } else {
    cat("Initializing renv...\n")
    renv::init(bare = TRUE)
  }
}

ensure_cran_repo <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
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

ensure_renv()
init_or_activate_renv()
install_if_missing(packages)
cat("Snapshotting renv lockfile...\n")
renv::snapshot(prompt = FALSE)

cat("\n")
cat("==========================================\n")
cat(" DONE - R Environment Ready\n")
cat("==========================================\n")
cat("R version:\n")
print(R.version.string)
cat("\n")
