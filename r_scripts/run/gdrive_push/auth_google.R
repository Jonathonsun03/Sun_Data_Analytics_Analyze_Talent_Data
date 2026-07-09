#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x
}

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) ""
)
if (!nzchar(script_path)) {
  script_path <- normalizePath("r_scripts/run/gdrive_push/auth_google.R", winslash = "/", mustWork = FALSE)
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = FALSE)

source(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data", "load_all.R"))
gdrive_talent_load_all(file.path(repo_root, "r_scripts", "lib", "gdrive_push_talent_data"))

config_path <- Sys.getenv(
  "TALENT_GDRIVE_PUSH_CONFIG",
  unset = gdrive_talent_default_config_path(repo_root)
)
config <- gdrive_talent_read_config(config_path)

email <- gdrive_talent_config_value(config, "google_email", Sys.getenv("TALENT_GDRIVE_EMAIL", unset = ""))
cache <- path.expand(gdrive_talent_config_value(config, "google_auth_cache", "~/.cache/gargle"))

if (!nzchar(email)) {
  stop("Missing google_email in config or TALENT_GDRIVE_EMAIL.", call. = FALSE)
}

dir.create(cache, recursive = TRUE, showWarnings = FALSE)

options(
  gargle_oob_default = TRUE,
  gargle_oauth_client_type = "web"
)

cat("Authenticating Google Drive and Sheets as: ", email, "\n", sep = "")
cat("Token cache: ", cache, "\n", sep = "")

googledrive::drive_auth(email = email, cache = cache, use_oob = TRUE)
googlesheets4::gs4_auth(email = email, cache = cache, use_oob = TRUE)

cat("\nDrive user:\n")
print(googledrive::drive_user())

cat("\nSheets user:\n")
print(googlesheets4::gs4_user())

cat("\nAuth complete.\n")
