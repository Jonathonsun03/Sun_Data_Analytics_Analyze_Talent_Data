bootstrap_get_script_dir <- function() {
  get_source_ofile <- function() {
    frames <- sys.frames()
    for (i in rev(seq_along(frames))) {
      of <- frames[[i]]$ofile
      if (!is.null(of) && nzchar(of)) {
        return(of)
      }
    }
    ""
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }

  ofile <- get_source_ofile()
  if (nzchar(ofile)) {
    return(dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

bootstrap_find_repo_root <- function(
  start_dirs = c(bootstrap_get_script_dir(), getwd()),
  marker_rel = file.path("scripts", "lib", "utils", "staging_root.R")
) {
  starts <- unique(normalizePath(start_dirs, winslash = "/", mustWork = FALSE))

  for (start in starts) {
    current <- start
    repeat {
      marker_path <- file.path(current, marker_rel)
      if (file.exists(marker_path)) {
        return(current)
      }
      parent <- dirname(current)
      if (identical(parent, current)) {
        break
      }
      current <- parent
    }
  }

  stop(
    "Could not locate repository root from: ",
    paste(starts, collapse = ", "),
    ". Ensure working directory is inside the project or run via Rscript path."
  )
}

repo_root <- bootstrap_find_repo_root()

source(file.path(repo_root, "scripts", "lib", "utils", "report_render_utils.R"))
source(file.path(repo_root, "scripts", "lib", "utils", "staging_root.R"))
source(file.path(repo_root, "scripts", "lib", "utils", "talent_select.R"))

repo_path <- function(...) rr_repo_path(repo_root, ...)
resolve_repo_or_abs <- function(path) {
  if (grepl("^/", path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  repo_path(path)
}

args <- commandArgs(trailingOnly = TRUE)

input_rmd <- rr_arg_value(args, "--input", repo_path("templates", "reports", "Bundle_A", "Bundle_A.Rmd"))
input_rmd <- resolve_repo_or_abs(input_rmd)
if (!file.exists(input_rmd)) {
  stop("Input Rmd not found: ", input_rmd)
}

output_dir <- rr_arg_value(args, "--output-dir", repo_path("reports", "bundle_A"))
output_dir <- resolve_repo_or_abs(output_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_prefix <- rr_arg_value(args, "--output-prefix", "Bundle_A")
quiet_render <- rr_has_flag(args, "--quiet")
window_days <- rr_parse_optional_positive_int(
  rr_arg_value(args, "--window-days", ""),
  flag_name = "--window-days"
)

talent_single <- trimws(rr_arg_value(args, "--talent", ""))
talent_list_csv <- rr_split_csv_values(rr_arg_value(args, "--talents", ""))
talent_list_file <- rr_read_values_file(
  rr_arg_value(args, "--talents-file", ""),
  path_resolver = resolve_repo_or_abs
)
talent_all <- rr_has_flag(args, "--all")

talents <- character()

if (talent_all) {
  talents <- c(talents, list_talents()$name)
}

if (nzchar(talent_single)) {
  talents <- c(talents, talent_single)
}

talents <- c(talents, talent_list_csv, talent_list_file)
talents <- unique(trimws(talents))
talents <- talents[nzchar(talents)]

if (length(talents) == 0) {
  talents <- "Ava"
}

cat("Bundle A render targets:", paste(talents, collapse = ", "), "\n")
cat("Input Rmd:", input_rmd, "\n")
cat("Output dir:", output_dir, "\n")
if (is.na(window_days)) {
  cat("Window: all available staged data\n")
} else {
  cat("Window: last ", window_days, " days\n", sep = "")
}

result_df <- rr_render_for_talents(
  talents = talents,
  input_rmd = input_rmd,
  output_dir = output_dir,
  output_prefix = output_prefix,
  params_builder = function(talent) {
    list(
      talent = talent,
      window_days = if (is.na(window_days)) NULL else window_days
    )
  },
  slugify_fn = talent_slugify,
  quiet_render = quiet_render,
  label = "Rendering talent"
)

if (any(!result_df$success)) {
  quit(status = 1)
}
