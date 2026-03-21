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
  marker_rel = file.path("r_scripts", "lib", "utils", "staging_root.R")
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

source(file.path(repo_root, "r_scripts", "lib", "utils", "report_render_utils.R"))
source(file.path(repo_root, "r_scripts", "lib", "utils", "staging_root.R"))
source(file.path(repo_root, "r_scripts", "lib", "utils", "datalake_root.r"))
source(file.path(repo_root, "r_scripts", "lib", "utils", "talent_select.R"))

repo_path <- function(...) rr_repo_path(repo_root, ...)
resolve_repo_or_abs <- function(path) {
  if (grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  repo_path(path)
}

parse_optional_date <- function(x, flag_name = "--date") {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(NA_character_)
  }
  txt <- trimws(x)
  d <- suppressWarnings(as.Date(txt))
  if (is.na(d)) {
    stop(flag_name, " must be YYYY-MM-DD.")
  }
  as.character(d)
}

args <- commandArgs(trailingOnly = TRUE)
render_generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
render_cli <- paste(args, collapse = " ")

input_rmd <- rr_arg_value(args, "--input", repo_path("templates", "reports", "Bundle_B", "Bundle_B.Rmd"))
input_rmd <- resolve_repo_or_abs(input_rmd)
if (!file.exists(input_rmd)) {
  stop("Input Rmd not found: ", input_rmd)
}

output_dir_flag <- rr_arg_value(args, "--output-dir", "")
output_dir_supplied <- rr_has_flag(args, "--output-dir") && nzchar(trimws(output_dir_flag))
output_dir <- if (output_dir_supplied) {
  resolve_repo_or_abs(output_dir_flag)
} else {
  normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)
}
if (output_dir_supplied) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

output_prefix <- rr_arg_value(args, "--output-prefix", "Bundle_B")
quiet_render <- rr_has_flag(args, "--quiet")
data_source <- tolower(trimws(rr_arg_value(args, "--data-source", "datalake")))
if (!(data_source %in% c("staging", "datalake"))) {
  stop("`--data-source` must be one of: staging, datalake.")
}
data_root_arg <- trimws(rr_arg_value(args, "--data-root", ""))
data_root <- if (nzchar(data_root_arg)) {
  resolve_repo_or_abs(data_root_arg)
} else if (identical(data_source, "datalake")) {
  normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(get_staging_root(), winslash = "/", mustWork = FALSE)
}
if (!dir.exists(data_root)) {
  stop("Data root does not exist: ", data_root)
}
window_days <- rr_parse_optional_positive_int(
  rr_arg_value(args, "--window-days", ""),
  flag_name = "--window-days"
)
start_date <- parse_optional_date(rr_arg_value(args, "--start-date", ""), "--start-date")
end_date <- parse_optional_date(rr_arg_value(args, "--end-date", ""), "--end-date")

if (!is.na(start_date) && !is.na(end_date) && as.Date(start_date) > as.Date(end_date)) {
  stop("`--start-date` cannot be after `--end-date`.")
}

included_date_range <- if (!is.na(start_date) || !is.na(end_date)) {
  start_out <- if (is.na(start_date)) as.character(as.Date("1900-01-01")) else start_date
  end_out <- if (is.na(end_date)) as.character(Sys.Date()) else end_date
  paste(start_out, "to", end_out)
} else if (!is.na(window_days)) {
  end_out <- Sys.Date()
  start_out <- end_out - as.integer(window_days) + 1L
  paste(as.character(start_out), "to", as.character(end_out))
} else {
  "All available dates"
}

talent_single <- trimws(rr_arg_value(args, "--talent", ""))
talent_list_csv <- rr_split_csv_values(rr_arg_value(args, "--talents", ""))
talent_list_file <- rr_read_values_file(
  rr_arg_value(args, "--talents-file", ""),
  path_resolver = resolve_repo_or_abs
)
talent_all <- rr_has_flag(args, "--all")

talents <- character()

if (talent_all) {
  talents <- c(talents, list_talents(root = data_root)$name)
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

cat("Bundle B render targets:", paste(talents, collapse = ", "), "\n")
cat("Input Rmd:", input_rmd, "\n")
if (output_dir_supplied) {
  cat("Output dir:", output_dir, "\n")
} else {
  cat("Output routing: talent-specific Bundle B folders under ", output_dir, "\n", sep = "")
}
cat("Data source:", data_source, "\n")
cat("Data root:", data_root, "\n")
if (is.na(window_days)) {
  if (!is.na(start_date) || !is.na(end_date)) {
    cat(
      "Window: explicit range ",
      if (is.na(start_date)) "open_start" else start_date,
      " to ",
      if (is.na(end_date)) "open_end" else end_date,
      "\n",
      sep = ""
    )
  } else {
    cat("Window: all available data in selected root\n")
  }
} else {
  cat("Window: last ", window_days, " days\n", sep = "")
}

result_df <- rr_render_for_talents(
  talents = talents,
  input_rmd = input_rmd,
  output_dir = if (output_dir_supplied) {
    output_dir
  } else {
    function(talent, slug) {
      normalizePath(
        file.path(output_dir, talent, "reports", "bundle_B"),
        winslash = "/",
        mustWork = FALSE
      )
    }
  },
  output_prefix = output_prefix,
  params_builder = function(talent, render_dir) {
    list(
      talent = talent,
      data_source = data_source,
      data_root = data_root,
      render_generated_at = render_generated_at,
      included_date_range = included_date_range,
      render_cli = render_cli,
      render_output_dir = render_dir,
      render_output_prefix = output_prefix,
      window_days = if (is.na(window_days)) NULL else window_days,
      start_date = if (is.na(start_date)) NULL else start_date,
      end_date = if (is.na(end_date)) NULL else end_date
    )
  },
  slugify_fn = talent_slugify,
  quiet_render = quiet_render,
  label = "Rendering talent"
)

if (any(!result_df$success)) {
  quit(status = 1)
}
