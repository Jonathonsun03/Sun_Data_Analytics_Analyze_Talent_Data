find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, "AGENTS.md")) &&
        dir.exists(file.path(current, "r_scripts"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate repository root from: ", start, call. = FALSE)
    }
    current <- parent
  }
}

script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

arg_value <- function(args, flag, default = "") {
  prefix <- paste0(flag, "=")
  inline <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(inline) > 0L) {
    return(sub(paste0("^", prefix), "", inline[[1]]))
  }

  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) {
    return(args[[idx + 1L]])
  }

  default
}

resolve_repo_or_abs <- function(path, repo_root) {
  if (grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(repo_root, path), winslash = "/", mustWork = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
repo_root <- find_repo_root(script_dir())
notebook_dir <- file.path(repo_root, "r_scripts", "run", "notebooks")
input_rmd <- file.path(notebook_dir, "ena_test.rmd")

if (!file.exists(input_rmd)) {
  stop("Input notebook not found: ", input_rmd, call. = FALSE)
}

output_dir <- arg_value(args, "--output-dir", notebook_dir)
output_dir <- resolve_repo_or_abs(output_dir, repo_root)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- arg_value(args, "--output-file", "ena_test.html")
quiet <- "--quiet" %in% args

rendered_path <- rmarkdown::render(
  input = input_rmd,
  output_file = output_file,
  output_dir = output_dir,
  knit_root_dir = repo_root,
  quiet = quiet,
  envir = new.env(parent = globalenv())
)

message("Rendered ENA test notebook: ", normalizePath(rendered_path, winslash = "/", mustWork = FALSE))
