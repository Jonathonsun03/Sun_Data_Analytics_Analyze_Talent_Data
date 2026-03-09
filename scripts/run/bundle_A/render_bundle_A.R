get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

repo_root <- normalizePath(
  file.path(get_script_dir(), "..", "..", ".."),
  winslash = "/",
  mustWork = FALSE
)
repo_path <- function(...) normalizePath(file.path(repo_root, ...), winslash = "/", mustWork = FALSE)

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) {
    return(default)
  }
  pos <- idx[[1]] + 1L
  if (pos > length(args)) {
    return(default)
  }
  args[[pos]]
}

has_flag <- function(flag) {
  any(args == flag)
}

split_csv_values <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(character())
  }
  vals <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  vals[nzchar(vals)]
}

read_talent_file <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(character())
  }
  full <- if (grepl("^/", path)) path else repo_path(path)
  if (!file.exists(full)) {
    stop("Talents file not found: ", full)
  }
  vals <- readLines(full, warn = FALSE, encoding = "UTF-8")
  vals <- trimws(vals)
  vals <- vals[nzchar(vals) & !grepl("^#", vals)]
  vals
}

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Package `rmarkdown` is required.")
}

source(repo_path("scripts", "lib", "utils", "staging_root.R"))
source(repo_path("scripts", "lib", "utils", "talent_select.R"))

input_rmd <- arg_value("--input", repo_path("templates", "reports", "Bundle_A", "Bundle_A.Rmd"))
if (!grepl("^/", input_rmd)) {
  input_rmd <- repo_path(input_rmd)
}
if (!file.exists(input_rmd)) {
  stop("Input Rmd not found: ", input_rmd)
}

output_dir <- arg_value("--output-dir", repo_path("reports", "bundle_A"))
if (!grepl("^/", output_dir)) {
  output_dir <- repo_path(output_dir)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_prefix <- arg_value("--output-prefix", "Bundle_A")
quiet_render <- has_flag("--quiet")

talent_single <- trimws(arg_value("--talent", ""))
talent_list_csv <- split_csv_values(arg_value("--talents", ""))
talent_list_file <- read_talent_file(arg_value("--talents-file", ""))
talent_all <- has_flag("--all")

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

results <- vector("list", length(talents))

for (i in seq_along(talents)) {
  talent <- talents[[i]]
  slug <- talent_slugify(talent)
  output_file <- paste0(output_prefix, "_", slug, ".html")

  cat("\n[", i, "/", length(talents), "] Rendering talent: ", talent, "\n", sep = "")
  ok <- TRUE
  err_msg <- ""

  tryCatch(
    {
      rmarkdown::render(
        input = input_rmd,
        output_file = output_file,
        output_dir = output_dir,
        params = list(talent = talent),
        envir = new.env(parent = globalenv()),
        quiet = quiet_render
      )
    },
    error = function(e) {
      ok <<- FALSE
      err_msg <<- conditionMessage(e)
    }
  )

  if (ok) {
    cat("Rendered:", file.path(output_dir, output_file), "\n")
  } else {
    cat("Failed:", talent, " -> ", err_msg, "\n", sep = "")
  }

  results[[i]] <- data.frame(
    talent = talent,
    output_file = output_file,
    success = ok,
    error = if (ok) NA_character_ else err_msg,
    stringsAsFactors = FALSE
  )
}

result_df <- do.call(rbind, results)
cat("\nRender summary:\n")
print(result_df, row.names = FALSE)

if (any(!result_df$success)) {
  quit(status = 1)
}

