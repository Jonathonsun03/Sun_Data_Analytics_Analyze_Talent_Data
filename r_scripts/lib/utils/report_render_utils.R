rr_get_script_dir <- function() {
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

rr_get_repo_root <- function(script_dir = rr_get_script_dir(), levels_up = 3L) {
  stopifnot(length(levels_up) == 1L, is.finite(levels_up), levels_up >= 0)
  path <- script_dir
  for (i in seq_len(as.integer(levels_up))) {
    path <- file.path(path, "..")
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

rr_repo_path <- function(repo_root, ...) {
  normalizePath(file.path(repo_root, ...), winslash = "/", mustWork = FALSE)
}

rr_arg_value <- function(args, flag, default = NULL) {
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

rr_has_flag <- function(args, flag) {
  any(args == flag)
}

rr_split_csv_values <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(character())
  }
  vals <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  vals[nzchar(vals)]
}

rr_read_values_file <- function(path, path_resolver = identity) {
  if (is.null(path) || !nzchar(path)) {
    return(character())
  }
  full <- path_resolver(path)
  if (!file.exists(full)) {
    stop("Values file not found: ", full)
  }
  vals <- readLines(full, warn = FALSE, encoding = "UTF-8")
  vals <- trimws(vals)
  vals[nzchar(vals) & !grepl("^#", vals)]
}

rr_parse_optional_positive_int <- function(x, flag_name = "--value") {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(NA_integer_)
  }
  val <- suppressWarnings(as.integer(trimws(x)))
  if (is.na(val) || val <= 0) {
    stop(flag_name, " must be a positive integer.")
  }
  val
}

rr_render_for_talents <- function(
  talents,
  input_rmd,
  output_dir,
  output_prefix,
  params_builder,
  slugify_fn,
  quiet_render = FALSE,
  label = "Rendering talent"
) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package `rmarkdown` is required.")
  }

  resolve_output_dir <- function(talent, slug) {
    if (is.function(output_dir)) {
      out <- output_dir(talent, slug)
    } else {
      out <- output_dir
    }
    normalizePath(out, winslash = "/", mustWork = FALSE)
  }

  build_params <- function(talent, render_dir) {
    if (length(formals(params_builder)) >= 2) {
      return(params_builder(talent, render_dir))
    }
    params_builder(talent)
  }

  results <- vector("list", length(talents))

  for (i in seq_along(talents)) {
    talent <- talents[[i]]
    slug <- slugify_fn(talent)
    output_file <- paste0(output_prefix, "_", slug, ".html")
    render_dir <- resolve_output_dir(talent, slug)
    dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)

    cat("\n[", i, "/", length(talents), "] ", label, ": ", talent, "\n", sep = "")
    ok <- TRUE
    err_msg <- ""

    tryCatch(
      {
        rmarkdown::render(
          input = input_rmd,
          output_file = output_file,
          output_dir = render_dir,
          params = build_params(talent, render_dir),
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
      cat("Rendered:", file.path(render_dir, output_file), "\n")
    } else {
      cat("Failed:", talent, " -> ", err_msg, "\n", sep = "")
    }

    results[[i]] <- data.frame(
      talent = talent,
      output_dir = render_dir,
      output_file = output_file,
      success = ok,
      error = if (ok) NA_character_ else err_msg,
      stringsAsFactors = FALSE
    )
  }

  result_df <- do.call(rbind, results)
  cat("\nRender summary:\n")
  print(result_df, row.names = FALSE)
  result_df
}
