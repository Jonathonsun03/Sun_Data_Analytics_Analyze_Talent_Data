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

rr_render_quarto <- function(input, output_file, output_dir, params, quiet = FALSE) {
  input <- normalizePath(input, winslash = "/", mustWork = TRUE)
  input_dir <- dirname(input)
  input_file <- basename(input)
  output_path <- normalizePath(file.path(output_dir, output_file), winslash = "/", mustWork = FALSE)
  input_dir_output_path <- normalizePath(file.path(input_dir, output_file), winslash = "/", mustWork = FALSE)
  working_dir_output_path <- normalizePath(file.path(getwd(), output_file), winslash = "/", mustWork = FALSE)
  candidate_output_paths <- unique(c(input_dir_output_path, working_dir_output_path))

  for (candidate in candidate_output_paths) {
    if (file.exists(candidate) && !identical(candidate, output_path)) {
      unlink(candidate)
    }
  }

  if (requireNamespace("quarto", quietly = TRUE)) {
    old_wd <- setwd(input_dir)
    on.exit(setwd(old_wd), add = TRUE)
    quarto::quarto_render(
      input = input_file,
      output_file = output_file,
      execute_params = params,
      quiet = quiet
    )
    rendered_path <- rr_find_quarto_output(output_file, candidate_output_paths)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(rendered_path, output_path, overwrite = TRUE)) {
      stop("Could not copy rendered Quarto output to: ", output_path)
    }
    if (!identical(rendered_path, output_path)) {
      unlink(rendered_path)
    }
    return(invisible(output_path))
  }

  quarto_bin <- Sys.which("quarto")
  if (!nzchar(quarto_bin)) {
    stop(
      "Quarto is required to render .qmd reports. Install the Quarto CLI ",
      "or the R package `quarto`, then rerun this report."
    )
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package `yaml` is required to pass parameters to Quarto CLI renders.")
  }

  params_file <- tempfile(fileext = ".yml")
  on.exit(unlink(params_file), add = TRUE)
  yaml::write_yaml(params, params_file)

  args <- c(
    "render",
    input_file,
    "--to",
    "html",
    "--output",
    output_file,
    "--execute-params",
    params_file
  )
  if (isTRUE(quiet)) {
    args <- c(args, "--quiet")
  }

  old_wd <- setwd(input_dir)
  on.exit(setwd(old_wd), add = TRUE)
  status <- system2(quarto_bin, args = args)
  if (!identical(status, 0L)) {
    stop("Quarto render failed with status ", status, ".")
  }
  rendered_path <- rr_find_quarto_output(output_file, candidate_output_paths)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(rendered_path, output_path, overwrite = TRUE)) {
    stop("Could not copy rendered Quarto output to: ", output_path)
  }
  if (!identical(rendered_path, output_path)) {
    unlink(rendered_path)
  }

  invisible(output_path)
}

rr_find_quarto_output <- function(output_file, candidate_paths) {
  hit <- candidate_paths[file.exists(candidate_paths)]
  if (length(hit) > 0) {
    return(hit[[1]])
  }

  stop(
    "Quarto render completed, but expected output was not found for ",
    output_file,
    ". Checked: ",
    paste(candidate_paths, collapse = ", ")
  )
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
  input_ext <- tolower(tools::file_ext(input_rmd))
  if (identical(input_ext, "qmd")) {
    can_render <- requireNamespace("quarto", quietly = TRUE) || nzchar(Sys.which("quarto"))
    if (!can_render) {
      stop(
        "Quarto is required to render .qmd reports. Install the Quarto CLI ",
        "or the R package `quarto`, then rerun this report."
      )
    }
  } else if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package `rmarkdown` is required to render .Rmd reports.")
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
        render_params <- build_params(talent, render_dir)
        if (identical(input_ext, "qmd")) {
          rr_render_quarto(
            input = input_rmd,
            output_file = output_file,
            output_dir = render_dir,
            params = render_params,
            quiet = quiet_render
          )
        } else {
          rmarkdown::render(
            input = input_rmd,
            output_file = output_file,
            output_dir = render_dir,
            params = render_params,
            envir = new.env(parent = globalenv()),
            quiet = quiet_render
          )
        }
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
