repo_env_repo_root <- function(repo_root = NULL) {
  if (!is.null(repo_root) && nzchar(repo_root)) {
    return(normalizePath(repo_root, winslash = "/", mustWork = FALSE))
  }

  env_root <- Sys.getenv("TALENT_REPO_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }

  start <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  candidates <- c(start, dirname(start))
  while (!identical(tail(candidates, 1), dirname(tail(candidates, 1)))) {
    candidates <- c(candidates, dirname(tail(candidates, 1)))
  }

  for (candidate in unique(candidates)) {
    if (
      file.exists(file.path(candidate, "AGENTS.md")) &&
        dir.exists(file.path(candidate, "r_scripts")) &&
        dir.exists(file.path(candidate, "py_scripts"))
    ) {
      return(candidate)
    }
  }

  stop("Could not locate repository root from working directory: ", getwd(), call. = FALSE)
}

repo_env_name <- function(line) {
  stripped <- trimws(line)
  if (!nzchar(stripped) || startsWith(stripped, "#")) {
    return("")
  }
  stripped <- sub("^export[[:space:]]+", "", stripped)
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*[[:space:]]*=", stripped)) {
    return("")
  }
  sub("[[:space:]]*=.*$", "", stripped)
}

repo_env_loaded_paths <- new.env(parent = emptyenv())

load_repo_env <- function(repo_root = NULL, env_file = ".env", override = FALSE) {
  root <- repo_env_repo_root(repo_root)
  env_path <- file.path(root, env_file)
  env_key <- normalizePath(env_path, winslash = "/", mustWork = FALSE)

  if (!override && isTRUE(repo_env_loaded_paths[[env_key]])) {
    return(invisible(env_path))
  }
  if (!file.exists(env_path)) {
    repo_env_loaded_paths[[env_key]] <- TRUE
    return(invisible(env_path))
  }

  lines <- readLines(env_path, warn = FALSE)
  if (!override) {
    keep <- vapply(
      lines,
      function(line) {
        name <- repo_env_name(line)
        nzchar(name) && !nzchar(Sys.getenv(name, unset = ""))
      },
      logical(1)
    )
    lines <- lines[keep]
  }

  if (length(lines) > 0) {
    temp_env <- tempfile("repo-env-")
    on.exit(unlink(temp_env), add = TRUE)
    writeLines(lines, temp_env)
    readRenviron(temp_env)
  }

  repo_env_loaded_paths[[env_key]] <- TRUE
  invisible(env_path)
}
