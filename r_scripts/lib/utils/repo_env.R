repo_env_repo_root <- function(repo_root = NULL, start = getwd()) {
  if (!is.null(repo_root) && nzchar(repo_root)) {
    return(normalizePath(repo_root, winslash = "/", mustWork = FALSE))
  }

  env_root <- Sys.getenv("TALENT_REPO_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }

  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  if (file.exists(current) && !dir.exists(current)) {
    current <- dirname(current)
  }

  repeat {
    git_marker <- file.path(current, ".git")
    if (dir.exists(git_marker) || file.exists(git_marker)) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }

  stop("Could not locate Git repository root from: ", start, call. = FALSE)
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
    repo_env_apply_aliases(override = override)
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

  repo_env_apply_aliases(override = override)
  repo_env_loaded_paths[[env_key]] <- TRUE
  invisible(env_path)
}

repo_env_apply_aliases <- function(override = FALSE) {
  apply_alias <- function(canonical, alias) {
    canonical_value <- Sys.getenv(canonical, unset = "")
    alias_value <- Sys.getenv(alias, unset = "")
    if ((override || !nzchar(canonical_value)) && nzchar(alias_value)) {
      do.call(Sys.setenv, stats::setNames(list(alias_value), canonical))
    }
  }

  apply_alias("TALENT_DATALAKE_ROOT", "TALENT_DATA_ROOT")
  apply_alias("TALENT_STAGING_ROOT", "STAGING_ROOT")
  invisible(NULL)
}
