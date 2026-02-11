chatgpt_repo_root <- function(repo_root = NULL) {
  if (!is.null(repo_root)) {
    return(normalizePath(repo_root, winslash = "/", mustWork = FALSE))
  }
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package `here` is required. Install it or pass `repo_root` explicitly.")
  }
  here::here()
}

chatgpt_load_env <- function(repo_root = NULL, env_file = ".env") {
  repo_root <- chatgpt_repo_root(repo_root)
  env_path <- file.path(repo_root, env_file)
  if (!file.exists(env_path)) {
    stop("Missing .env at repo root: ", env_path)
  }
  readRenviron(env_path)
  invisible(env_path)
}

chatgpt_get_api_key <- function(
    env_name = "OPENAI_API_KEY",
    repo_root = NULL,
    env_file = ".env",
    load_env = TRUE
) {
  if (load_env) {
    chatgpt_load_env(repo_root = repo_root, env_file = env_file)
  }
  key <- Sys.getenv(env_name)
  if (!nzchar(key)) {
    stop("Missing ", env_name, " in .env or environment.")
  }
  key
}

chatgpt_get_base_url <- function(
    env_name = "OPENAI_BASE_URL",
    default = "https://api.openai.com/v1"
) {
  base_url <- Sys.getenv(env_name, unset = default)
  if (!nzchar(base_url)) {
    base_url <- default
  }
  sub("/$", "", base_url)
}
