get_staging_root <- function() {
  if (!exists("load_repo_env", mode = "function")) {
    repo_env_path <- file.path("r_scripts", "lib", "utils", "repo_env.R")
    if (!file.exists(repo_env_path)) {
      repo_root <- Sys.getenv("TALENT_REPO_ROOT", unset = "")
      repo_env_path <- file.path(repo_root, "r_scripts", "lib", "utils", "repo_env.R")
    }
    source(repo_env_path)
  }
  load_repo_env()

  env_root <- Sys.getenv("TALENT_STAGING_ROOT", unset = "")
  if (nzchar(env_root)) return(env_root)

  env_root <- Sys.getenv("STAGING_ROOT", unset = "")
  if (nzchar(env_root)) return(env_root)

  if (.Platform$OS.type == "windows") {
    return("X:/staging/Talent_data")
  }

  linux_default <- "/mnt/staging/staging/Talent_data"
  if (dir.exists(linux_default)) return(linux_default)

  stop("Staging root not configured. Set TALENT_STAGING_ROOT or STAGING_ROOT.")
}
