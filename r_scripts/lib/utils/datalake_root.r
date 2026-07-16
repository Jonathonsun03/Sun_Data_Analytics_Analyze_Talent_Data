get_datalake_root <- function() {
  if (!exists("load_repo_env", mode = "function")) {
    repo_env_path <- file.path("r_scripts", "lib", "utils", "repo_env.R")
    if (!file.exists(repo_env_path)) {
      repo_root <- Sys.getenv("TALENT_REPO_ROOT", unset = "")
      repo_env_path <- file.path(repo_root, "r_scripts", "lib", "utils", "repo_env.R")
    }
    source(repo_env_path)
  }
  load_repo_env()

  env_root <- Sys.getenv("TALENT_DATALAKE_ROOT", unset = "")
  if (nzchar(env_root)) return(env_root)

  env_root <- Sys.getenv("TALENT_DATA_ROOT", unset = "")
  if (nzchar(env_root)) return(env_root)

  chat_output_root <- Sys.getenv("CHAT_OUTPUT_ROOT", unset = "")
  if (nzchar(chat_output_root)) return(chat_output_root)

  if (.Platform$OS.type == "windows") {
    return("X:/datalake/DataLake/Sun_Data_Analytics/Talent_data")
  }

  linux_candidates <- c(
    "/mnt/router_data/DataLake/Sun_Data_Analytics/Talent_data",
    "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data",
    "/mnt/datalake/Datalake/Sun_Data_Analytics/Talent_data"
  )
  linux_existing <- linux_candidates[dir.exists(linux_candidates)]
  if (length(linux_existing) > 0) return(linux_existing[[1]])

  stop("Datalake root not configured. Set TALENT_DATALAKE_ROOT or TALENT_DATA_ROOT.")
}
