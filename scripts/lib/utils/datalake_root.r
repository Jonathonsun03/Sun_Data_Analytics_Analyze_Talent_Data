get_datalake_root <- function() {
  env_root <- Sys.getenv("TALENT_DATALAKE_ROOT", unset = "")
  if (nzchar(env_root)) return(env_root)

  if (.Platform$OS.type == "windows") {
    return("X:/datalake/DataLake/Sun_Data_Analytics/Talent_data")
  }

  linux_candidates <- c(
    "/mnt/datalake/Datalake/Sun_Data_Analytics/Talent_data",
    "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data"
  )
  linux_existing <- linux_candidates[dir.exists(linux_candidates)]
  if (length(linux_existing) > 0) return(linux_existing[[1]])

  stop("Datalake root not configured. Set TALENT_DATALAKE_ROOT.")
}
