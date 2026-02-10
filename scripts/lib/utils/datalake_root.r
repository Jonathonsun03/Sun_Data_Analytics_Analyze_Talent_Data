get_datalake_root <- function() {
  env_root <- Sys.getenv("TALENT_DATALAKE_ROOT", unset = "")
  if (nzchar(env_root)) return(env_root)

  if (.Platform$OS.type == "windows") {
    return("X:/datalake/DataLake/Sun_Data_Analytics/Talent_data")
  }

  linux_default <- "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data"
  if (dir.exists(linux_default)) return(linux_default)

  stop("Datalake root not configured. Set TALENT_DATALAKE_ROOT.")
}
