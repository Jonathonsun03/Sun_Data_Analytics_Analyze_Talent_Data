get_staging_root <- function() {
  env_root <- Sys.getenv("TALENT_STAGING_ROOT", unset = "")
  if (nzchar(env_root)) return(env_root)
  
  if (.Platform$OS.type == "windows") {
    return("X:/staging/Talent_data")
  }
  
  linux_default <- "/mnt/staging/staging/Talent_data"
  if (dir.exists(linux_default)) return(linux_default)
  
  stop("Staging root not configured. Set TALENT_STAGING_ROOT.")
}
