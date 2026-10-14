# Keep the flock inode outside the removable runtime directory. The child holds
# it until this R process closes its input pipe (including on SIGTERM/SIGKILL).
inference_machine_lock <- function(path) {
  if (!nzchar(Sys.which("flock"))) stop("Required program missing: flock", call. = FALSE)
  ready <- tempfile("inference-lock-ready-")
  failed <- tempfile("inference-lock-failed-")
  on.exit(unlink(c(ready, failed)), add = TRUE)
  command <- paste(
    "flock -n --", shQuote(paste0(path, ".flock")),
    "sh -c", shQuote(paste("touch", shQuote(ready), "; cat >/dev/null")),
    "|| touch", shQuote(failed)
  )
  connection <- pipe(command, open = "w")
  acquired <- FALSE
  on.exit(if (!acquired) close(connection), add = TRUE)
  deadline <- Sys.time() + 5
  repeat {
    if (file.exists(ready)) break
    if (file.exists(failed) || Sys.time() >= deadline) {
      stop("Inference machine is reserved or its process lock is unavailable: ", path, call. = FALSE)
    }
    Sys.sleep(0.02)
  }
  acquired <- TRUE
  connection
}

inference_machine_save_owner <- function(config, batch_token) {
  owner <- list(
    version = 1L,
    hostname = unname(Sys.info()[["nodename"]]),
    pid = Sys.getpid(),
    container_host = config$container_host,
    container_user = config$container_user,
    token = batch_token
  )
  owner_path <- file.path(config$lock_path, "owner.rds")
  saveRDS(owner, paste0(owner_path, ".tmp"))
  if (!file.rename(paste0(owner_path, ".tmp"), owner_path)) {
    stop("Could not persist inference reservation ownership.", call. = FALSE)
  }
}


inference_machine_recover <- function(config) {
  if (!dir.exists(config$lock_path)) return(invisible(NULL))
  owner <- tryCatch(
    suppressWarnings(readRDS(file.path(config$lock_path, "owner.rds"))),
    error = function(error) NULL
  )
  # Only metadata written under our process-held lock can be recovered. Never
  # guess a token or remove a legacy lock whose owner cannot be established.
  valid <- is.list(owner) && identical(owner$version, 1L) &&
    identical(owner$hostname, unname(Sys.info()[["nodename"]])) &&
    identical(owner$container_host, config$container_host) &&
    identical(owner$container_user, config$container_user) &&
    is.character(owner$token) && length(owner$token) == 1L &&
    !is.na(owner$token) && grepl("^[A-Za-z0-9_-]{32,128}$", owner$token)
  if (!valid) {
    stop("Inference lock has missing or incompatible ownership metadata; manual inspection required: ",
         config$lock_path, call. = FALSE)
  }
  if (!inference_machine_release_batch(config, owner$token)) {
    stop("Could not release stale inference reservation; runtime state retained at ",
         config$lock_path, call. = FALSE)
  }
  if (unlink(config$lock_path, recursive = TRUE) != 0L) {
    stop("Could not remove stale inference runtime state: ", config$lock_path, call. = FALSE)
  }
  message("Released stale inference reservation and cleared its runtime state.")
}
