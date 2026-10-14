inference_machine_finish <- function() {
  config <- inference_machine_state$config
  inference_machine_state$active <- FALSE
  on.exit({
    close(inference_machine_state$lock_connection)
    inference_machine_state$lock_connection <- NULL
  }, add = TRUE)
  if (!inference_machine_release_batch(config, inference_machine_state$batch_token)) {
    warning("Inference batch reservation could not be released; lock retained at ",
            config$lock_path, call. = FALSE)
    return(invisible(NULL))
  }
  # An error/timeout may leave a request running server-side. Never infer idle
  # from completion of this R scope, SSH reachability, or the health endpoint.
  if (!nzchar(config$guard)) {
    message("Inference batch reservation released.")
    unlink(config$lock_path, recursive = TRUE)
    return(invisible(NULL))
  }
  # Guard runs on Proxmox, must atomically drain/check all work and invoke its
  # argument only when safe. The old separate idle-check setting is not used.
  status <- tryCatch(inference_machine_ssh(
    paste0(config$user, "@", config$host),
    paste(config$guard, shQuote("shutdown -h now"))
  ), error = function(e) 255L)
  if (identical(status, 75L)) {
    message("Inference host left on: shutdown guard reports busy or inconclusive.")
    unlink(config$lock_path, recursive = TRUE)
  } else if (inference_machine_wait_off(config$host)) {
    message("Inference host shutdown confirmed.")
    unlink(config$lock_path, recursive = TRUE)
  } else {
    warning("Shutdown guard failed or shutdown was not confirmed; lock retained at ",
            config$lock_path, call. = FALSE)
  }
}
