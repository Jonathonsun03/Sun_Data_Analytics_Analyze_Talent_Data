# Nested batch scope. Physical host and inference container are separate.
if (!exists("inference_machine_state", inherits = FALSE)) {
  inference_machine_state <- new.env(parent = emptyenv())
  inference_machine_state$scope <- FALSE
  inference_machine_state$active <- FALSE
}

with_inference_machine <- function(code) {
  if (isTRUE(inference_machine_state$scope)) {
    return(eval(substitute(code), envir = parent.frame()))
  }
  inference_machine_state$scope <- TRUE
  on.exit({
    inference_machine_state$scope <- FALSE
    if (isTRUE(inference_machine_state$active)) inference_machine_finish()
  }, add = TRUE)
  eval(substitute(code), envir = parent.frame())
}

ensure_inference_machine <- function(url) {
  parsed <- httr::parse_url(url)
  host <- Sys.getenv("INFERENCE_MACHINE_HOST", "192.168.1.161")
  container_host <- Sys.getenv("INFERENCE_CONTAINER_HOST", "192.168.1.173")
  if (identical(parsed$hostname, host)) {
    stop("Inference URL targets the physical host; use the container API address.", call. = FALSE)
  }
  if (!identical(parsed$hostname, container_host)) return(invisible(FALSE))
  if (!isTRUE(inference_machine_state$scope)) {
    stop("Use with_inference_machine() around managed requests.", call. = FALSE)
  }
  if (isTRUE(inference_machine_state$active)) return(invisible(TRUE))
  config <- inference_machine_config()
  for (program in c("wakeonlan", "ssh")) {
    if (!nzchar(Sys.which(program))) stop("Required program missing: ", program, call. = FALSE)
  }
  lock_connection <- inference_machine_lock(config$lock_path)
  started <- FALSE
  created <- FALSE
  reservation_attempted <- FALSE
  on.exit({
    if (!started) {
      if (created && (!reservation_attempted ||
          inference_machine_release_batch(config, batch_token))) {
        unlink(config$lock_path, recursive = TRUE)
      }
      close(lock_connection)
    }
  }, add = TRUE)
  inference_machine_recover(config)
  if (!dir.create(config$lock_path, showWarnings = FALSE, mode = "0700")) {
    stop("Inference machine is reserved, or a stale lock needs inspection: ", config$lock_path, call. = FALSE)
  }
  created <- TRUE
  batch_token <- paste0(sample(c(letters, LETTERS, 0:9, "_", "-"), 64L, replace = TRUE),
                        collapse = "")
  inference_machine_save_owner(config, batch_token)
  inference_machine_ready(config)
  reservation_attempted <- TRUE
  inference_machine_reserve_batch(config, batch_token)
  inference_machine_state$config <- config
  inference_machine_state$batch_token <- batch_token
  inference_machine_state$lock_connection <- lock_connection
  inference_machine_state$active <- TRUE
  started <- TRUE
  invisible(TRUE)
}
