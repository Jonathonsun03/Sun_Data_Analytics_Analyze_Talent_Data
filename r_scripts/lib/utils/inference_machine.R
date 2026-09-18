# Nested batch scope. Physical host and inference container are separate.
if (!exists("inference_machine_state", inherits = FALSE)) {
  inference_machine_state <- new.env(parent = emptyenv())
  inference_machine_state$scope <- FALSE
  inference_machine_state$active <- FALSE
}

inference_machine_config <- function() {
  config <- list(
    host = Sys.getenv("INFERENCE_MACHINE_HOST", "192.168.1.161"),
    user = Sys.getenv("INFERENCE_MACHINE_SSH_USER", "root"),
    container_host = Sys.getenv("INFERENCE_CONTAINER_HOST", "192.168.1.173"),
    container_user = Sys.getenv("INFERENCE_CONTAINER_SSH_USER", "jonathon"),
    ctid = Sys.getenv("INFERENCE_CONTAINER_ID", "106"),
    manage_container = Sys.getenv("INFERENCE_MANAGE_CONTAINER", "false"),
    api_url = Sys.getenv("NLP_INFERENCE_URL", "http://192.168.1.173:8000"),
    mac = Sys.getenv("INFERENCE_MACHINE_MAC", "10:7B:44:93:28:E2"),
    timeout = suppressWarnings(as.numeric(Sys.getenv("INFERENCE_MACHINE_BOOT_TIMEOUT_SEC", "180"))),
    lock_path = Sys.getenv("INFERENCE_MACHINE_LOCK_DIR", "/tmp/sun-data-inference-machine.lock"),
    guard = Sys.getenv("INFERENCE_MACHINE_SHUTDOWN_GUARD_COMMAND", "")
  )
  for (name in c("host", "container_host")) {
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9.-]*$", config[[name]])) stop("Invalid ", name, call. = FALSE)
  }
  for (name in c("user", "container_user")) {
    if (!grepl("^[A-Za-z_][A-Za-z0-9_-]*$", config[[name]])) stop("Invalid ", name, call. = FALSE)
  }
  if (!grepl("^[1-9][0-9]*$", config$ctid) ||
      !grepl("^([[:xdigit:]]{2}:){5}[[:xdigit:]]{2}$", config$mac) ||
      !is.finite(config$timeout) || config$timeout <= 0 ||
      !config$manage_container %in% c("true", "false")) {
    stop("Invalid inference lifecycle configuration.", call. = FALSE)
  }
  api <- httr::parse_url(config$api_url)
  if (!identical(api$hostname, config$container_host) || !api$scheme %in% c("http", "https")) {
    stop("NLP_INFERENCE_URL must address INFERENCE_CONTAINER_HOST.", call. = FALSE)
  }
  config$manage_container <- identical(config$manage_container, "true")
  config
}

inference_punctuation_url <- function() {
  Sys.getenv("SUBTITLE_PUNCTUATION_URL", paste0(sub("/+$", "", Sys.getenv(
    "NLP_INFERENCE_URL", "http://192.168.1.173:8000"
  )), "/v1/punctuate"))
}

inference_machine_ssh <- function(target, command, timeout = 30) {
  system2("ssh", c(
    "-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
    shQuote(target), shQuote(command)
  ), stdout = FALSE, stderr = FALSE, timeout = timeout)
}

inference_machine_wait <- function(check, timeout, label) {
  deadline <- Sys.time() + timeout
  repeat {
    ready <- tryCatch(isTRUE(check()), error = function(e) FALSE)
    if (ready) return(invisible(TRUE))
    if (Sys.time() >= deadline) stop("Timed out waiting for ", label, call. = FALSE)
    Sys.sleep(2)
  }
}

inference_machine_api_ready <- function(url) {
  response <- httr::GET(paste0(sub("/+$", "", url), "/health"), httr::timeout(5))
  if (httr::status_code(response) != 200L) return(FALSE)
  body <- httr::content(response, as = "parsed", type = "application/json")
  identical(body$status, "ok")
}

inference_machine_wait_off <- function(host) {
  deadline <- Sys.time() + 60
  repeat {
    reachable <- tryCatch({
      connection <- suppressWarnings(socketConnection(host, 22L, open = "r+b", timeout = 2))
      close(connection)
      TRUE
    }, error = function(e) FALSE)
    if (!reachable) {
      Sys.sleep(15)
      return(TRUE)
    }
    if (Sys.time() >= deadline) return(FALSE)
    Sys.sleep(2)
  }
}

inference_machine_finish <- function() {
  config <- inference_machine_state$config
  inference_machine_state$active <- FALSE
  container_target <- paste0(config$container_user, "@", config$container_host)
  release_command <- paste0(
    "curl -fsS -o /dev/null -X POST http://127.0.0.1:8000/",
    "internal/batches/release/",
    inference_machine_state$batch_token
  )
  release_status <- tryCatch(
    inference_machine_ssh(container_target, release_command),
    error = function(e) 255L
  )
  if (!identical(release_status, 0L)) {
    warning("Inference batch reservation could not be released; host left on and lock retained at ",
            config$lock_path, call. = FALSE)
    return(invisible(NULL))
  }
  # An error/timeout may leave a request running server-side. Never infer idle
  # from completion of this R scope, SSH reachability, or the health endpoint.
  if (!nzchar(config$guard)) {
    message("Inference host left on: no verified atomic workload/shutdown guard configured.")
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
  if (!dir.create(config$lock_path, showWarnings = FALSE, mode = "0700")) {
    stop("Inference machine is reserved, or a stale lock needs inspection: ", config$lock_path, call. = FALSE)
  }
  # Failed startup releases the reservation, leaving the host on for inspection.
  started <- FALSE
  on.exit(if (!started) unlink(config$lock_path, recursive = TRUE), add = TRUE)
  api_ready <- tryCatch(
    isTRUE(inference_machine_api_ready(config$api_url)),
    error = function(error) FALSE
  )
  if (api_ready) {
    message("Inference API already ready; wake-on-LAN skipped.")
  } else {
    message("Inference API unavailable; sending wake-on-LAN.")
    status <- system2("wakeonlan", shQuote(config$mac))
    if (!identical(status, 0L)) stop("Wake-on-LAN failed.", call. = FALSE)
    host_target <- paste0(config$user, "@", config$host)
    inference_machine_wait(function() {
      identical(inference_machine_ssh(host_target, "true", timeout = 10), 0L)
    }, config$timeout, paste0("Proxmox SSH: ", host_target))
    if (config$manage_container) {
      command <- paste0(
        "state=$(pct status ", config$ctid, ") || exit 1; ",
        "case \"$state\" in 'status: running') exit 0;; ",
        "'status: stopped') pct start ", config$ctid, ";; *) exit 1;; esac"
      )
      if (!identical(inference_machine_ssh(
        host_target,
        command,
        timeout = config$timeout
      ), 0L)) {
        stop("Could not ensure CT ", config$ctid, " is running.", call. = FALSE)
      }
    }
  }
  container_target <- paste0(config$container_user, "@", config$container_host)
  inference_machine_wait(function() {
    identical(inference_machine_ssh(container_target, "true", timeout = 10), 0L)
  }, config$timeout, paste0("container SSH: ", container_target))
  if (!api_ready) {
    inference_machine_wait(
      function() inference_machine_api_ready(config$api_url),
      config$timeout,
      paste0("inference API: ", config$api_url)
    )
  }
  batch_token <- paste0(sample(c(letters, LETTERS, 0:9, "_", "-"), 64L, replace = TRUE),
                        collapse = "")
  reserve_command <- paste0(
    "curl -fsS -o /dev/null -X POST http://127.0.0.1:8000/",
    "internal/batches/reserve/",
    batch_token
  )
  if (!identical(inference_machine_ssh(container_target, reserve_command), 0L)) {
    stop("Could not reserve the inference machine for this batch.", call. = FALSE)
  }
  inference_machine_state$config <- config
  inference_machine_state$batch_token <- batch_token
  inference_machine_state$active <- TRUE
  started <- TRUE
  invisible(TRUE)
}
