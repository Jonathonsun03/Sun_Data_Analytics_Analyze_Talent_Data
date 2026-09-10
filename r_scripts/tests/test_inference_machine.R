# Network and power operations are mocked; no host is powered on or off.
local({
  source("r_scripts/lib/utils/inference_machine.R", local = TRUE)
  names <- c(
    "INFERENCE_MACHINE_HOST", "INFERENCE_MACHINE_SSH_USER", "INFERENCE_MACHINE_MAC",
    "INFERENCE_CONTAINER_HOST", "INFERENCE_CONTAINER_SSH_USER", "INFERENCE_CONTAINER_ID",
    "INFERENCE_MANAGE_CONTAINER", "NLP_INFERENCE_URL", "SUBTITLE_PUNCTUATION_URL",
    "INFERENCE_MACHINE_BOOT_TIMEOUT_SEC", "INFERENCE_MACHINE_LOCK_DIR",
    "INFERENCE_MACHINE_SHUTDOWN_GUARD_COMMAND", "INFERENCE_MACHINE_IDLE_CHECK_COMMAND"
  )
  previous <- Sys.getenv(names, unset = NA_character_)
  lock_path <- tempfile("inference-lock-")
  on.exit({
    unlink(lock_path, recursive = TRUE)
    for (name in names(previous)) {
      if (is.na(previous[[name]])) Sys.unsetenv(name) else
        do.call(Sys.setenv, setNames(list(previous[[name]]), name))
    }
  })
  Sys.unsetenv(names)
  Sys.setenv(INFERENCE_MACHINE_LOCK_DIR = lock_path)
  config <- inference_machine_config()
  stopifnot(config$host == "192.168.1.161", config$user == "root",
            config$container_host == "192.168.1.173", config$container_user == "jonathon",
            config$ctid == "106", !config$manage_container)
  url <- inference_punctuation_url()
  stopifnot(url == "http://192.168.1.173:8000/v1/punctuate")
  calls <- list()
  record <- function(...) calls[[length(calls) + 1L]] <<- list(...)
  Sys.which <- function(x) setNames(paste0("/mock/", x), x)
  system2 <- function(command, args, ...) {
    stopifnot(command == "wakeonlan", args == shQuote("10:7B:44:93:28:E2"))
    record("wake", args)
    0L
  }
  guard_status <- 0L
  fail_stage <- ""
  inference_machine_ssh <- function(target, command, ...) {
    record("ssh", target, command)
    if (startsWith(command, "verified-guard")) {
      stopifnot(target == "root@192.168.1.161", grepl("shutdown -h now", command, fixed = TRUE))
      return(guard_status)
    }
    if (startsWith(command, "state=")) {
      stopifnot(target == "root@192.168.1.161",
                grepl("pct status 106", command, fixed = TRUE),
                grepl("pct start 106", command, fixed = TRUE))
      return(if (fail_stage == "ct") 1L else 0L)
    }
    stopifnot(command == "true", target %in% c("root@192.168.1.161", "jonathon@192.168.1.173"))
    if (target == fail_stage) 255L else 0L
  }
  inference_machine_api_ready <- function(url) {
    record("health", url)
    stopifnot(url == "http://192.168.1.173:8000")
    fail_stage != "health"
  }
  inference_machine_wait <- function(check, ...) {
    if (!check()) stop("mock readiness failure")
  }
  inference_machine_wait_off <- function(host) {
    stopifnot(host == "192.168.1.161")
    record("off", host)
    TRUE
  }
  fails <- function(code) {
    failed <- tryCatch({ force(code); FALSE }, error = function(e) TRUE)
    stopifnot(failed)
  }
  kinds <- function() vapply(calls, `[[`, character(1), 1L)
  with_inference_machine(NULL)
  with_inference_machine(ensure_inference_machine("https://example.com"))
  fails(with_inference_machine(ensure_inference_machine("http://192.168.1.161:8000/v1/punctuate")))
  stopifnot(length(calls) == 0L)
  # Missing guard and legacy idle check never issue shutdown.
  Sys.setenv(INFERENCE_MACHINE_IDLE_CHECK_COMMAND = "true")
  with_inference_machine({
    ensure_inference_machine(url)
    with_inference_machine(ensure_inference_machine(url))
  })
  stopifnot(identical(kinds(), c("wake", "ssh", "ssh", "health")), !dir.exists(lock_path))
  calls <- list()
  Sys.setenv(INFERENCE_MACHINE_SHUTDOWN_GUARD_COMMAND = "verified-guard", INFERENCE_MANAGE_CONTAINER = "true")
  with_inference_machine({
    ensure_inference_machine(url)
    with_inference_machine(ensure_inference_machine(url))
  })
  stopifnot(identical(kinds(), c("wake", "ssh", "ssh", "ssh", "health", "ssh", "off")),
            !dir.exists(lock_path))
  # Errors still run the guard; busy never reaches the shutdown wait.
  calls <- list()
  guard_status <- 75L
  fails(with_inference_machine({ ensure_inference_machine(url); stop("model failure") }))
  stopifnot(!"off" %in% kinds(), !dir.exists(lock_path))
  # Transport/check errors keep the host on and retain the reservation.
  calls <- list()
  guard_status <- 255L
  suppressWarnings(with_inference_machine(ensure_inference_machine(url)))
  stopifnot(!"off" %in% kinds(), dir.exists(lock_path))
  calls <- list()
  fails(with_inference_machine(ensure_inference_machine(url)))
  stopifnot(length(calls) == 0L)
  unlink(lock_path, recursive = TRUE)
  # Failed startup never runs a shutdown guard, even if configured.
  guard_status <- 0L
  for (stage in c("root@192.168.1.161", "ct", "jonathon@192.168.1.173", "health")) {
    calls <- list()
    fail_stage <- stage
    fails(with_inference_machine(ensure_inference_machine(url)))
    stopifnot(!dir.exists(lock_path), !inference_machine_state$active,
              !any(vapply(calls, function(x) any(grepl("verified-guard", unlist(x))), logical(1))))
  }
})
cat("inference machine lifecycle tests passed\n")
