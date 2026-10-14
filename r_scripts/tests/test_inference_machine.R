# Network and power operations are mocked; no host is powered on or off.
local({
  source("r_scripts/lib/nlp_inference/load.R", local = TRUE)
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
    unlink(c(lock_path, paste0(lock_path, ".flock")), recursive = TRUE)
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
  api_ready <- TRUE
  Sys.which <- function(x) setNames(paste0("/mock/", x), x)
  system2 <- function(command, args, ...) {
    stopifnot(command == "wakeonlan", args == shQuote("10:7B:44:93:28:E2"))
    record("wake", args)
    api_ready <<- TRUE
    0L
  }
  guard_status <- 0L
  batch_status <- 0L
  batch_release_status <- 0L
  fail_stage <- ""
  inference_machine_ssh <- function(target, command, ...) {
    record("ssh", target, command)
    if (grepl("internal/batches/", command, fixed = TRUE)) {
      stopifnot(target == "jonathon@192.168.1.173")
      if (grepl("internal/batches/release/", command, fixed = TRUE)) {
        return(batch_release_status)
      }
      return(batch_status)
    }
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
    fail_stage != "health" && api_ready
  }
  inference_machine_wait <- function(check, ...) {
    if (!check()) stop("mock readiness failure")
  }
  inference_machine_wait_off <- function(host) {
    stopifnot(host == "192.168.1.161")
    record("off", host)
    guard_status %in% c(0L, 255L)
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
  stopifnot(
    identical(kinds(), c("health", "ssh", "ssh", "ssh")),
    !dir.exists(lock_path)
  )
  # An unavailable API sends one wake packet, then waits for the stack.
  calls <- list()
  api_ready <- FALSE
  with_inference_machine(ensure_inference_machine(url))
  stopifnot(
    identical(
      kinds(),
      c("health", "wake", "ssh", "ssh", "health", "ssh", "ssh")
    ),
    !dir.exists(lock_path)
  )
  calls <- list()
  api_ready <- FALSE
  Sys.setenv(INFERENCE_MACHINE_SHUTDOWN_GUARD_COMMAND = "verified-guard", INFERENCE_MANAGE_CONTAINER = "true")
  with_inference_machine({
    ensure_inference_machine(url)
    with_inference_machine(ensure_inference_machine(url))
  })
  stopifnot(identical(
    kinds(),
    c("health", "wake", "ssh", "ssh", "ssh", "health", "ssh", "ssh", "ssh", "off")
  ),
            !dir.exists(lock_path))
  # Errors still run the guard; busy never reaches the shutdown wait.
  calls <- list()
  api_ready <- TRUE
  guard_status <- 75L
  fails(with_inference_machine({ ensure_inference_machine(url); stop("model failure") }))
  stopifnot(!"off" %in% kinds(), !dir.exists(lock_path))
  # Transport/check errors keep the host on and retain the reservation.
  calls <- list()
  api_ready <- TRUE
  guard_status <- 255L
  suppressWarnings(with_inference_machine(ensure_inference_machine(url)))
  stopifnot("off" %in% kinds(), !dir.exists(lock_path))
  # A failed guard retains the reservation when the host remains reachable.
  calls <- list()
  api_ready <- TRUE
  guard_status <- 1L
  suppressWarnings(with_inference_machine(ensure_inference_machine(url)))
  stopifnot("off" %in% kinds(), dir.exists(lock_path))
  # The next scope can recover retained state under the process lock.
  calls <- list()
  guard_status <- 75L
  with_inference_machine(ensure_inference_machine(url))
  stopifnot(!dir.exists(lock_path), kinds()[[1]] == "ssh")
  unlink(lock_path, recursive = TRUE)
  # A reservation release failure leaves both the host and local lock in place.
  calls <- list()
  api_ready <- TRUE
  batch_release_status <- 1L
  ensure_error <- tryCatch({
    with_inference_machine(ensure_inference_machine(url))
    FALSE
  }, warning = function(e) TRUE)
  stopifnot(ensure_error, !"off" %in% kinds(), dir.exists(lock_path))
  unlink(lock_path, recursive = TRUE)
  batch_release_status <- 0L
  # Failed startup never runs a shutdown guard, even if configured.
  guard_status <- 0L
  for (stage in c("root@192.168.1.161", "ct", "jonathon@192.168.1.173", "health")) {
    calls <- list()
    api_ready <- FALSE
    fail_stage <- stage
    fails(with_inference_machine(ensure_inference_machine(url)))
    stopifnot(!dir.exists(lock_path), !inference_machine_state$active,
              !any(vapply(calls, function(x) any(grepl("verified-guard", unlist(x))), logical(1))))
  }
  fail_stage <- ""
  api_ready <- TRUE
  Sys.unsetenv("INFERENCE_MACHINE_SHUTDOWN_GUARD_COMMAND")
  # An ambiguous reserve failure still releases its persisted token.
  batch_status <- 255L
  calls <- list()
  fails(with_inference_machine(ensure_inference_machine(url)))
  stopifnot(!dir.exists(lock_path), any(vapply(calls, function(call) {
    any(grepl("internal/batches/release/", unlist(call), fixed = TRUE))
  }, logical(1))))
  batch_status <- 0L
  # Legacy/invalid metadata cannot silently clear an unknown reservation.
  dir.create(lock_path)
  calls <- list()
  fails(with_inference_machine(ensure_inference_machine(url)))
  stopifnot(dir.exists(lock_path), length(calls) == 0L)
  unlink(lock_path, recursive = TRUE)

  # Exercise real process death and flock contention, with remote calls mocked.
  for (signal in c(15L, 9L)) {
    child_ready <- tempfile("inference-child-ready-")
    child <- parallel::mcparallel({
      with_inference_machine({
        ensure_inference_machine(url)
        file.create(child_ready)
        Sys.sleep(30)
      })
    }, silent = TRUE)
    deadline <- Sys.time() + 5
    while (!file.exists(child_ready) && Sys.time() < deadline) Sys.sleep(0.02)
    stopifnot(file.exists(child_ready))
    old_owner <- readRDS(file.path(lock_path, "owner.rds"))
    calls <- list()
    fails(with_inference_machine(ensure_inference_machine(url)))
    stopifnot(length(calls) == 0L, dir.exists(lock_path))
    tools::pskill(child$pid, signal = signal)
    suppressWarnings(parallel::mccollect(child))
    unlink(child_ready)
    # Failed release preserves the exact token for a later retry.
    batch_release_status <- 255L
    fails(with_inference_machine(ensure_inference_machine(url)))
    stopifnot(identical(readRDS(file.path(lock_path, "owner.rds")), old_owner))
    batch_release_status <- 0L
    calls <- list()
    with_inference_machine(ensure_inference_machine(url))
    stopifnot(
      !dir.exists(lock_path),
      grepl(paste0("internal/batches/release/", old_owner$token), calls[[1]][[3]], fixed = TRUE),
      !"off" %in% kinds()
    )
  }
})
# SSH may be ready while Proxmox status/start still fails during host boot.
local({
  source("r_scripts/lib/nlp_inference/load.R", local = TRUE)
  config <- inference_machine_config()
  config$manage_container <- TRUE
  config$timeout <- 10
  start_attempts <- 0L
  inference_machine_api_ready <- function(url) FALSE
  system2 <- function(...) 0L
  inference_machine_ssh <- function(target, command, ...) {
    if (startsWith(command, "state=")) {
      start_attempts <<- start_attempts + 1L
      return(if (start_attempts < 3L) 1L else 0L)
    }
    0L
  }
  # The final API poll is ready; no actual network or power calls occur.
  health_checks <- 0L
  inference_machine_api_ready <- function(url) {
    health_checks <<- health_checks + 1L
    health_checks > 1L
  }
  inference_machine_ready(config)
  stopifnot(start_attempts == 3L, health_checks == 2L)
})
cat("inference machine lifecycle tests passed\n")
