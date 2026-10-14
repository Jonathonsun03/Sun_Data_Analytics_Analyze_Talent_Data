# Cooperative stop controls and a monotonic per-run processing budget.
if (!exists("subtitle_backfill_budget", inherits = FALSE)) {
  subtitle_backfill_budget <- new.env(parent = emptyenv())
  subtitle_backfill_budget$deadline <- Inf
}

subtitle_backfill_start_budget <- function(seconds = 14400) {
  if (length(seconds) != 1L || !is.finite(seconds) || seconds < 0) {
    stop("Runtime budget must be a finite non-negative number.", call. = FALSE)
  }
  subtitle_backfill_budget$deadline <- if (seconds == 0) Inf else proc.time()[["elapsed"]] + seconds
}

subtitle_backfill_deadline_reached <- function() {
  proc.time()[["elapsed"]] >= subtitle_backfill_budget$deadline
}

subtitle_backfill_stop_request_path <- function() {
  subtitle_backfill_env_text(
    "SUBTITLE_BACKFILL_STOP_REQUEST",
    file.path(
      get_datalake_root(),
      "Logs",
      "subtitle_sentence_backfill", "control",
      "stop.request"
    )
  )
}

subtitle_backfill_active_marker_path <- function() {
  subtitle_backfill_env_text(
    "SUBTITLE_BACKFILL_ACTIVE_MARKER",
    file.path(
      get_datalake_root(),
      "Logs",
      "subtitle_sentence_backfill", "control",
      "active"
    )
  )
}

subtitle_backfill_stop_requested <- function(path) {
  if (subtitle_backfill_deadline_reached()) return(TRUE)
  if (!file.exists(path)) return(FALSE)
  active_path <- file.path(dirname(path), "active")
  if (!file.exists(active_path)) return(TRUE)
  identical(readLines(path, n = 1L, warn = FALSE),
            readLines(active_path, n = 1L, warn = FALSE))
}

subtitle_backfill_request_stop <- function(path) {
  directory <- dirname(path)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, mode = "0770")) {
    stop("Could not create transcript stop-request directory.", call. = FALSE)
  }
  temporary_path <- tempfile("stop-request-", tmpdir = directory)
  on.exit(unlink(temporary_path), add = TRUE)
  writeLines(
    c(
      if (file.exists(file.path(directory, "active"))) {
        readLines(file.path(directory, "active"), n = 1L, warn = FALSE)
      } else {
        paste0("run-", Sys.getpid(), "-", as.numeric(Sys.time()))
      },
      paste0("requested_at_utc=", format(
        Sys.time(),
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )),
      paste0("requesting_pid=", Sys.getpid())
    ),
    temporary_path,
    useBytes = TRUE
  )
  if (!file.rename(temporary_path, path)) {
    stop("Could not publish the transcript stop request.", call. = FALSE)
  }
  invisible(path)
}

subtitle_backfill_clear_marker <- function(path, label) {
  if (file.exists(path) && unlink(path) != 0L) {
    stop("Could not clear the transcript ", label, ".", call. = FALSE)
  }
  invisible(TRUE)
}

subtitle_backfill_clear_stop_request <- function(path) {
  subtitle_backfill_clear_marker(path, "stop request")
}

subtitle_backfill_mark_active <- function(path) {
  subtitle_backfill_request_stop(path)
}

subtitle_backfill_control_status <- function(active_path, stop_path) {
  directory <- dirname(active_path)
  lock_path <- file.path(directory, "run.lock")
  if (!file.exists(lock_path) || file.access(directory, 2L) != 0L) {
    return(list(active = FALSE, requested = FALSE, controllable = FALSE,
                message = "Transcript control channel is unavailable."))
  }
  lock_status <- suppressWarnings(system2(
    "flock", c("--shared", "--nonblock", shQuote(lock_path), "true"),
    stdout = FALSE, stderr = FALSE
  ))
  if (!lock_status %in% c(0L, 1L)) {
    return(list(active = FALSE, requested = FALSE, controllable = FALSE,
                message = "Could not verify transcript runner status."))
  }
  active <- lock_status == 1L
  if (active && !file.exists(active_path)) {
    return(list(active = TRUE, requested = FALSE, controllable = FALSE,
                message = "Transcript runner is starting or finishing; checking again shortly."))
  }
  requested <- active && subtitle_backfill_stop_requested(stop_path)
  result_path <- file.path(directory, "last.result")
  exit_failed <- file.exists(result_path) &&
    !"exit_status=0" %in% readLines(result_path, warn = FALSE)
  list(active = active, requested = requested, controllable = TRUE,
       message = if (requested) {
         "Stop requested; waiting for the transcript runner to exit safely."
       } else if (active) {
         "Transcript backfill is running."
       } else if (exit_failed) {
         "Transcript run exited with an error. Saved checkpoints remain available for the next scheduled run."
       } else if (file.exists(result_path)) {
         "Transcript run stopped. Saved checkpoints are available for the next scheduled run."
       } else {
         "No transcript backfill is running."
       })
}

subtitle_backfill_clear_active <- function(path) {
  subtitle_backfill_clear_marker(path, "active marker")
}

subtitle_backfill_stop_condition <- function() {
  errorCondition(
    message = if (subtitle_backfill_deadline_reached()) {
      "Transcript backfill reached its runtime budget; progress saved for the next run."
    } else "Transcript backfill stopped by dashboard request.",
    class = "subtitle_backfill_stop_requested"
  )
}

subtitle_backfill_assert_running <- function(path) {
  if (subtitle_backfill_stop_requested(path)) {
    stop(subtitle_backfill_stop_condition())
  }
  invisible(TRUE)
}
