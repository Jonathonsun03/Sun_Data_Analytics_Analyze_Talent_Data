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
