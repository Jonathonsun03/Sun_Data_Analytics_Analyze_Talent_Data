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
