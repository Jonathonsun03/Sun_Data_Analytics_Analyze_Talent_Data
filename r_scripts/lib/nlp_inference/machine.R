inference_machine_ready <- function(config) {
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
        "'status: stopped') pct start ", config$ctid,
        " || test \"$(pct status ", config$ctid, ")\" = 'status: running';; ",
        "*) exit 1;; esac"
      )
      # SSH can accept connections before Proxmox finishes booting its guests.
      # Retry transient status/start failures, including another starter's lock.
      inference_machine_wait(function() {
        identical(inference_machine_ssh(
          host_target, command, timeout = config$timeout
        ), 0L)
      }, config$timeout, paste0("running inference CT ", config$ctid))
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
