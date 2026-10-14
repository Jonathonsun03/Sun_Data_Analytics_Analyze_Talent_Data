inference_machine_api_ready <- function(url) {
  response <- httr::GET(paste0(sub("/+$", "", url), "/health"), httr::timeout(5))
  if (httr::status_code(response) != 200L) return(FALSE)
  body <- httr::content(response, as = "parsed", type = "application/json")
  identical(body$status, "ok")
}

inference_machine_reserve_batch <- function(config, token) {
  container_target <- paste0(config$container_user, "@", config$container_host)
  reserve_command <- paste0(
    "curl -fsS -o /dev/null -X POST http://127.0.0.1:8000/",
    "internal/batches/reserve/",
    token
  )
  if (!identical(inference_machine_ssh(container_target, reserve_command), 0L)) {
    stop("Could not reserve the inference machine for this batch.", call. = FALSE)
  }
}


inference_machine_release_batch <- function(config, token) {
  command <- paste0(
    "curl -fsS -o /dev/null -X POST http://127.0.0.1:8000/",
    "internal/batches/release/", token
  )
  tryCatch(
    identical(inference_machine_ssh(
      paste0(config$container_user, "@", config$container_host), command
    ), 0L),
    error = function(error) FALSE
  )
}
