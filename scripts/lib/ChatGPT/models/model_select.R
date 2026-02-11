chatgpt_get_model <- function(
    model = NULL,
    env_name = "OPENAI_MODEL",
    default_model = NULL
) {
  if (!is.null(model) && nzchar(model)) {
    return(model)
  }
  from_env <- Sys.getenv(env_name)
  if (nzchar(from_env)) {
    return(from_env)
  }
  if (!is.null(default_model) && nzchar(default_model)) {
    return(default_model)
  }
  stop(
    "No model provided. Set `model` explicitly or define ", env_name, " in your .env."
  )
}
