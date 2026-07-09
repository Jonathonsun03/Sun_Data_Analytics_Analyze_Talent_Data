chatgpt_make_messages <- function(
    system_prompt = NULL,
    user_prompt = NULL,
    data_text = NULL
) {
  messages <- list()
  if (!is.null(system_prompt) && nzchar(system_prompt)) {
    messages <- append(messages, list(list(role = "system", content = system_prompt)))
  }
  content <- paste(
    c(user_prompt, data_text),
    collapse = "\n\n"
  )
  if (!nzchar(content)) {
    stop("User content is empty. Provide `user_prompt` or `data_text`.")
  }
  messages <- append(messages, list(list(role = "user", content = content)))
  messages
}
