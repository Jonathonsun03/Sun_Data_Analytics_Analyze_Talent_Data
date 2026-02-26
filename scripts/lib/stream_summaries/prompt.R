ss_load_prompt_parts <- function(prompt_path) {
  lines <- readLines(prompt_path, warn = FALSE)
  sys_idx <- which(toupper(lines) == "---SYSTEM---")
  user_idx <- which(toupper(lines) == "---USER---")
  if (length(sys_idx) != 1 || length(user_idx) != 1 || sys_idx >= user_idx) {
    stop("Prompt file must contain ---SYSTEM--- followed by ---USER---.")
  }

  list(
    system_prompt = paste(lines[(sys_idx + 1):(user_idx - 1)], collapse = "\n"),
    user_prompt = paste(lines[(user_idx + 1):length(lines)], collapse = "\n"),
    prompt_md5 = unname(tools::md5sum(prompt_path))
  )
}

ss_safe_title <- function(x) {
  gsub("[/\\\\]", "_", x)
}

