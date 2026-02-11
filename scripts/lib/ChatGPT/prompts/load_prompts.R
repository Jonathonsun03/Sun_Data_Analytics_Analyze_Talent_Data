chatgpt_prompts_dir <- function(repo_root = NULL, rel_path = file.path("scripts", "lib", "ChatGPT", "prompts", "text")) {
  repo_root <- chatgpt_repo_root(repo_root)
  file.path(repo_root, rel_path)
}

chatgpt_list_prompts <- function(prompts_dir = NULL, ext = ".txt", repo_root = NULL) {
  if (is.null(prompts_dir)) {
    prompts_dir <- chatgpt_prompts_dir(repo_root = repo_root)
  }
  if (!dir.exists(prompts_dir)) {
    return(character(0))
  }
  files <- list.files(prompts_dir, pattern = paste0("\\", ext, "$"), full.names = FALSE)
  sub(paste0("\\", ext, "$"), "", files)
}

chatgpt_load_prompt <- function(name, prompts_dir = NULL, ext = ".txt", repo_root = NULL) {
  if (!nzchar(name)) {
    stop("Prompt name is required.")
  }
  if (is.null(prompts_dir)) {
    prompts_dir <- chatgpt_prompts_dir(repo_root = repo_root)
  }
  path <- file.path(prompts_dir, paste0(name, ext))
  if (!file.exists(path)) {
    stop("Prompt not found: ", path)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

chatgpt_render_prompt <- function(prompt, data = list()) {
  if (length(data) == 0) {
    return(prompt)
  }
  out <- prompt
  for (nm in names(data)) {
    out <- gsub(paste0("\\{\\{", nm, "\\}\\}"), as.character(data[[nm]]), out, fixed = FALSE)
  }
  out
}
