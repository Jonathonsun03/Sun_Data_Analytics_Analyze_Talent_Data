chatgpt_section_dir <- function(
    template,
    section,
    repo_root = NULL,
    templates_root = file.path("Templates"),
    sections_dir = "sections"
) {
  if (!nzchar(template)) {
    stop("Template name is required.")
  }
  if (!nzchar(section)) {
    stop("Section name is required.")
  }
  repo_root <- chatgpt_repo_root(repo_root)
  file.path(repo_root, templates_root, template, sections_dir, section)
}

chatgpt_section_paths <- function(
    template,
    section,
    repo_root = NULL,
    prompt_file = "prompt.txt",
    input_file = "input.md",
    output_file = "output.md",
    templates_root = file.path("Templates"),
    sections_dir = "sections"
) {
  section_dir <- chatgpt_section_dir(
    template = template,
    section = section,
    repo_root = repo_root,
    templates_root = templates_root,
    sections_dir = sections_dir
  )
  list(
    section_dir = section_dir,
    prompt_path = file.path(section_dir, prompt_file),
    input_path = file.path(section_dir, input_file),
    output_path = file.path(section_dir, output_file)
  )
}

chatgpt_load_section_prompt <- function(
    template,
    section,
    repo_root = NULL,
    prompt_file = "prompt.txt",
    templates_root = file.path("Templates"),
    sections_dir = "sections"
) {
  paths <- chatgpt_section_paths(
    template = template,
    section = section,
    repo_root = repo_root,
    prompt_file = prompt_file,
    templates_root = templates_root,
    sections_dir = sections_dir
  )
  if (!file.exists(paths$prompt_path)) {
    stop("Prompt not found: ", paths$prompt_path)
  }
  paste(readLines(paths$prompt_path, warn = FALSE), collapse = "\n")
}

chatgpt_load_section_prompt_parts <- function(
    template,
    section,
    repo_root = NULL,
    prompt_file = "prompt.txt",
    templates_root = file.path("Templates"),
    sections_dir = "sections"
) {
  text <- chatgpt_load_section_prompt(
    template = template,
    section = section,
    repo_root = repo_root,
    prompt_file = prompt_file,
    templates_root = templates_root,
    sections_dir = sections_dir
  )
  parts <- strsplit(text, "\n")[[1]]
  sys_idx <- which(toupper(parts) == "---SYSTEM---")
  user_idx <- which(toupper(parts) == "---USER---")

  if (length(sys_idx) == 0 && length(user_idx) == 0) {
    return(list(system = NULL, user = text))
  }
  if (length(sys_idx) == 0 || length(user_idx) == 0) {
    stop("Prompt must include both ---SYSTEM--- and ---USER--- markers.")
  }
  if (sys_idx > user_idx) {
    stop("---SYSTEM--- must appear before ---USER---.")
  }

  system_text <- paste(parts[(sys_idx + 1):(user_idx - 1)], collapse = "\n")
  user_text <- paste(parts[(user_idx + 1):length(parts)], collapse = "\n")
  list(system = system_text, user = user_text)
}

chatgpt_read_section_input <- function(
    template,
    section,
    repo_root = NULL,
    input_file = "input.md",
    default = "",
    templates_root = file.path("Templates"),
    sections_dir = "sections"
) {
  paths <- chatgpt_section_paths(
    template = template,
    section = section,
    repo_root = repo_root,
    input_file = input_file,
    templates_root = templates_root,
    sections_dir = sections_dir
  )
  if (!file.exists(paths$input_path)) {
    return(default)
  }
  paste(readLines(paths$input_path, warn = FALSE), collapse = "\n")
}

chatgpt_write_section_output <- function(
    template,
    section,
    text,
    repo_root = NULL,
    output_file = "output.md",
    overwrite = TRUE,
    templates_root = file.path("Templates"),
    sections_dir = "sections"
) {
  paths <- chatgpt_section_paths(
    template = template,
    section = section,
    repo_root = repo_root,
    output_file = output_file,
    templates_root = templates_root,
    sections_dir = sections_dir
  )
  if (!dir.exists(paths$section_dir)) {
    dir.create(paths$section_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (file.exists(paths$output_path) && !overwrite) {
    stop("Output exists and overwrite = FALSE: ", paths$output_path)
  }
  writeLines(text, con = paths$output_path)
  invisible(paths$output_path)
}

chatgpt_section_output_path <- function(
    template,
    section,
    repo_root = NULL,
    reports_root = file.path("Reports"),
    sections_dir = "sections",
    output_file = "output.md"
) {
  if (!nzchar(template)) {
    stop("Template name is required.")
  }
  if (!nzchar(section)) {
    stop("Section name is required.")
  }
  repo_root <- chatgpt_repo_root(repo_root)
  file.path(repo_root, reports_root, template, sections_dir, section, output_file)
}

chatgpt_write_report_output <- function(
    template,
    section,
    text,
    repo_root = NULL,
    reports_root = file.path("Reports"),
    sections_dir = "sections",
    output_file = "output.md",
    overwrite = TRUE
) {
  out_path <- chatgpt_section_output_path(
    template = template,
    section = section,
    repo_root = repo_root,
    reports_root = reports_root,
    sections_dir = sections_dir,
    output_file = output_file
  )
  out_dir <- dirname(out_path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (file.exists(out_path) && !overwrite) {
    stop("Output exists and overwrite = FALSE: ", out_path)
  }
  writeLines(text, con = out_path)
  invisible(out_path)
}
