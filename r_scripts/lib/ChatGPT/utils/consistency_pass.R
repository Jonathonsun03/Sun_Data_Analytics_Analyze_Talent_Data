chatgpt_list_report_sections <- function(
    template,
    repo_root = NULL,
    reports_root = file.path("Reports"),
    sections_dir = "sections"
) {
  if (!nzchar(template)) {
    stop("Template name is required.")
  }
  repo_root <- chatgpt_repo_root(repo_root)
  base_dir <- file.path(repo_root, reports_root, template, sections_dir)
  if (!dir.exists(base_dir)) {
    stop("Reports sections directory not found: ", base_dir)
  }
  sections <- list.dirs(base_dir, full.names = FALSE, recursive = FALSE)
  sections <- sections[grepl("^\\d+_", sections)]
  if (length(sections) == 0) {
    return(character(0))
  }
  idx <- as.integer(sub("^([0-9]+)_.*", "\\1", sections))
  sections[order(idx)]
}

chatgpt_read_report_output <- function(
    template,
    section,
    repo_root = NULL,
    reports_root = file.path("Reports"),
    sections_dir = "sections",
    output_file = "output.md",
    default = NULL
) {
  if (!nzchar(template)) {
    stop("Template name is required.")
  }
  if (!nzchar(section)) {
    stop("Section name is required.")
  }
  repo_root <- chatgpt_repo_root(repo_root)
  path <- file.path(repo_root, reports_root, template, sections_dir, section, output_file)
  if (!file.exists(path)) {
    if (!is.null(default)) {
      return(default)
    }
    stop("Report output not found: ", path)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

chatgpt_run_consistency_pass <- function(
    template,
    model = "gpt-4.1-mini",
    repo_root = NULL,
    input_sections_dir = "sections",
    output_sections_dir = "sections_v2",
    prompt_template = "Global",
    prompt_sections = list(
      edit = "consistency_edit",
      summary = "consistency_summary",
      intro = "introduction",
      conclusion = "conclusion"
    ),
    intro_section = "0_introduction",
    conclusion_section = "5_conclusion",
    summary_file = "findings_summary.md",
    temperature = 0.2
) {
  sections <- chatgpt_list_report_sections(
    template = template,
    repo_root = repo_root,
    sections_dir = input_sections_dir
  )
  if (length(sections) == 0) {
    stop("No report sections found for template: ", template)
  }

  rolling_summary <- ""
  edited_sections <- list()

  edit_parts <- chatgpt_load_section_prompt_parts(
    template = prompt_template,
    section = prompt_sections$edit,
    repo_root = repo_root
  )
  summary_parts <- chatgpt_load_section_prompt_parts(
    template = prompt_template,
    section = prompt_sections$summary,
    repo_root = repo_root
  )
  intro_parts <- chatgpt_load_section_prompt_parts(
    template = prompt_template,
    section = prompt_sections$intro,
    repo_root = repo_root
  )
  conclusion_parts <- chatgpt_load_section_prompt_parts(
    template = prompt_template,
    section = prompt_sections$conclusion,
    repo_root = repo_root
  )

  for (section in sections) {
    section_text <- chatgpt_read_report_output(
      template = template,
      section = section,
      repo_root = repo_root,
      sections_dir = input_sections_dir
    )

    user_prompt <- chatgpt_render_prompt(
      edit_parts$user,
      list(
        ROLLING_SUMMARY = if (nzchar(rolling_summary)) rolling_summary else "(none yet)",
        SECTION_TEXT = section_text
      )
    )

    messages <- chatgpt_make_messages(
      system_prompt = edit_parts$system,
      user_prompt = user_prompt
    )

    response <- chatgpt_send_chat(
      messages,
      model = model,
      temperature = temperature
    )
    edited_text <- chatgpt_extract_text(response)

    chatgpt_write_report_output(
      template = template,
      section = section,
      text = edited_text,
      sections_dir = output_sections_dir,
      overwrite = TRUE
    )
    edited_sections[[section]] <- edited_text

    summary_prompt <- chatgpt_render_prompt(
      summary_parts$user,
      list(
        PRIOR_SUMMARY = if (nzchar(rolling_summary)) rolling_summary else "(none yet)",
        NEW_SECTION_TEXT = edited_text
      )
    )
    summary_messages <- chatgpt_make_messages(
      system_prompt = summary_parts$system,
      user_prompt = summary_prompt
    )
    summary_response <- chatgpt_send_chat(
      summary_messages,
      model = model,
      temperature = temperature
    )
    rolling_summary <- chatgpt_extract_text(summary_response)
  }

  intro_user <- chatgpt_render_prompt(
    intro_parts$user,
    list(FINDINGS_SUMMARY = if (nzchar(rolling_summary)) rolling_summary else "(no summary)")
  )
  intro_messages <- chatgpt_make_messages(
    system_prompt = intro_parts$system,
    user_prompt = intro_user
  )
  intro_response <- chatgpt_send_chat(
    intro_messages,
    model = model,
    temperature = temperature
  )
  intro_text <- chatgpt_extract_text(intro_response)

  chatgpt_write_report_output(
    template = template,
    section = intro_section,
    text = intro_text,
    sections_dir = output_sections_dir,
    overwrite = TRUE
  )

  conclusion_user <- chatgpt_render_prompt(
    conclusion_parts$user,
    list(FINDINGS_SUMMARY = if (nzchar(rolling_summary)) rolling_summary else "(no summary)")
  )
  conclusion_messages <- chatgpt_make_messages(
    system_prompt = conclusion_parts$system,
    user_prompt = conclusion_user
  )
  conclusion_response <- chatgpt_send_chat(
    conclusion_messages,
    model = model,
    temperature = temperature
  )
  conclusion_text <- chatgpt_extract_text(conclusion_response)

  chatgpt_write_report_output(
    template = template,
    section = conclusion_section,
    text = conclusion_text,
    sections_dir = output_sections_dir,
    overwrite = TRUE
  )

  if (nzchar(summary_file)) {
    summary_dir <- file.path(
      chatgpt_repo_root(repo_root),
      "Reports",
      template,
      output_sections_dir
    )
    if (!dir.exists(summary_dir)) {
      dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
    }
    writeLines(rolling_summary, con = file.path(summary_dir, summary_file))
  }

  invisible(list(
    sections = sections,
    output_sections_dir = output_sections_dir,
    rolling_summary = rolling_summary,
    edited_sections = edited_sections
  ))
}
