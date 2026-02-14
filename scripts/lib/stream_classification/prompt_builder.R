library(jsonlite)

load_definition_texts <- function(definitions_dir) {
  if (!dir.exists(definitions_dir)) {
    stop("Missing definitions directory: ", definitions_dir)
  }
  files <- sort(list.files(definitions_dir, pattern = "\\.txt$", full.names = TRUE))
  if (length(files) == 0) {
    stop("No definition files found in: ", definitions_dir)
  }
  lapply(files, function(path) {
    list(
      name = basename(path),
      text = read_text_file(path)
    )
  })
}

compile_user_prompt_template <- function(instructions_text, definitions, overlay_text) {
  definition_sections <- vapply(
    definitions,
    function(x) {
      paste0("### ", x$name, "\n", x$text)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  definitions_text <- paste(definition_sections, collapse = "\n\n")

  paste(
    instructions_text,
    "",
    "Talent context:",
    "- talent_name: {{talent_name}}",
    "- talent_profile: {{talent_profile}}",
    "",
    "Primary taxonomy definitions:",
    definitions_text,
    "",
    "Talent overlay:",
    overlay_text,
    "",
    "Output schema (JSON):",
    "{{schema_json}}",
    "",
    "Input records (JSON):",
    "{{records_json}}",
    sep = "\n"
  )
}

load_prompt_bundle <- function(talent_name, classification_root = "classification") {
  config_path <- file.path(classification_root, "config", "talent_profiles.json")
  cfg <- load_talent_profiles(config_path = config_path)
  selected <- select_talent_profile(
    talent_name = talent_name,
    cfg = cfg,
    classification_root = classification_root
  )
  profile_name <- selected$profile_name
  profile <- selected$profile

  system_path <- resolve_classification_path(classification_root, cfg$paths$system_prompt)
  schema_path <- resolve_classification_path(classification_root, cfg$paths$schema)
  overlay_path <- resolve_classification_path(classification_root, profile$overlay)

  instructions_path <- NULL
  definitions_dir <- NULL
  user_template_path <- NULL

  if (!is.null(cfg$paths$instructions) && nzchar(cfg$paths$instructions)) {
    instructions_path <- resolve_classification_path(classification_root, cfg$paths$instructions)
  }
  if (!is.null(cfg$paths$definitions_dir) && nzchar(cfg$paths$definitions_dir)) {
    definitions_dir <- resolve_classification_path(classification_root, cfg$paths$definitions_dir)
  }
  if (!is.null(cfg$paths$user_template) && nzchar(cfg$paths$user_template)) {
    user_template_path <- resolve_classification_path(classification_root, cfg$paths$user_template)
  }

  required_paths <- c(system_path, schema_path, overlay_path)
  if (!is.null(instructions_path)) {
    required_paths <- c(required_paths, instructions_path)
  }
  if (is.null(instructions_path) || is.null(definitions_dir)) {
    required_paths <- c(required_paths, user_template_path)
  }
  missing_paths <- required_paths[!file.exists(required_paths)]
  if (length(missing_paths) > 0) {
    stop("Missing prompt assets: ", paste(missing_paths, collapse = ", "))
  }

  system_prompt <- read_text_file(system_path)
  overlay_text <- read_text_file(overlay_path)
  schema_text <- read_text_file(schema_path)
  schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)

  used_compiler <- !is.null(instructions_path) && !is.null(definitions_dir)
  if (used_compiler) {
    definitions <- load_definition_texts(definitions_dir)
    instructions_text <- read_text_file(instructions_path)
    user_prompt_template <- compile_user_prompt_template(
      instructions_text = instructions_text,
      definitions = definitions,
      overlay_text = overlay_text
    )
    talent_rules_text <- overlay_text
  } else {
    user_prompt_template <- read_text_file(user_template_path)
    definitions <- list()
    talent_rules_text <- overlay_text
  }

  list(
    taxonomy_version = cfg$taxonomy_version,
    prompt_version = cfg$prompt_version,
    profile_name = profile_name,
    used_compiler = used_compiler,
    system_prompt = system_prompt,
    user_prompt_template = user_prompt_template,
    talent_rules_text = talent_rules_text,
    definitions = definitions,
    schema_path = schema_path,
    schema_text = schema_text,
    schema = schema
  )
}
