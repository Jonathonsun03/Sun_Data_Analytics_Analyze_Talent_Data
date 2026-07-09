library(jsonlite)

normalize_code_field <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

extract_primary_code <- function(text, fallback_name) {
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  idx <- grep("^\\s*PRIMARY CODE\\s*:\\s*", lines, ignore.case = TRUE)
  if (length(idx) == 0) {
    return(fallback_name)
  }
  code <- sub("^\\s*PRIMARY CODE\\s*:\\s*", "", lines[[idx[[1]]]], ignore.case = TRUE)
  code <- trimws(code)
  if (!nzchar(code)) {
    fallback_name
  } else {
    code
  }
}

load_definition_texts <- function(definitions_dir) {
  if (!dir.exists(definitions_dir)) {
    stop("Missing definitions directory: ", definitions_dir)
  }
  files <- sort(list.files(definitions_dir, pattern = "\\.txt$", full.names = TRUE))
  if (length(files) == 0) {
    stop("No definition files found in: ", definitions_dir)
  }
  lapply(files, function(path) {
    text <- read_text_file(path)
    fallback <- tools::file_path_sans_ext(basename(path))
    primary_code <- extract_primary_code(text, fallback_name = toupper(fallback))
    field_name <- normalize_code_field(primary_code)
    if (!nzchar(field_name)) {
      field_name <- normalize_code_field(fallback)
    }
    list(
      name = basename(path),
      text = text,
      primary_code = primary_code,
      field_name = field_name
    )
  })
}

merge_definitions <- function(base_definitions, talent_definitions) {
  if (length(talent_definitions) == 0) {
    return(base_definitions)
  }
  merged <- base_definitions
  by_field <- vapply(merged, `[[`, character(1), "field_name")
  for (defn in talent_definitions) {
    idx <- which(by_field == defn$field_name)
    if (length(idx) > 0) {
      merged[[idx[[1]]]] <- defn
      by_field[[idx[[1]]]] <- defn$field_name
    } else {
      merged[[length(merged) + 1L]] <- defn
      by_field <- c(by_field, defn$field_name)
    }
  }
  merged
}

compile_user_prompt_template <- function(
    instructions_text,
    definitions,
    overlay_text,
    content_type_rules_text = NULL
) {
  definition_sections <- vapply(
    definitions,
    function(x) {
      paste0("### ", x$name, "\n", x$text)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  definitions_text <- paste(definition_sections, collapse = "\n\n")
  definition_columns <- paste(
    vapply(
      definitions,
      function(x) {
        paste0("- ", x$field_name, " (from ", x$primary_code, "): boolean")
      },
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    ),
    collapse = "\n"
  )

  parts <- c(
    instructions_text,
    ""
  )
  if (!is.null(content_type_rules_text) && nzchar(content_type_rules_text)) {
    parts <- c(
      parts,
      "Content type moderation rules:",
      content_type_rules_text,
      ""
    )
  }
  parts <- c(
    parts,
    "Definition output columns (required in classification object):",
    definition_columns,
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
    "{{records_json}}"
  )

  paste(parts, collapse = "\n")
}

extend_schema_with_definitions <- function(schema, definitions) {
  if (length(definitions) == 0) {
    return(schema)
  }

  class_props <- schema$properties$items$items$properties$classification$properties
  class_required <- schema$properties$items$items$properties$classification$required

  for (defn in definitions) {
    field_name <- defn$field_name
    if (!nzchar(field_name)) {
      next
    }
    class_props[[field_name]] <- list(
      type = "boolean",
      description = paste0("Derived from definition ", defn$primary_code)
    )
    class_required <- unique(c(class_required, field_name))
  }

  schema$properties$items$items$properties$classification$properties <- class_props
  schema$properties$items$items$properties$classification$required <- class_required
  schema
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
  content_type_rules_path <- NULL
  profile_dir <- dirname(overlay_path)
  talent_definitions_dir <- file.path(profile_dir, "definitions")
  talent_content_type_rules_path <- file.path(profile_dir, "content_type_rules.txt")

  if (!is.null(cfg$paths$instructions) && nzchar(cfg$paths$instructions)) {
    instructions_path <- resolve_classification_path(classification_root, cfg$paths$instructions)
  }
  if (!is.null(cfg$paths$definitions_dir) && nzchar(cfg$paths$definitions_dir)) {
    definitions_dir <- resolve_classification_path(classification_root, cfg$paths$definitions_dir)
  }
  if (!is.null(cfg$paths$user_template) && nzchar(cfg$paths$user_template)) {
    user_template_path <- resolve_classification_path(classification_root, cfg$paths$user_template)
  }
  if (!is.null(cfg$paths$content_type_rules) && nzchar(cfg$paths$content_type_rules)) {
    content_type_rules_path <- resolve_classification_path(classification_root, cfg$paths$content_type_rules)
  }

  required_paths <- c(system_path, schema_path, overlay_path)
  if (!is.null(instructions_path)) {
    required_paths <- c(required_paths, instructions_path)
  }
  if (is.null(instructions_path) || is.null(definitions_dir)) {
    required_paths <- c(required_paths, user_template_path)
  } else {
    if (!is.null(content_type_rules_path) && !file.exists(talent_content_type_rules_path)) {
      required_paths <- c(required_paths, content_type_rules_path)
    }
  }
  missing_paths <- required_paths[!file.exists(required_paths)]
  if (length(missing_paths) > 0) {
    stop("Missing prompt assets: ", paste(missing_paths, collapse = ", "))
  }

  system_prompt <- read_text_file(system_path)
  overlay_text <- read_text_file(overlay_path)
  base_schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  schema <- base_schema
  schema_text <- jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE, null = "null")

  used_compiler <- !is.null(instructions_path) && !is.null(definitions_dir)
  if (used_compiler) {
    definitions <- load_definition_texts(definitions_dir)
    if (dir.exists(talent_definitions_dir)) {
      talent_definition_files <- list.files(
        talent_definitions_dir,
        pattern = "\\.txt$",
        full.names = TRUE
      )
      if (length(talent_definition_files) > 0) {
        talent_definitions <- load_definition_texts(talent_definitions_dir)
        definitions <- merge_definitions(definitions, talent_definitions)
      }
    }
    schema <- extend_schema_with_definitions(base_schema, definitions)
    schema_text <- jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE, null = "null")
    instructions_text <- read_text_file(instructions_path)
    if (file.exists(talent_content_type_rules_path)) {
      content_type_rules_text <- read_text_file(talent_content_type_rules_path)
    } else {
      content_type_rules_text <- if (!is.null(content_type_rules_path)) read_text_file(content_type_rules_path) else NULL
    }
    user_prompt_template <- compile_user_prompt_template(
      instructions_text = instructions_text,
      definitions = definitions,
      overlay_text = overlay_text,
      content_type_rules_text = content_type_rules_text
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
    definition_fields = unique(vapply(definitions, `[[`, character(1), "field_name")),
    profile_dir = profile_dir,
    schema_path = schema_path,
    schema_text = schema_text,
    schema = schema
  )
}
