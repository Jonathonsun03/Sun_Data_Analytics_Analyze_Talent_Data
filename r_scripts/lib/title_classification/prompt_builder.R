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
