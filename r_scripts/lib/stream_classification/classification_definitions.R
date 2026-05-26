classification_definition_read_text <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

classification_definition_normalize_field <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

classification_definition_extract_primary_code <- function(text, fallback_name) {
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  idx <- grep("^\\s*PRIMARY CODE\\s*:\\s*", lines, ignore.case = TRUE)
  if (length(idx) == 0L) {
    return(fallback_name)
  }

  code <- sub("^\\s*PRIMARY CODE\\s*:\\s*", "", lines[[idx[[1]]]], ignore.case = TRUE)
  code <- trimws(code)
  if (nzchar(code)) code else fallback_name
}

classification_definition_extract_section <- function(text, heading) {
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  heading_pattern <- paste0("^\\s*", gsub(" ", "\\\\s+", heading), "\\s*:\\s*$")
  start <- grep(heading_pattern, lines, ignore.case = TRUE)
  if (length(start) == 0L) {
    return(NA_character_)
  }

  section_heading_pattern <- paste0(
    "^\\s*(",
    paste(
      c(
        "Definition",
        "Inclusion Criteria",
        "Exclusion Criteria",
        "Important Exception",
        "Mapping Guidance"
      ),
      collapse = "|"
    ),
    ")\\s*:\\s*$"
  )

  start <- start[[1]]
  later_headings <- grep(section_heading_pattern, lines, ignore.case = TRUE)
  later_headings <- later_headings[later_headings > start]
  end <- if (length(later_headings) > 0L) later_headings[[1]] - 1L else length(lines)
  if (end <= start) {
    return(NA_character_)
  }

  section <- lines[(start + 1L):end]
  section <- trimws(section)
  section <- section[nzchar(section)]
  if (length(section) == 0L) {
    return(NA_character_)
  }

  paste(section, collapse = "\n")
}

classification_definition_label <- function(primary_code) {
  label <- gsub("_+", " ", tolower(primary_code))
  words <- unlist(strsplit(label, "\\s+"))
  words <- paste0(toupper(substr(words, 1, 1)), substr(words, 2, nchar(words)))
  paste(words, collapse = " ")
}

read_classification_definitions_table <- function(
  definitions_dir = file.path("classification", "prompts", "definitions")
) {
  if (!dir.exists(definitions_dir)) {
    stop("Missing classification definitions directory: ", definitions_dir, call. = FALSE)
  }

  files <- sort(list.files(definitions_dir, pattern = "\\.txt$", full.names = TRUE))
  if (length(files) == 0L) {
    stop("No classification definition files found in: ", definitions_dir, call. = FALSE)
  }

  rows <- lapply(files, function(path) {
    text <- classification_definition_read_text(path)
    fallback <- toupper(tools::file_path_sans_ext(basename(path)))
    primary_code <- classification_definition_extract_primary_code(text, fallback)
    field_name <- classification_definition_normalize_field(primary_code)

    tibble::tibble(
      definition_file = basename(path),
      field_name = field_name,
      primary_code = primary_code,
      label = classification_definition_label(primary_code),
      definition = classification_definition_extract_section(text, "Definition"),
      inclusion_criteria = classification_definition_extract_section(text, "Inclusion Criteria"),
      exclusion_criteria = classification_definition_extract_section(text, "Exclusion Criteria"),
      important_exception = classification_definition_extract_section(text, "Important Exception"),
      mapping_guidance = classification_definition_extract_section(text, "Mapping Guidance"),
      definition_path = normalizePath(path, winslash = "/", mustWork = FALSE),
      full_text = text
    )
  })

  dplyr::bind_rows(rows)
}
