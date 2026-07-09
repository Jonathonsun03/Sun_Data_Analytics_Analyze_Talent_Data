library(jsonlite)
library(stringr)

read_text_file <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

normalize_talent_key <- function(x) {
  x <- enc2utf8(as.character(x))

  # Strip exported unicode marker artifacts before normalization.
  x <- gsub("(?i)<\\s*u\\+[0-9a-f]{4,6}\\s*>", " ", x, perl = TRUE)
  x <- gsub("(?i)<\\s*[0-9a-f]{2}\\s*>", " ", x, perl = TRUE)

  # Transliterate to ASCII for locale-safe matching (e.g., LC_ALL=C).
  x_ascii <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  fallback_ascii <- iconv(x, to = "ASCII//TRANSLIT", sub = " ")
  use_ascii <- ifelse(is.na(x_ascii), fallback_ascii, x_ascii)
  x <- ifelse(is.na(use_ascii), x, use_ascii)

  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_+|_+$", "")
  x
}

load_talent_profiles <- function(config_path = file.path("classification", "config", "talent_profiles.json")) {
  if (!file.exists(config_path)) {
    stop("Missing talent profile config: ", config_path)
  }
  cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)

  required_top <- c("taxonomy_version", "prompt_version", "default_profile", "paths", "profiles")
  missing_top <- setdiff(required_top, names(cfg))
  if (length(missing_top) > 0) {
    stop("Missing keys in profile config: ", paste(missing_top, collapse = ", "))
  }
  cfg
}

resolve_classification_path <- function(classification_root, rel_path) {
  file.path(classification_root, rel_path)
}

select_talent_profile <- function(talent_name, cfg, classification_root = "classification") {
  normalized <- normalize_talent_key(talent_name)
  profile_name <- cfg$default_profile
  profile <- cfg$profiles[[profile_name]]

  # Prefer exact profile key match first so a talent-specific profile can be
  # selected without needing a regex matcher update.
  if (!is.null(cfg$profiles[[normalized]])) {
    profile_name <- normalized
    profile <- cfg$profiles[[profile_name]]
    return(list(profile_name = profile_name, profile = profile))
  }

  matchers <- cfg$matchers
  if (is.null(matchers)) {
    matchers <- list()
  }
  if (length(matchers) > 0) {
    for (matcher in matchers) {
      pattern <- matcher$pattern
      if (is.null(pattern)) {
        pattern <- ""
      }
      pattern <- normalize_talent_key(pattern)

      candidate <- matcher$profile
      if (is.null(candidate) || !nzchar(candidate)) {
        candidate <- cfg$default_profile
      }
      if (nzchar(pattern) && str_detect(normalized, pattern)) {
        profile_name <- candidate
        profile <- cfg$profiles[[profile_name]]
        break
      }
    }
  }

  if (is.null(profile)) {
    # Final fallback: infer profile from standard overlay location.
    # This supports new talent profiles without immediate config edits.
    inferred_overlay <- file.path("prompts", "talents", normalized, "overlay.txt")
    inferred_overlay_abs <- file.path(classification_root, inferred_overlay)
    if (file.exists(inferred_overlay_abs)) {
      profile_name <- normalized
      profile <- list(overlay = inferred_overlay)
    } else {
      stop("Profile not found in config: ", profile_name)
    }
  }

  list(
    profile_name = profile_name,
    profile = profile
  )
}
