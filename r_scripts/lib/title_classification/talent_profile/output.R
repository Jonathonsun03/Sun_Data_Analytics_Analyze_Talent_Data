library(jsonlite)

tp_update_master_config <- function(master_config_path, slug) {
  cfg <- fromJSON(master_config_path, simplifyVector = FALSE)
  if (is.null(cfg$profiles[[slug]])) {
    cfg$profiles[[slug]] <- list(
      overlay = file.path("prompts", "talents", slug, "overlay.txt")
    )
  }

  if (is.null(cfg$matchers)) {
    cfg$matchers <- list()
  }
  has_matcher <- any(vapply(cfg$matchers, function(m) identical(m$profile, slug), logical(1)))
  if (!has_matcher) {
    cfg$matchers[[length(cfg$matchers) + 1L]] <- list(pattern = slug, profile = slug)
  }

  writeLines(toJSON(cfg, pretty = TRUE, auto_unbox = TRUE), con = master_config_path, useBytes = TRUE)
}

tp_build_overlay_text <- function(talent_name, bracket_semantics, top_brackets) {
  lines <- c(
    paste0("Talent profile: ", talent_name, "."),
    "",
    "Rules:"
  )
  if (identical(bracket_semantics, "topic")) {
    lines <- c(lines, "- Square brackets usually contain the primary topic/game.")
  } else if (identical(bracket_semantics, "show_format")) {
    lines <- c(lines, "- Square brackets usually indicate stream format/series labels.")
  } else if (identical(bracket_semantics, "mixed")) {
    lines <- c(lines, "- Square brackets are mixed signals and require free-text confirmation.")
  } else {
    lines <- c(lines, "- Square brackets are weak or rare signals.")
  }
  lines <- c(
    lines,
    "- Parentheses often indicate modifiers (versions, events, live cuts, edits).",
    "- Apply conservative confidence for short, meme-like, or context-poor titles.",
    "- Use explicit karaoke/cover/concert/acapella/music words as strong `is_music` signals."
  )
  if (length(top_brackets) > 0) {
    lines <- c(lines, paste0("- Frequent bracket examples: ", paste(top_brackets[seq_len(min(5, length(top_brackets)))], collapse = ", "), "."))
  }
  paste(lines, collapse = "\n")
}

tp_build_overlay_from_gpt <- function(talent_name, gpt_obj, fallback_text) {
  if (is.null(gpt_obj)) {
    return(fallback_text)
  }

  lines <- c(
    paste0("Talent profile: ", talent_name, "."),
    "",
    "Rules:",
    paste0("- ", gpt_obj$structural_pattern_summary),
    paste0("- Bracket interpretation: ", gpt_obj$bracket_interpretation, "."),
    paste0("- Parentheses interpretation: ", gpt_obj$parentheses_interpretation, ".")
  )

  rule_candidates <- unlist(gpt_obj$rule_candidates, use.names = FALSE)
  if (length(rule_candidates) > 0) {
    lines <- c(lines, paste0("- ", head(rule_candidates, 6)))
  }

  conf_recs <- unlist(gpt_obj$confidence_recommendations, use.names = FALSE)
  if (length(conf_recs) > 0) {
    lines <- c(lines, paste0("- Confidence policy: ", paste(head(conf_recs, 3), collapse = "; ")))
  }

  paste(lines, collapse = "\n")
}
