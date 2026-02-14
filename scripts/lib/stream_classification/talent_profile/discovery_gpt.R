library(jsonlite)
library(stringr)

tp_strip_code_fences <- function(x) {
  if (!is.character(x) || length(x) == 0 || is.na(x[[1]])) {
    return(NA_character_)
  }
  text <- x[[1]]
  text <- sub("^\\s*```(?:json)?\\s*", "", text)
  text <- sub("\\s*```\\s*$", "", text)
  trimws(text)
}

tp_validate_gpt_discovery <- function(obj) {
  required <- c(
    "structural_pattern_summary",
    "bracket_interpretation",
    "parentheses_interpretation",
    "topic_extractability",
    "shorts_ambiguity_patterns",
    "event_signals",
    "monetization_signals",
    "rule_candidates",
    "confidence_recommendations"
  )
  missing <- setdiff(required, names(obj))
  if (length(missing) > 0) {
    stop("GPT discovery missing keys: ", paste(missing, collapse = ", "))
  }

  if (!(obj$bracket_interpretation %in% c("topic", "show_format", "mixed", "rare"))) {
    stop("Invalid bracket_interpretation: ", obj$bracket_interpretation)
  }
  if (!(obj$topic_extractability %in% c("high", "medium", "low"))) {
    stop("Invalid topic_extractability: ", obj$topic_extractability)
  }

  vec_fields <- c(
    "shorts_ambiguity_patterns",
    "event_signals",
    "monetization_signals",
    "rule_candidates",
    "confidence_recommendations"
  )
  for (nm in vec_fields) {
    vals <- unlist(obj[[nm]], use.names = FALSE)
    if (!is.character(vals)) {
      stop("`", nm, "` must be an array of strings.")
    }
  }
  obj
}

tp_run_gpt_discovery <- function(
    talent,
    rows,
    title_col,
    content_type_col,
    payload,
    sample_size,
    discovery_system_path,
    discovery_user_path,
    discovery_schema_path,
    gpt_model
) {
  for (p in c(discovery_system_path, discovery_user_path, discovery_schema_path)) {
    if (!file.exists(p)) {
      stop("Missing GPT discovery prompt asset: ", p)
    }
  }

  n_take <- min(nrow(rows), sample_size)
  idx <- if (nrow(rows) > n_take) sample.int(nrow(rows), n_take) else seq_len(nrow(rows))
  sample_rows <- rows[idx, , drop = FALSE]

  records <- lapply(seq_len(nrow(sample_rows)), function(i) {
    list(
      title = enc2utf8(trimws(sample_rows[[title_col]][[i]])),
      content_type = str_to_lower(trimws(enc2utf8(sample_rows[[content_type_col]][[i]])))
    )
  })

  records_json <- toJSON(records, pretty = TRUE, auto_unbox = TRUE, null = "null")
  summary_json <- toJSON(payload, pretty = TRUE, auto_unbox = TRUE, null = "null")
  schema_text <- paste(readLines(discovery_schema_path, warn = FALSE), collapse = "\n")
  system_prompt <- paste(readLines(discovery_system_path, warn = FALSE), collapse = "\n")
  user_template <- paste(readLines(discovery_user_path, warn = FALSE), collapse = "\n")

  user_prompt <- chatgpt_render_prompt(
    user_template,
    list(
      talent_name = talent,
      summary_json = summary_json,
      records_json = records_json,
      schema_json = schema_text
    )
  )

  messages <- chatgpt_make_messages(system_prompt = system_prompt, user_prompt = user_prompt)
  response <- chatgpt_send_chat(messages = messages, model = gpt_model, temperature = 1)
  text <- tp_strip_code_fences(chatgpt_extract_text(response))
  if (!is.character(text) || !nzchar(text)) {
    stop("GPT discovery returned empty response.")
  }
  parsed <- fromJSON(text, simplifyVector = FALSE)
  tp_validate_gpt_discovery(parsed)
}

tp_merge_gpt_discovery <- function(payload, gpt_discovery) {
  payload$gpt_discovery <- gpt_discovery
  payload$structure$bracket_semantics <- gpt_discovery$bracket_interpretation
  payload$structure$topic_extractability <- gpt_discovery$topic_extractability
  payload$signals$event_keywords <- unique(c(
    payload$signals$event_keywords,
    unlist(gpt_discovery$event_signals, use.names = FALSE)
  ))
  payload$signals$monetization_keywords <- unlist(gpt_discovery$monetization_signals, use.names = FALSE)
  payload$rule_hints$gpt_rule_candidates <- unlist(gpt_discovery$rule_candidates, use.names = FALSE)
  payload
}
