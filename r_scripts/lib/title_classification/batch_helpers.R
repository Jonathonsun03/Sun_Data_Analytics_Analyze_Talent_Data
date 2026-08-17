strip_title_classification_code_fences <- function(x) {
  if (!is.character(x) || length(x) == 0 || is.na(x[[1]])) {
    return(NA_character_)
  }
  text <- x[[1]]
  text <- sub("^\\s*```(?:json)?\\s*", "", text)
  text <- sub("\\s*```\\s*$", "", text)
  trimws(text)
}

build_title_classification_messages <- function(
    batch_df,
    user_prompt_template,
    schema_text,
    system_prompt,
    talent_name,
    talent_profile
) {
  records <- lapply(seq_len(nrow(batch_df)), function(i) {
    list(
      video_id = batch_df$video_id[[i]],
      title = batch_df$title_raw[[i]],
      content_type = batch_df$content_type[[i]],
      published_at = as.character(batch_df$published_at[[i]])
    )
  })
  records_json <- jsonlite::toJSON(records, auto_unbox = TRUE, pretty = TRUE, null = "null")
  user_prompt <- chatgpt_render_prompt(
    user_prompt_template,
    list(
      records_json = records_json,
      schema_json = schema_text,
      talent_name = talent_name,
      talent_profile = talent_profile
    )
  )
  chatgpt_make_messages(
    system_prompt = system_prompt,
    user_prompt = user_prompt
  )
}

validate_title_classification_batch_response <- function(
    response_text,
    expected_video_ids,
    schema,
    definition_fields = character(0)
) {
  parsed <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)
  if (!is.list(parsed) || is.null(parsed$items) || !is.list(parsed$items)) {
    stop("Model response must include an `items` array.")
  }

  class_required <- schema$properties$items$items$properties$classification$required
  class_props <- schema$properties$items$items$properties$classification$properties
  topic_enum <- class_props$topic$enum
  language_enum <- class_props$language$enum
  max_tags <- class_props$tags$maxItems
  max_references <- class_props$referenced_entities$maxItems

  rows <- lapply(parsed$items, function(item) {
    if (is.null(item$video_id) || !is.character(item$video_id) || !nzchar(item$video_id)) {
      stop("Each item must include a non-empty string `video_id`.")
    }
    if (is.null(item$confidence) || !is.numeric(item$confidence)) {
      stop("Each item must include numeric `confidence`.")
    }
    confidence <- as.numeric(item$confidence[[1]])
    if (is.na(confidence) || confidence < 0 || confidence > 1) {
      stop("`confidence` must be between 0 and 1.")
    }
    if (is.null(item$classification) || !is.list(item$classification)) {
      stop("Each item must include object `classification`.")
    }

    classification <- item$classification
    missing_fields <- setdiff(class_required, names(classification))
    if (length(missing_fields) > 0) {
      stop("Missing classification fields: ", paste(missing_fields, collapse = ", "))
    }

    if (!(classification$topic %in% topic_enum)) {
      stop("Invalid `topic` value: ", classification$topic)
    }
    if (!(classification$language %in% language_enum)) {
      stop("Invalid `language` value: ", classification$language)
    }
    if (!is.list(classification$tags) && !is.character(classification$tags)) {
      stop("`tags` must be an array of strings.")
    }
    tags <- unlist(classification$tags, use.names = FALSE)
    if (length(tags) > max_tags || any(!nzchar(tags))) {
      stop("`tags` must be non-empty strings and respect maxItems.")
    }
    if (is.null(classification$primary_reference) || !is.character(classification$primary_reference)) {
      stop("`primary_reference` must be a string.")
    }
    primary_reference <- trimws(classification$primary_reference[[1]])
    if (!is.list(classification$referenced_entities) && !is.character(classification$referenced_entities)) {
      stop("`referenced_entities` must be an array of strings.")
    }
    referenced_entities <- unlist(classification$referenced_entities, use.names = FALSE)
    referenced_entities <- trimws(as.character(referenced_entities))
    referenced_entities <- referenced_entities[nzchar(referenced_entities)]
    if (length(referenced_entities) > max_references) {
      stop("`referenced_entities` must be non-empty strings and respect maxItems.")
    }

    if (classification$topic %in% c("gaming", "music")) {
      if (!nzchar(primary_reference) && length(tags) > 0) {
        primary_reference <- tags[[1]]
      }
      if (length(referenced_entities) == 0 && length(tags) > 0) {
        referenced_entities <- unique(tags)
        if (length(referenced_entities) > max_references) {
          referenced_entities <- referenced_entities[seq_len(max_references)]
        }
      }
      if (!nzchar(primary_reference) || length(referenced_entities) == 0) {
        stop("Gaming/music rows must provide non-empty `primary_reference` and at least one `referenced_entities` item.")
      }
      classification$primary_reference <- primary_reference
      classification$referenced_entities <- as.list(referenced_entities)
    }

    extra_bool_fields <- setdiff(
      class_required,
      c("topic", "language", "tags", "primary_reference", "referenced_entities")
    )
    if (length(extra_bool_fields) > 0) {
      for (field_nm in extra_bool_fields) {
        value <- classification[[field_nm]]
        if (!is.logical(value) || length(value) != 1 || is.na(value)) {
          stop("`", field_nm, "` must be boolean.")
        }
      }
    }

    row <- data.frame(
      video_id = item$video_id,
      classification_json = as.character(
        jsonlite::toJSON(
          classification,
          auto_unbox = TRUE,
          null = "null"
        )
      ),
      confidence = confidence,
      stringsAsFactors = FALSE
    )

    for (field_nm in definition_fields) {
      if (field_nm %in% names(classification)) {
        row[[field_nm]] <- as.logical(classification[[field_nm]])
      } else {
        row[[field_nm]] <- NA
      }
    }

    row
  })

  out <- do.call(rbind, rows)
  if (anyDuplicated(out$video_id)) {
    stop("Model response contains duplicate `video_id` values.")
  }
  if (!setequal(out$video_id, expected_video_ids)) {
    missing_ids <- setdiff(expected_video_ids, out$video_id)
    extra_ids <- setdiff(out$video_id, expected_video_ids)
    stop(
      "Model response ids mismatch. Missing: ",
      paste(missing_ids, collapse = ", "),
      "; Extra: ",
      paste(extra_ids, collapse = ", ")
    )
  }
  out
}
