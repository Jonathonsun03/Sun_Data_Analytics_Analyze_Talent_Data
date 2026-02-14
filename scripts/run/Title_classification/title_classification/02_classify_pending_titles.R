library(dplyr)
library(stringr)
library(jsonlite)

if (!exists("Classification")) {
  stop("Classification data frame not found. Build it before running.")
}
if (!exists("Talent")) {
  stop("Talent variable not found. Set Talent before running.")
}

source("scripts/lib/utils/datalake_root.r")
source("scripts/lib/utils/talent_select.R")
source("scripts/lib/duckdb/db_connect.R")
source("scripts/lib/duckdb/db_schema.R")
source("scripts/lib/duckdb/ingest_videos.R")
source("scripts/lib/duckdb/pending.R")
source("scripts/lib/ChatGPT/chatgpt_load_all.R")
source("scripts/lib/stream_classification/talent_rules.R")
source("scripts/lib/stream_classification/prompt_builder.R")

con <- duckdb_connect()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

init_duckdb_schema(con)
chatgpt_load_all(exclude_dirs = c("examples"))

talent_root <- select_talent(Talent)
talent_name <- safe_basename(talent_root)
talent_meta <- ensure_talent_id(con, talent_name)
talent_id <- talent_meta$talent_id[[1]]

to_ingest <- build_to_ingest(Classification, talent_id, talent_name)

# Classification run controls
model <- Sys.getenv("OPENAI_MODEL", unset = "gpt-5-mini")

# Choose how to avoid rework:
# - "video_id": classify once per video per version tuple (default)
# - "title_hash": classify again if title changes
classification_key <- "video_id"
batch_size <- suppressWarnings(
  as.integer(Sys.getenv("CLASSIFICATION_BATCH_SIZE", unset = "25"))
)
if (is.na(batch_size) || batch_size < 1) {
  batch_size <- 25L
}
max_retries <- suppressWarnings(
  as.integer(Sys.getenv("CLASSIFICATION_MAX_RETRIES", unset = "2"))
)
if (is.na(max_retries) || max_retries < 0) {
  max_retries <- 2L
}

prompt_bundle <- load_prompt_bundle(talent_name = talent_name)
taxonomy_version <- prompt_bundle$taxonomy_version
prompt_version <- prompt_bundle$prompt_version
system_prompt <- prompt_bundle$system_prompt
user_prompt_template <- prompt_bundle$user_prompt_template
talent_profile <- prompt_bundle$profile_name
talent_rules_text <- prompt_bundle$talent_rules_text
schema_text <- prompt_bundle$schema_text
schema <- prompt_bundle$schema
message("Using talent prompt profile: ", talent_profile)

strip_code_fences <- function(x) {
  if (!is.character(x) || length(x) == 0 || is.na(x[[1]])) {
    return(NA_character_)
  }
  text <- x[[1]]
  text <- sub("^\\s*```(?:json)?\\s*", "", text)
  text <- sub("\\s*```\\s*$", "", text)
  trimws(text)
}

validate_batch_response <- function(response_text, expected_video_ids, schema) {
  parsed <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)
  if (!is.list(parsed) || is.null(parsed$items) || !is.list(parsed$items)) {
    stop("Model response must include an `items` array.")
  }

  class_required <- schema$properties$items$items$properties$classification$required
  class_props <- schema$properties$items$items$properties$classification$properties
  topic_enum <- class_props$topic$enum
  format_enum <- class_props$format$enum
  language_enum <- class_props$language$enum
  max_tags <- class_props$tags$maxItems

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
    if (!(classification$format %in% format_enum)) {
      stop("Invalid `format` value: ", classification$format)
    }
    if (!(classification$language %in% language_enum)) {
      stop("Invalid `language` value: ", classification$language)
    }
    if (!is.logical(classification$is_music) || length(classification$is_music) != 1) {
      stop("`is_music` must be boolean.")
    }
    if (!is.list(classification$tags) && !is.character(classification$tags)) {
      stop("`tags` must be an array of strings.")
    }
    tags <- unlist(classification$tags, use.names = FALSE)
    if (length(tags) > max_tags || any(!nzchar(tags))) {
      stop("`tags` must be non-empty strings and respect maxItems.")
    }

    data.frame(
      video_id = item$video_id,
      classification_json = jsonlite::toJSON(
        classification,
        auto_unbox = TRUE,
        null = "null"
      ),
      confidence = confidence,
      stringsAsFactors = FALSE
    )
  })

  out <- dplyr::bind_rows(rows)
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

classify_batch <- function(
    batch_df,
    user_prompt_template,
    schema_text,
    schema,
    system_prompt,
    model,
    max_retries,
    talent_name,
    talent_profile,
    talent_rules_text
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
      talent_profile = talent_profile,
      talent_rules_text = talent_rules_text
    )
  )
  messages <- chatgpt_make_messages(
    system_prompt = system_prompt,
    user_prompt = user_prompt
  )

  attempts <- max_retries + 1L
  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    parsed <- tryCatch(
      {
        response <- chatgpt_send_chat(
          messages = messages,
          model = model,
          temperature = 1
        )
        text <- strip_code_fences(chatgpt_extract_text(response))
        if (!is.character(text) || !nzchar(text)) {
          stop("Model returned empty response.")
        }
        validate_batch_response(
          response_text = text,
          expected_video_ids = batch_df$video_id,
          schema = schema
        )
      },
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(parsed)) {
      return(parsed)
    }

    if (attempt < attempts) {
      Sys.sleep(min(2^attempt, 8))
    }
  }

  stop(
    "Batch classification failed after ",
    attempts,
    " attempts: ",
    conditionMessage(last_error)
  )
}

insert_classification_batch <- function(
    con,
    rows,
    taxonomy_version,
    prompt_version,
    model
) {
  DBI::dbExecute(
    con,
    "INSERT INTO classifications (
       title_hash, video_id, talent_id, taxonomy_version, prompt_version, model,
       classification_json, confidence
     )
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)
     ON CONFLICT (video_id, taxonomy_version, prompt_version, model) DO NOTHING",
    params = list(
      rows$title_hash,
      rows$video_id,
      rows$talent_id,
      rep(taxonomy_version, nrow(rows)),
      rep(prompt_version, nrow(rows)),
      rep(model, nrow(rows)),
      rows$classification_json,
      rows$confidence
    )
  )
}

pending <- get_pending_titles(
  con,
  to_ingest,
  taxonomy_version,
  prompt_version,
  model,
  key = classification_key
)

message("Pending titles: ", nrow(pending))
if (nrow(pending) == 0) {
  message("No pending titles to classify.")
} else {
  classified_total <- 0L
  batch_starts <- seq.int(1L, nrow(pending), by = batch_size)
  for (batch_index in seq_along(batch_starts)) {
    start_i <- batch_starts[[batch_index]]
    end_i <- min(start_i + batch_size - 1L, nrow(pending))
    batch_df <- pending[start_i:end_i, , drop = FALSE]

    batch_result <- classify_batch(
      batch_df = batch_df,
      user_prompt_template = user_prompt_template,
      schema_text = schema_text,
      schema = schema,
      system_prompt = system_prompt,
      model = model,
      max_retries = max_retries,
      talent_name = talent_name,
      talent_profile = talent_profile,
      talent_rules_text = talent_rules_text
    )

    to_insert <- batch_df %>%
      dplyr::select(title_hash, video_id, talent_id) %>%
      dplyr::inner_join(batch_result, by = "video_id")

    insert_classification_batch(
      con = con,
      rows = to_insert,
      taxonomy_version = taxonomy_version,
      prompt_version = prompt_version,
      model = model
    )

    classified_total <- classified_total + nrow(to_insert)
    message(
      "Classified batch ",
      batch_index,
      "/",
      length(batch_starts),
      " (",
      nrow(to_insert),
      " rows)"
    )
  }
  message("Classification completed. Rows attempted: ", classified_total)
}
