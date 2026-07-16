pr_get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

pr_repo_root <- normalizePath(file.path(pr_get_script_dir(), "..", "..", "..", ".."), winslash = "/", mustWork = FALSE)
pr_repo_path <- function(...) normalizePath(file.path(pr_repo_root, ...), winslash = "/", mustWork = FALSE)

source(pr_repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(pr_repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) {
    return(default)
  }
  pos <- idx[[1]] + 1L
  if (pos > length(args)) {
    return(default)
  }
  args[[pos]]
}

model <- arg_value("--model", "gpt-5-mini")
limit <- suppressWarnings(as.integer(arg_value("--limit", "20")))
if (is.na(limit) || limit < 1) {
  limit <- 20L
}

con <- duckdb_connect()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

cat("DuckDB path: ", duckdb_get_db_path(), "\n", sep = "")
cat("Model filter: ", model, "\n\n", sep = "")

total <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM classifications")
cat("Total rows in classifications:\n")
print(total)
cat("\n")

by_model <- DBI::dbGetQuery(
  con,
  "SELECT model, COUNT(*) AS n
   FROM classifications
   GROUP BY model
   ORDER BY n DESC, model ASC"
)
cat("Rows by model:\n")
print(by_model)
cat("\n")

by_profile <- DBI::dbGetQuery(
  con,
  "SELECT talent_profile, COUNT(*) AS n
   FROM classifications
   WHERE model = ?
   GROUP BY talent_profile
   ORDER BY n DESC, talent_profile ASC",
  params = list(model)
)
cat("Rows by talent_profile for model ", model, ":\n", sep = "")
print(by_profile)
cat("\n")

latest_video <- DBI::dbGetQuery(
  con,
  "SELECT
     MAX(v.published_at) AS most_recent_video_published_at,
     COUNT(DISTINCT c.video_id) AS classified_videos_with_model
   FROM classifications c
   LEFT JOIN videos v
     ON c.video_id = v.video_id
    AND c.talent_id = v.talent_id
   WHERE c.model = ?",
  params = list(model)
)
cat("Most recent video date included for model ", model, ":\n", sep = "")
print(latest_video)
cat("\n")

sample_rows <- DBI::dbGetQuery(
  con,
  sprintf(
    "SELECT
       c.created_at,
       v.published_at,
       c.video_id,
       v.title_raw,
       c.talent_profile,
       c.model,
       c.confidence,
       c.collaborative_energy,
       c.community_milestones,
       c.interactive_entertainment,
       c.meme_viral,
       c.monetization,
       c.narrative_serialization,
       c.performance_artistry,
       c.personality_conversation,
       c.classification_json
     FROM classifications c
     LEFT JOIN videos v
       ON c.video_id = v.video_id
      AND c.talent_id = v.talent_id
     WHERE c.model = ?
     ORDER BY c.created_at DESC
     LIMIT %d",
    as.integer(limit)
  ),
  params = list(model)
)

if (nrow(sample_rows) == 0) {
  cat("No rows found for model: ", model, "\n", sep = "")
  quit(status = 0)
}

parse_or_null <- function(x) {
  tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
}

topic <- character(nrow(sample_rows))
language <- character(nrow(sample_rows))
tags <- character(nrow(sample_rows))
primary_reference <- character(nrow(sample_rows))
referenced_entities <- character(nrow(sample_rows))

for (i in seq_len(nrow(sample_rows))) {
  parsed <- parse_or_null(sample_rows$classification_json[[i]])
  if (is.null(parsed)) {
    topic[[i]] <- NA_character_
    language[[i]] <- NA_character_
    tags[[i]] <- NA_character_
    primary_reference[[i]] <- NA_character_
    referenced_entities[[i]] <- NA_character_
    next
  }
  topic[[i]] <- if (!is.null(parsed$topic)) as.character(parsed$topic) else NA_character_
  language[[i]] <- if (!is.null(parsed$language)) as.character(parsed$language) else NA_character_
  primary_reference[[i]] <- if (!is.null(parsed$primary_reference)) as.character(parsed$primary_reference) else NA_character_
  if (!is.null(parsed$tags)) {
    tags[[i]] <- paste(as.character(unlist(parsed$tags, use.names = FALSE)), collapse = ", ")
  } else {
    tags[[i]] <- NA_character_
  }
  if (!is.null(parsed$referenced_entities)) {
    referenced_entities[[i]] <- paste(as.character(unlist(parsed$referenced_entities, use.names = FALSE)), collapse = ", ")
  } else {
    referenced_entities[[i]] <- NA_character_
  }
}

preview <- data.frame(
  created_at = sample_rows$created_at,
  published_at = sample_rows$published_at,
  video_id = sample_rows$video_id,
  title_raw = sample_rows$title_raw,
  talent_profile = sample_rows$talent_profile,
  model = sample_rows$model,
  confidence = sample_rows$confidence,
  topic = topic,
  language = language,
  tags = tags,
  primary_reference = primary_reference,
  referenced_entities = referenced_entities,
  collaborative_energy = sample_rows$collaborative_energy,
  community_milestones = sample_rows$community_milestones,
  interactive_entertainment = sample_rows$interactive_entertainment,
  meme_viral = sample_rows$meme_viral,
  monetization = sample_rows$monetization,
  narrative_serialization = sample_rows$narrative_serialization,
  performance_artistry = sample_rows$performance_artistry,
  personality_conversation = sample_rows$personality_conversation,
  stringsAsFactors = FALSE
)

cat("Sample rows (most recent first, limit=", limit, "):\n", sep = "")
print(preview)
