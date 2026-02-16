ex_get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

ex_repo_root <- normalizePath(file.path(ex_get_script_dir(), "..", "..", "..", ".."), winslash = "/", mustWork = FALSE)
ex_repo_path <- function(...) normalizePath(file.path(ex_repo_root, ...), winslash = "/", mustWork = FALSE)

source(ex_repo_path("scripts", "lib", "utils", "datalake_root.r"))
source(ex_repo_path("scripts", "lib", "duckdb", "db_connect.R"))

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
talent_profile <- arg_value("--talent-profile", "")
prompt_version <- arg_value("--prompt-version", "")
taxonomy_version <- arg_value("--taxonomy-version", "")
limit <- suppressWarnings(as.integer(arg_value("--limit", "0")))
out_csv <- arg_value(
  "--out",
  file.path(
    ex_repo_path("classification", "output"),
    paste0("classification_export_", gsub("[^A-Za-z0-9._-]+", "_", model), ".csv")
  )
)

con <- duckdb_connect()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

where_parts <- c("c.model = ?")
params <- list(model)
if (nzchar(talent_profile)) {
  where_parts <- c(where_parts, "c.talent_profile = ?")
  params[[length(params) + 1L]] <- talent_profile
}
if (nzchar(prompt_version)) {
  where_parts <- c(where_parts, "c.prompt_version = ?")
  params[[length(params) + 1L]] <- prompt_version
}
if (nzchar(taxonomy_version)) {
  where_parts <- c(where_parts, "c.taxonomy_version = ?")
  params[[length(params) + 1L]] <- taxonomy_version
}
where_sql <- paste(where_parts, collapse = " AND ")

limit_sql <- ""
if (!is.na(limit) && limit > 0) {
  limit_sql <- sprintf(" LIMIT %d", as.integer(limit))
}

query <- paste0(
  "SELECT
     c.*,
     v.talent_name,
     v.title_raw,
     v.content_type,
     v.published_at
   FROM classifications c
   LEFT JOIN videos v
     ON c.video_id = v.video_id
    AND c.talent_id = v.talent_id
   WHERE ",
  where_sql,
  "
   ORDER BY c.created_at DESC",
  limit_sql
)

rows <- DBI::dbGetQuery(con, query, params = params)
if (nrow(rows) == 0) {
  cat(
    "No rows found for model=", model,
    if (nzchar(talent_profile)) paste0(", talent_profile=", talent_profile) else "",
    if (nzchar(prompt_version)) paste0(", prompt_version=", prompt_version) else "",
    if (nzchar(taxonomy_version)) paste0(", taxonomy_version=", taxonomy_version) else "",
    "\n",
    sep = ""
  )
  quit(status = 0)
}

parse_or_null <- function(x) {
  tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
}

topic <- character(nrow(rows))
language <- character(nrow(rows))
tags <- character(nrow(rows))
primary_reference <- character(nrow(rows))
referenced_entities <- character(nrow(rows))

for (i in seq_len(nrow(rows))) {
  parsed <- parse_or_null(rows$classification_json[[i]])
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

rows$topic <- topic
rows$language <- language
rows$tags <- tags
rows$primary_reference <- primary_reference
rows$referenced_entities <- referenced_entities

cols_front <- c(
  "video_id", "talent_name", "talent_profile", "model", "confidence",
  "title_raw", "content_type", "published_at",
  "topic", "language", "tags", "primary_reference", "referenced_entities",
  "taxonomy_version", "prompt_version", "created_at"
)

existing_front <- cols_front[cols_front %in% names(rows)]
remaining <- setdiff(names(rows), existing_front)
rows <- rows[, c(existing_front, remaining), drop = FALSE]

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
write.csv(rows, out_csv, row.names = FALSE, na = "")

cat("Wrote: ", out_csv, "\n", sep = "")
cat("Rows: ", nrow(rows), "\n", sep = "")
