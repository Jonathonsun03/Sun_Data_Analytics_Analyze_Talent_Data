get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/")))
  }
  normalizePath(getwd(), winslash = "/")
}

repo_root <- rprojroot::find_root(rprojroot::is_git_root, path = get_script_dir())
repo_path <- function(...) file.path(repo_root, ...)
args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  index <- which(args == flag)
  if (length(index) == 0L || index[[1]] == length(args)) return(default)
  args[[index[[1]] + 1L]]
}

source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "title_classification", "store.R"))

model <- arg_value("--model", "")
title_version_id <- arg_value("--title-version-id", "")
db_path_arg <- arg_value("--db-path", "")
limit <- suppressWarnings(as.integer(arg_value("--limit", "0")))
export_root <- file.path(
  dirname(normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)),
  "Processed",
  "Title_classification"
)
out_csv <- arg_value(
  "--out",
  file.path(export_root, "current", "classification_export_current.csv")
)

db_path <- if (nzchar(db_path_arg)) db_path_arg else talent_lakehouse_db_path()
con <- title_classification_connect(db_path = db_path, read_only = TRUE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
version <- load_active_title_version(con, title_version_id)

where_parts <- c(
  "result.title_version_id = ?",
  "video.is_available"
)
params <- list(version$title_version_id[[1]])
if (nzchar(model)) {
  where_parts <- c(where_parts, "result.model = ?")
  params <- c(params, list(model))
}
limit_sql <- if (!is.na(limit) && limit > 0L) {
  sprintf(" LIMIT %d", limit)
} else {
  ""
}

query <- paste0(
  "WITH ranked AS (",
  " SELECT result.* EXCLUDE (title_hash),",
  " result.title_hash AS classified_title_hash,",
  " status.title_hash AS current_title_hash,",
  " video.title AS title_raw, video.content_type, video.published_at,",
  " talent.talent_name, status.classification_status, status.is_classified,",
  " ROW_NUMBER() OVER (",
  "   PARTITION BY result.video_id, result.title_version_id",
  "   ORDER BY result.created_at DESC",
  " ) AS result_rank",
  " FROM classification.title_classification_results AS result",
  " JOIN catalog.videos AS video USING (video_id)",
  " LEFT JOIN catalog.talents AS talent ON talent.talent_code = result.talent_code",
  " LEFT JOIN classification.title_classification_status AS status",
  "   ON status.video_id = result.video_id",
  "  AND status.title_version_id = result.title_version_id",
  " WHERE ", paste(where_parts, collapse = " AND "),
  ") SELECT * EXCLUDE (result_rank)",
  " FROM ranked WHERE result_rank = 1",
  " ORDER BY published_at, video_id",
  limit_sql
)
rows <- DBI::dbGetQuery(con, query, params = params)

format_csv_datetime <- function(values) {
  parsed <- suppressWarnings(as.POSIXct(values, tz = "UTC"))
  ifelse(
    is.na(parsed),
    as.character(values),
    format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}
for (column_name in intersect(c("published_at", "created_at"), names(rows))) {
  rows[[column_name]] <- format_csv_datetime(rows[[column_name]])
}

parse_json_field <- function(json_text, field_name) {
  vapply(
    json_text,
    function(value) {
      parsed <- tryCatch(
        jsonlite::fromJSON(value, simplifyVector = TRUE),
        error = function(e) NULL
      )
      field <- parsed[[field_name]]
      if (is.null(field)) return(NA_character_)
      paste(as.character(field), collapse = ", ")
    },
    character(1)
  )
}

if (nrow(rows) > 0L) {
  for (field_name in c(
    "topic", "language", "tags", "primary_reference", "referenced_entities"
  )) {
    rows[[field_name]] <- parse_json_field(rows$classification_json, field_name)
  }
}

front_columns <- c(
  "video_id", "talent_code", "talent_name", "profile_id", "talent_profile",
  "title_raw", "content_type", "published_at", "model", "confidence",
  "topic", "language", "tags", "primary_reference", "referenced_entities",
  "classification_status", "is_classified", "classified_title_hash", "current_title_hash",
  "title_version_id", "taxonomy_version", "prompt_version", "created_at"
)
front_columns <- front_columns[front_columns %in% names(rows)]
rows <- rows[, c(front_columns, setdiff(names(rows), front_columns)), drop = FALSE]

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
write.csv(
  rows,
  out_csv,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
message("Wrote: ", out_csv)
message("Rows: ", nrow(rows))
message("Title version: ", version$title_version_id[[1]])
