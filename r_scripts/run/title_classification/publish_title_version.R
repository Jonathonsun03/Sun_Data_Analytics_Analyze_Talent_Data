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
  if (length(index) == 0L || index[[1]] == length(args)) {
    return(default)
  }
  args[[index[[1]] + 1L]]
}

execute <- "--execute" %in% args
title_version_id <- arg_value("--title-version-id", "title-v7")
taxonomy_version <- arg_value("--taxonomy-version", "v7")
prompt_version <- arg_value("--prompt-version", "v7")
db_path_arg <- arg_value("--db-path", "")
prompt_root <- repo_path("prompts", "title_classification")

source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "title_classification", "classification_definitions.R"))
source(repo_path("r_scripts", "lib", "title_classification", "talent_rules.R"))
source(repo_path("r_scripts", "lib", "title_classification", "prompt_builder.R"))
source(repo_path("r_scripts", "lib", "title_classification", "schema.R"))
source(repo_path("r_scripts", "lib", "title_classification", "store.R"))

asset_paths <- c(
  system_prompt = file.path(prompt_root, "base", "system.txt"),
  instructions = file.path(prompt_root, "base", "instructions.txt"),
  content_type_rules = file.path(prompt_root, "base", "content_type_rules.txt"),
  user_template = file.path(prompt_root, "base", "user_template.txt"),
  output_schema = file.path(prompt_root, "base", "output_schema.json")
)
missing_paths <- asset_paths[!file.exists(asset_paths)]
if (length(missing_paths) > 0L) {
  stop("Missing title prompt assets: ", paste(missing_paths, collapse = ", "))
}

definitions <- load_definition_texts(file.path(prompt_root, "definitions"))
base_schema <- jsonlite::fromJSON(asset_paths[["output_schema"]], simplifyVector = FALSE)
output_schema <- extend_schema_with_definitions(base_schema, definitions)
system_prompt <- read_text_file(asset_paths[["system_prompt"]])
instructions <- read_text_file(asset_paths[["instructions"]])
content_type_rules <- read_text_file(asset_paths[["content_type_rules"]])
user_template <- read_text_file(asset_paths[["user_template"]])
source_revision <- tryCatch(
  system2("git", c("-C", shQuote(repo_root), "rev-parse", "HEAD"), stdout = TRUE)[[1]],
  error = function(e) NA_character_
)

material <- paste(
  system_prompt,
  instructions,
  content_type_rules,
  jsonlite::toJSON(definitions, auto_unbox = TRUE, null = "null"),
  user_template,
  jsonlite::toJSON(output_schema, auto_unbox = TRUE, null = "null"),
  sep = "\n---\n"
)
checksum <- digest::digest(material, algo = "sha256")

message("Title version: ", title_version_id)
message("Definitions: ", length(definitions))
message("Checksum: ", checksum)
message("Mode: ", if (execute) "execute" else "dry-run")
if (!execute) {
  quit(status = 0L)
}

db_path <- if (nzchar(db_path_arg)) db_path_arg else talent_lakehouse_db_path()
con <- title_classification_connect(db_path = db_path)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
ensure_title_classification_schema(con)

publish_title_version(
  con = con,
  title_version_id = title_version_id,
  taxonomy_version = taxonomy_version,
  prompt_version = prompt_version,
  system_prompt = system_prompt,
  instructions = instructions,
  content_type_rules = content_type_rules,
  definitions = definitions,
  user_template = user_template,
  output_schema = output_schema,
  active = TRUE,
  source_revision = source_revision
)

definition_paths <- list.files(
  file.path(prompt_root, "definitions"),
  pattern = "\\.txt$",
  full.names = TRUE
)
for (source_path in c(unname(asset_paths), definition_paths)) {
  register_title_classification_source_file(
    con,
    source_path = source_path,
    source_type = "title_classification_prompt"
  )
}
message("Published ", title_version_id, " to ", db_path)
