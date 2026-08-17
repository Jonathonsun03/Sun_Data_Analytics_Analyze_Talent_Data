get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0L) return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/")))
  normalizePath(getwd(), winslash = "/")
}

repo_root <- rprojroot::find_root(rprojroot::is_git_root, path = get_script_dir())
repo_path <- function(...) file.path(repo_root, ...)
assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "title_classification", "talent_rules.R"))
source(repo_path("r_scripts", "lib", "title_classification", "prompt_builder.R"))
source(repo_path("r_scripts", "lib", "title_classification", "schema.R"))
source(repo_path("r_scripts", "lib", "title_classification", "store.R"))

tmp_db <- tempfile(pattern = "classification_self_test_", fileext = ".duckdb")
con <- duckdb_connect(db_path = tmp_db)
on.exit({
  DBI::dbDisconnect(con, shutdown = TRUE)
  unlink(tmp_db, force = TRUE)
}, add = TRUE)
DBI::dbExecute(con, "CREATE SCHEMA catalog")
DBI::dbExecute(con, "CREATE TABLE catalog.talents (talent_code VARCHAR PRIMARY KEY, talent_name VARCHAR NOT NULL, legacy_talent_id VARCHAR, active BOOLEAN NOT NULL DEFAULT TRUE)")
DBI::dbExecute(con, "CREATE TABLE catalog.videos (video_id VARCHAR PRIMARY KEY, channel_id VARCHAR NOT NULL, talent_code VARCHAR NOT NULL, title VARCHAR, published_at TIMESTAMP, content_type VARCHAR, title_hash VARCHAR, is_available BOOLEAN NOT NULL DEFAULT TRUE)")
ensure_title_classification_schema(con)

prompt_root <- repo_path("prompts", "title_classification")
definitions <- load_definition_texts(file.path(prompt_root, "definitions"))
schema <- jsonlite::fromJSON(file.path(prompt_root, "base", "output_schema.json"), simplifyVector = FALSE)
schema <- extend_schema_with_definitions(schema, definitions)
read_asset <- function(...) read_text_file(file.path(prompt_root, ...))
publish_test_version <- function(id) {
  publish_title_version(
    con = con,
    title_version_id = id,
    taxonomy_version = sub("title-", "", id),
    prompt_version = sub("title-", "", id),
    system_prompt = read_asset("base", "system.txt"),
    instructions = read_asset("base", "instructions.txt"),
    content_type_rules = read_asset("base", "content_type_rules.txt"),
    definitions = definitions,
    user_template = read_asset("base", "user_template.txt"),
    output_schema = schema
  )
}
publish_test_version("title-v7")
publish_talent_profile(
  con = con,
  profile_id = "talent-default-v7",
  profile_version = "v7",
  display_name = "default",
  profile = list(contexts = list(title_classification = list(overlay_text = "Default title guidance.")))
)
DBI::dbExecute(con, "INSERT INTO catalog.talents VALUES ('T01', 'Test Talent', 'legacy_test', TRUE)")
DBI::dbExecute(con, "INSERT INTO catalog.videos VALUES ('video-1', 'channel-1', 'T01', 'Original title', CURRENT_TIMESTAMP, 'live', 'hash-1', TRUE)")

pending <- list_pending_title_classifications(con)
assert_true(nrow(pending) == 1L, "A new title must be pending.")
bundle <- load_title_prompt_bundle(con, "T01")
assert_true(bundle$title_version_id == "title-v7", "The active title version was not loaded.")
assert_true(length(bundle$definition_fields) == 8L, "Expected eight definition fields.")

result <- pending
result$classification_json <- "{}"
result$confidence <- 1
for (field_name in bundle$definition_fields) result[[field_name]] <- FALSE
run_id <- title_classification_pipeline_run_id("self_test")
register_title_classification_run(con, run_id)
inserted <- insert_title_classification_results(con, result, bundle, "test-model", run_id)
complete_title_classification_run(con, run_id)
assert_true(inserted == 1L, "The test result was not inserted.")
assert_true(nrow(list_pending_title_classifications(con)) == 0L, "An unchanged title was requeued.")

DBI::dbExecute(con, "UPDATE catalog.videos SET title = 'Changed title', title_hash = 'hash-2' WHERE video_id = 'video-1'")
assert_true(nrow(list_pending_title_classifications(con)) == 1L, "A changed title was not requeued.")
DBI::dbExecute(con, "UPDATE catalog.videos SET title_hash = 'hash-1' WHERE video_id = 'video-1'")

DBI::dbExecute(
  con,
  "INSERT INTO catalog.videos VALUES (
     'video-null-hash', 'channel-1', 'T01', 'Title without a stored hash',
     CURRENT_TIMESTAMP, 'video', NULL, TRUE
   )"
)
null_hash_pending <- list_pending_title_classifications(con)
null_hash_pending <- null_hash_pending[
  null_hash_pending$video_id == "video-null-hash",
  ,
  drop = FALSE
]
assert_true(nrow(null_hash_pending) == 1L, "A title with a null catalog hash was not pending.")
assert_true(
  !is.na(null_hash_pending$title_hash[[1]]) && nzchar(null_hash_pending$title_hash[[1]]),
  "A deterministic fallback hash was not generated."
)
null_hash_result <- null_hash_pending
null_hash_result$classification_json <- "{}"
null_hash_result$confidence <- 1
for (field_name in bundle$definition_fields) null_hash_result[[field_name]] <- FALSE
inserted_null_hash <- insert_title_classification_results(
  con,
  null_hash_result,
  bundle,
  "test-model",
  run_id
)
assert_true(inserted_null_hash == 1L, "The null-hash fallback result was not inserted.")
assert_true(
  nrow(list_pending_title_classifications(con)) == 0L,
  "A title classified with its fallback hash was requeued."
)

publish_test_version("title-v8")
assert_true(nrow(list_pending_title_classifications(con)) == 2L, "A new active version did not requeue all titles.")

cat("Self-test passed:\n")
cat("- new titles are pending\n")
cat("- unchanged titles are skipped\n")
cat("- changed title hashes are requeued\n")
cat("- null catalog hashes receive deterministic fallback hashes\n")
cat("- activating a new title version requeues all current titles\n")
cat("- default reusable talent profiles are available\n")
