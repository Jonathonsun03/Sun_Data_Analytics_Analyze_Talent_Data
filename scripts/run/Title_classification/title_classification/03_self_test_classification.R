tc_get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

tc_repo_root <- normalizePath(file.path(tc_get_script_dir(), "..", "..", "..", ".."), winslash = "/", mustWork = FALSE)
tc_repo_path <- function(...) normalizePath(file.path(tc_repo_root, ...), winslash = "/", mustWork = FALSE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

source(tc_repo_path("scripts", "lib", "stream_classification", "talent_rules.R"))
source(tc_repo_path("scripts", "lib", "stream_classification", "prompt_builder.R"))
source(tc_repo_path("scripts", "lib", "duckdb", "db_schema.R"))

bundle <- load_prompt_bundle(
  talent_name = "Leia Memoria【Variance Project】",
  classification_root = tc_repo_path("classification")
)

assert_true(
  identical(bundle$profile_name, "leia_memoria_variance_project"),
  "Expected Leia talent to map to leia_memoria_variance_project profile."
)
assert_true(isTRUE(bundle$used_compiler), "Expected modular prompt compiler to be active.")
assert_true(length(bundle$definition_fields) > 0, "No definition fields were generated.")
assert_true(
  length(bundle$definition_fields) == length(unique(bundle$definition_fields)),
  "Definition fields must be unique."
)

required_fields <- bundle$schema$properties$items$items$properties$classification$required
missing_in_schema <- setdiff(bundle$definition_fields, required_fields)
assert_true(
  length(missing_in_schema) == 0,
  paste("Definition fields missing from schema:", paste(missing_in_schema, collapse = ", "))
)

if (!requireNamespace("duckdb", quietly = TRUE)) {
  stop("Package `duckdb` is required.", call. = FALSE)
}
if (!requireNamespace("DBI", quietly = TRUE)) {
  stop("Package `DBI` is required.", call. = FALSE)
}

tmp_db <- tempfile(pattern = "classification_self_test_", fileext = ".duckdb")
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmp_db, read_only = FALSE)
on.exit({
  DBI::dbDisconnect(con, shutdown = TRUE)
  if (file.exists(tmp_db)) {
    unlink(tmp_db, force = TRUE)
  }
}, add = TRUE)

invisible(init_duckdb_schema(con))
invisible(ensure_classification_boolean_columns(con, bundle$definition_fields))

cols <- DBI::dbGetQuery(con, "PRAGMA table_info('classifications')")
actual_names <- cols$name
required_db_cols <- c("talent_profile", bundle$definition_fields)
missing_db_cols <- setdiff(required_db_cols, actual_names)
assert_true(
  length(missing_db_cols) == 0,
  paste("DuckDB classifications table missing columns:", paste(missing_db_cols, collapse = ", "))
)

cat("Self-test passed:\n")
cat("- profile mapping is correct\n")
cat("- schema includes definition-derived fields\n")
cat("- DuckDB classifications table includes talent_profile + definition columns\n")
