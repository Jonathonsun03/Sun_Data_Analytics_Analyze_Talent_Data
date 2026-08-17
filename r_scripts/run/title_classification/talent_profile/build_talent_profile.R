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

source(repo_path("r_scripts", "lib", "title_classification", "talent_profile", "load_all.R"))
tp_load_all(repo_path("r_scripts", "lib", "title_classification", "talent_profile"))
source(repo_path("r_scripts", "lib", "utils", "datalake_root.r"))
source(repo_path("r_scripts", "lib", "duckdb", "db_connect.R"))
source(repo_path("r_scripts", "lib", "title_classification", "schema.R"))
source(repo_path("r_scripts", "lib", "title_classification", "store.R"))

args <- tp_parse_args(commandArgs(trailingOnly = TRUE))
talent_filter <- tp_or(args$talent, "")
all_talents <- isTRUE(args$all_talents)
execute <- isTRUE(args$execute)
use_gpt <- isTRUE(args$use_gpt)
profile_version <- tp_or(args$profile_version, "v7")
db_path <- tp_or(args$db_path, talent_lakehouse_db_path())
sample_size <- suppressWarnings(as.integer(tp_or(args$sample_size, "250")))
if (is.na(sample_size) || sample_size < 1L) sample_size <- 250L
if (!all_talents && !nzchar(talent_filter)) {
  stop("Pass --talent NAME_OR_CODE or --all-talents.")
}

con <- title_classification_connect(db_path = db_path, read_only = !execute)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
if (execute) ensure_title_classification_schema(con)

where_sql <- "WHERE video.is_available"
params <- list()
if (!all_talents) {
  where_sql <- paste0(
    where_sql,
    " AND (LOWER(talent.talent_code) = LOWER(?)",
    " OR LOWER(talent.talent_name) = LOWER(?))"
  )
  params <- list(talent_filter, talent_filter)
}
rows <- DBI::dbGetQuery(
  con,
  paste(
    "SELECT talent.talent_code, talent.talent_name, video.title, video.content_type",
    "FROM catalog.talents AS talent",
    "JOIN catalog.videos AS video USING (talent_code)",
    where_sql,
    "ORDER BY talent.talent_code, video.published_at"
  ),
  params = params
)
if (nrow(rows) == 0L) stop("No canonical title rows matched the talent selection.")

if (use_gpt) {
  source(repo_path("r_scripts", "lib", "ChatGPT", "chatgpt_load_all.R"))
  chatgpt_load_all(exclude_dirs = c("examples"))
}
discovery_root <- repo_path("prompts", "title_classification", "discovery")
pipeline_run_id <- title_classification_pipeline_run_id("talent_profile_builder")
if (execute) {
  register_title_classification_run(
    con,
    pipeline_run_id,
    pipeline_name = "talent_profile_builder"
  )
}

for (talent_code in unique(rows$talent_code)) {
  talent_rows <- rows[rows$talent_code == talent_code, , drop = FALSE]
  talent_name <- talent_rows$talent_name[[1]]
  baseline <- tp_build_baseline_payload(
    talent = talent_name,
    rows = talent_rows,
    title_col = "title",
    content_type_col = "content_type"
  )
  derived_profile <- baseline$payload
  discovery <- NULL
  if (use_gpt) {
    discovery <- tp_run_gpt_discovery(
      talent = talent_name,
      rows = talent_rows,
      title_col = "title",
      content_type_col = "content_type",
      payload = derived_profile,
      sample_size = sample_size,
      discovery_system_path = file.path(discovery_root, "system.txt"),
      discovery_user_path = file.path(discovery_root, "user_template.txt"),
      discovery_schema_path = file.path(discovery_root, "schema.json"),
      gpt_model = tp_or(args$model, Sys.getenv("OPENAI_MODEL", unset = "gpt-5-mini"))
    )
    derived_profile <- tp_merge_gpt_discovery(derived_profile, discovery)
  }
  fallback_overlay <- tp_build_overlay_text(
    talent_name,
    derived_profile$structure$bracket_semantics,
    baseline$top_brackets
  )
  overlay_text <- tp_build_overlay_from_gpt(talent_name, discovery, fallback_overlay)
  profile_id <- paste("talent", talent_code, profile_version, sep = "-")
  profile <- list(
    identity = list(talent_code = talent_code, talent_name = talent_name),
    contexts = list(
      title_classification = list(
        overlay_text = overlay_text,
        derived_profile = derived_profile
      )
    )
  )
  message(if (execute) "Publishing " else "Would publish ", profile_id)
  if (execute) {
    publish_talent_profile(
      con = con,
      profile_id = profile_id,
      talent_code = talent_code,
      profile_version = profile_version,
      display_name = tp_slugify(talent_name),
      profile = profile,
      active = TRUE,
      source_pipeline_run_id = pipeline_run_id
    )
  }
}
if (execute) complete_title_classification_run(con, pipeline_run_id)
message("Mode: ", if (execute) "execute" else "dry-run")
