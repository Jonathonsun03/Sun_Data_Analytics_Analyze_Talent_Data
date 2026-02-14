tp_get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

tp_repo_root <- normalizePath(file.path(tp_get_script_dir(), "..", "..", "..", ".."), winslash = "/", mustWork = FALSE)
tp_repo_path <- function(...) normalizePath(file.path(tp_repo_root, ...), winslash = "/", mustWork = FALSE)

source(tp_repo_path("scripts", "lib", "stream_classification", "talent_profile", "load_all.R"))
tp_load_all(tp_repo_path("scripts", "lib", "stream_classification", "talent_profile"))

args <- tp_parse_args(commandArgs(trailingOnly = TRUE))

csv_path <- tp_or(args$csv, stop("Missing --csv"))
talent <- args$talent
all_talents <- isTRUE(args$all_talents)
talent_col <- tp_or(args$talent_col, "talent")
title_col <- tp_or(args$title_col, "title")
content_type_col <- tp_or(args$content_type_col, "content_type")
out_dir <- tp_or(args$out_dir, tp_repo_path("classification", "config", "talents"))
overlay_dir <- tp_or(args$overlay_dir, tp_repo_path("classification", "prompts", "talents"))
master_config <- tp_or(args$master_config, tp_repo_path("classification", "config", "talent_profiles.json"))
write_overlay <- isTRUE(args$write_overlay)
update_master <- isTRUE(args$update_master_config)
use_gpt <- isTRUE(args$use_gpt)
sample_size <- suppressWarnings(as.integer(tp_or(args$sample_size, "250")))
if (is.na(sample_size) || sample_size < 1) {
  sample_size <- 250L
}
discovery_prompt_dir <- tp_or(args$discovery_prompt_dir, tp_repo_path("classification", "prompts", "discovery"))
discovery_system_path <- tp_or(args$discovery_system, file.path(discovery_prompt_dir, "system.txt"))
discovery_user_path <- tp_or(args$discovery_user, file.path(discovery_prompt_dir, "user_template.txt"))
discovery_schema_path <- tp_or(args$discovery_schema, file.path(discovery_prompt_dir, "schema.json"))
gpt_model <- tp_or(args$model, Sys.getenv("OPENAI_MODEL", unset = "gpt-5-mini"))
talent_map_path <- args$talent_map

if (!all_talents && (is.null(talent) || !nzchar(trimws(as.character(talent))))) {
  stop("Missing --talent (or pass --all-talents).")
}

df <- tp_read_titles_csv(
  csv_path = csv_path,
  talent_col = talent_col,
  title_col = title_col,
  content_type_col = content_type_col
)

source_talents <- if (all_talents) tp_distinct_talents(df, talent_col) else trimws(as.character(talent))
source_talents <- source_talents[nzchar(source_talents)]

canon_map <- data.frame(
  source_talent = source_talents,
  canonical_talent = source_talents,
  stringsAsFactors = FALSE
)

if (!is.null(talent_map_path) && nzchar(talent_map_path)) {
  if (!file.exists(talent_map_path)) {
    stop("Talent map file not found: ", talent_map_path)
  }
  m <- read.csv(talent_map_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  need_map_cols <- c("source_talent", "canonical_talent")
  miss_map <- setdiff(need_map_cols, names(m))
  if (length(miss_map) > 0) {
    stop("Talent map must include columns: source_talent, canonical_talent")
  }

  m$source_talent <- trimws(enc2utf8(as.character(m$source_talent)))
  m$canonical_talent <- trimws(enc2utf8(as.character(m$canonical_talent)))
  m <- m[nzchar(m$source_talent) & nzchar(m$canonical_talent), , drop = FALSE]
  if (nrow(m) > 0) {
    idx <- match(tolower(canon_map$source_talent), tolower(m$source_talent))
    has <- !is.na(idx)
    canon_map$canonical_talent[has] <- m$canonical_talent[idx[has]]
  }
}

groups <- split(canon_map$source_talent, canon_map$canonical_talent)

if (use_gpt) {
  source(tp_repo_path("scripts", "lib", "ChatGPT", "chatgpt_load_all.R"))
  chatgpt_load_all(exclude_dirs = c("examples"))
}

for (canonical_talent in names(groups)) {
  rows <- tp_rows_for_talents(df, talent_col, groups[[canonical_talent]])
  if (nrow(rows) == 0) {
    next
  }

  baseline <- tp_build_baseline_payload(
    talent = canonical_talent,
    rows = rows,
    title_col = title_col,
    content_type_col = content_type_col
  )
  payload <- baseline$payload
  top_brackets <- baseline$top_brackets

  gpt_discovery <- NULL
  if (use_gpt) {
    gpt_discovery <- tp_run_gpt_discovery(
      talent = canonical_talent,
      rows = rows,
      title_col = title_col,
      content_type_col = content_type_col,
      payload = payload,
      sample_size = sample_size,
      discovery_system_path = discovery_system_path,
      discovery_user_path = discovery_user_path,
      discovery_schema_path = discovery_schema_path,
      gpt_model = gpt_model
    )
    payload <- tp_merge_gpt_discovery(payload, gpt_discovery)
  }

  slug <- tp_slugify(canonical_talent)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_json <- file.path(out_dir, paste0(slug, ".json"))
  writeLines(jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE), con = out_json, useBytes = TRUE)
  cat("Generated talent JSON: ", out_json, "\n", sep = "")

  if (write_overlay) {
    overlay_subdir <- file.path(overlay_dir, slug)
    dir.create(overlay_subdir, recursive = TRUE, showWarnings = FALSE)
    overlay_path <- file.path(overlay_subdir, "overlay.txt")
    overlay_fallback <- tp_build_overlay_text(canonical_talent, payload$structure$bracket_semantics, top_brackets)
    overlay_text <- tp_build_overlay_from_gpt(canonical_talent, gpt_discovery, overlay_fallback)
    writeLines(overlay_text, con = overlay_path, useBytes = TRUE)
    cat("Generated overlay: ", overlay_path, "\n", sep = "")
  }

  if (update_master) {
    tp_update_master_config(master_config, slug)
    cat("Updated master config: ", master_config, "\n", sep = "")
  }
}
