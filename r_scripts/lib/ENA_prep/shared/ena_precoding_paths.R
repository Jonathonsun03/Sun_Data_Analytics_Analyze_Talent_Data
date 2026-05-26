ena_precoding_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, "AGENTS.md")) &&
        dir.exists(file.path(current, "r_scripts"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate repository root from: ", start)
    }
    current <- parent
  }
}

ena_precoding_source_repo_file <- function(...) {
  path <- file.path(ena_precoding_repo_root(), ...)
  if (!file.exists(path)) {
    stop("Required repository file not found: ", path)
  }
  source(path)
  invisible(path)
}

if (!exists("get_datalake_root", mode = "function")) {
  ena_precoding_source_repo_file("r_scripts", "lib", "utils", "datalake_root.r")
}

ena_precoding_talent_data_root <- function(text_playback_root = NULL) {
  if (!is.null(text_playback_root) && nzchar(trimws(text_playback_root))) {
    return(normalizePath(text_playback_root, winslash = "/", mustWork = FALSE))
  }
  normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)
}

ena_precoding_processed_root <- function(talent_data_root = NULL) {
  env_root <- Sys.getenv("TALENT_PROCESSED_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }
  talent_data_root <- ena_precoding_talent_data_root(talent_data_root)
  file.path(dirname(talent_data_root), "Processed", "Talent_Data")
}

ena_precoding_qualitative_prep_root <- function(talent_data_root = NULL) {
  file.path(ena_precoding_processed_root(talent_data_root), "qualitative_prep")
}

ena_precoding_codebook_path <- function(codebook_path = NULL, talent_data_root = NULL) {
  if (!is.null(codebook_path) && nzchar(trimws(codebook_path))) {
    return(normalizePath(codebook_path, winslash = "/", mustWork = FALSE))
  }
  file.path(
    ena_precoding_processed_root(talent_data_root),
    "Qualitative Codebooks",
    "concept_areas",
    "definitions",
    "monetary_personality",
    "current",
    "personality_qualitative_code_log.csv"
  )
}

ena_precoding_output_root <- function(output_root = NULL, talent_data_root = NULL) {
  if (!is.null(output_root) && nzchar(trimws(output_root))) {
    return(normalizePath(output_root, winslash = "/", mustWork = FALSE))
  }
  file.path(
    ena_precoding_processed_root(talent_data_root),
    "ena_extracts",
    "pre_coding_runs"
  )
}

ena_precoding_make_run_id <- function(prefix = "precoding") {
  paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S_%z"))
}

ena_precoding_run_dir <- function(run_id, output_root = NULL, talent_data_root = NULL) {
  if (is.null(run_id) || !nzchar(trimws(run_id))) {
    stop("`run_id` is required.")
  }
  file.path(ena_precoding_output_root(output_root, talent_data_root), run_id)
}

ena_precoding_classifications_db_path <- function(talent_data_root = NULL) {
  file.path(ena_precoding_talent_data_root(talent_data_root), "classifications.duckdb")
}
