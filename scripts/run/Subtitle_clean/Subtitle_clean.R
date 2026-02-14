library(here)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(lubridate)

source(here("scripts", "lib", "utils", "staging_root.R"))
source(here("scripts", "lib", "utils", "datalake_root.r"))
source(here("scripts", "lib", "utils", "talent_select.R"))
source(here("scripts", "lib", "clean_data", "clean_subtitles", "Clean_subtitles.R"))
source(here("scripts", "lib", "clean_data", "clean_subtitles", "subtitle_units.R"))

ensure_utf8_locale <- function() {
  cur <- suppressWarnings(Sys.getlocale("LC_CTYPE"))
  if (grepl("UTF-8", cur, ignore.case = TRUE)) return(invisible(cur))

  for (loc in c("C.UTF-8", "en_US.UTF-8")) {
    changed <- suppressWarnings(tryCatch(Sys.setlocale("LC_CTYPE", loc), error = function(e) NA_character_))
    if (!is.na(changed) && grepl("UTF-8", changed, ignore.case = TRUE)) {
      message("Using LC_CTYPE locale: ", changed)
      return(invisible(changed))
    }
  }

  warning(
    "Could not switch LC_CTYPE to UTF-8. Unicode paths may fail. ",
    "Set locale before running, e.g. Sys.setlocale('LC_CTYPE', 'C.UTF-8')."
  )
  invisible(cur)
}

ensure_utf8_locale()

resolve_processed_talent_root <- function(datalake_root) {
  env_root <- Sys.getenv("TALENT_PROCESSED_ROOT", unset = "")
  if (nzchar(env_root)) return(env_root)

  analytics_root <- dirname(datalake_root)
  file.path(analytics_root, "Processed", "Talent_Data")
}

talent_query <- Sys.getenv("TALENT_QUERY", unset = "all")
n_quotes <- as.integer(Sys.getenv("SUBTITLE_QUOTES_PER_TALENT", unset = "3"))
context_rows <- as.integer(Sys.getenv("SUBTITLE_CONTEXT_ROWS", unset = "10"))
top_k_sheets <- as.integer(Sys.getenv("SUBTITLE_TOP_K_SHEETS", unset = "1"))
pause_gap_sec <- as.numeric(Sys.getenv("SUBTITLE_PAUSE_GAP_SEC", unset = "2.0"))
write_ena_txt <- tolower(Sys.getenv("SUBTITLE_WRITE_ENA_TXT", unset = "false")) %in% c("1", "true", "yes")
reclean <- tolower(Sys.getenv("SUBTITLE_RECLEAN", unset = "false")) %in% c("1", "true", "yes")
n_cores <- as.integer(Sys.getenv("SUBTITLE_N_CORES", unset = "1"))

if (is.na(n_quotes) || n_quotes < 1) n_quotes <- 3
if (is.na(context_rows) || context_rows < 0) context_rows <- 10
if (is.na(top_k_sheets) || top_k_sheets < 1) top_k_sheets <- 1
if (is.na(pause_gap_sec) || pause_gap_sec < 0) pause_gap_sec <- 2.0
if (is.na(n_cores) || n_cores < 1) n_cores <- 1

max_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (!is.na(max_cores) && max_cores > 0) {
  n_cores <- min(n_cores, max_cores)
}

run_apply <- function(x, fn, n_cores) {
  if (n_cores > 1 && .Platform$OS.type != "windows") {
    return(parallel::mclapply(x, fn, mc.cores = n_cores, mc.preschedule = TRUE))
  }
  lapply(x, fn)
}

datalake_root <- get_datalake_root()
if (!dir.exists(datalake_root)) {
  stop("Datalake root does not exist: ", datalake_root)
}

selected_talent_paths <- select_talent(talent_query, root = datalake_root)
selected_talent_paths <- as.character(selected_talent_paths)
selected_talent_names <- safe_basename(selected_talent_paths)
selected_subtitle_roots <- file.path(selected_talent_paths, "Subtitles")

message("Using datalake root: ", datalake_root)
message("Selected talents: ", paste(selected_talent_names, collapse = ", "))
message("Using subtitle worker cores: ", n_cores)

TalentSubtitlePath <- function(talent_name) {
  candidates <- c(
    file.path(datalake_root, talent_name, "Subtitles"),
    file.path(datalake_root, "Output", talent_name, "Subtitles"),
    file.path("Output", talent_name, "Subtitles")
  )

  existing <- candidates[dir.exists(candidates)]
  if (length(existing) > 0) return(existing[[1]])

  # Default write target if subtitles path has not been created yet.
  candidates[[1]]
}

talent_indices <- seq_along(selected_talent_names)
message("Starting subtitle cleaning stage...")
subtitle_results <- run_apply(
  talent_indices,
  function(i) {
    nm <- selected_talent_names[[i]]
    root <- selected_subtitle_roots[[i]]
    message(sprintf("[clean] (%d/%d) %s - start", i, length(talent_indices), nm))
    dfs <- process_talent_subtitle(
      nm,
      save_rdata = TRUE,
      skip_existing = !reclean,
      subtitle_root = root
    )
    n_sheets <- if (is.null(dfs)) 0L else length(dfs)
    message(sprintf("[clean] (%d/%d) %s - done (sheets=%d)", i, length(talent_indices), nm, n_sheets))
    list(name = nm, dfs = dfs)
  },
  n_cores = n_cores
)

subtitle_dfs_by_talent <- subtitle_results |>
  purrr::keep(~ !is.null(.x$dfs)) |>
  purrr::set_names(purrr::map_chr(., "name")) |>
  purrr::map("dfs")

if (length(subtitle_dfs_by_talent) == 0) {
  warning("No subtitle data processed for selected talents.")
}
message("Completed subtitle cleaning stage.")

subtitle_summary <- imap_dfr(subtitle_dfs_by_talent, function(dfs, talent_name) {
  sheet_stats <- imap_dfr(dfs, function(df, sheet_name) {
    tibble(
      talent = talent_name,
      sheet = sheet_name,
      rows = nrow(df),
      duration_sec = suppressWarnings(max(df$stop_sec, na.rm = TRUE))
    )
  })

  if (nrow(sheet_stats) == 0) {
    return(tibble(
      talent = talent_name,
      files = 0L,
      total_rows = 0L,
      total_duration_hours = 0,
      max_duration_hours = 0
    ))
  }

  sheet_stats %>%
    summarize(
      talent = first(talent),
      files = n(),
      total_rows = sum(rows, na.rm = TRUE),
      total_duration_hours = sum(duration_sec, na.rm = TRUE) / 3600,
      max_duration_hours = max(duration_sec, na.rm = TRUE) / 3600,
      .groups = "drop"
    )
})

subtitle_quotes <- imap_dfr(
  subtitle_dfs_by_talent,
  ~ pick_random_quotes_for_talent(
    talent_name = .y,
    dfs = .x,
    n_quotes = n_quotes,
    context_rows = context_rows,
    top_k_sheets = top_k_sheets
  )
)

ena_names <- names(subtitle_dfs_by_talent)
message("Starting ENA unitization stage...")
ena_chunks <- run_apply(
  seq_along(ena_names),
  function(i) {
    nm <- ena_names[[i]]
    message(sprintf("[ena] (%d/%d) %s - start", i, length(ena_names), nm))
    out <- build_ena_units_for_talent(
      talent_name = nm,
      dfs = subtitle_dfs_by_talent[[nm]],
      pause_gap_sec = pause_gap_sec
    )
    message(sprintf("[ena] (%d/%d) %s - done (units=%d)", i, length(ena_names), nm, nrow(out)))
    out
  },
  n_cores = n_cores
)
subtitle_ena_units <- dplyr::bind_rows(ena_chunks)
message("Completed ENA unitization stage.")

processed_talent_root <- resolve_processed_talent_root(datalake_root)
analysis_dir <- file.path(processed_talent_root, "subtitle_analysis")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

summary_path <- file.path(analysis_dir, "subtitle_summary.csv")
quotes_path <- file.path(analysis_dir, "subtitle_quotes_sample.csv")
ena_units_path <- file.path(analysis_dir, "subtitle_ena_units.csv")
ena_units_txt_path <- file.path(analysis_dir, "subtitle_ena_units.txt")

write_csv(subtitle_summary, summary_path)
write_csv(subtitle_quotes, quotes_path)
write_csv(subtitle_ena_units, ena_units_path)
if (write_ena_txt) {
  write_ena_units_txt(subtitle_ena_units, ena_units_txt_path)
}

message("Wrote subtitle summary: ", summary_path)
message("Wrote subtitle quote sample: ", quotes_path)
message("Wrote subtitle ENA units: ", ena_units_path)
if (write_ena_txt) {
  message("Wrote subtitle ENA text: ", ena_units_txt_path)
}
