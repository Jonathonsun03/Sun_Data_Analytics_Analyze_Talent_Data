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
ena_as_final <- tolower(Sys.getenv("SUBTITLE_ENA_AS_FINAL", unset = "false")) %in% c("1", "true", "yes")

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

log_worker <- function(stage, idx, total, label, status, extra = NULL) {
  pid <- Sys.getpid()
  suffix <- if (is.null(extra) || !nzchar(extra)) "" else paste0(" (", extra, ")")
  message(sprintf("[%s][pid=%d] (%d/%d) %s - %s%s", stage, pid, idx, total, label, status, suffix))
}

datalake_root <- get_datalake_root()
if (!dir.exists(datalake_root)) {
  stop("Datalake root does not exist: ", datalake_root)
}

selected_talent_paths <- select_talent(talent_query, root = datalake_root)
selected_talent_paths <- as.character(selected_talent_paths)
selected_talent_names <- safe_basename(selected_talent_paths)
selected_subtitle_roots <- file.path(selected_talent_paths, "Subtitles")
subtitle_root_by_talent <- stats::setNames(selected_subtitle_roots, selected_talent_names)

message("Using datalake root: ", datalake_root)
message("Selected talents: ", paste(selected_talent_names, collapse = ", "))
message("Using subtitle worker cores: ", n_cores)
message("ENA as final per-video output: ", ena_as_final)

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
clean_talent_cores <- if (length(talent_indices) > 1) min(n_cores, length(talent_indices)) else 1L
clean_file_cores <- if (length(talent_indices) == 1) n_cores else 1L
message("Starting subtitle cleaning stage...")
subtitle_results <- run_apply(
  talent_indices,
  function(i) {
    nm <- selected_talent_names[[i]]
    root <- selected_subtitle_roots[[i]]
    log_worker("clean", i, length(talent_indices), nm, "start")
    dfs <- process_talent_subtitle(
      nm,
      save_rdata = TRUE,
      skip_existing = !reclean,
      subtitle_root = root,
      n_cores = clean_file_cores
    )
    n_sheets <- if (is.null(dfs)) 0L else length(dfs)
    log_worker("clean", i, length(talent_indices), nm, "done", sprintf("sheets=%d", n_sheets))
    list(name = nm, dfs = dfs)
  },
  n_cores = clean_talent_cores
)

subtitle_results_kept <- purrr::keep(subtitle_results, ~ !is.null(.x$dfs))
subtitle_dfs_by_talent <- purrr::set_names(
  purrr::map(subtitle_results_kept, "dfs"),
  purrr::map_chr(subtitle_results_kept, "name")
)

if (length(subtitle_dfs_by_talent) == 0) {
  warning("No subtitle data processed for selected talents.")
}
message("Completed subtitle cleaning stage.")

# Flatten all subtitle sheets across talents so ENA work can parallelize at file level.
sheet_tasks <- subtitle_dfs_by_talent |>
  purrr::imap(function(dfs, talent_name) {
    purrr::imap(dfs, function(df, sheet_name) {
      list(
        talent = talent_name,
        sheet = sheet_name,
        df = df
      )
    })
  }) |>
  unlist(recursive = FALSE)

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

message("Starting ENA unitization stage... (files=", length(sheet_tasks), ")")
ena_results <- run_apply(
  seq_along(sheet_tasks),
  function(i) {
    task <- sheet_tasks[[i]]
    label <- paste0(task$talent, " / ", task$sheet)
    log_worker("ena", i, length(sheet_tasks), label, "start")
    out <- build_ena_units_from_clean_df(
      df = task$df,
      pause_gap_sec = pause_gap_sec
    )
    log_worker("ena", i, length(sheet_tasks), label, "done", sprintf("units=%d", nrow(out)))
    list(
      talent = task$talent,
      sheet = task$sheet,
      units = out
    )
  },
  n_cores = n_cores
)
subtitle_ena_units <- dplyr::bind_rows(purrr::map(ena_results, "units"))
if (nrow(subtitle_ena_units) > 0) {
  subtitle_ena_units <- subtitle_ena_units |>
    dplyr::mutate(
      unit_id = dplyr::row_number(),
      start_sec = round(.data$start_sec, 3),
      end_sec = round(.data$end_sec, 3)
    ) |>
    dplyr::select("unit_id", "unit_type", "start_sec", "end_sec", "start", "end", "text")
}
message("Completed ENA unitization stage.")

if (ena_as_final && length(ena_results) > 0) {
  message("Writing ENA units as final per-video outputs...")
  final_results <- run_apply(
    seq_along(ena_results),
    function(i) {
      res <- ena_results[[i]]
      nm <- res$talent
      sheet_name <- res$sheet
      label <- paste0(nm, " / ", sheet_name)
      target_dir <- file.path(subtitle_root_by_talent[[nm]], "Processed")
      dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
      log_worker("ena-final", i, length(ena_results), label, "start")
      units <- res$units
      if (nrow(units) > 0) {
        units <- units |>
          dplyr::mutate(
            unit_id = dplyr::row_number(),
            start_sec = round(.data$start_sec, 3),
            end_sec = round(.data$end_sec, 3)
          ) |>
          dplyr::select("unit_id", "unit_type", "start_sec", "end_sec", "start", "end", "text")
      }
      readr::write_csv(units, file.path(target_dir, sheet_name))
      log_worker("ena-final", i, length(ena_results), label, "done", sprintf("units=%d", nrow(units)))
      1L
    },
    n_cores = n_cores
  )
  message("Completed ENA final per-video write stage. Total files written: ", sum(unlist(final_results)))
}

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
