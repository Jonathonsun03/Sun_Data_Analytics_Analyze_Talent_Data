library(here)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

source(here("scripts", "lib", "utils", "staging_root.R"))
source(here("scripts", "lib", "utils", "datalake_root.r"))
source(here("scripts", "lib", "utils", "talent_select.R"))

ensure_utf8_locale <- function() {
  cur <- suppressWarnings(Sys.getlocale("LC_CTYPE"))
  if (grepl("UTF-8", cur, ignore.case = TRUE)) return(invisible(cur))

  for (loc in c("C.UTF-8", "en_US.UTF-8")) {
    changed <- suppressWarnings(
      tryCatch(Sys.setlocale("LC_CTYPE", loc), error = function(e) NA_character_)
    )
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

resolve_processed_talent_root <- function(datalake_root) {
  env_root <- Sys.getenv("TALENT_PROCESSED_ROOT", unset = "")
  if (nzchar(env_root)) return(env_root)

  analytics_root <- dirname(datalake_root)
  file.path(analytics_root, "Processed", "Talent_Data")
}

read_chat_csv <- function(path) {
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) {
      message("Skipping unreadable chat file ", path, " (", conditionMessage(e), ")")
      NULL
    }
  )
}

collect_chat_files <- function(chat_original_dir) {
  if (!dir.exists(chat_original_dir)) return(character(0))

  files <- list.files(chat_original_dir, full.names = TRUE)
  files[grepl("_chat\\.csv$", files, ignore.case = TRUE)]
}

ensure_utf8_locale()

talent_query <- Sys.getenv("TALENT_QUERY", unset = "all")
n_cores <- as.integer(Sys.getenv("CHAT_N_CORES", unset = "1"))
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

selected_talent_paths <- as.character(select_talent(talent_query, root = datalake_root))
selected_talent_names <- safe_basename(selected_talent_paths)
selected_chat_roots <- file.path(selected_talent_paths, "Chat")

chat_original_by_talent <- stats::setNames(file.path(selected_chat_roots, "Original"), selected_talent_names)
chat_processed_by_talent <- stats::setNames(file.path(selected_chat_roots, "Processed"), selected_talent_names)
chat_rdata_by_talent <- stats::setNames(file.path(selected_chat_roots, "RData"), selected_talent_names)

purrr::walk(chat_processed_by_talent, dir.create, recursive = TRUE, showWarnings = FALSE)
purrr::walk(chat_rdata_by_talent, dir.create, recursive = TRUE, showWarnings = FALSE)

message("Using datalake root: ", datalake_root)
message("Selected talents: ", paste(selected_talent_names, collapse = ", "))
message("Using chat worker cores: ", n_cores)

talent_indices <- seq_along(selected_talent_names)
message("Starting chat ingest stage...")

chat_results <- run_apply(
  talent_indices,
  function(i) {
    talent_name <- selected_talent_names[[i]]
    original_dir <- chat_original_by_talent[[talent_name]]
    message(sprintf("[chat] (%d/%d) %s - start", i, length(talent_indices), talent_name))

    files <- collect_chat_files(original_dir)
    if (length(files) == 0) {
      message(sprintf("[chat] (%d/%d) %s - no files found in %s", i, length(talent_indices), talent_name, original_dir))
      return(list(talent = talent_name, files = tibble(), data = list()))
    }

    data_list <- purrr::map(files, read_chat_csv)
    keep_idx <- purrr::map_lgl(data_list, ~ !is.null(.x))
    files_kept <- files[keep_idx]
    data_kept <- data_list[keep_idx]

    file_summary <- tibble::tibble(
      talent = talent_name,
      file_name = basename(files_kept),
      file_path = files_kept,
      rows = purrr::map_int(data_kept, nrow),
      cols = purrr::map_int(data_kept, ncol)
    )

    message(sprintf("[chat] (%d/%d) %s - done (files=%d)", i, length(talent_indices), talent_name, nrow(file_summary)))
    list(talent = talent_name, files = file_summary, data = data_kept)
  },
  n_cores = n_cores
)

chat_file_summary <- purrr::map_dfr(chat_results, "files")
chat_summary <- chat_file_summary |>
  dplyr::group_by(.data$talent) |>
  dplyr::summarize(
    files = dplyr::n(),
    total_rows = sum(.data$rows, na.rm = TRUE),
    avg_rows_per_file = mean(.data$rows, na.rm = TRUE),
    .groups = "drop"
  )

processed_talent_root <- resolve_processed_talent_root(datalake_root)
analysis_dir <- file.path(processed_talent_root, "chat_analysis")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

summary_path <- file.path(analysis_dir, "chat_summary.csv")
files_path <- file.path(analysis_dir, "chat_file_summary.csv")

readr::write_csv(chat_summary, summary_path)
readr::write_csv(chat_file_summary, files_path)

message("Wrote chat summary: ", summary_path)
message("Wrote chat file summary: ", files_path)
