library(here)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(data.table)

source(here("scripts", "lib", "utils", "staging_root.R"))
source(here("scripts", "lib", "utils", "datalake_root.r"))
source(here("scripts", "lib", "utils", "talent_select.R"))

list.files(here("scripts", "lib", "clean_data", "clean_chat"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)

list.files(here("scripts", "lib", "clean_data", "clean_subtitles"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)

list.files(here("scripts", "lib", "clean_data", "text_stream_replay"), pattern = "[rR]$", full.names = TRUE) %>%
  walk(source)

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

  warning("Could not switch LC_CTYPE to UTF-8.")
  invisible(cur)
}

extract_video_id_from_name <- function(path, suffix) {
  pat <- paste0("_([A-Za-z0-9_-]{11})_", suffix, "\\.csv$")
  stringr::str_match(basename(path), pat)[, 2]
}

load_chat_clean <- function(path) {
  df <- data.table::fread(path)
  df %>%
    prep_chat_time() %>%
    select(username, user_id, message, timestamp, message_type, video_id, starts_with("t_"), timecode)
}

run_apply <- function(x, fn, n_cores) {
  if (n_cores > 1 && .Platform$OS.type != "windows") {
    return(parallel::mclapply(x, fn, mc.cores = n_cores, mc.preschedule = TRUE))
  }
  lapply(x, fn)
}

process_talent <- function(talent_path, file_cores = 1L) {
  talent_name <- basename(talent_path)
  chat_dir <- file.path(talent_path, "Chat", "Original")
  subtitle_dir <- file.path(talent_path, "Subtitles", "Processed")
  out_dir <- file.path(talent_path, "text_playback")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  sub_files <- list.files(subtitle_dir, full.names = TRUE, recursive = TRUE, pattern = "_subtitles\\.csv$")
  chat_files <- list.files(chat_dir, full.names = TRUE, recursive = TRUE, pattern = "_chat\\.csv$")

  if (length(sub_files) == 0) {
    message("[text_replay] ", talent_name, " - no subtitle files found.")
    return(tibble::tibble())
  }

  if (length(chat_files) == 0) {
    message("[text_replay] ", talent_name, " - no chat files found.")
    return(tibble::tibble())
  }

  chat_index <- tibble::tibble(
    chat_path = chat_files,
    video_id = extract_video_id_from_name(chat_files, "chat")
  ) %>%
    filter(!is.na(video_id), video_id != "")

  if (nrow(chat_index) == 0) {
    message("[text_replay] ", talent_name, " - no chat files with parseable video id.")
    return(tibble::tibble())
  }

  file_results <- run_apply(seq_along(sub_files), function(i) {
    sub_path <- sub_files[[i]]
    stream_name <- stringr::str_remove(basename(sub_path), "_subtitles\\.csv$")
    video_id <- extract_video_id_from_name(sub_path, "subtitles")

    if (is.na(video_id) || video_id == "") {
      message("[text_replay] ", talent_name, " / ", basename(sub_path), " - skipped (no subtitle video id).")
      return(tibble::tibble(
        talent = talent_name, stream_name = stream_name, video_id = NA_character_,
        status = "skipped_no_sub_video_id", rows = NA_integer_, output_path = NA_character_
      ))
    }

    chat_match <- chat_index %>% filter(.data$video_id == !!video_id) %>% slice(1)
    if (nrow(chat_match) == 0) {
      message("[text_replay] ", talent_name, " / ", stream_name, " - skipped (no matching chat).")
      return(tibble::tibble(
        talent = talent_name, stream_name = stream_name, video_id = video_id,
        status = "skipped_no_chat_match", rows = NA_integer_, output_path = NA_character_
      ))
    }

    chat_clean <- tryCatch(load_chat_clean(chat_match$chat_path[[1]]), error = function(e) NULL)
    subs <- tryCatch(data.table::fread(sub_path), error = function(e) NULL)
    if (is.null(chat_clean) || is.null(subs)) {
      return(tibble::tibble(
        talent = talent_name, stream_name = stream_name, video_id = video_id,
        status = "failed_read", rows = NA_integer_, output_path = NA_character_
      ))
    }

    subs_clean <- normalize_subs_for_replay(subs, sub_path)
    replay <- build_stream_replay(chat_clean, subs_clean)

    out_path <- file.path(out_dir, paste0(stream_name, ".csv"))
    readr::write_csv(replay, out_path)

    message("[text_replay] ", talent_name, " / ", stream_name, " - wrote ", nrow(replay), " rows.")
    tibble::tibble(
      talent = talent_name,
      stream_name = stream_name,
      video_id = video_id,
      status = "written",
      rows = nrow(replay),
      output_path = out_path
    )
  }, n_cores = file_cores)

  dplyr::bind_rows(file_results)
}

ensure_utf8_locale()

datalake_root <- get_datalake_root()
if (!dir.exists(datalake_root)) {
  stop("Datalake root does not exist: ", datalake_root)
}

talent_query <- Sys.getenv("TALENT_QUERY", unset = "all")
n_cores <- as.integer(Sys.getenv("TEXT_REPLAY_N_CORES", unset = "1"))
if (is.na(n_cores) || n_cores < 1) n_cores <- 1
max_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (!is.na(max_cores) && max_cores > 0) {
  n_cores <- min(n_cores, max_cores)
}

selected_talent_paths <- as.character(select_talent(talent_query, root = datalake_root))
selected_talent_paths <- selected_talent_paths[file.exists(selected_talent_paths)]

if (length(selected_talent_paths) == 0) {
  stop("No talents selected.")
}

message("Using datalake root: ", datalake_root)
message("Selected talents: ", paste(basename(selected_talent_paths), collapse = ", "))
message("Using text replay worker cores: ", n_cores)

talent_cores <- if (length(selected_talent_paths) > 1) min(n_cores, length(selected_talent_paths)) else 1L
file_cores <- if (length(selected_talent_paths) == 1) n_cores else 1L

results <- run_apply(
  selected_talent_paths,
  function(path) process_talent(path, file_cores = file_cores),
  n_cores = talent_cores
) %>%
  dplyr::bind_rows()

message("Text replay generation complete.")
message("Files written: ", sum(results$status == "written", na.rm = TRUE))
