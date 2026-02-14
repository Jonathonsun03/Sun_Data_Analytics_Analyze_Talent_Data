TalentSubtitlePath <- function(talent_name) {
    path <- file.path("Output", talent_name, "Subtitles")
    return(path)
}

decode_placeholder_bytes <- function(x) {
  if (!is.character(x)) return(x)

  stringr::str_replace_all(x, "(?:<[0-9a-fA-F]{2}>)+", function(seq) {
    unname(vapply(seq, function(one) {
      hex_bytes <- stringr::str_extract_all(one, "[0-9a-fA-F]{2}")[[1]]
      raw_bytes <- as.raw(strtoi(hex_bytes, 16L))
      out <- suppressWarnings(tryCatch(rawToChar(raw_bytes), error = function(e) one))
      Encoding(out) <- "UTF-8"
      out
    }, character(1)))
  })
}

process_talent_subtitle <- function(talent_name,
                                    save_rdata = TRUE,
                                    rdata_filename = NULL,
                                    skip_existing = TRUE,
                                    subtitle_root = NULL) {
  path <- if (is.null(subtitle_root)) TalentSubtitlePath(talent_name) else subtitle_root
  input_dir <- file.path(path, "Original")
  output_dir <- file.path(path, "Processed")
  rdata_dir <- file.path(path, "RData")

  if (!dir.exists(input_dir)) {
    decoded_input_dir <- decode_placeholder_bytes(input_dir)
    if (dir.exists(decoded_input_dir)) {
      input_dir <- decoded_input_dir
    } else {
      message("Skipping ", talent_name, ": no input dir ", input_dir)
      return(NULL)
    }
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(rdata_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(rdata_filename)) {
    rdata_filename <- sprintf("processed_subtitles_%s.RData", talent_name)
  }
  save_path <- file.path(rdata_dir, rdata_filename)

  # Start with any previously saved dfs so the RData accumulates across runs.
  existing_dfs <- list()
  if (save_rdata && file.exists(save_path)) {
    env <- new.env()
    loaded <- load(save_path, envir = env)
    if ("dfs" %in% loaded && exists("dfs", envir = env)) {
      existing_dfs <- get("dfs", envir = env)
    }
  }

  files <- list.files(input_dir, full.names = TRUE)
  files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  files <- files[!grepl("failed_subtitles\\.csv$", files, ignore.case = TRUE)]
  files <- vapply(files, function(f) {
    if (file.exists(f)) return(f)
    f_decoded <- decode_placeholder_bytes(f)
    if (file.exists(f_decoded)) return(f_decoded)
    f
  }, character(1), USE.NAMES = FALSE)

  if (length(files) == 0) {
    message("Skipping ", talent_name, ": no subtitle files found in ", input_dir)
    return(NULL)
  }

  dfs <- existing_dfs

  for (f in files) {
    if (!file.exists(f)) {
      f_decoded <- decode_placeholder_bytes(f)
      if (file.exists(f_decoded)) {
        f <- f_decoded
      } else {
        message("Skipping missing subtitle file: ", f)
        next
      }
    }

    out_name <- basename(f)
    out_path <- file.path(output_dir, out_name)

    # If already processed and skipping, reuse the existing processed CSV or prior RData entry.
    if (skip_existing && file.exists(out_path)) {
      message("Skipping re-clean (exists): ", out_name)
      if (is.null(dfs[[out_name]])) {
        df <- read_csv(out_path, show_col_types = FALSE)
        dfs[[out_name]] <- df
      }
      next
    }

    df <- tryCatch(
      read_csv(f, show_col_types = FALSE),
      error = function(e) {
        message("Skipping unreadable subtitle file ", f, " (", conditionMessage(e), ")")
        NULL
      }
    )
    if (is.null(df)) next

    df <- SplitTimeStamp(df)
    df <- CleanDuplicateTimestamps(df)

    write_csv(df, out_path)
    dfs[[out_name]] <- df
  }

  if (save_rdata && length(dfs) > 0) {
    save(dfs, file = save_path)
  }

  dfs
}


# Load the previously saved subtitle RData list for a talent.
load_talent_subtitle_rdata <- function(talent_name, rdata_filename = NULL) {
  path <- TalentSubtitlePath(talent_name)
  rdata_dir <- file.path(path, "RData")

  if (is.null(rdata_filename)) {
    rdata_filename <- sprintf("processed_subtitles_%s.RData", talent_name)
  }

  rdata_path <- file.path(rdata_dir, rdata_filename)
  if (!file.exists(rdata_path)) {
    message("No RData found for ", talent_name, " at ", rdata_path)
    return(NULL)
  }

  env <- new.env()
  loaded <- load(rdata_path, envir = env)
  if ("dfs" %in% loaded && exists("dfs", envir = env)) {
    return(get("dfs", envir = env))
  }

  message("RData for ", talent_name, " missing `dfs` object; loaded: ", paste(loaded, collapse = ", "))
  NULL
}

# Load RData for a vector of talents; returns a named list of dfs lists.
load_all_talent_rdata <- function(talents, rdata_filename = NULL) {
  results <- lapply(setNames(talents, talents), load_talent_subtitle_rdata, rdata_filename = rdata_filename)
  Filter(Negate(is.null), results)
}

SplitTimeStamp <- function(df) {
  nm <- names(df)
  nm_lower <- tolower(nm)

  find_col <- function(candidates) {
    idx <- match(tolower(candidates), nm_lower, nomatch = 0)
    idx <- idx[idx > 0]
    if (length(idx) == 0) return(NA_character_)
    nm[[idx[[1]]]]
  }

  timestamp_col <- find_col(c("Timestamp"))
  start_col <- find_col(c("subtitle_start", "start", "start_time"))
  stop_col <- find_col(c("subtitle_end", "end", "end_time", "stop_time"))
  video_id_col <- find_col(c("VideoID", "video_id"))
  video_title_col <- find_col(c("VideoTitle", "video_title"))
  text_col <- find_col(c("Text", "subtitle_text", "text"))

  if (!is.na(timestamp_col)) {
    df <- df %>%
      tidyr::separate(.data[[timestamp_col]], c("start_chr", "stop_chr"), sep = "\\s*-->\\s*", remove = FALSE)
  } else if (!is.na(start_col) && !is.na(stop_col)) {
    df <- df %>%
      dplyr::mutate(
        start_chr = as.character(.data[[start_col]]),
        stop_chr = as.character(.data[[stop_col]])
      )
  } else {
    stop("Missing subtitle timestamp columns. Found columns: ", paste(nm, collapse = ", "))
  }

  if (is.na(video_id_col) || is.na(video_title_col) || is.na(text_col)) {
    stop("Missing subtitle text/id/title columns. Found columns: ", paste(nm, collapse = ", "))
  }

  clean_df <- df %>%
    dplyr::mutate(
      start_time = lubridate::hms(.data$start_chr),
      stop_time = lubridate::hms(.data$stop_chr),
      start_sec = as.numeric(.data$start_time),
      stop_sec = as.numeric(.data$stop_time),
      VideoID = as.character(.data[[video_id_col]]),
      VideoTitle = as.character(.data[[video_title_col]]),
      Text = as.character(.data[[text_col]])
    ) %>%
    dplyr::select(.data$start_sec, .data$stop_sec, .data$start_time, .data$stop_time, .data$VideoID, .data$VideoTitle, .data$Text)

  return(clean_df)
}

CleanDuplicateTimestamps <- function(df) {

  # helper: remove overlap where curr starts with the end of prev (word-boundary)
  trim_overlap <- function(prev, curr, max_words = 20) {
    prev <- stringr::str_squish(dplyr::coalesce(prev, ""))
    curr <- stringr::str_squish(dplyr::coalesce(curr, ""))

    if (prev == "" || curr == "") return(curr)

    prev_w <- unlist(stringr::str_split(prev, "\\s+"))
    curr_w <- unlist(stringr::str_split(curr, "\\s+"))

    max_k <- min(length(prev_w), length(curr_w), max_words)

    # largest overlap: suffix(prev, k) == prefix(curr, k)
    for (k in seq(max_k, 1, by = -1)) {
      if (identical(tail(prev_w, k), head(curr_w, k))) {
        out <- if (k < length(curr_w)) {
          paste(curr_w[(k + 1):length(curr_w)], collapse = " ")
        } else {
          ""
        }
        return(stringr::str_squish(out))
      }
    }

    curr
  }

  df %>%
    dplyr::arrange(.data$VideoID, .data$start_sec, .data$stop_sec) %>%
    dplyr::group_by(.data$VideoID) %>%
    dplyr::mutate(
      # basic cleanup
      Text = dplyr::coalesce(.data$Text, ""),
      Text = stringr::str_replace_all(.data$Text, "&gt;&gt;", ">>"),
      Text = stringr::str_replace_all(.data$Text, "&nbsp;", " "),
      txt  = stringr::str_squish(.data$Text),

      next_txt = dplyr::lead(.data$txt),

      # PASS 1: keep only "finalized" lines (drop if next is longer + contains current)
      drop = !is.na(.data$next_txt) &
        nchar(.data$next_txt) > nchar(.data$txt) &
        stringr::str_detect(.data$next_txt, stringr::fixed(.data$txt))
    ) %>%
    dplyr::filter(!.data$drop) %>%
    dplyr::mutate(
      # PASS 2: remove overlap with previous kept line
      prev_kept = dplyr::lag(.data$txt),
      txt = purrr::map2_chr(.data$prev_kept, .data$txt, trim_overlap),
      Text = .data$txt
    ) %>%
    dplyr::select(-.data$txt, -.data$next_txt, -.data$drop, -.data$prev_kept) %>%
    dplyr::ungroup()
}

ProcessChatData <- function(df){
  df <- SplitTimeStamp(df)
  df <- CleanDuplicateTimestamps(df)

  return(df)
}

# Pick random quotes from the longest subtitle sheets for a single talent
pick_random_quotes_for_talent <- function(talent_name,
                                          dfs,
                                          n_quotes = 3,
                                          context_rows = 10,
                                          top_k_sheets = 1) {
  if (length(dfs) == 0) return(tibble())

  sheet_lengths <- vapply(dfs, function(df) max(df$stop_sec, na.rm = TRUE), numeric(1))
  target_sheets <- names(sort(sheet_lengths, decreasing = TRUE))[seq_len(min(top_k_sheets, length(sheet_lengths)))]

  purrr::map_dfr(target_sheets, function(sheet) {
    df <- dfs[[sheet]]
    if (nrow(df) == 0) return(tibble())

    selected_rows <- sample(seq_len(nrow(df)), size = min(n_quotes, nrow(df)))

    purrr::imap_dfr(selected_rows, function(idx, quote_num) {
      start_idx <- max(idx - context_rows, 1)
      snippet <- df[start_idx:idx, ]

      snippet %>%
        mutate(
          talent = talent_name,
          sheet = sheet,
          selected_row = idx,
          quote_num = quote_num,
          context_line = dplyr::row_number()
        )
    })
  })
}
