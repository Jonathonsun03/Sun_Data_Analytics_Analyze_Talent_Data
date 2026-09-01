build_ena_units_from_clean_df <- function(df, pause_gap_sec = 2.0) {
  required_cols <- c("start_sec", "stop_sec", "Text")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  fmt_hhmmss <- function(seconds) {
    if (is.na(seconds)) return("")
    s <- max(0, as.numeric(seconds))
    h <- floor(s / 3600)
    s <- s - 3600 * h
    m <- floor(s / 60)
    s <- s - 60 * m
    sprintf("%02d:%02d:%06.3f", h, m, s)
  }

  sentence_spans <- function(text) {
    text <- stringr::str_trim(text)
    if (!nzchar(text)) return(list())

    boundaries <- stringr::str_locate_all(
      text,
      stringr::regex("[.!?][\"']?(?=\\s+|$)")
    )[[1]]

    if (nrow(boundaries) == 0) {
      return(list(c(1L, nchar(text))))
    }

    starts <- c(1L, boundaries[, 2] + 1L)
    ends <- c(boundaries[, 2], nchar(text))
    keep <- starts <= ends
    Map(c, starts[keep], ends[keep])
  }

  add_unit <- function(units, unit_type, start_sec, end_sec, text) {
    tibble::add_row(
      units,
      unit_id = nrow(units) + 1L,
      unit_type = unit_type,
      start_sec = start_sec,
      end_sec = end_sec,
      start = fmt_hhmmss(start_sec),
      end = fmt_hhmmss(end_sec),
      text = text
    )
  }

  work <- df |>
    dplyr::transmute(
      start_sec = suppressWarnings(as.numeric(.data$start_sec)),
      stop_sec = suppressWarnings(as.numeric(.data$stop_sec)),
      text = stringr::str_squish(as.character(.data$Text))
    ) |>
    dplyr::filter(!is.na(.data$text), .data$text != "") |>
    dplyr::arrange(.data$start_sec, .data$stop_sec)

  units <- tibble::tibble(
    unit_id = integer(),
    unit_type = character(),
    start_sec = numeric(),
    end_sec = numeric(),
    start = character(),
    end = character(),
    text = character()
  )

  buffer_text <- ""
  buffer_start <- NA_real_
  buffer_end <- NA_real_
  prev_end <- NA_real_

  seg_start_idx <- integer()
  seg_end_idx <- integer()
  seg_start_sec <- numeric()
  seg_stop_sec <- numeric()

  flush_buffer <- function() {
    if (!nzchar(stringr::str_trim(buffer_text))) {
      buffer_text <<- ""
      buffer_start <<- NA_real_
      buffer_end <<- NA_real_
      seg_start_idx <<- integer()
      seg_end_idx <<- integer()
      seg_start_sec <<- numeric()
      seg_stop_sec <<- numeric()
      return(invisible(NULL))
    }

    original <- buffer_text
    trimmed <- stringr::str_trim(original)
    left_trim <- nchar(original) - nchar(stringr::str_trim(original, side = "left"))
    spans <- sentence_spans(trimmed)

    for (sp in spans) {
      s <- sp[[1]]
      e <- sp[[2]]
      seg_text <- stringr::str_trim(substr(trimmed, s, e))
      if (!nzchar(seg_text)) next

      map_start <- s + left_trim
      map_end <- e + left_trim
      overlaps <- which(!(seg_end_idx < map_start | seg_start_idx > map_end))

      if (length(overlaps) > 0) {
        s_sec <- min(seg_start_sec[overlaps], na.rm = TRUE)
        e_sec <- max(seg_stop_sec[overlaps], na.rm = TRUE)
      } else {
        s_sec <- buffer_start
        e_sec <- buffer_end
      }

      units <<- add_unit(units, "TEXT", s_sec, e_sec, seg_text)
    }

    buffer_text <<- ""
    buffer_start <<- NA_real_
    buffer_end <<- NA_real_
    seg_start_idx <<- integer()
    seg_end_idx <<- integer()
    seg_start_sec <<- numeric()
    seg_stop_sec <<- numeric()
    invisible(NULL)
  }

  for (i in seq_len(nrow(work))) {
    start_s <- work$start_sec[[i]]
    stop_s <- work$stop_sec[[i]]
    txt <- work$text[[i]]

    if (!is.na(prev_end) && !is.na(start_s)) {
      gap <- start_s - prev_end
      if (gap >= pause_gap_sec && !nzchar(stringr::str_trim(buffer_text))) {
        units <- add_unit(units, "PAUSE", prev_end, start_s, "[PAUSE]")
      }
    }

    if (nzchar(stringr::str_trim(buffer_text)) && !is.na(prev_end) && !is.na(start_s) &&
      (start_s - prev_end) >= pause_gap_sec) {
      flush_buffer()
      units <- add_unit(units, "PAUSE", prev_end, start_s, "[PAUSE]")
    }

    if (!nzchar(buffer_text)) buffer_start <- start_s
    spacer <- if (!nzchar(buffer_text)) "" else " "
    start_idx <- nchar(buffer_text) + if (!nzchar(spacer)) 1L else nchar(spacer) + 1L
    buffer_text <- paste0(buffer_text, spacer, txt)
    end_idx <- nchar(buffer_text)

    seg_start_idx <- c(seg_start_idx, start_idx)
    seg_end_idx <- c(seg_end_idx, end_idx)
    seg_start_sec <- c(seg_start_sec, start_s)
    seg_stop_sec <- c(seg_stop_sec, stop_s)
    buffer_end <- stop_s

    if (stringr::str_detect(stringr::str_trim(buffer_text), "[.!?][\"']?$")) {
      flush_buffer()
    }

    if (!is.na(stop_s)) {
      prev_end <- if (is.na(prev_end)) stop_s else max(prev_end, stop_s)
    }
  }

  flush_buffer()
  units
}

build_ena_units_for_talent <- function(talent_name, dfs, pause_gap_sec = 2.0, add_context = FALSE) {
  if (length(dfs) == 0) return(tibble::tibble())

  purrr::imap_dfr(dfs, function(df, sheet_name) {
    out <- build_ena_units_from_clean_df(df, pause_gap_sec = pause_gap_sec)
    if (isTRUE(add_context)) {
      out <- out |>
        dplyr::mutate(
          talent = talent_name,
          sheet = sheet_name,
          .before = 1
        )
    }
    out
  })
}

write_ena_units_txt <- function(units_df, txt_path) {
  if (nrow(units_df) == 0) {
    writeLines(character(0), txt_path, useBytes = TRUE)
    return(invisible(txt_path))
  }

  lines <- character()
  for (i in seq_len(nrow(units_df))) {
    lines <- c(
      lines,
      sprintf("[%s - %s] (%s) | %s | %s",
        units_df$start[[i]],
        units_df$end[[i]],
        units_df$unit_type[[i]],
        units_df$talent[[i]],
        units_df$sheet[[i]]
      ),
      units_df$text[[i]],
      ""
    )
  }

  writeLines(lines, txt_path, useBytes = TRUE)
  invisible(txt_path)
}

subtitle_word_count <- function(text) {
  text <- stringr::str_squish(as.character(text))
  vapply(text, function(value) {
    if (is.na(value) || !nzchar(value)) return(0L)
    length(stringr::str_split(value, "\\s+")[[1]])
  }, integer(1))
}

subtitle_pick_column <- function(df, candidates, required = TRUE) {
  matched <- match(tolower(candidates), tolower(names(df)), nomatch = 0L)
  matched <- matched[matched > 0L]
  if (length(matched) > 0L) return(names(df)[[matched[[1]]]])
  if (!isTRUE(required)) return(NA_character_)
  stop(
    "Missing required subtitle column; expected one of: ",
    paste(candidates, collapse = ", "),
    call. = FALSE
  )
}

subtitle_language_is_english <- function(language, allow_unknown = TRUE) {
  normalized <- tolower(trimws(as.character(language)))
  unknown <- is.na(normalized) | normalized == ""
  english <- normalized == "english" | grepl("^en([_-].*)?$", normalized)
  english | (unknown & isTRUE(allow_unknown))
}

subtitle_word_locations <- function(text) {
  text <- as.character(text)
  locations <- stringr::str_locate_all(
    text,
    stringr::regex("[[:alnum:]]+(?:['’][[:alnum:]]+)*")
  )[[1]]
  if (nrow(locations) == 0L) {
    return(tibble::tibble(start = integer(), end = integer(), word = character()))
  }

  starts <- locations[, "start"]
  ends <- locations[, "end"]
  tibble::tibble(
    start = starts,
    end = ends,
    word = stringr::str_sub(text, starts, ends)
  )
}

largest_exact_word_overlap <- function(existing_words, new_words) {
  max_overlap <- min(length(existing_words), length(new_words))
  if (max_overlap == 0L) return(0L)

  existing_compare <- tolower(existing_words)
  new_compare <- tolower(new_words)
  for (overlap in seq.int(max_overlap, 1L)) {
    if (identical(
      utils::tail(existing_compare, overlap),
      utils::head(new_compare, overlap)
    )) {
      return(as.integer(overlap))
    }
  }
  0L
}

deduplicate_caption_overlaps <- function(caption_text) {
  caption_text <- stringr::str_squish(as.character(caption_text))
  accepted_words <- character()
  appended_text <- rep("", length(caption_text))

  for (i in seq_along(caption_text)) {
    text <- caption_text[[i]]
    if (is.na(text) || !nzchar(text)) next

    word_locations <- subtitle_word_locations(text)
    new_words <- word_locations$word
    overlap <- largest_exact_word_overlap(accepted_words, new_words)

    if (overlap == 0L) {
      appended_text[[i]] <- text
    } else if (overlap < length(new_words)) {
      next_word_start <- word_locations$start[[overlap + 1L]]
      appended_text[[i]] <- stringr::str_squish(stringr::str_sub(text, next_word_start))
    }

    if (overlap < length(new_words)) {
      accepted_words <- c(accepted_words, new_words[seq.int(overlap + 1L, length(new_words))])
    }
  }

  appended_text
}

normalize_punctuation_model_input <- function(text) {
  text <- as.character(text)
  text <- stringr::str_replace_all(text, stringr::fixed(">>"), " ")
  text <- stringr::str_replace_all(text, "[?!:;]+", " ")
  text <- stringr::str_replace_all(text, "(?<![0-9])\\.+(?![0-9])", " ")
  text <- stringr::str_replace_all(text, "(?<![0-9]),+(?![0-9])", " ")
  stringr::str_squish(text)
}

normalize_fullstop_punctuation <- function(text) {
  text <- stringr::str_replace_all(as.character(text), "\\.{2,}", ".")
  text <- stringr::str_replace_all(text, "\\?{2,}", "?")
  stringr::str_replace_all(text, ",{2,}", ",")
}

capitalize_first_alphabetic <- function(text) {
  text <- as.character(text)
  location <- stringr::str_locate(text, stringr::regex("[[:alpha:]]"))
  if (length(text) != 1L || is.na(text) || is.na(location[[1]])) return(text)

  position <- location[[1]]
  paste0(
    stringr::str_sub(text, 1L, position - 1L),
    toupper(stringr::str_sub(text, position, position)),
    stringr::str_sub(text, position + 1L)
  )
}

expand_speaker_turn_segments <- function(work) {
  if (nrow(work) == 0L) {
    work$speaker_turn_id <- integer()
    work$speaker_turn_marked <- logical()
    return(work)
  }

  segments <- list()
  for (video_id in unique(work$video_id)) {
    video_rows <- which(work$video_id == video_id)
    current_turn <- 0L
    pending_boundary <- FALSE

    add_segment <- function(row_index, value, starts_marked = FALSE) {
      value <- stringr::str_squish(value)
      if (!nzchar(value)) return(invisible(NULL))

      effective_marker <- isTRUE(starts_marked) || pending_boundary
      if (effective_marker) {
        current_turn <<- if (current_turn == 0L) 1L else current_turn + 1L
      } else if (current_turn == 0L) {
        current_turn <<- 1L
      }

      row <- work[row_index, , drop = FALSE]
      row$text <- value
      row$speaker_turn_id <- current_turn
      row$speaker_turn_marked <- effective_marker
      segments[[length(segments) + 1L]] <<- row
      pending_boundary <<- FALSE
      invisible(NULL)
    }

    for (row_index in video_rows) {
      text <- work$text[[row_index]]
      marker_count <- stringr::str_count(text, stringr::fixed(">>"))
      parts <- strsplit(text, ">>", fixed = TRUE)[[1]]
      expected_parts <- marker_count + 1L
      if (length(parts) < expected_parts) {
        parts <- c(parts, rep("", expected_parts - length(parts)))
      }

      add_segment(row_index, parts[[1]], starts_marked = FALSE)
      if (marker_count == 0L) next

      for (part_index in seq_len(marker_count) + 1L) {
        pending_boundary <- TRUE
        add_segment(row_index, parts[[part_index]], starts_marked = TRUE)
      }
    }
  }

  dplyr::bind_rows(segments)
}

build_punctuation_blocks <- function(
    df,
    target_words = 175L,
    max_words = 200L,
    subtitle_language = NULL,
    talent_name = NULL) {
  if (target_words < 1L || max_words < target_words) {
    stop("Block word limits must satisfy 1 <= target_words <= max_words.", call. = FALSE)
  }

  video_col <- subtitle_pick_column(df, c("VideoID", "video_id"))
  start_col <- subtitle_pick_column(df, c("start_sec", "subtitle_start"))
  stop_col <- subtitle_pick_column(df, c("stop_sec", "end_sec", "subtitle_end"))
  text_col <- subtitle_pick_column(df, c("Text", "text", "subtitle_text"))
  language_col <- subtitle_pick_column(
    df,
    c("subtitle_language", "language", "lang"),
    required = FALSE
  )

  language_values <- if (!is.null(subtitle_language)) {
    rep(as.character(subtitle_language[[1]]), nrow(df))
  } else if (!is.na(language_col)) {
    as.character(df[[language_col]])
  } else {
    rep(NA_character_, nrow(df))
  }

  work <- tibble::tibble(
    source_order = seq_len(nrow(df)),
    video_id = as.character(df[[video_col]]),
    start_sec = suppressWarnings(as.numeric(df[[start_col]])),
    end_sec = suppressWarnings(as.numeric(df[[stop_col]])),
    text = stringr::str_squish(as.character(df[[text_col]])),
    subtitle_language = language_values
  ) |>
    dplyr::filter(
      !is.na(.data$video_id),
      .data$video_id != "",
      !is.na(.data$text),
      .data$text != ""
    ) |>
    dplyr::arrange(.data$video_id, .data$start_sec, .data$end_sec, .data$source_order)

  work <- expand_speaker_turn_segments(work) |>
    dplyr::group_by(.data$video_id, .data$speaker_turn_id) |>
    dplyr::mutate(
      speaker_turn_marked = any(.data$speaker_turn_marked),
      deduplicated_text = deduplicate_caption_overlaps(.data$text)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      model_input_text = normalize_punctuation_model_input(.data$deduplicated_text),
      word_count = subtitle_word_count(.data$model_input_text)
    ) |>
    dplyr::filter(.data$word_count > 0L)

  empty_blocks <- tibble::tibble(
    video_id = character(),
    talent_name = character(),
    speaker_turn_id = integer(),
    speaker_turn_marked = logical(),
    speaker_turn_block_number = integer(),
    block_number = integer(),
    start_sec = numeric(),
    end_sec = numeric(),
    original_text = character(),
    model_input_text = character(),
    word_count = integer(),
    subtitle_language = character()
  )
  if (nrow(work) == 0L) return(empty_blocks)

  make_block <- function(indices, block_number, turn_block_number) {
    block_rows <- work[indices, , drop = FALSE]
    start_sec <- suppressWarnings(min(block_rows$start_sec, na.rm = TRUE))
    end_sec <- suppressWarnings(max(block_rows$end_sec, na.rm = TRUE))
    if (!is.finite(start_sec)) start_sec <- NA_real_
    if (!is.finite(end_sec)) end_sec <- NA_real_

    languages <- unique(block_rows$subtitle_language)
    languages <- languages[!is.na(languages) & nzchar(trimws(languages))]

    original_text <- stringr::str_squish(paste(block_rows$deduplicated_text, collapse = " "))
    model_input_text <- normalize_punctuation_model_input(original_text)

    tibble::tibble(
      video_id = block_rows$video_id[[1]],
      talent_name = if (is.null(talent_name)) NA_character_ else as.character(talent_name[[1]]),
      speaker_turn_id = as.integer(block_rows$speaker_turn_id[[1]]),
      speaker_turn_marked = isTRUE(block_rows$speaker_turn_marked[[1]]),
      speaker_turn_block_number = as.integer(turn_block_number),
      block_number = as.integer(block_number),
      start_sec = start_sec,
      end_sec = end_sec,
      original_text = original_text,
      model_input_text = model_input_text,
      word_count = subtitle_word_count(model_input_text),
      subtitle_language = if (length(languages) == 0L) NA_character_ else languages[[1]]
    )
  }

  blocks <- list()
  for (video_id in unique(work$video_id)) {
    video_indices <- which(work$video_id == video_id)
    block_number <- 0L

    for (turn_id in unique(work$speaker_turn_id[video_indices])) {
      turn_indices <- video_indices[work$speaker_turn_id[video_indices] == turn_id]
      current_indices <- integer()
      current_words <- 0L
      turn_block_number <- 0L

      emit_current <- function() {
        if (length(current_indices) == 0L) return(invisible(NULL))
        block_number <<- block_number + 1L
        turn_block_number <<- turn_block_number + 1L
        blocks[[length(blocks) + 1L]] <<- make_block(
          current_indices,
          block_number,
          turn_block_number
        )
        current_indices <<- integer()
        current_words <<- 0L
        invisible(NULL)
      }

      for (row_index in turn_indices) {
        next_words <- work$word_count[[row_index]]
        if (length(current_indices) > 0L && current_words + next_words > max_words) {
          emit_current()
        }

        current_indices <- c(current_indices, row_index)
        current_words <- current_words + next_words
        if (current_words >= target_words) emit_current()
      }
      emit_current()
    }
  }

  dplyr::bind_rows(blocks)
}

split_punctuated_sentences <- function(text) {
  text <- stringr::str_squish(as.character(text))
  if (length(text) != 1L || is.na(text) || !nzchar(text)) return(character())

  marked <- stringr::str_replace_all(
    text,
    "([.!?]+[\\\"'’”]*)[[:space:]]+",
    "\\1\n"
  )
  sentences <- unlist(strsplit(marked, "\n", fixed = TRUE), use.names = FALSE)
  sentences <- stringr::str_squish(sentences)
  sentences <- vapply(sentences, capitalize_first_alphabetic, character(1), USE.NAMES = FALSE)
  sentences[!is.na(sentences) & nzchar(sentences)]
}

empty_sentence_units <- function() {
  tibble::tibble(
    video_id = character(),
    talent_name = character(),
    speaker_turn_id = integer(),
    speaker_change = logical(),
    block_number = integer(),
    sentence_number = integer(),
    start_sec = numeric(),
    end_sec = numeric(),
    text = character(),
    punctuation_model = character(),
    timestamps_approximate = logical(),
    timestamp_method = character()
  )
}

sentence_units_from_block <- function(block, punctuated_text, punctuation_model = NA_character_) {
  if (nrow(block) != 1L) stop("`block` must contain exactly one row.", call. = FALSE)

  sentences <- split_punctuated_sentences(punctuated_text)
  if (length(sentences) == 0L) {
    return(empty_sentence_units())
  }

  word_counts <- subtitle_word_count(sentences)
  total_words <- sum(word_counts)
  if (total_words <= 0L) return(empty_sentence_units())

  block_start <- as.numeric(block$start_sec[[1]])
  block_end <- as.numeric(block$end_sec[[1]])
  duration <- max(0, block_end - block_start)
  cumulative_fraction <- cumsum(word_counts) / total_words
  end_sec <- block_start + duration * cumulative_fraction
  start_sec <- c(block_start, utils::head(end_sec, -1L))

  speaker_turn_id <- if ("speaker_turn_id" %in% names(block)) {
    as.integer(block$speaker_turn_id[[1]])
  } else {
    1L
  }
  speaker_turn_marked <- if ("speaker_turn_marked" %in% names(block)) {
    isTRUE(block$speaker_turn_marked[[1]])
  } else {
    FALSE
  }
  turn_block_number <- if ("speaker_turn_block_number" %in% names(block)) {
    as.integer(block$speaker_turn_block_number[[1]])
  } else {
    1L
  }
  speaker_change <- rep(FALSE, length(sentences))
  speaker_change[[1]] <- speaker_turn_marked && turn_block_number == 1L

  tibble::tibble(
    video_id = as.character(block$video_id[[1]]),
    talent_name = as.character(block$talent_name[[1]]),
    speaker_turn_id = speaker_turn_id,
    speaker_change = speaker_change,
    block_number = as.integer(block$block_number[[1]]),
    sentence_number = seq_along(sentences),
    start_sec = start_sec,
    end_sec = end_sec,
    text = sentences,
    punctuation_model = as.character(punctuation_model[[1]]),
    timestamps_approximate = TRUE,
    timestamp_method = "block_word_proportion"
  )
}

reconstruct_sentence_units <- function(
    blocks,
    url = "http://192.168.1.165:8000/v1/punctuate",
    timeout_sec = 120,
    allow_unknown_language = TRUE,
    punctuate_fn = punctuate_text) {
  if (nrow(blocks) == 0L) return(empty_sentence_units())

  results <- vector("list", nrow(blocks))
  for (i in seq_len(nrow(blocks))) {
    block <- blocks[i, , drop = FALSE]
    if (!subtitle_language_is_english(
      block$subtitle_language[[1]],
      allow_unknown = allow_unknown_language
    )) {
      next
    }

    model_input_text <- if ("model_input_text" %in% names(block)) {
      block$model_input_text[[1]]
    } else {
      normalize_punctuation_model_input(block$original_text[[1]])
    }
    response <- punctuate_fn(
      text = model_input_text,
      url = url,
      timeout_sec = timeout_sec,
      include_model = TRUE
    )
    if (is.character(response)) {
      response <- list(text = response[[1]], model = NA_character_)
    }
    response$text <- normalize_fullstop_punctuation(response$text)
    results[[i]] <- sentence_units_from_block(
      block,
      punctuated_text = response$text,
      punctuation_model = response$model
    )
  }

  sentences <- dplyr::bind_rows(results)
  if (nrow(sentences) == 0L) return(empty_sentence_units())

  sentences |>
    dplyr::arrange(.data$video_id, .data$block_number, .data$sentence_number) |>
    dplyr::group_by(.data$video_id) |>
    dplyr::mutate(sentence_number = dplyr::row_number()) |>
    dplyr::ungroup()
}

write_sentence_units_parquet <- function(sentence_units, output_path) {
  required <- c(
    "video_id", "speaker_turn_id", "speaker_change", "block_number",
    "sentence_number", "start_sec", "end_sec", "text", "punctuation_model"
  )
  missing <- setdiff(required, names(sentence_units))
  if (length(missing) > 0L) {
    stop("Sentence units are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  output_dir <- dirname(output_path)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  temporary_path <- tempfile("sentence-units-", tmpdir = output_dir, fileext = ".parquet")
  on.exit(unlink(temporary_path), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "sentence_units_output", as.data.frame(sentence_units))
  on.exit(duckdb::duckdb_unregister(con, "sentence_units_output"), add = TRUE)

  quoted_path <- as.character(DBI::dbQuoteString(con, temporary_path))
  DBI::dbExecute(
    con,
    paste0(
      "COPY sentence_units_output TO ", quoted_path,
      " (FORMAT PARQUET, COMPRESSION ZSTD)"
    )
  )
  if (!file.copy(temporary_path, output_path, overwrite = TRUE)) {
    stop("Could not write sentence Parquet: ", output_path, call. = FALSE)
  }

  invisible(output_path)
}

reconstruct_sentence_file <- function(
    input_path,
    output_path,
    subtitle_language = NULL,
    talent_name = NULL,
    target_words = 175L,
    max_words = 200L,
    url = "http://192.168.1.165:8000/v1/punctuate",
    timeout_sec = 120,
    allow_unknown_language = TRUE,
    punctuate_fn = punctuate_text) {
  cleaned <- readr::read_csv(input_path, show_col_types = FALSE)
  blocks <- build_punctuation_blocks(
    cleaned,
    target_words = target_words,
    max_words = max_words,
    subtitle_language = subtitle_language,
    talent_name = talent_name
  )
  sentences <- reconstruct_sentence_units(
    blocks,
    url = url,
    timeout_sec = timeout_sec,
    allow_unknown_language = allow_unknown_language,
    punctuate_fn = punctuate_fn
  )
  write_sentence_units_parquet(sentences, output_path)

  list(
    input_path = input_path,
    output_path = output_path,
    cleaned_rows = nrow(cleaned),
    blocks = blocks,
    sentences = sentences
  )
}
