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
