if (!exists("ENA_PRECODING_EXPECTED_REPLAY_COLUMNS", mode = "function") &&
    !exists("ENA_PRECODING_EXPECTED_REPLAY_COLUMNS")) {
  source(file.path("r_scripts", "lib", "ENA_prep", "shared", "ena_precoding_sources.R"))
}

ena_precoding_normalize_text_for_id <- function(x) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- ""
  x <- tolower(trimws(x))
  gsub("\\s+", " ", x, perl = TRUE)
}

ena_precoding_line_hash <- function(...) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package `digest` is required for stable line IDs.")
  }
  digest::digest(paste(..., sep = "||"), algo = "xxhash64")
}

ena_precoding_read_replay_file <- function(source_row, run_id) {
  path <- source_row$source_path[[1]]
  df <- tryCatch(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) {
      warning("Could not read text_playback file: ", path, " (", conditionMessage(e), ")")
      NULL
    }
  )
  if (is.null(df)) return(data.frame())

  for (col in ENA_PRECODING_EXPECTED_REPLAY_COLUMNS) {
    if (!(col %in% names(df))) {
      df[[col]] <- NA
    }
  }
  df <- df[, ENA_PRECODING_EXPECTED_REPLAY_COLUMNS, drop = FALSE]
  df$video_id <- as.character(df$video_id)
  fallback_video_id <- as.character(source_row$video_id[[1]])
  missing_video <- is.na(df$video_id) | !nzchar(df$video_id)
  if (nzchar(fallback_video_id) && any(missing_video)) {
    df$video_id[missing_video] <- fallback_video_id
  }
  line_index <- seq_len(nrow(df))
  stream_id <- paste0(source_row$talent_id[[1]], ":", df$video_id)
  normalized_text <- ena_precoding_normalize_text_for_id(df$text)
  line_id <- vapply(line_index, function(i) {
    ena_precoding_line_hash(
      source_row$talent_id[[1]],
      df$video_id[[i]],
      source_row$file_name[[1]],
      line_index[[i]],
      df$sec[[i]],
      df$source[[i]],
      df$speaker[[i]],
      normalized_text[[i]]
    )
  }, character(1))

  stable <- data.frame(
    run_id = run_id,
    talent_id = source_row$talent_id[[1]],
    talent_name = source_row$talent_name[[1]],
    talent_slug = source_row$talent_slug[[1]],
    stream_id = stream_id,
    source_path = path,
    source_file = source_row$file_name[[1]],
    line_index = line_index,
    line_id = line_id,
    unit_id = stream_id,
    conversation_id = stream_id,
    stanza_id = line_index,
    stringsAsFactors = FALSE
  )
  cbind(stable, df, stringsAsFactors = FALSE)
}

ena_precoding_build_line_table <- function(source_inventory, run_id) {
  ok_sources <- source_inventory[
    !is.na(source_inventory$source_path) &
      nzchar(source_inventory$source_path) &
      file.exists(source_inventory$source_path),
    ,
    drop = FALSE
  ]
  if (nrow(ok_sources) == 0L) {
    return(data.frame())
  }
  pieces <- lapply(seq_len(nrow(ok_sources)), function(i) {
    ena_precoding_read_replay_file(ok_sources[i, , drop = FALSE], run_id = run_id)
  })
  out <- do.call(rbind, pieces)
  if (nrow(out) == 0L) return(out)
  out <- out[order(out$talent_name, out$video_id, out$line_index), , drop = FALSE]
  row.names(out) <- NULL
  out
}

