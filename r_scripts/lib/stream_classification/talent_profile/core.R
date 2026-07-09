library(jsonlite)
library(stringr)

tp_or <- function(x, y) {
  if (is.null(x) || (is.character(x) && length(x) == 1 && !nzchar(x))) y else x
}

tp_parse_args <- function(args) {
  out <- list()
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (!startsWith(key, "--")) {
      stop("Unexpected argument: ", key)
    }
    key <- sub("^--", "", key)
    key <- gsub("-", "_", key)
    if (i == length(args) || startsWith(args[[i + 1L]], "--")) {
      out[[key]] <- TRUE
      i <- i + 1L
    } else {
      out[[key]] <- args[[i + 1L]]
      i <- i + 2L
    }
  }
  out
}

tp_slugify <- function(x) {
  x <- str_to_lower(enc2utf8(as.character(x)))
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_+|_+$", "")
  if (!nzchar(x)) "talent" else x
}

tp_load_talent_rows <- function(csv_path, talent, talent_col, title_col, content_type_col) {
  if (!file.exists(csv_path)) {
    stop("CSV not found: ", csv_path)
  }

  df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  needed <- c(talent_col, title_col, content_type_col)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("CSV missing columns: ", paste(missing_cols, collapse = ", "))
  }

  rows <- df[str_to_lower(trimws(df[[talent_col]])) == str_to_lower(trimws(talent)), , drop = FALSE]
  if (nrow(rows) == 0) {
    stop("No rows found for talent: ", talent)
  }
  rows
}

tp_read_titles_csv <- function(csv_path, talent_col, title_col, content_type_col) {
  if (!file.exists(csv_path)) {
    stop("CSV not found: ", csv_path)
  }
  df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  needed <- c(talent_col, title_col, content_type_col)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("CSV missing columns: ", paste(missing_cols, collapse = ", "))
  }
  df
}

tp_distinct_talents <- function(df, talent_col) {
  vals <- unique(str_trim(enc2utf8(as.character(df[[talent_col]]))))
  vals <- vals[nzchar(vals)]
  sort(vals)
}

tp_rows_for_talents <- function(df, talent_col, talent_values) {
  norm <- str_to_lower(str_trim(enc2utf8(as.character(df[[talent_col]]))))
  wanted <- str_to_lower(str_trim(enc2utf8(as.character(talent_values))))
  df[norm %in% wanted, , drop = FALSE]
}

tp_extract_tokens <- function(titles, pattern) {
  all_tokens <- character(0)
  for (t in titles) {
    matches <- str_match_all(t, pattern)[[1]]
    if (nrow(matches) > 0) {
      tokens <- str_trim(matches[, 2])
      tokens <- tokens[nzchar(tokens)]
      all_tokens <- c(all_tokens, tokens)
    }
  }
  sort(table(all_tokens), decreasing = TRUE)
}

tp_infer_bracket_semantics <- function(top_brackets, bracket_rate) {
  if (bracket_rate < 0.1) {
    return("rare")
  }
  if (length(top_brackets) == 0) {
    return("mixed")
  }
  format_hints <- c("rantcast", "barcast", "zatsu", "podcast", "talk", "radio", "art/zatsu")
  format_hits <- sum(vapply(
    names(top_brackets),
    function(tok) any(vapply(format_hints, function(h) str_detect(str_to_lower(tok), fixed(h)), logical(1))),
    logical(1)
  ))
  format_share <- format_hits / length(top_brackets)
  if (format_share >= 0.4) {
    return("show_format")
  }
  if (bracket_rate >= 0.35) {
    return("topic")
  }
  "mixed"
}

tp_infer_extractability <- function(bracket_semantics, bracket_rate, paren_rate, short_share) {
  if (identical(bracket_semantics, "topic") && bracket_rate >= 0.35) {
    return("high")
  }
  if (bracket_semantics %in% c("show_format", "mixed") || paren_rate >= 0.2) {
    return("medium")
  }
  if (short_share >= 0.35) {
    return("low")
  }
  "medium"
}

tp_count_keyword_hits <- function(texts, keywords) {
  counts <- setNames(integer(length(keywords)), keywords)
  low <- str_to_lower(texts)
  for (k in keywords) {
    counts[[k]] <- sum(str_detect(low, fixed(k)))
  }
  counts <- counts[counts > 0]
  names(sort(counts, decreasing = TRUE))
}

tp_build_baseline_payload <- function(talent, rows, title_col, content_type_col) {
  titles <- enc2utf8(trimws(rows[[title_col]]))
  content_types <- str_to_lower(trimws(enc2utf8(rows[[content_type_col]])))
  total <- length(titles)

  bracket_counts <- tp_extract_tokens(titles, "\\[([^\\]]+)\\]")
  paren_counts <- tp_extract_tokens(titles, "\\(([^)]+)\\)")
  has_bracket <- str_detect(titles, "\\[[^\\]]+\\]")
  has_paren <- str_detect(titles, "\\([^)]+\\)")
  bracket_rate <- mean(has_bracket)
  paren_rate <- mean(has_paren)

  dist <- table(content_types)
  live_n <- as.integer(tp_or(dist[["live"]], 0L))
  video_n <- as.integer(tp_or(dist[["video"]], 0L))
  short_n <- as.integer(tp_or(dist[["short"]], 0L))
  short_share <- if (total > 0) short_n / total else 0

  top_brackets <- names(bracket_counts)[seq_len(min(10, length(bracket_counts)))]
  top_parens <- names(paren_counts)[seq_len(min(10, length(paren_counts)))]
  bracket_semantics <- tp_infer_bracket_semantics(top_brackets, bracket_rate)
  topic_extractability <- tp_infer_extractability(bracket_semantics, bracket_rate, paren_rate, short_share)

  event_hints <- c("anniversary", "birthday", "debut", "celebration", "fundraiser", "charity", "special", "milestone")
  music_hints <- c("karaoke", "cover", "acapella", "concert", "song", "live music", "mv", "vocal")
  slug <- tp_slugify(talent)

  payload <- list(
    talent_name = talent,
    talent_slug = slug,
    sample_size = total,
    distribution = list(
      live = live_n,
      video = video_n,
      short = short_n
    ),
    structure = list(
      titles_with_brackets_pct = round(bracket_rate * 100, 2),
      titles_with_parentheses_pct = round(paren_rate * 100, 2),
      bracket_semantics = bracket_semantics,
      topic_extractability = topic_extractability,
      top_bracket_tokens = unname(top_brackets),
      top_parentheses_tokens = unname(top_parens)
    ),
    signals = list(
      event_keywords = tp_count_keyword_hits(titles, event_hints),
      music_keywords = tp_count_keyword_hits(titles, music_hints)
    ),
    rule_hints = list(
      use_bracket_as_primary_topic = identical(bracket_semantics, "topic"),
      use_bracket_as_show_format = identical(bracket_semantics, "show_format"),
      parentheses_as_modifier = paren_rate >= 0.15,
      requires_low_confidence_fallback_for_shorts = short_share >= 0.3
    )
  )

  list(
    payload = payload,
    bracket_semantics = bracket_semantics,
    top_brackets = top_brackets
  )
}
