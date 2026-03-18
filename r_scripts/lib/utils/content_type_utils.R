content_type_pick_col <- function(df, col = NULL, candidates = character()) {
  if (!is.null(col) && (col %in% names(df))) {
    return(col)
  }
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) {
    return(hit[[1]])
  }
  NULL
}

content_type_normalize <- function(x) {
  vals <- tolower(trimws(as.character(x)))
  vals[vals %in% c("", "na", "null")] <- NA_character_

  out <- rep(NA_character_, length(vals))
  out[grepl("short", vals, perl = TRUE)] <- "short"
  out[is.na(out) & grepl("live|stream", vals, perl = TRUE)] <- "live"
  out[is.na(out) & grepl("video|vod|upload|regular", vals, perl = TRUE)] <- "video"
  out
}

content_type_parse_iso8601 <- function(x) {
  vals <- toupper(trimws(as.character(x)))
  pattern <- "^PT(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)S)?$"

  out <- rep(NA_real_, length(vals))
  for (i in seq_along(vals)) {
    xi <- vals[[i]]
    if (is.na(xi) || !nzchar(xi)) {
      next
    }
    m <- regmatches(xi, regexec(pattern, xi, perl = TRUE))[[1]]
    if (length(m) == 0) {
      next
    }
    h <- suppressWarnings(as.numeric(m[[2]]))
    mn <- suppressWarnings(as.numeric(m[[3]]))
    s <- suppressWarnings(as.numeric(m[[4]]))
    if (is.na(h)) h <- 0
    if (is.na(mn)) mn <- 0
    if (is.na(s)) s <- 0
    out[[i]] <- h * 3600 + mn * 60 + s
  }
  out
}

resolve_content_type <- function(
  df,
  raw_col = "Content Type",
  classified_col = "content_type",
  title_col = NULL,
  tags_col = NULL,
  duration_seconds_col = NULL,
  duration_iso_col = NULL,
  live_min_seconds = 20 * 60,
  short_max_seconds = 70
) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  n <- nrow(df)
  if (n == 0) {
    return(data.frame(
      content_type = character(),
      content_type_source = character(),
      content_type_rule = character(),
      stringsAsFactors = FALSE
    ))
  }

  raw_col <- content_type_pick_col(df, raw_col, c("Content Type", "content_type_raw"))
  classified_col <- content_type_pick_col(df, classified_col, c("content_type", "Content Type"))
  title_col <- content_type_pick_col(df, title_col, c("Title", "Video Title", "title", "title_raw", "video_title"))
  tags_col <- content_type_pick_col(df, tags_col, c("tags", "Tags"))
  duration_seconds_col <- content_type_pick_col(
    df,
    duration_seconds_col,
    c("DurationSeconds", "duration_seconds")
  )
  duration_iso_col <- content_type_pick_col(
    df,
    duration_iso_col,
    c("DurationISO", "duration_iso", "duration")
  )

  raw_vals <- if (!is.null(raw_col)) content_type_normalize(df[[raw_col]]) else rep(NA_character_, n)
  classified_vals <- if (!is.null(classified_col)) content_type_normalize(df[[classified_col]]) else rep(NA_character_, n)

  resolved <- ifelse(!is.na(classified_vals), classified_vals, raw_vals)
  source <- ifelse(
    !is.na(classified_vals), "classified",
    ifelse(!is.na(raw_vals), "raw", "fallback")
  )
  rule <- ifelse(
    !is.na(classified_vals), "classified",
    ifelse(!is.na(raw_vals), "raw", "fallback_video")
  )

  duration_seconds <- rep(NA_real_, n)
  if (!is.null(duration_seconds_col)) {
    duration_seconds <- suppressWarnings(as.numeric(df[[duration_seconds_col]]))
  }
  if (!is.null(duration_iso_col)) {
    iso_seconds <- content_type_parse_iso8601(df[[duration_iso_col]])
    idx <- is.na(duration_seconds) & !is.na(iso_seconds)
    duration_seconds[idx] <- iso_seconds[idx]
  }

  title_txt <- if (!is.null(title_col)) tolower(trimws(as.character(df[[title_col]]))) else rep("", n)
  tags_txt <- if (!is.null(tags_col)) tolower(trimws(as.character(df[[tags_col]]))) else rep("", n)
  txt <- paste(title_txt, tags_txt)

  has_live_cue <- grepl(
    "(^|[^a-z])(live|livestream|stream|q\\s*&\\s*a|q\\s*a|ama|podcast|full\\s+stream|concert\\s+live)([^a-z]|$)",
    txt,
    perl = TRUE
  )
  has_video_cue <- grepl(
    "(^|[^a-z])(official(\\s+music)?\\s+video|lyric\\s+video|original\\s+song|cover|music\\s+video|mv)([^a-z]|$)",
    txt,
    perl = TRUE
  )
  has_short_cue <- grepl("(^|[^a-z])shorts([^a-z]|$)|#shorts", txt, perl = TRUE)

  short_override <- has_short_cue | (!is.na(duration_seconds) & duration_seconds <= short_max_seconds)
  idx_short <- short_override & resolved != "short"
  resolved[idx_short] <- "short"
  source[idx_short] <- ifelse(has_short_cue[idx_short], "title_tags", "duration")
  rule[idx_short] <- "short_override"

  live_to_video <- resolved == "live" & (
    (!is.na(duration_seconds) & duration_seconds < live_min_seconds & !has_live_cue) |
      (has_video_cue & (is.na(duration_seconds) | duration_seconds < live_min_seconds * 3))
  )
  resolved[live_to_video] <- "video"
  source[live_to_video] <- ifelse(
    has_video_cue[live_to_video],
    "title_tags",
    ifelse(!is.na(duration_seconds[live_to_video]), "duration", source[live_to_video])
  )
  rule[live_to_video] <- "live_to_video_filter"

  unknown <- is.na(resolved) | !resolved %in% c("short", "video", "live")
  resolved[unknown] <- "video"
  source[unknown] <- "fallback"
  rule[unknown] <- "fallback_video"

  data.frame(
    content_type = resolved,
    content_type_source = source,
    content_type_rule = rule,
    stringsAsFactors = FALSE
  )
}

apply_content_type_filter <- function(
  df,
  output_col = "Content Type",
  keep_diagnostics = TRUE,
  source_col = "content_type_source",
  rule_col = "content_type_rule",
  ...
) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  resolved <- resolve_content_type(df, ...)
  df[[output_col]] <- resolved$content_type

  # Keep both naming conventions aligned so downstream code is consistent.
  if ("content_type" %in% names(df) || output_col == "Content Type") {
    df[["content_type"]] <- resolved$content_type
  }

  if (isTRUE(keep_diagnostics)) {
    df[[source_col]] <- resolved$content_type_source
    df[[rule_col]] <- resolved$content_type_rule
  }

  df
}
