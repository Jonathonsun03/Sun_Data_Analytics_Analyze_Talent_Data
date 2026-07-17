source(file.path("r_scripts", "lib", "utils", "content_type_utils.R"))

assert_true <- function(x, message) {
  if (!isTRUE(x)) {
    stop(message, call. = FALSE)
  }
}

assert_equal <- function(x, y, message, tolerance = 1e-12) {
  comparison <- all.equal(x, y, check.attributes = TRUE, tolerance = tolerance)
  if (!isTRUE(comparison)) {
    stop(message, ": ", paste(comparison, collapse = " | "), call. = FALSE)
  }
}

legacy_content_type_parse_iso8601 <- function(x) {
  vals <- toupper(trimws(as.character(x)))
  pattern <- "^PT(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)S)?$"

  out <- rep(NA_real_, length(vals))
  for (i in seq_along(vals)) {
    xi <- vals[[i]]
    if (is.na(xi) || !nzchar(xi)) {
      next
    }
    match <- regmatches(xi, regexec(pattern, xi, perl = TRUE))[[1]]
    if (length(match) == 0) {
      next
    }
    hours <- suppressWarnings(as.numeric(match[[2]]))
    minutes <- suppressWarnings(as.numeric(match[[3]]))
    seconds <- suppressWarnings(as.numeric(match[[4]]))
    if (is.na(hours)) hours <- 0
    if (is.na(minutes)) minutes <- 0
    if (is.na(seconds)) seconds <- 0
    out[[i]] <- hours * 3600 + minutes * 60 + seconds
  }
  out
}

duration_values <- c(
  NA_character_,
  "",
  "   ",
  "PT",
  "PT0S",
  "pt45s",
  " PT5M ",
  "PT2H",
  "PT1H2M3S",
  "P1D",
  "PT1.5S",
  "PT-1S",
  rep(c("PT45S", "PT5M", "PT1H2M3S", NA_character_), 1000)
)

assert_equal(
  content_type_parse_iso8601(duration_values),
  legacy_content_type_parse_iso8601(duration_values),
  "Optimized ISO-8601 parsing should match the legacy parser"
)
assert_equal(
  content_type_parse_iso8601(character()),
  numeric(),
  "Empty duration vectors should return an empty numeric vector"
)
assert_true(
  is.double(content_type_parse_iso8601("PT1M")),
  "Parsed durations should remain double vectors"
)

classification_fixture <- data.frame(
  `Content Type` = c("live", "live", "video", NA, "live", "live"),
  content_type = c("live", "live", "video", NA, "live", "live"),
  Title = c(
    "Quick update",
    "Long stream",
    "Standard upload",
    "Unclassified upload",
    "Official music video",
    "Morning #shorts"
  ),
  tags = c("", "", "", "", "music", "shorts"),
  DurationSeconds = rep(NA_real_, 6),
  DurationISO = c("PT10M", "PT25M", "PT2M", NA, "PT4M", "PT25M"),
  check.names = FALSE
)

resolved <- resolve_content_type(classification_fixture)
assert_equal(
  resolved$content_type,
  c("video", "live", "video", "video", "video", "short"),
  "Duration and title overrides should preserve content classifications"
)
assert_equal(
  resolved$content_type_source,
  c("duration", "classified", "classified", "fallback", "title_tags", "title_tags"),
  "Content-type diagnostic sources changed"
)
assert_equal(
  resolved$content_type_rule,
  c(
    "live_to_video_filter",
    "classified",
    "classified",
    "fallback_video",
    "live_to_video_filter",
    "short_override"
  ),
  "Content-type diagnostic rules changed"
)

filtered <- apply_content_type_filter(classification_fixture)
assert_equal(
  filtered$`Content Type`,
  resolved$content_type,
  "Content-type filtering should use the resolved classifications"
)
assert_equal(
  filtered$content_type_source,
  resolved$content_type_source,
  "Content-type filtering should retain source diagnostics"
)

cat("content type utility tests passed\n")
