args <- commandArgs(trailingOnly = TRUE)

library(dplyr)

arg_value <- function(args, flag, default = "") {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit[[1]] == length(args)) {
    return(default)
  }
  args[[hit[[1]] + 1]]
}

has_flag <- function(args, flag) {
  any(args == flag)
}

usage <- function() {
  cat(
    "Usage: Rscript r_scripts/run/bundles/bundle_A/check_bundle_A_interpretations.R --output-dir DIR\n",
    "       Rscript r_scripts/run/bundles/bundle_A/check_bundle_A_interpretations.R --interpret-root DIR\n",
    sep = ""
  )
}

if (has_flag(args, "-h") || has_flag(args, "--help")) {
  usage()
  quit(status = 0)
}

output_dir <- trimws(arg_value(args, "--output-dir", ""))
interpret_root <- trimws(arg_value(args, "--interpret-root", ""))
if (!nzchar(interpret_root) && nzchar(output_dir)) {
  interpret_root <- file.path(output_dir, "interpretations")
}
if (!nzchar(interpret_root)) {
  stop("Provide --output-dir or --interpret-root.")
}
if (!dir.exists(interpret_root)) {
  stop("Interpretation root does not exist: ", interpret_root)
}

result_path <- file.path(interpret_root, "bundle_a_interpretation_results.csv")
if (!file.exists(result_path)) {
  stop("Missing structured result file: ", result_path)
}

results <- readr::read_csv(result_path, show_col_types = FALSE)
output_files <- list.files(interpret_root, pattern = "output[.]md$", recursive = TRUE, full.names = TRUE)
if (length(output_files) == 0) {
  stop("No output.md files found under: ", interpret_root)
}
texts <- vapply(output_files, function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), character(1))
all_text <- paste(texts, collapse = "\n\n")

checks <- tibble::tibble(
  check = character(),
  passed = logical(),
  detail = character()
)

add_check <- function(name, condition, detail = "") {
  checks <<- dplyr::bind_rows(
    checks,
    tibble::tibble(check = name, passed = isTRUE(condition), detail = detail)
  )
}

slot_text <- function(slot) {
  path <- file.path(interpret_root, slot, "output.md")
  if (!file.exists(path)) {
    return("")
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

combined <- slot_text("01_overall_performance_snapshot/02_combined_performance_trends")
exec_summary <- slot_text("05_report_bookends/01_executive_summary")
conclusion <- slot_text("05_report_bookends/02_conclusion")

causal_pattern <- "\\b(proves?|caused|guaranteed|best strategy|works because|successful because)\\b"
raw_growth_pattern <- "\\b(raw audience growth|audience growth|viewer growth|grew its audience|declined audience)\\b"
weekday_abbrev_pattern <- "\\b(Mon|Tue|Wed|Thu|Fri|Sat|Sun)\\b"
meta_pattern <- "\\b(A conclusion should|This interpretation should|The rule detected|The template|renderer logic|report-writing instructions)\\b"
bad_punctuation_pattern <- "\\.\\.|,:|;\\s*,|:\\s*;"
bad_transition_pattern <- "\\b(Start by|Then|Also)\\s+(this|that|it|the)\\s+(as|for|to)\\b"
bad_fragment_pattern <- "\\b(Then inspecting|Also treating|Then checking|Start by inspect)\\b"
bad_stitched_phrase_pattern <- paste(
  c(
    "is:\\s+(live|short|personality_short)",
    "The main limitation to watch is:",
    "reviewing this group for monetization review",
    "This synthesis combines descriptive rules",
    "\\.\\s+the middle compared",
    "\\.\\s+the comparison baseline",
    "\\.\\s+format performance",
    "\\.\\s+collaborations",
    "both a total [a-z ]+ and per-video [a-z ]+ leader",
    "Average revenue per video is led by"
  ),
  collapse = "|"
)
raw_label_sentence_start_pattern <- "(^|[.!?]\\s+)(live|short|video|personality_short|vtuber|survival|male age25-34)\\s+(contributes|averages|has|is|lead|leads)\\b"
word_counts <- vapply(strsplit(texts, "\\s+"), length, integer(1))
repeated_prioritize <- vapply(texts, function(x) {
  starts <- unlist(regmatches(x, gregexpr("(^|[.!?]\\s+)Prioritize\\b", x, ignore.case = FALSE)))
  length(starts) > 1
}, logical(1))

add_check(
  "combined trend uses metric-safe views and revenue language",
  grepl("\\bviews\\b", combined, ignore.case = TRUE) &&
    grepl("estimated revenue", combined, ignore.case = TRUE) &&
    !grepl("reporting periods", combined, ignore.case = TRUE),
  "Combined trend text should mention views, estimated revenue, and clear period labels."
)
add_check(
  "no dollar formatting attached to view language",
  !grepl("\\$[0-9,]+(?:[.][0-9]+)?[^\\n.]{0,80}\\bviews\\b|\\bviews\\b[^\\n.]{0,80}\\$[0-9,]+", all_text, ignore.case = TRUE),
  "Views should not be formatted as dollars."
)
add_check(
  "revenue uses dollar formatting when numeric revenue is shown",
  !grepl("estimated revenue[^\\n.]{0,80}\\b[0-9][0-9,]*(?: views)?\\b", all_text, ignore.case = TRUE) ||
    grepl("estimated revenue[^\\n.]{0,100}\\$", all_text, ignore.case = TRUE),
  "Estimated revenue should be formatted as dollars when a numeric value is shown."
)
add_check(
  "no vague reporting periods phrase",
  !grepl("reporting periods", all_text, ignore.case = TRUE),
  "Use monthly periods or weekly periods when period type is known."
)
add_check(
  "no generic planning prompt phrase",
  !grepl("use (this|these) .*planning prompt", all_text, ignore.case = TRUE),
  "Interpretations should give a specific recommendation or next check."
)
add_check(
  "no internal or meta report-writing language",
  !grepl(meta_pattern, all_text, ignore.case = TRUE),
  "Rendered text should speak to the client, not describe the template or writing task."
)
add_check(
  "no repeated semicolon chains",
  !any(vapply(gregexpr(";", texts), function(x) sum(x > 0) > 1, logical(1))),
  "Avoid semicolon-heavy stitched lists in one paragraph."
)
add_check(
  "no repeated Prioritize sentence starts",
  !any(repeated_prioritize),
  "Avoid consecutive recommendation sentences starting with Prioritize."
)
add_check(
  "recommendation transitions keep their action verbs",
  !grepl(bad_transition_pattern, all_text, ignore.case = FALSE),
  "Synthesized recommendations should not drop verbs, such as 'Then this as'."
)
add_check(
  "no gerund transition fragments",
  !grepl(bad_fragment_pattern, all_text, ignore.case = FALSE),
  "Avoid stitched fragments such as Then inspecting, Also treating, Then checking, or Start by inspect."
)
add_check(
  "no stitched prose phrases from rendered examples",
  !grepl(bad_stitched_phrase_pattern, all_text, ignore.case = TRUE, perl = TRUE),
  "Catch known awkward phrases from rendered Bundle A interpretation output."
)
add_check(
  "raw data labels do not start client-facing sentences",
  !grepl(raw_label_sentence_start_pattern, all_text, ignore.case = FALSE, perl = TRUE),
  "Sentence starts should use display labels such as Live streams, Shorts, VTuber, or title-cased labels."
)
add_check(
  "no duplicated tag review phrasing",
  !grepl("actual tag examples", all_text, ignore.case = TRUE) ||
    !grepl("actual videos carrying this tag", all_text, ignore.case = TRUE),
  "Do not repeat the same tag-review idea with two phrasings."
)
add_check(
  "weekday labels are client-facing",
  !grepl(weekday_abbrev_pattern, all_text),
  "Use Monday, Tuesday, etc. instead of abbreviated weekday labels."
)
add_check(
  "no double spaces or punctuation glitches",
  !grepl("  ", all_text, fixed = TRUE) && !grepl(bad_punctuation_pattern, all_text),
  "Clean repeated spaces and awkward punctuation."
)
add_check(
  "paragraphs stay within length threshold",
  max(word_counts, na.rm = TRUE) <= 220,
  "Interpretations should usually stay under 180-220 words."
)
add_check(
  "no causal language",
  !grepl(causal_pattern, all_text, ignore.case = TRUE),
  "Avoid causal or guarantee language."
)
add_check(
  "no scale action below sample threshold",
  nrow(results %>% dplyr::filter(.data$action_level == "scale_or_prioritize", is.na(.data$sample_size) | .data$sample_size < 8)) == 0,
  "scale_or_prioritize requires sample size >= 8 unless a rule explicitly overrides it."
)
add_check(
  "no strong confidence below minimum sample",
  nrow(results %>% dplyr::filter(.data$confidence == "strong", is.na(.data$sample_size) | .data$sample_size < 5)) == 0,
  "Strong recommendations should not appear on very small samples."
)
add_check(
  "no tag controlled-test below 3 videos",
  nrow(results %>% dplyr::filter(grepl("tag", .data$rule_id), .data$action_level %in% c("controlled_test", "scale_or_prioritize"), is.na(.data$sample_size) | .data$sample_size < 3)) == 0,
  "Tag controlled tests require at least 3 videos."
)
add_check(
  "no audience raw-growth claim from share data",
  !grepl(raw_growth_pattern, slot_text("03_audience_composition/01_audience_age_gender_trends"), ignore.case = TRUE),
  "Audience demographic text should discuss shares, not raw growth."
)
add_check(
  "executive summary and conclusion differ",
  nzchar(exec_summary) && nzchar(conclusion) && !identical(trimws(exec_summary), trimws(conclusion)),
  "Executive summary and conclusion should serve different roles."
)
add_check(
  "every non-fallback result has recommendation next check and caveat",
  nrow(results %>% dplyr::filter(!.data$fallback, !nzchar(.data$recommendation) | !nzchar(.data$next_check) | !nzchar(.data$caveat))) == 0,
  "Each non-fallback result needs recommendation, next_check, and caveat."
)

print(checks)
failed <- checks %>% dplyr::filter(!.data$passed)
if (nrow(failed) > 0) {
  cat("\nFailed Bundle A interpretation checks:\n")
  print(failed)
  quit(status = 1)
}

cat("\nBundle A interpretation smoke checks passed.\n")
