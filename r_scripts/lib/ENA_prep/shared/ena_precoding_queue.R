ena_precoding_build_coding_queue <- function(line_table) {
  cols <- c(
    "run_id",
    "line_id",
    "talent_name",
    "stream_id",
    "video_id",
    "line_index",
    "sec",
    "timecode",
    "source",
    "speaker",
    "text",
    "replay_line"
  )
  if (!is.data.frame(line_table) || nrow(line_table) == 0L) {
    out <- as.data.frame(setNames(replicate(length(cols), character(), simplify = FALSE), cols))
    return(out)
  }
  missing <- setdiff(cols, names(line_table))
  for (col in missing) line_table[[col]] <- NA
  line_table[, cols, drop = FALSE]
}

ena_precoding_assignment_template <- function() {
  data.frame(
    line_id = character(),
    code_id = character(),
    value = character(),
    coder = character(),
    coded_at = character(),
    notes = character(),
    stringsAsFactors = FALSE
  )
}

