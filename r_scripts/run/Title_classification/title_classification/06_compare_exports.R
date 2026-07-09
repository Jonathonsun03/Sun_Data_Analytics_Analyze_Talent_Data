cmp_get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

cmp_repo_root <- normalizePath(file.path(cmp_get_script_dir(), "..", "..", "..", ".."), winslash = "/", mustWork = FALSE)
cmp_repo_path <- function(...) normalizePath(file.path(cmp_repo_root, ...), winslash = "/", mustWork = FALSE)

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) {
    return(default)
  }
  pos <- idx[[1]] + 1L
  if (pos > length(args)) {
    return(default)
  }
  args[[pos]]
}

left_csv <- arg_value("--left", NULL)
right_csv <- arg_value("--right", NULL)
key_col <- arg_value("--key", "video_id")
out_csv <- arg_value("--out", "")

if (is.null(left_csv) || !nzchar(left_csv)) {
  stop("Missing --left <path/to/left_export.csv>")
}
if (is.null(right_csv) || !nzchar(right_csv)) {
  stop("Missing --right <path/to/right_export.csv>")
}
if (!file.exists(left_csv)) {
  stop("Left file not found: ", left_csv)
}
if (!file.exists(right_csv)) {
  stop("Right file not found: ", right_csv)
}

left <- read.csv(left_csv, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
right <- read.csv(right_csv, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")

if (!(key_col %in% names(left))) {
  stop("Key column missing in left file: ", key_col)
}
if (!(key_col %in% names(right))) {
  stop("Key column missing in right file: ", key_col)
}

left <- left[!is.na(left[[key_col]]) & nzchar(as.character(left[[key_col]])), , drop = FALSE]
right <- right[!is.na(right[[key_col]]) & nzchar(as.character(right[[key_col]])), , drop = FALSE]

left <- left[!duplicated(left[[key_col]]), , drop = FALSE]
right <- right[!duplicated(right[[key_col]]), , drop = FALSE]

left_ids <- as.character(left[[key_col]])
right_ids <- as.character(right[[key_col]])

common_ids <- intersect(left_ids, right_ids)
left_only <- setdiff(left_ids, right_ids)
right_only <- setdiff(right_ids, left_ids)

cat("Left file:  ", normalizePath(left_csv, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("Right file: ", normalizePath(right_csv, winslash = "/", mustWork = FALSE), "\n\n", sep = "")

cat("Rows:\n")
cat("- left:  ", nrow(left), "\n", sep = "")
cat("- right: ", nrow(right), "\n", sep = "")
cat("- common key count: ", length(common_ids), "\n", sep = "")
cat("- left only: ", length(left_only), "\n", sep = "")
cat("- right only: ", length(right_only), "\n\n", sep = "")

if (length(common_ids) == 0) {
  cat("No common keys to compare.\n")
  quit(status = 0)
}

left_idx <- match(common_ids, left_ids)
right_idx <- match(common_ids, right_ids)

left_common <- left[left_idx, , drop = FALSE]
right_common <- right[right_idx, , drop = FALSE]

compare_fields <- intersect(names(left_common), names(right_common))
compare_fields <- setdiff(compare_fields, key_col)

norm_value <- function(v) {
  v <- as.character(v)
  v[is.na(v)] <- ""
  trimws(v)
}

field_changes <- data.frame(
  field = character(0),
  changed_rows = integer(0),
  change_rate_pct = numeric(0),
  stringsAsFactors = FALSE
)

change_matrix <- matrix(FALSE, nrow = length(common_ids), ncol = length(compare_fields))
colnames(change_matrix) <- compare_fields

for (j in seq_along(compare_fields)) {
  f <- compare_fields[[j]]
  lv <- norm_value(left_common[[f]])
  rv <- norm_value(right_common[[f]])
  changed <- lv != rv
  change_matrix[, j] <- changed
  n_changed <- sum(changed)
  field_changes <- rbind(
    field_changes,
    data.frame(
      field = f,
      changed_rows = n_changed,
      change_rate_pct = round(100 * n_changed / length(common_ids), 2),
      stringsAsFactors = FALSE
    )
  )
}

field_changes <- field_changes[order(-field_changes$changed_rows, field_changes$field), , drop = FALSE]

any_changed <- rowSums(change_matrix) > 0
changed_ids <- common_ids[any_changed]

cat("Changed rows on common keys: ", sum(any_changed), "/", length(common_ids), "\n\n", sep = "")
cat("Field-level changes:\n")
print(field_changes, row.names = FALSE)
cat("\n")

if ("confidence" %in% compare_fields) {
  left_conf <- suppressWarnings(as.numeric(norm_value(left_common[["confidence"]])))
  right_conf <- suppressWarnings(as.numeric(norm_value(right_common[["confidence"]])))
  valid <- !is.na(left_conf) & !is.na(right_conf)
  if (any(valid)) {
    delta <- right_conf[valid] - left_conf[valid]
    cat("Confidence deltas (right - left):\n")
    cat("- mean:   ", sprintf("%.4f", mean(delta)), "\n", sep = "")
    cat("- median: ", sprintf("%.4f", median(delta)), "\n", sep = "")
    cat("- min:    ", sprintf("%.4f", min(delta)), "\n", sep = "")
    cat("- max:    ", sprintf("%.4f", max(delta)), "\n\n", sep = "")
  }
}

if (sum(any_changed) == 0) {
  cat("No row-level differences found on shared keys.\n")
  quit(status = 0)
}

left_changed <- left_common[any_changed, , drop = FALSE]
right_changed <- right_common[any_changed, , drop = FALSE]

left_renamed <- left_changed
right_renamed <- right_changed
names(left_renamed) <- paste0("left__", names(left_renamed))
names(right_renamed) <- paste0("right__", names(right_renamed))

diff_rows <- cbind(
  data.frame(video_id = changed_ids, stringsAsFactors = FALSE),
  left_renamed,
  right_renamed,
  stringsAsFactors = FALSE
)

if (nzchar(out_csv)) {
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  write.csv(diff_rows, out_csv, row.names = FALSE, na = "")
  cat("Wrote detailed row-level diff: ", out_csv, "\n", sep = "")
}
