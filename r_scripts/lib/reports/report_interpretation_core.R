report_interp_first_col <- function(df, candidates) {
  if (!is.data.frame(df)) {
    return(NULL)
  }
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NULL else hit[[1]]
}

report_interp_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

report_interp_fmt_number <- function(x, accuracy = 1) {
  x <- report_interp_num(x)
  if (!is.finite(x)) {
    return("not available")
  }
  scales::number(x, accuracy = accuracy, big.mark = ",")
}

report_interp_fmt_count <- function(x) {
  report_interp_fmt_number(round(report_interp_num(x)), accuracy = 1)
}

report_interp_fmt_percent <- function(x, accuracy = 0.1) {
  x <- report_interp_num(x)
  if (!is.finite(x)) {
    return("not available")
  }
  scales::percent(x, accuracy = accuracy)
}

report_interp_fmt_dollar <- function(x, accuracy = 0.01) {
  x <- report_interp_num(x)
  if (!is.finite(x)) {
    return("not available")
  }
  scales::dollar(x, accuracy = accuracy)
}

report_interp_clean_label <- function(x, fallback = "this group") {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) {
    return(fallback)
  }
  out <- trimws(as.character(x[[1]]))
  if (!nzchar(out)) fallback else out
}

report_interp_human_list <- function(items, lower = TRUE) {
  items <- trimws(as.character(items))
  items <- items[nzchar(items) & !is.na(items)]
  if (isTRUE(lower)) {
    items <- tolower(items)
  }
  items <- unique(items)
  if (length(items) == 0) {
    return("")
  }
  if (length(items) == 1) {
    return(items[[1]])
  }
  if (length(items) == 2) {
    return(paste(items, collapse = " and "))
  }
  paste0(paste(items[-length(items)], collapse = ", "), ", and ", items[[length(items)]])
}

report_interp_expand_weekdays <- function(text) {
  replacements <- c(
    Mon = "Monday",
    Tue = "Tuesday",
    Wed = "Wednesday",
    Thu = "Thursday",
    Fri = "Friday",
    Sat = "Saturday",
    Sun = "Sunday"
  )
  for (abbr in names(replacements)) {
    text <- gsub(paste0("\\b", abbr, "\\b"), replacements[[abbr]], text)
  }
  text
}

report_interp_polish_text <- function(text) {
  text <- paste(as.character(text), collapse = " ")
  text <- report_interp_expand_weekdays(text)
  text <- gsub("\\s+", " ", text)
  text <- gsub("\\s+([,.;:])", "\\1", text)
  text <- gsub("([,.;:])([^[:space:]])", "\\1 \\2", text)
  text <- gsub("([0-9]),\\s+([0-9])", "\\1,\\2", text)
  text <- gsub("([0-9])\\.\\s+([0-9])", "\\1.\\2", text)
  if (gregexpr(";", text)[[1]][[1]] > 0 && length(gregexpr(";", text)[[1]]) > 1) {
    text <- gsub(";\\s+", ". ", text)
  }
  text <- gsub("\\.\\.+", ".", text)
  text <- gsub(",\\s*:", ":", text)
  text <- gsub(";\\s*,", ",", text)
  text <- gsub(":\\s*;", ":", text)
  text <- gsub("\\bA conclusion should translate evidence into review priorities, not repeat the executive summary\\.\\s*", "", text)
  text <- gsub("\\bThis interpretation should[^.]*\\.\\s*", "", text, ignore.case = TRUE)
  text <- gsub("\\bThe rule detected[^.]*\\.\\s*", "", text, ignore.case = TRUE)
  text <- gsub("\\bThe template[^.]*\\.\\s*", "", text, ignore.case = TRUE)
  text <- gsub("\\bUse this as a planning prompt[^.]*\\.\\s*", "", text, ignore.case = TRUE)
  trimws(text)
}

report_interp_pct_rank <- function(values, focal_value, higher_is_better = TRUE) {
  values <- report_interp_num(values)
  values <- values[is.finite(values)]
  focal_value <- report_interp_num(focal_value)
  if (length(values) < 2 || !is.finite(focal_value)) {
    return(NA_real_)
  }
  if (isTRUE(higher_is_better)) {
    mean(values <= focal_value)
  } else {
    mean(values >= focal_value)
  }
}

report_interp_distribution_confidence <- function(
  sample_size,
  group_count,
  percentile_rank,
  lift_vs_median = NA_real_,
  lift_vs_weighted = NA_real_,
  volatility_flag = FALSE,
  outlier_flag = FALSE,
  min_sample = 2,
  moderate_sample = 4,
  strong_sample = 8,
  moderate_lift = 0.10,
  strong_lift = 0.20
) {
  sample_size <- report_interp_num(sample_size)
  group_count <- report_interp_num(group_count)
  percentile_rank <- report_interp_num(percentile_rank)
  lift_vs_median <- report_interp_num(lift_vs_median)
  lift_vs_weighted <- report_interp_num(lift_vs_weighted)
  lift_for_score <- if (is.finite(lift_vs_median)) lift_vs_median else lift_vs_weighted

  if (
    !is.finite(sample_size) || sample_size < min_sample ||
      !is.finite(group_count) || group_count < 2 ||
      !is.finite(percentile_rank)
  ) {
    return("insufficient")
  }
  if (
    sample_size >= strong_sample &&
      percentile_rank >= 0.75 &&
      is.finite(lift_for_score) &&
      lift_for_score >= strong_lift &&
      !isTRUE(volatility_flag) &&
      !isTRUE(outlier_flag)
  ) {
    return("strong")
  }
  if (
    sample_size >= moderate_sample &&
      percentile_rank >= 0.75 &&
      (!is.finite(lift_for_score) || lift_for_score >= moderate_lift)
  ) {
    return("moderate")
  }
  "weak"
}

report_interp_distribution_signal <- function(
  df,
  group_col,
  metric_col,
  sample_col = NULL,
  weight_col = NULL,
  higher_is_better = TRUE,
  min_sample = 2,
  moderate_sample = 4,
  strong_sample = 8
) {
  empty <- list(
    available = FALSE,
    confidence = "insufficient",
    reason = "Required comparison data was unavailable."
  )
  if (!is.data.frame(df) || nrow(df) == 0 || !all(c(group_col, metric_col) %in% names(df))) {
    return(empty)
  }

  use_df <- df %>%
    dplyr::transmute(
      group = as.character(.data[[group_col]]),
      metric = report_interp_num(.data[[metric_col]]),
      sample = if (!is.null(sample_col) && sample_col %in% names(df)) {
        report_interp_num(.data[[sample_col]])
      } else {
        rep(NA_real_, dplyr::n())
      },
      weight = if (!is.null(weight_col) && weight_col %in% names(df)) {
        report_interp_num(.data[[weight_col]])
      } else if (!is.null(sample_col) && sample_col %in% names(df)) {
        report_interp_num(.data[[sample_col]])
      } else {
        rep(1, dplyr::n())
      }
    ) %>%
    dplyr::filter(!is.na(.data$group), nzchar(trimws(.data$group)), is.finite(.data$metric))

  if (nrow(use_df) < 2) {
    empty$reason <- "Fewer than two comparable groups were available."
    return(empty)
  }

  use_df <- use_df %>%
    dplyr::arrange(if (isTRUE(higher_is_better)) dplyr::desc(.data$metric) else .data$metric)
  focal <- use_df[1, , drop = FALSE]
  peer_median <- stats::median(use_df$metric, na.rm = TRUE)
  weight <- ifelse(is.finite(use_df$weight) & use_df$weight > 0, use_df$weight, 1)
  weighted_baseline <- stats::weighted.mean(use_df$metric, w = weight, na.rm = TRUE)
  lift_vs_median <- if (is.finite(peer_median) && peer_median != 0) focal$metric[[1]] / peer_median - 1 else NA_real_
  lift_vs_weighted <- if (is.finite(weighted_baseline) && weighted_baseline != 0) {
    focal$metric[[1]] / weighted_baseline - 1
  } else {
    NA_real_
  }
  sample_size <- if (is.finite(focal$sample[[1]])) focal$sample[[1]] else NA_real_
  percentile_rank <- report_interp_pct_rank(use_df$metric, focal$metric[[1]], higher_is_better = higher_is_better)
  q1 <- stats::quantile(use_df$metric, probs = 0.25, na.rm = TRUE, names = FALSE)
  q3 <- stats::quantile(use_df$metric, probs = 0.75, na.rm = TRUE, names = FALSE)
  iqr <- q3 - q1
  volatility_flag <- is.finite(iqr) && is.finite(peer_median) && abs(peer_median) > 0 && abs(iqr / peer_median) > 1.5
  outlier_flag <- is.finite(iqr) && iqr > 0 && focal$metric[[1]] > q3 + 1.5 * iqr
  confidence <- report_interp_distribution_confidence(
    sample_size = sample_size,
    group_count = nrow(use_df),
    percentile_rank = percentile_rank,
    lift_vs_median = lift_vs_median,
    lift_vs_weighted = lift_vs_weighted,
    volatility_flag = volatility_flag,
    outlier_flag = outlier_flag,
    min_sample = min_sample,
    moderate_sample = moderate_sample,
    strong_sample = strong_sample
  )

  list(
    available = TRUE,
    group = focal$group[[1]],
    metric = focal$metric[[1]],
    sample_size = sample_size,
    group_count = nrow(use_df),
    peer_median = peer_median,
    weighted_baseline = weighted_baseline,
    lift_vs_median = lift_vs_median,
    lift_vs_weighted = lift_vs_weighted,
    percentile_rank = percentile_rank,
    quartile = dplyr::case_when(
      percentile_rank >= 0.75 ~ "top quartile",
      percentile_rank >= 0.50 ~ "upper half",
      percentile_rank >= 0.25 ~ "lower half",
      TRUE ~ "bottom quartile"
    ),
    confidence = confidence,
    volatility_flag = volatility_flag,
    outlier_flag = outlier_flag,
    data = use_df
  )
}

report_interp_result <- function(
  slot,
  finding,
  evidence,
  implication,
  caveat,
  title = NA_character_,
  why_it_matters = NA_character_,
  recommendation = implication,
  next_check = NA_character_,
  confidence = c("weak", "moderate", "strong", "insufficient"),
  priority = c("low", "medium", "high"),
  action_level = c("monitor", "controlled_test", "scale_or_prioritize", "review_examples", "do_not_recommend"),
  sample_size = NA_real_,
  metric_name = NA_character_,
  metric_value = NA_real_,
  comparison_name = NA_character_,
  comparison_value = NA_real_,
  source_table = NA_character_,
  rule_id = NA_character_,
  fallback = FALSE
) {
  confidence <- match.arg(confidence)
  priority <- match.arg(priority)
  action_level <- match.arg(action_level)
  tibble::tibble(
    slot = slot,
    title = title,
    finding = finding,
    why_it_matters = why_it_matters,
    evidence = evidence,
    implication = implication,
    recommendation = recommendation,
    next_check = next_check,
    caveat = caveat,
    confidence = confidence,
    priority = priority,
    action_level = action_level,
    sample_size = as.numeric(sample_size),
    metric_name = metric_name,
    metric_value = as.numeric(metric_value),
    comparison_name = comparison_name,
    comparison_value = as.numeric(comparison_value),
    source_table = source_table,
    rule_id = rule_id,
    fallback = isTRUE(fallback)
  )
}

report_interp_markdown <- function(result) {
  if (!is.data.frame(result) || nrow(result) == 0) {
    return("")
  }
  preferred <- c("finding", "why_it_matters", "evidence", "recommendation", "next_check", "caveat")
  fallback <- c("finding", "evidence", "implication", "caveat")
  cols <- preferred[preferred %in% names(result)]
  if (length(cols) == 0) {
    cols <- fallback[fallback %in% names(result)]
  }
  parts <- unlist(result[1, cols], use.names = FALSE)
  parts <- trimws(as.character(parts))
  parts <- parts[nzchar(parts) & !is.na(parts)]
  report_interp_polish_text(paste(parts, collapse = " "))
}
