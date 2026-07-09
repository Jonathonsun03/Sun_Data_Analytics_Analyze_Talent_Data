DOW_REQUIRED_PACKAGES <- c("dplyr", "ggplot2", "readr", "tibble", "magrittr")
DOW_MISSING_PACKAGES <- DOW_REQUIRED_PACKAGES[
  !vapply(DOW_REQUIRED_PACKAGES, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(DOW_MISSING_PACKAGES) > 0L) {
  stop(
    "Missing required package(s) for day-of-week modeling: ",
    paste(DOW_MISSING_PACKAGES, collapse = ", ")
  )
}

`%>%` <- magrittr::`%>%`

DOW_WEEKDAY_ORDER <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)

DOW_WEEKDAY_LOOKUP <- c(
  Mon = "Monday",
  Monday = "Monday",
  Tue = "Tuesday",
  Tues = "Tuesday",
  Tuesday = "Tuesday",
  Wed = "Wednesday",
  Wednesday = "Wednesday",
  Thu = "Thursday",
  Thur = "Thursday",
  Thurs = "Thursday",
  Thursday = "Thursday",
  Fri = "Friday",
  Friday = "Friday",
  Sat = "Saturday",
  Saturday = "Saturday",
  Sun = "Sunday",
  Sunday = "Sunday"
)

dow_slugify <- function(x) {
  # Small local fallback so this library can work even when talent_select.R has
  # not been sourced before the weekday helpers.
  if (exists("talent_slugify", mode = "function")) {
    return(talent_slugify(x))
  }

  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "talent")
}

dow_default_output_dir <- function(talent_root, talent_name) {
  file.path(
    dirname(talent_root),
    "Processed",
    "weekday_model",
    dow_slugify(talent_name)
  )
}

dow_inspect_weekday <- function(df, weekday_col = "publish_wday") {
  # Print the exact factor state before modeling. Ordered factors are the source
  # of publish_wday.L / publish_wday.Q polynomial terms in lm().
  if (!(weekday_col %in% names(df))) {
    stop("Missing weekday column: ", weekday_col)
  }

  x <- df[[weekday_col]]
  cat("\nWeekday diagnostics\n")
  cat("Column:", weekday_col, "\n")
  cat("Class:", paste(class(x), collapse = ", "), "\n")
  cat("Ordered factor:", is.ordered(x), "\n")
  cat("Levels:", paste(levels(as.factor(x)), collapse = ", "), "\n")

  if (is.ordered(x)) {
    cat(
      "Diagnostic: publish_wday is ordered, so lm() will use polynomial ",
      "contrasts unless it is recoded.\n",
      sep = ""
    )
  } else if (is.factor(x)) {
    cat("Diagnostic: publish_wday is an unordered factor; treatment contrasts are appropriate.\n")
  } else {
    cat("Diagnostic: publish_wday is not a factor; it will be converted to an unordered factor.\n")
  }

  invisible(list(
    class = class(x),
    levels = levels(as.factor(x)),
    ordered = is.ordered(x)
  ))
}

dow_prepare_weekday_df <- function(
  df,
  views_col = "views",
  weekday_col = "publish_wday",
  reference_day = "Monday"
) {
  # Convert weekday to a plain unordered factor in calendar order. This is the
  # key modeling guardrail: lm() then uses treatment contrasts, not polynomials.
  required_cols <- c(views_col, weekday_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0L) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  out <- df %>%
    dplyr::mutate(
      views = suppressWarnings(as.numeric(.data[[views_col]])),
      publish_wday_raw = as.character(.data[[weekday_col]]),
      publish_wday_clean = dplyr::recode(
        .data$publish_wday_raw,
        !!!DOW_WEEKDAY_LOOKUP,
        .default = .data$publish_wday_raw
      ),
      publish_wday = factor(
        .data$publish_wday_clean,
        levels = DOW_WEEKDAY_ORDER,
        ordered = FALSE
      ),
      log1p_views = log1p(.data$views)
    ) %>%
    dplyr::filter(!is.na(.data$views), !is.na(.data$publish_wday))

  present_days <- levels(droplevels(out$publish_wday))
  if (length(present_days) == 0L) {
    stop("No rows remain after filtering for non-missing views and publish_wday.")
  }

  ref_day <- if (reference_day %in% present_days) {
    reference_day
  } else {
    present_days[[1]]
  }

  out %>%
    dplyr::mutate(
      publish_wday = stats::relevel(droplevels(.data$publish_wday), ref = ref_day)
    )
}

dow_summarise_weekdays <- function(df, value_col = "views") {
  # Descriptive table for practical interpretation before trusting any model.
  df %>%
    dplyr::group_by(.data$publish_wday) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_views = mean(.data[[value_col]], na.rm = TRUE),
      median_views = stats::median(.data[[value_col]], na.rm = TRUE),
      sd_views = stats::sd(.data[[value_col]], na.rm = TRUE),
      min_views = min(.data[[value_col]], na.rm = TRUE),
      max_views = max(.data[[value_col]], na.rm = TRUE),
      se_views = .data$sd_views / sqrt(.data$n),
      t_crit = dplyr::if_else(.data$n > 1, stats::qt(0.975, .data$n - 1), NA_real_),
      ci_low = .data$mean_views - .data$t_crit * .data$se_views,
      ci_high = .data$mean_views + .data$t_crit * .data$se_views,
      .groups = "drop"
    ) %>%
    dplyr::select(-"t_crit")
}

dow_save_weekday_plots <- function(df, summary_df, output_dir = NULL) {
  # Build raw and log-scale plots. When output_dir is supplied, also save PNGs.
  # The log plots are usually more stable because viral streams can dominate
  # raw-view plots.
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  boxplot_views <- ggplot2::ggplot(df, ggplot2::aes(x = .data$publish_wday, y = .data$views)) +
    ggplot2::geom_boxplot(outlier.alpha = 0.45) +
    ggplot2::labs(x = "Publish weekday", y = "Views", title = "Livestream views by weekday") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))

  mean_ci_views <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(x = .data$publish_wday, y = .data$mean_views)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
      width = 0.18
    ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(
      x = "Publish weekday",
      y = "Mean views",
      title = "Mean livestream views with 95% CI"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))

  log_df <- df %>%
    dplyr::mutate(log1p_views = log1p(.data$views))
  log_summary <- dow_summarise_weekdays(log_df, value_col = "log1p_views")

  boxplot_log_views <- ggplot2::ggplot(
    log_df,
    ggplot2::aes(x = .data$publish_wday, y = .data$log1p_views)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.45) +
    ggplot2::labs(x = "Publish weekday", y = "log1p(views)", title = "Log views by weekday") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))

  mean_ci_log_views <- ggplot2::ggplot(
    log_summary,
    ggplot2::aes(x = .data$publish_wday, y = .data$mean_views)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
      width = 0.18
    ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(
      x = "Publish weekday",
      y = "Mean log1p(views)",
      title = "Mean log views with 95% CI"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))

  if (!is.null(output_dir)) {
    ggplot2::ggsave(file.path(output_dir, "boxplot_views_by_weekday.png"), boxplot_views, width = 8, height = 5)
    ggplot2::ggsave(file.path(output_dir, "mean_views_ci_by_weekday.png"), mean_ci_views, width = 8, height = 5)
    ggplot2::ggsave(file.path(output_dir, "boxplot_log1p_views_by_weekday.png"), boxplot_log_views, width = 8, height = 5)
    ggplot2::ggsave(file.path(output_dir, "mean_log1p_views_ci_by_weekday.png"), mean_ci_log_views, width = 8, height = 5)
  }

  list(
    boxplot_views = boxplot_views,
    mean_ci_views = mean_ci_views,
    boxplot_log_views = boxplot_log_views,
    mean_ci_log_views = mean_ci_log_views
  )
}

dow_model_f_p_value <- function(model) {
  fstat <- summary(model)$fstatistic
  if (is.null(fstat)) {
    return(NA_real_)
  }

  stats::pf(fstat[["value"]], fstat[["numdf"]], fstat[["dendf"]], lower.tail = FALSE)
}

dow_tidy_lm_coefficients <- function(model, log_response = FALSE) {
  # Base R coefficient table converted into a writeable tibble. For log models,
  # weekday terms also get exp(beta) - 1 as an approximate percent difference.
  coef_mat <- summary(model)$coefficients
  out <- as.data.frame(coef_mat)
  names(out) <- c("estimate", "std_error", "statistic", "p_value")
  out$term <- rownames(out)
  rownames(out) <- NULL
  out <- out %>%
    dplyr::select("term", dplyr::everything())

  if (isTRUE(log_response)) {
    out <- out %>%
      dplyr::mutate(
        approx_percent_change = dplyr::if_else(
          .data$term == "(Intercept)",
          NA_real_,
          (exp(.data$estimate) - 1) * 100
        )
      )
  }

  out
}

dow_report_model <- function(model, model_name, log_response = FALSE, output_dir = NULL) {
  model_summary <- summary(model)
  model_formula <- paste(deparse(stats::formula(model)), collapse = " ")
  overall_f_p_value <- dow_model_f_p_value(model)

  metrics <- tibble::tibble(
    model = model_name,
    formula = model_formula,
    r_squared = model_summary$r.squared,
    adj_r_squared = model_summary$adj.r.squared,
    overall_f_p_value = overall_f_p_value
  )
  coefficients <- dow_tidy_lm_coefficients(model, log_response = log_response)

  cat("\n", model_name, "\n", sep = "")
  cat("Formula:", metrics$formula, "\n")
  cat("R-squared:", round(metrics$r_squared, 4), "\n")
  cat("Adjusted R-squared:", round(metrics$adj_r_squared, 4), "\n")
  cat("Overall F-test p-value:", signif(metrics$overall_f_p_value, 4), "\n")
  print(coefficients)

  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(metrics, file.path(output_dir, paste0(model_name, "_metrics.csv")))
    readr::write_csv(coefficients, file.path(output_dir, paste0(model_name, "_coefficients.csv")))
  }

  list(metrics = metrics, coefficients = coefficients)
}

dow_explain_weekday_coefficients <- function(model, log_response = FALSE) {
  ref_day <- levels(model$model$publish_wday)[[1]]
  cat("\nCoefficient interpretation\n")
  cat("Intercept: estimated outcome for the reference day, ", ref_day, ".\n", sep = "")
  cat("Weekday coefficients: estimated difference from ", ref_day, ".\n", sep = "")
  if (isTRUE(log_response)) {
    cat("For log1p(views), exp(coef) - 1 gives an approximate percent change.\n")
  }
}

dow_select_optional_predictors <- function(df, candidates = c("stream_duration", "content_type", "topic", "talent_name")) {
  # Only include optional predictors that exist and have more than one observed
  # value. This avoids lm() failures for one-talent / live-only notebook slices.
  candidate_optional_predictors <- intersect(candidates, names(df))
  optional_predictors <- candidate_optional_predictors[
    vapply(
      candidate_optional_predictors,
      function(col) dplyr::n_distinct(stats::na.omit(df[[col]])) > 1,
      logical(1)
    )
  ]

  dropped <- setdiff(candidate_optional_predictors, optional_predictors)
  if (length(dropped) > 0L) {
    cat(
      "\nModel C optional predictor(s) skipped because they do not vary in weekday_df: ",
      paste(dropped, collapse = ", "),
      "\n",
      sep = ""
    )
  }

  optional_predictors
}

dow_run_pairwise_weekday_tests <- function(model, model_name, output_dir = NULL) {
  anova_table <- stats::anova(model)
  weekday_p <- anova_table["publish_wday", "Pr(>F)"]

  if (is.na(weekday_p) || weekday_p >= 0.05) {
    cat("\nPairwise weekday comparisons skipped for ", model_name, ": weekday p = ", signif(weekday_p, 4), ".\n", sep = "")
    return(NULL)
  }

  if (requireNamespace("emmeans", quietly = TRUE)) {
    emm <- emmeans::emmeans(model, ~ publish_wday)
    pairs_df <- as.data.frame(emmeans::pairs(emm, adjust = "tukey"))
    if (!is.null(output_dir)) {
      readr::write_csv(pairs_df, file.path(output_dir, paste0(model_name, "_weekday_pairwise_tukey.csv")))
    }
    cat("\nPairwise weekday comparisons for ", model_name, " used emmeans with Tukey adjustment.\n", sep = "")
    print(pairs_df)
    return(pairs_df)
  }

  cat(
    "\nemmeans is not installed, so using pairwise.t.test() fallback with Holm adjustment. ",
    "This fallback is unadjusted for non-weekday covariates.\n",
    sep = ""
  )
  response <- model$model[[1]]
  pairs <- stats::pairwise.t.test(
    x = response,
    g = model$model$publish_wday,
    p.adjust.method = "holm"
  )
  pairs_df <- as.data.frame(as.table(pairs$p.value)) %>%
    dplyr::rename(day_1 = .data$Var1, day_2 = .data$Var2, p_value = .data$Freq) %>%
    dplyr::filter(!is.na(.data$p_value))
  if (!is.null(output_dir)) {
    readr::write_csv(pairs_df, file.path(output_dir, paste0(model_name, "_weekday_pairwise_holm.csv")))
  }
  print(pairs_df)
  pairs_df
}

dow_residual_diagnostics <- function(model, model_name, source_df, output_dir = NULL) {
  # Join residuals back to the modeled rows so the largest misses can be traced
  # to actual stream titles and video IDs.
  model_rows <- as.integer(rownames(stats::model.frame(model)))
  residual_df <- source_df[model_rows, , drop = FALSE] %>%
    dplyr::mutate(
      fitted_value = stats::fitted(model),
      residual = stats::residuals(model),
      std_residual = stats::rstandard(model)
    )

  largest_positive <- residual_df %>%
    dplyr::arrange(dplyr::desc(.data$residual)) %>%
    dplyr::select(
      dplyr::any_of(c("Video ID", "Title", "title_raw", "publish_wday", "views")),
      "fitted_value",
      "residual",
      "std_residual"
    ) %>%
    utils::head(10)

  largest_negative <- residual_df %>%
    dplyr::arrange(.data$residual) %>%
    dplyr::select(
      dplyr::any_of(c("Video ID", "Title", "title_raw", "publish_wday", "views")),
      "fitted_value",
      "residual",
      "std_residual"
    ) %>%
    utils::head(10)

  residual_plot <- ggplot2::ggplot(
    residual_df,
    ggplot2::aes(x = .data$fitted_value, y = .data$residual)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray45") +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::labs(
      x = "Fitted value",
      y = "Residual",
      title = paste("Residuals vs fitted:", model_name)
    ) +
    ggplot2::theme_minimal(base_size = 12)

  if (!is.null(output_dir)) {
    ggplot2::ggsave(
      file.path(output_dir, paste0(model_name, "_residuals_vs_fitted.png")),
      residual_plot,
      width = 7,
      height = 5
    )
    readr::write_csv(residual_df, file.path(output_dir, paste0(model_name, "_residuals.csv")))
  }

  cat("\nResidual diagnostics:", model_name, "\n")
  print(summary(stats::residuals(model)))
  cat("Largest positive residuals:\n")
  print(largest_positive)
  cat("Largest negative residuals:\n")
  print(largest_negative)

  outlier_count <- sum(abs(residual_df$std_residual) > 3, na.rm = TRUE)
  if (outlier_count > 0) {
    cat("Potential outlier streams with |standardized residual| > 3:", outlier_count, "\n")
  } else {
    cat("No streams exceeded |standardized residual| > 3.\n")
  }

  list(
    residuals = residual_df,
    largest_positive = largest_positive,
    largest_negative = largest_negative,
    plot = residual_plot,
    outlier_count = outlier_count
  )
}

dow_print_final_recommendation <- function(weekday_summary, model_b, diagnostics_a, diagnostics_b) {
  highest_mean_day <- weekday_summary %>%
    dplyr::arrange(dplyr::desc(.data$mean_views)) %>%
    dplyr::slice(1)

  highest_median_day <- weekday_summary %>%
    dplyr::arrange(dplyr::desc(.data$median_views)) %>%
    dplyr::slice(1)

  weekday_p_model_b <- stats::anova(model_b)["publish_wday", "Pr(>F)"]
  meaningful_weekday <- !is.na(weekday_p_model_b) && weekday_p_model_b < 0.05
  preferred_scale <- if (diagnostics_b$outlier_count <= diagnostics_a$outlier_count) {
    "log1p views"
  } else {
    "raw views"
  }

  cat("\nFinal recommendation\n")
  cat("Does day of week appear to predict viewership? ")
  if (meaningful_weekday) {
    cat("Yes, the log1p weekday model has p = ", signif(weekday_p_model_b, 4), ".\n", sep = "")
  } else {
    cat("Not strongly in this model; the log1p weekday model has p = ", signif(weekday_p_model_b, 4), ".\n", sep = "")
  }
  cat(
    "Highest average views:",
    as.character(highest_mean_day$publish_wday),
    "with mean",
    round(highest_mean_day$mean_views, 1),
    "\n"
  )
  cat(
    "Highest median views:",
    as.character(highest_median_day$publish_wday),
    "with median",
    round(highest_median_day$median_views, 1),
    "\n"
  )
  cat(
    "Preferred scale for interpretation:",
    preferred_scale,
    "because it is less sensitive to large viral streams in this run.\n"
  )
  cat(
    "Schedule recommendation: treat this as directional evidence, not a scheduling decision by itself. ",
    "Before changing schedule, control for duration, topic/content type, stream start time, seasonality, ",
    "collabs, special events, and talent-specific audience habits.\n",
    sep = ""
  )

  invisible(list(
    weekday_p_model_b = weekday_p_model_b,
    meaningful_weekday = meaningful_weekday,
    highest_mean_day = highest_mean_day,
    highest_median_day = highest_median_day,
    preferred_scale = preferred_scale
  ))
}

dow_run_weekday_workflow <- function(
  df,
  output_dir = NULL,
  reference_day = "Monday",
  optional_predictors = c("stream_duration", "content_type", "topic", "talent_name")
) {
  # End-to-end convenience wrapper for notebooks. It returns every major object
  # invisibly while also printing diagnostics. Set output_dir to save CSV/PNG
  # outputs; leave it NULL to keep the analysis notebook-only.
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  dow_inspect_weekday(df)
  weekday_df <- dow_prepare_weekday_df(df, reference_day = reference_day)

  cat("\nAfter recoding\n")
  cat("Class:", paste(class(weekday_df$publish_wday), collapse = ", "), "\n")
  cat("Ordered factor:", is.ordered(weekday_df$publish_wday), "\n")
  cat("Reference day:", levels(weekday_df$publish_wday)[[1]], "\n")
  cat("Contrasts:\n")
  print(stats::contrasts(weekday_df$publish_wday))

  weekday_summary <- dow_summarise_weekdays(weekday_df, value_col = "views") %>%
    dplyr::arrange(match(as.character(.data$publish_wday), DOW_WEEKDAY_ORDER))
  weekday_summary_by_median <- weekday_summary %>%
    dplyr::arrange(dplyr::desc(.data$median_views))

  if (!is.null(output_dir)) {
    readr::write_csv(weekday_summary, file.path(output_dir, "weekday_summary_by_order.csv"))
    readr::write_csv(weekday_summary_by_median, file.path(output_dir, "weekday_summary_by_median.csv"))
  }

  plots <- dow_save_weekday_plots(weekday_df, weekday_summary, output_dir)

  model_a <- stats::lm(views ~ publish_wday, data = weekday_df)
  model_b <- stats::lm(log1p_views ~ publish_wday, data = weekday_df)

  model_c_predictors <- dow_select_optional_predictors(weekday_df, candidates = optional_predictors)
  model_c <- NULL
  if (length(model_c_predictors) > 0L) {
    model_c_formula <- stats::reformulate(c("publish_wday", model_c_predictors), response = "log1p_views")
    model_c <- stats::lm(model_c_formula, data = weekday_df)
  }

  model_a_report <- dow_report_model(model_a, "model_a_views_weekday", log_response = FALSE, output_dir = output_dir)
  dow_explain_weekday_coefficients(model_a, log_response = FALSE)

  model_b_report <- dow_report_model(model_b, "model_b_log1p_views_weekday", log_response = TRUE, output_dir = output_dir)
  dow_explain_weekday_coefficients(model_b, log_response = TRUE)

  model_c_report <- NULL
  if (!is.null(model_c)) {
    model_c_report <- dow_report_model(model_c, "model_c_log1p_views_richer", log_response = TRUE, output_dir = output_dir)
    dow_explain_weekday_coefficients(model_c, log_response = TRUE)
  } else {
    cat("\nModel C skipped: no optional predictors exist and vary in weekday_df.\n")
  }

  pairwise_model <- if (!is.null(model_c)) model_c else model_b
  pairwise_model_name <- if (!is.null(model_c)) {
    "model_c_log1p_views_richer"
  } else {
    "model_b_log1p_views_weekday"
  }
  pairwise <- dow_run_pairwise_weekday_tests(pairwise_model, pairwise_model_name, output_dir = output_dir)

  diagnostics_a <- dow_residual_diagnostics(model_a, "model_a_views_weekday", weekday_df, output_dir = output_dir)
  diagnostics_b <- dow_residual_diagnostics(model_b, "model_b_log1p_views_weekday", weekday_df, output_dir = output_dir)
  diagnostics_c <- NULL
  if (!is.null(model_c)) {
    diagnostics_c <- dow_residual_diagnostics(model_c, "model_c_log1p_views_richer", weekday_df, output_dir = output_dir)
  }

  recommendation <- dow_print_final_recommendation(weekday_summary, model_b, diagnostics_a, diagnostics_b)
  if (!is.null(output_dir)) {
    cat("Outputs saved to:", output_dir, "\n")
  } else {
    cat("No output_dir supplied; results are kept in the returned weekday_results object.\n")
  }

  invisible(list(
    weekday_df = weekday_df,
    weekday_summary = weekday_summary,
    weekday_summary_by_median = weekday_summary_by_median,
    plots = plots,
    models = list(model_a = model_a, model_b = model_b, model_c = model_c),
    reports = list(model_a = model_a_report, model_b = model_b_report, model_c = model_c_report),
    pairwise = pairwise,
    diagnostics = list(model_a = diagnostics_a, model_b = diagnostics_b, model_c = diagnostics_c),
    recommendation = recommendation,
    output_dir = output_dir
  ))
}
