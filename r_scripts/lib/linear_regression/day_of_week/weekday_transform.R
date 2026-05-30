DOW_TRANSFORM_REQUIRED_PACKAGES <- c("dplyr", "magrittr")
DOW_TRANSFORM_MISSING_PACKAGES <- DOW_TRANSFORM_REQUIRED_PACKAGES[
  !vapply(DOW_TRANSFORM_REQUIRED_PACKAGES, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(DOW_TRANSFORM_MISSING_PACKAGES) > 0L) {
  stop(
    "Missing required package(s) for weekday transforms: ",
    paste(DOW_TRANSFORM_MISSING_PACKAGES, collapse = ", ")
  )
}

`%>%` <- magrittr::`%>%`

DOW_WEEKDAY_LEVELS <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)

DOW_WEEKDAY_LABELS <- c(
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

dow_recode_weekday_labels <- function(x) {
  # Convert weekday abbreviations from lubridate/ordered factors into full labels.
  # This keeps the mapping explicit and reusable before we create model factors.
  x <- as.character(x)
  dplyr::recode(
    x,
    !!!DOW_WEEKDAY_LABELS,
    .default = x
  )
}

dow_factor_weekday <- function(x, reference_day = "Monday") {
  # Turn full weekday labels into an unordered factor for interpretable lm()
  # treatment contrasts. The reference day is used as the intercept in lm().
  weekday_factor <- factor(
    x,
    levels = DOW_WEEKDAY_LEVELS,
    ordered = FALSE
  )

  present_days <- levels(droplevels(weekday_factor))
  if (length(present_days) == 0L) {
    return(droplevels(weekday_factor))
  }

  ref_day <- if (reference_day %in% present_days) {
    reference_day
  } else {
    present_days[[1]]
  }

  stats::relevel(droplevels(weekday_factor), ref = ref_day)
}

dow_add_model_weekday <- function(df, weekday_col = "publish_wday", reference_day = "Monday") {
  # Add transparent intermediate columns:
  # - publish_wday_label: full weekday text
  # - publish_wday: unordered model factor with the requested reference day
  if (!(weekday_col %in% names(df))) {
    stop("Missing weekday column: ", weekday_col)
  }

  df %>%
    dplyr::mutate(
      publish_wday_label = dow_recode_weekday_labels(.data[[weekday_col]]),
      publish_wday = dow_factor_weekday(
        .data$publish_wday_label,
        reference_day = reference_day
      )
    )
}

dow_prep <- function(
  df,
  views_col = "views",
  weekday_col = "publish_wday",
  reference_day = "Monday"
) {
  # Prepare any content subset, such as content$live or content$videos, for a
  # weekday lm(). The original df is not modified.
  required_cols <- c(views_col, weekday_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0L) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  df %>%
    dplyr::filter(
      !is.na(.data[[views_col]]),
      !is.na(.data[[weekday_col]])
    ) %>%
    dplyr::mutate(
      views = suppressWarnings(as.numeric(.data[[views_col]]))
    ) %>%
    dow_add_model_weekday(
      weekday_col = weekday_col,
      reference_day = reference_day
    ) %>%
    dplyr::mutate(
      log_views = log1p(.data$views)
    )
}

dow_summary <- function(df, views_col = "views") {
  # Summarize viewership by weekday. Use this before the model so the practical
  # day-by-day pattern is visible.
  df %>%
    dplyr::group_by(.data$publish_wday) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_views = mean(.data[[views_col]], na.rm = TRUE),
      median_views = stats::median(.data[[views_col]], na.rm = TRUE),
      sd_views = stats::sd(.data[[views_col]], na.rm = TRUE),
      se_views = .data$sd_views / sqrt(.data$n),
      t_crit = dplyr::if_else(.data$n > 1, stats::qt(0.975, .data$n - 1), NA_real_),
      ci_low = .data$mean_views - .data$t_crit * .data$se_views,
      ci_high = .data$mean_views + .data$t_crit * .data$se_views,
      min_views = min(.data[[views_col]], na.rm = TRUE),
      max_views = max(.data[[views_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select(-"t_crit")
}

dow_fit <- function(df) {
  # Fit the two simple, interpretable weekday models:
  # - raw views for direct view-count differences
  # - log1p views for a skew-resistant percent-change interpretation
  if (!("publish_wday" %in% names(df))) {
    stop("Missing model weekday column: publish_wday. Run dow_prep() first.")
  }
  if (!("views" %in% names(df))) {
    stop("Missing model outcome column: views. Run dow_prep() first.")
  }
  if (!("log_views" %in% names(df))) {
    stop("Missing log outcome column: log_views. Run dow_prep() first.")
  }

  raw <- stats::lm(views ~ publish_wday, data = df)
  log <- stats::lm(log_views ~ publish_wday, data = df)

  effects <- tibble::tibble(
    term = names(stats::coef(log)),
    estimate = as.numeric(stats::coef(log)),
    approx_percent_change_from_reference = (exp(.data$estimate) - 1) * 100
  )

  list(
    raw = raw,
    log = log,
    log_effects = effects
  )
}

dow_plot <- function(df, summary_df = dow_summary(df)) {
  # Return notebook-ready plots without saving files.
  list(
    boxplot_views = ggplot2::ggplot(df, ggplot2::aes(x = .data$publish_wday, y = .data$views)) +
      ggplot2::geom_boxplot(outlier.alpha = 0.45) +
      ggplot2::labs(
        x = "Publish weekday",
        y = "Views",
        title = "Views by weekday"
      ) +
      ggplot2::theme_minimal(),
    mean_ci_views = ggplot2::ggplot(summary_df, ggplot2::aes(x = .data$publish_wday, y = .data$mean_views)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
        width = 0.15
      ) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::labs(
        x = "Publish weekday",
        y = "Mean views",
        title = "Mean views by weekday with 95% CI"
      ) +
      ggplot2::theme_minimal(),
    boxplot_log_views = ggplot2::ggplot(df, ggplot2::aes(x = .data$publish_wday, y = .data$log_views)) +
      ggplot2::geom_boxplot(outlier.alpha = 0.45) +
      ggplot2::labs(
        x = "Publish weekday",
        y = "log1p(views)",
        title = "Log views by weekday"
      ) +
      ggplot2::theme_minimal()
  )
}
