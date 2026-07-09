DOW_REPORT_REQUIRED_PACKAGES <- c("DT", "dplyr", "tibble", "magrittr")
DOW_REPORT_MISSING_PACKAGES <- DOW_REPORT_REQUIRED_PACKAGES[
  !vapply(DOW_REPORT_REQUIRED_PACKAGES, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(DOW_REPORT_MISSING_PACKAGES) > 0L) {
  stop(
    "Missing required package(s) for day-of-week report helpers: ",
    paste(DOW_REPORT_MISSING_PACKAGES, collapse = ", ")
  )
}

`%>%` <- magrittr::`%>%`

dow_report_datatable <- function(df, caption, page_length = 10) {
  DT::datatable(
    df,
    rownames = FALSE,
    caption = caption,
    options = list(
      dom = "tip",
      pageLength = page_length,
      scrollX = TRUE
    )
  )
}

dow_report_summary_datatable <- function(df, caption) {
  dow_report_datatable(df, caption, page_length = 7) %>%
    DT::formatRound(
      columns = c(
        "mean_views",
        "median_views",
        "sd_views",
        "se_views",
        "ci_low",
        "ci_high",
        "min_views",
        "max_views"
      ),
      digits = 0,
      mark = ","
    )
}

dow_report_model_coefficient_table <- function(model) {
  stats::coef(summary(model)) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::rename(
      estimate = "Estimate",
      std_error = "Std. Error",
      statistic = "t value",
      p_value = "Pr(>|t|)"
    )
}

dow_report_model_anova_table <- function(model) {
  stats::anova(model) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::rename(
      df = "Df",
      sum_sq = "Sum Sq",
      mean_sq = "Mean Sq",
      f_value = "F value",
      p_value = "Pr(>F)"
    )
}

dow_report_model_fit_table <- function(model) {
  model_summary <- summary(model)
  f_statistic <- model_summary$fstatistic
  model_p_value <- stats::pf(
    f_statistic[[1]],
    f_statistic[[2]],
    f_statistic[[3]],
    lower.tail = FALSE
  )

  tibble::tibble(
    metric = c(
      "R-squared",
      "Adjusted R-squared",
      "Residual standard error",
      "F-statistic",
      "Model p-value"
    ),
    value = c(
      model_summary$r.squared,
      model_summary$adj.r.squared,
      model_summary$sigma,
      f_statistic[[1]],
      model_p_value
    )
  )
}

dow_report_model_coefficient_datatable <- function(model, caption) {
  dow_report_datatable(dow_report_model_coefficient_table(model), caption) %>%
    DT::formatRound(
      columns = c("estimate", "std_error", "statistic"),
      digits = 3
    ) %>%
    DT::formatSignif(columns = "p_value", digits = 3)
}

dow_report_model_anova_datatable <- function(model, caption) {
  dow_report_datatable(dow_report_model_anova_table(model), caption, page_length = 5) %>%
    DT::formatRound(
      columns = c("sum_sq", "mean_sq", "f_value"),
      digits = 3
    ) %>%
    DT::formatSignif(columns = "p_value", digits = 3)
}

dow_report_model_fit_datatable <- function(model, caption) {
  dow_report_datatable(dow_report_model_fit_table(model), caption, page_length = 5) %>%
    DT::formatSignif(columns = "value", digits = 4)
}

dow_report_log_effects_datatable <- function(df, caption) {
  dow_report_datatable(df, caption) %>%
    DT::formatRound(
      columns = c("estimate", "approx_percent_change_from_reference"),
      digits = 2
    )
}

dow_report_content_title <- function(content_type) {
  key <- tolower(trimws(as.character(content_type)))
  dplyr::case_when(
    key == "live" ~ "Live Content",
    key == "video" ~ "Video Content",
    key == "videos" ~ "Video Content",
    key == "short" ~ "Short Content",
    key == "shorts" ~ "Short Content",
    TRUE ~ tools::toTitleCase(key)
  )
}

dow_report_content_label <- function(content_type) {
  key <- tolower(trimws(as.character(content_type)))
  dplyr::case_when(
    key == "live" ~ "Live streams",
    key %in% c("video", "videos") ~ "Videos",
    key %in% c("short", "shorts") ~ "Shorts",
    TRUE ~ tools::toTitleCase(key)
  )
}
