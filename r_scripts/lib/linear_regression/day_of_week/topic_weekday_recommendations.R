TOPIC_WEEKDAY_REQUIRED_PACKAGES <- c("dplyr", "ggplot2", "magrittr", "tibble")
TOPIC_WEEKDAY_MISSING_PACKAGES <- TOPIC_WEEKDAY_REQUIRED_PACKAGES[
  !vapply(TOPIC_WEEKDAY_REQUIRED_PACKAGES, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(TOPIC_WEEKDAY_MISSING_PACKAGES) > 0L) {
  stop(
    "Missing required package(s) for topic-weekday recommendations: ",
    paste(TOPIC_WEEKDAY_MISSING_PACKAGES, collapse = ", ")
  )
}

`%>%` <- magrittr::`%>%`

topic_clean <- function(df, topic_col = "topic", min_topic_n = 3) {
  if (!(topic_col %in% names(df))) {
    df$topic <- "Unknown"
  }

  df %>%
    dplyr::mutate(
      topic_model = trimws(as.character(.data[[topic_col]])),
      topic_model = dplyr::if_else(
        is.na(.data$topic_model) | .data$topic_model == "",
        "Unknown",
        .data$topic_model
      )
    ) %>%
    dplyr::add_count(.data$topic_model, name = "topic_n") %>%
    dplyr::mutate(
      topic_lumped = dplyr::if_else(
        .data$topic_n >= min_topic_n,
        .data$topic_model,
        "Other / sparse"
      )
    ) %>%
    dplyr::select(-dplyr::all_of("topic_n"))
}

ensure_topic_log_views <- function(df) {
  if ("log_views" %in% names(df)) {
    return(df)
  }
  if (!("views" %in% names(df))) {
    stop("Missing `views` or `log_views` column.")
  }

  df %>%
    dplyr::mutate(log_views = log1p(.data$views))
}

topic_weekday_summary <- function(df) {
  df <- ensure_topic_log_views(df)

  df %>%
    dplyr::group_by(.data$topic_lumped, .data$publish_wday) %>%
    dplyr::summarise(
      streams = dplyr::n(),
      mean_views = mean(.data$views, na.rm = TRUE),
      median_views = stats::median(.data$views, na.rm = TRUE),
      median_log_views = stats::median(.data$log_views, na.rm = TRUE),
      min_views = min(.data$views, na.rm = TRUE),
      max_views = max(.data$views, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$topic_lumped, dplyr::desc(.data$median_views))
}

observed_best_weekday_by_topic <- function(summary_df, min_streams = 2) {
  summary_df %>%
    dplyr::filter(.data$streams >= min_streams) %>%
    dplyr::group_by(.data$topic_lumped) %>%
    dplyr::slice_max(
      order_by = .data$median_views,
      n = 1,
      with_ties = FALSE
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(.data$median_views))
}

fit_topic_day_models <- function(df) {
  df <- ensure_topic_log_views(df)
  topic_levels <- dplyr::n_distinct(df$topic_lumped)
  weekday_levels <- dplyr::n_distinct(df$publish_wday)

  models <- list(day_only = stats::lm(log_views ~ publish_wday, data = df))

  if (topic_levels > 1L) {
    models$topic_only <- stats::lm(log_views ~ topic_lumped, data = df)
    models$day_plus_topic <- stats::lm(log_views ~ publish_wday + topic_lumped, data = df)
  }

  repeated_cells <- df %>%
    dplyr::count(.data$topic_lumped, .data$publish_wday) %>%
    dplyr::filter(.data$n >= 2L)

  if (topic_levels > 1L && weekday_levels > 1L && nrow(repeated_cells) >= topic_levels + weekday_levels) {
    models$day_topic_interaction <- tryCatch(
      stats::lm(log_views ~ publish_wday * topic_lumped, data = df),
      error = function(e) NULL
    )
  }

  keep <- !vapply(models, is.null, logical(1))
  models[keep]
}

model_comparison <- function(models) {
  dplyr::bind_rows(
    lapply(
      names(models),
      function(model_name) {
        model <- models[[model_name]]
        model_summary <- summary(model)
        formula_text <- paste(deparse(stats::formula(model)), collapse = " ")
        model_aic <- stats::AIC(model)
        residual_standard_error <- model_summary$sigma

        tibble::tibble(
          model = model_name,
          formula = formula_text,
          adj_r_squared = model_summary$adj.r.squared,
          aic = model_aic,
          residual_standard_error = residual_standard_error
        )
      }
    )
  ) %>%
    dplyr::arrange(.data$aic)
}

is_stable_lm <- function(model) {
  inherits(model, "lm") &&
    is.finite(stats::AIC(model)) &&
    stats::df.residual(model) > 0L &&
    !anyNA(stats::coef(model)) &&
    model$rank == length(stats::coef(model)) &&
    model$rank < stats::nobs(model)
}

select_recommendation_model <- function(models) {
  if ("day_topic_interaction" %in% names(models) && is_stable_lm(models$day_topic_interaction)) {
    return(list(model = models$day_topic_interaction, model_name = "day_topic_interaction"))
  }
  if ("day_plus_topic" %in% names(models) && is_stable_lm(models$day_plus_topic)) {
    return(list(model = models$day_plus_topic, model_name = "day_plus_topic"))
  }
  list(model = models$day_only, model_name = "day_only")
}

model_recommend_weekday_by_topic <- function(
  df,
  models,
  min_topic_streams = 5,
  min_cell_streams = 2,
  min_predicted_gap_pct = 0.10
) {
  df <- ensure_topic_log_views(df)
  model_choice <- select_recommendation_model(models)
  recommendation_model <- model_choice$model
  recommendation_model_name <- model_choice$model_name

  weekday_levels <- if (is.factor(df$publish_wday)) {
    levels(droplevels(df$publish_wday))
  } else {
    unique(as.character(df$publish_wday))
  }
  topic_levels <- sort(unique(as.character(df$topic_lumped)))

  prediction_grid <- expand.grid(
    topic_lumped = topic_levels,
    publish_wday = weekday_levels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      publish_wday = factor(.data$publish_wday, levels = weekday_levels)
    )

  prediction_grid$predicted_log_views <- as.numeric(
    stats::predict(recommendation_model, newdata = prediction_grid)
  )

  observed_summary <- topic_weekday_summary(df) %>%
    dplyr::mutate(publish_wday = as.character(.data$publish_wday))

  topic_totals <- df %>%
    dplyr::count(.data$topic_lumped, name = "total_topic_streams")

  ranked_predictions <- prediction_grid %>%
    dplyr::mutate(
      publish_wday = as.character(.data$publish_wday),
      predicted_views = pmax(expm1(.data$predicted_log_views), 0)
    ) %>%
    dplyr::left_join(
      observed_summary,
      by = c("topic_lumped", "publish_wday")
    ) %>%
    dplyr::left_join(topic_totals, by = "topic_lumped") %>%
    dplyr::mutate(
      streams = dplyr::coalesce(.data$streams, 0L),
      observed_streams_for_topic_day = .data$streams
    ) %>%
    dplyr::arrange(.data$topic_lumped, dplyr::desc(.data$predicted_views)) %>%
    dplyr::group_by(.data$topic_lumped) %>%
    dplyr::mutate(recommendation_rank = dplyr::row_number()) %>%
    dplyr::ungroup()

  first_choice <- ranked_predictions %>%
    dplyr::filter(.data$recommendation_rank == 1L) %>%
    dplyr::select(
      topic_lumped,
      recommended_weekday = publish_wday,
      observed_streams_for_topic_day,
      total_topic_streams,
      observed_median_views = median_views,
      observed_median_log_views = median_log_views,
      predicted_log_views,
      predicted_views
    )

  second_choice <- ranked_predictions %>%
    dplyr::filter(.data$recommendation_rank == 2L) %>%
    dplyr::select(
      topic_lumped,
      second_best_weekday = publish_wday,
      second_best_predicted_views = predicted_views
    )

  first_choice %>%
    dplyr::left_join(second_choice, by = "topic_lumped") %>%
    dplyr::mutate(
      predicted_view_gap = .data$predicted_views - .data$second_best_predicted_views,
      predicted_gap_pct = dplyr::if_else(
        .data$second_best_predicted_views > 0,
        .data$predicted_view_gap / .data$second_best_predicted_views,
        NA_real_
      ),
      recommendation_strength = dplyr::case_when(
        recommendation_model_name == "day_only" ~ "Not topic-specific",
        .data$total_topic_streams < min_topic_streams ~ "Sparse / exploratory",
        .data$observed_streams_for_topic_day < min_cell_streams ~ "Sparse / exploratory",
        is.na(.data$predicted_gap_pct) | .data$predicted_gap_pct < min_predicted_gap_pct ~ "Tentative",
        TRUE ~ "Strong"
      ),
      recommendation_note = dplyr::case_when(
        recommendation_model_name == "day_only" ~ "Only the day-only model was available, so this recommendation is not topic-specific.",
        .data$total_topic_streams < min_topic_streams ~ "Topic has too few total live streams for a strong recommendation.",
        .data$observed_streams_for_topic_day < min_cell_streams ~ "Recommended topic/day cell has too few observed streams.",
        is.na(.data$predicted_gap_pct) | .data$predicted_gap_pct < min_predicted_gap_pct ~ "Predicted advantage over the second-best day is small.",
        TRUE ~ "Topic has repeated observations and a meaningful predicted advantage."
      )
    ) %>%
    dplyr::select(
      topic_lumped,
      recommended_weekday,
      observed_streams_for_topic_day,
      total_topic_streams,
      observed_median_views,
      observed_median_log_views,
      predicted_log_views,
      predicted_views,
      second_best_weekday,
      second_best_predicted_views,
      predicted_view_gap,
      recommendation_strength,
      recommendation_note
    ) %>%
    dplyr::arrange(
      factor(
        .data$recommendation_strength,
        levels = c("Strong", "Tentative", "Sparse / exploratory", "Not topic-specific")
      ),
      .data$topic_lumped
    )
}

topic_day_heatmap <- function(summary_df) {
  plot_theme <- if (exists("theme_nyt", mode = "function")) {
    theme_nyt()
  } else {
    ggplot2::theme_minimal()
  }

  ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
      x = .data$publish_wday,
      y = .data$topic_lumped,
      fill = .data$median_views
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data$streams), size = 3) +
    ggplot2::scale_fill_viridis_c(option = "C", labels = scales::comma) +
    ggplot2::labs(
      x = "Publish weekday",
      y = "Topic",
      fill = "Median views",
      title = "Observed median views by live stream topic and weekday",
      subtitle = "Tile color is observed median views; labels are stream counts."
    ) +
    plot_theme
}
