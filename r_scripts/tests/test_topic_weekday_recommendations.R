source(file.path("r_scripts", "lib", "linear_regression", "day_of_week", "weekday_transform.R"))
source(file.path("r_scripts", "lib", "linear_regression", "day_of_week", "topic_weekday_recommendations.R"))

assert_true <- function(x, message) {
  if (!isTRUE(x)) {
    stop(message, call. = FALSE)
  }
}

assert_equal <- function(x, y, message) {
  if (!identical(x, y)) {
    stop(message, call. = FALSE)
  }
}

raw_df <- tibble::tibble(
  views = c(100, 110, 120, 80, 85, 90, 95, 105, 130, 140, 150, 160, 170, 180),
  publish_wday = c(
    "Monday", "Monday", "Tuesday", "Monday", "Tuesday", "Wednesday", "Wednesday",
    "Thursday", "Friday", "Friday", "Saturday", "Sunday", "Sunday", "Tuesday"
  ),
  topic = c(
    "alpha", "alpha", "alpha", "beta", "beta", "", NA, "gamma", "gamma", "gamma",
    "gamma", "gamma", "gamma", "alpha"
  )
)

prepared_df <- raw_df %>%
  dow_prep(reference_day = "Monday") %>%
  topic_clean(min_topic_n = 3)

assert_true(
  "Unknown" %in% prepared_df$topic_model,
  "topic_clean() should convert missing or blank topics to Unknown."
)
assert_true(
  all(prepared_df$topic_lumped[prepared_df$topic_model == "beta"] == "Other / sparse"),
  "topic_clean() should lump topics below min_topic_n."
)
assert_true(
  all(prepared_df$topic_lumped[prepared_df$topic_model == "alpha"] == "alpha"),
  "topic_clean() should preserve topics meeting min_topic_n."
)

summary_df <- topic_weekday_summary(prepared_df)
alpha_monday <- summary_df %>%
  dplyr::filter(.data$topic_lumped == "alpha", as.character(.data$publish_wday) == "Monday")

assert_equal(
  nrow(summary_df),
  nrow(dplyr::distinct(prepared_df, .data$topic_lumped, .data$publish_wday)),
  "topic_weekday_summary() should return one row per topic/day combination."
)
assert_equal(alpha_monday$streams[[1]], 2L, "topic_weekday_summary() should count streams.")
assert_equal(alpha_monday$median_views[[1]], 105, "topic_weekday_summary() should calculate median views.")
assert_true("median_log_views" %in% names(summary_df), "topic_weekday_summary() should include median_log_views.")

observed_test_summary <- dplyr::bind_rows(
  summary_df,
  tibble::tibble(
    topic_lumped = "alpha",
    publish_wday = factor("Sunday", levels = levels(prepared_df$publish_wday)),
    streams = 1L,
    mean_views = 1000,
    median_views = 1000,
    median_log_views = log1p(1000),
    min_views = 1000,
    max_views = 1000
  )
)
observed_best <- observed_best_weekday_by_topic(observed_test_summary, min_streams = 2)
alpha_best <- observed_best %>%
  dplyr::filter(.data$topic_lumped == "alpha")

assert_equal(nrow(alpha_best), 1L, "observed_best_weekday_by_topic() should return one row per topic.")
assert_equal(
  as.character(alpha_best$publish_wday[[1]]),
  "Tuesday",
  "observed_best_weekday_by_topic() should ignore topic/day cells below min_streams."
)

models <- fit_topic_day_models(prepared_df)
assert_true("day_only" %in% names(models), "fit_topic_day_models() should always return day_only.")
assert_true("day_plus_topic" %in% names(models), "fit_topic_day_models() should return topic-aware models with multiple topics.")

sparse_interaction_df <- raw_df[seq_len(6), ] %>%
  dow_prep(reference_day = "Monday") %>%
  topic_clean(min_topic_n = 1)
sparse_models <- fit_topic_day_models(sparse_interaction_df)
assert_true("day_only" %in% names(sparse_models), "fit_topic_day_models() should handle sparse interaction data.")

recommendations <- model_recommend_weekday_by_topic(prepared_df, models)
required_recommendation_cols <- c(
  "topic_lumped",
  "recommended_weekday",
  "observed_streams_for_topic_day",
  "total_topic_streams",
  "observed_median_views",
  "observed_median_log_views",
  "predicted_log_views",
  "predicted_views",
  "second_best_weekday",
  "second_best_predicted_views",
  "predicted_view_gap",
  "recommendation_strength",
  "recommendation_note"
)

assert_equal(
  nrow(recommendations),
  dplyr::n_distinct(prepared_df$topic_lumped),
  "model_recommend_weekday_by_topic() should return one recommendation per topic."
)
assert_true(
  all(required_recommendation_cols %in% names(recommendations)),
  "model_recommend_weekday_by_topic() should include all required columns."
)
assert_true(
  all(!is.na(recommendations$recommended_weekday)),
  "model_recommend_weekday_by_topic() should produce non-missing recommended weekdays."
)
assert_true(
  all(!is.na(recommendations$recommendation_strength)),
  "model_recommend_weekday_by_topic() should produce recommendation strength labels."
)

one_topic_df <- raw_df %>%
  dplyr::mutate(topic = "single_topic") %>%
  dow_prep(reference_day = "Monday") %>%
  topic_clean(min_topic_n = 1)
one_topic_models <- fit_topic_day_models(one_topic_df)
one_topic_recommendations <- model_recommend_weekday_by_topic(one_topic_df, one_topic_models)

assert_true(
  all(one_topic_recommendations$recommendation_strength == "Not topic-specific"),
  "model_recommend_weekday_by_topic() should fall back gracefully when only day_only is available."
)

cat("topic weekday recommendation tests passed\n")
