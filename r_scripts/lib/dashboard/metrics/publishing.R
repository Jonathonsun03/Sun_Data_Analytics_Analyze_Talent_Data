# Publishing-time and topic-weekday metric builders.

dashboard_build_topic_weekday <- function(content_df, reference_day = "Monday", min_topic_n = 3) {
  topic_df <- content_df %>%
    dow_prep(reference_day = reference_day) %>%
    topic_clean(min_topic_n = min_topic_n)

  topic_weekday <- topic_weekday_summary(topic_df)
  models <- fit_topic_day_models(topic_df)
  recommendations <- model_recommend_weekday_by_topic(topic_df, models)

  list(
    topic_df = topic_df,
    topic_weekday = topic_weekday,
    observed_best = observed_best_weekday_by_topic(topic_weekday, min_streams = 2),
    model_check = model_comparison(models),
    recommendations = recommendations
  )
}
