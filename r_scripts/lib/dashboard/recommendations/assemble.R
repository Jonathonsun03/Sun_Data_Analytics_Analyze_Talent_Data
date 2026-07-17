# Recommendation story assembly.

dashboard_build_recommendations <- function(dashboard_data) {
  all <- dashboard_recommendation_bind(
    dashboard_recommendations_publishing(dashboard_data),
    dashboard_recommendations_content_strategy(dashboard_data),
    dashboard_recommendations_audience(dashboard_data),
    dashboard_recommendations_performance(dashboard_data)
  ) %>%
    dplyr::mutate(
      priority = factor(.data$priority, levels = c("high", "medium", "low"), ordered = TRUE),
      confidence = factor(.data$confidence, levels = c("strong", "moderate", "weak", "insufficient"), ordered = TRUE)
    ) %>%
    dplyr::arrange(.data$data_limit_flag, .data$priority, .data$confidence, .data$sort_order) %>%
    dplyr::mutate(priority = as.character(.data$priority), confidence = as.character(.data$confidence))

  actionable <- all %>% dplyr::filter(!.data$data_limit_flag, .data$confidence != "insufficient")
  top <- actionable %>% dplyr::slice_head(n = 5)
  caveats <- all %>%
    dplyr::filter(.data$data_limit_flag | .data$confidence %in% c("weak", "insufficient")) %>%
    dplyr::select("domain", "title", "finding", "caveat", "confidence", "rule_id")

  list(
    all = all,
    top = top,
    publishing = all %>% dplyr::filter(.data$domain == "publishing"),
    content = all %>% dplyr::filter(.data$domain == "content"),
    audience = all %>% dplyr::filter(.data$domain == "audience"),
    performance = all %>% dplyr::filter(.data$domain == "performance"),
    caveats = caveats
  )
}
