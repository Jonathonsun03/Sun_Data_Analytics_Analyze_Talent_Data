# Company dashboard metric preparation.

dashboard_company_totals <- function(analytics) {
  total_hours <- sum(analytics$duration_seconds, na.rm = TRUE) / 3600
  tibble::tibble(
    total_hours = total_hours,
    total_days = total_hours / 24,
    total_views = sum(suppressWarnings(as.numeric(analytics$views)), na.rm = TRUE),
    video_count = dplyr::n_distinct(analytics$`Video ID`),
    subscribers_gained = sum(
      suppressWarnings(as.numeric(analytics$subscribersGained)),
      na.rm = TRUE
    )
  )
}

dashboard_company_talent_summary <- function(analytics) {
  analytics %>%
    dplyr::group_by(.data$talent_name) %>%
    dplyr::summarise(
      `Videos / Streams` = dplyr::n_distinct(.data$`Video ID`),
      `Streamed Hours` = sum(.data$duration_seconds, na.rm = TRUE) / 3600,
      `Total Views` = sum(suppressWarnings(as.numeric(.data$views)), na.rm = TRUE),
      `Subscribers Gained` = sum(
        suppressWarnings(as.numeric(.data$subscribersGained)),
        na.rm = TRUE
      ),
      `Snapshot Date` = max(.data$snapshot_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$`Total Views`))
}

dashboard_company_month_summary <- function(analytics) {
  analytics %>%
    dplyr::filter(!is.na(.data$publish_date)) %>%
    dplyr::count(
      publish_month = lubridate::floor_date(.data$publish_date, unit = "month"),
      name = "Content Releases"
    ) %>%
    dplyr::arrange(.data$publish_month)
}

dashboard_company_weekday_summary <- function(analytics) {
  weekday_levels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  analytics %>%
    dplyr::filter(!is.na(.data$publish_wday)) %>%
    dplyr::mutate(
      publish_wday = factor(as.character(.data$publish_wday), levels = weekday_levels)
    ) %>%
    dplyr::group_by(.data$talent_name, .data$publish_wday, .drop = FALSE) %>%
    dplyr::summarise(
      `Content Releases` = dplyr::n_distinct(.data$`Video ID`),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$publish_wday, .data$talent_name)
}

dashboard_company_topic_summary <- function(analytics) {
  analytics %>%
    dplyr::group_by(.data$topic) %>%
    dplyr::summarise(
      `Videos / Streams` = dplyr::n_distinct(.data$`Video ID`),
      `Streamed Hours` = sum(.data$duration_seconds, na.rm = TRUE) / 3600,
      `Subscribers Gained` = sum(
        suppressWarnings(as.numeric(.data$subscribersGained)),
        na.rm = TRUE
      ),
      `Total Views` = sum(suppressWarnings(as.numeric(.data$views)), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$`Streamed Hours`))
}

dashboard_company_reference_summary <- function(analytics) {
  analytics %>%
    dplyr::mutate(
      content_reference = stringr::str_remove(
        .data$content_reference,
        "\\s*pt\\.\\d+$"
      )
    ) %>%
    dplyr::group_by(.data$topic, .data$content_reference) %>%
    dplyr::summarise(
      `Videos / Streams` = dplyr::n_distinct(.data$`Video ID`),
      `Streamed Hours` = sum(.data$duration_seconds, na.rm = TRUE) / 3600,
      `Subscribers Gained` = sum(
        suppressWarnings(as.numeric(.data$subscribersGained)),
        na.rm = TRUE
      ),
      `Total Views` = sum(suppressWarnings(as.numeric(.data$views)), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$`Streamed Hours`))
}

dashboard_company_collaboration_summary <- function(analytics) {
  analytics %>%
    dplyr::filter(.data$is_collab, !is.na(.data$publish_date)) %>%
    dplyr::count(
      publish_month = lubridate::floor_date(.data$publish_date, unit = "month"),
      name = "Collaborations"
    ) %>%
    dplyr::arrange(.data$publish_month)
}
