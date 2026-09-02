# Company dashboard metric preparation.

dashboard_company_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

dashboard_company_safe_divide <- function(numerator, denominator) {
  if (length(denominator) == 0 || is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }
  numerator / denominator
}

dashboard_company_mean <- function(x) {
  x <- dashboard_company_numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  mean(x)
}

dashboard_company_quantile <- function(x, probability) {
  x <- dashboard_company_numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  unname(stats::quantile(x, probs = probability, names = FALSE, type = 7))
}

dashboard_company_weighted_mean <- function(value, weight) {
  value <- dashboard_company_numeric(value)
  weight <- dashboard_company_numeric(weight)
  valid <- is.finite(value) & is.finite(weight) & weight > 0
  if (!any(valid)) {
    return(NA_real_)
  }
  stats::weighted.mean(value[valid], weight[valid])
}

dashboard_company_totals <- function(analytics) {
  total_hours <- sum(analytics$duration_seconds, na.rm = TRUE) / 3600
  tibble::tibble(
    total_hours = total_hours,
    total_views = sum(suppressWarnings(as.numeric(analytics$views)), na.rm = TRUE),
    video_count = dplyr::n_distinct(analytics$`Video ID`),
    video_page_subscriptions = sum(
      suppressWarnings(as.numeric(analytics$subscribersGained)),
      na.rm = TRUE
    )
  )
}

dashboard_company_performance_summary <- function(
  analytics,
  group_columns = character()
) {
  missing_columns <- setdiff(group_columns, names(analytics))
  if (length(missing_columns) > 0) {
    stop(
      "Company performance grouping columns are missing: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  prepared <- analytics %>%
    dplyr::mutate(
      .company_views = dashboard_company_numeric(.data$views),
      .company_watch_minutes = dashboard_company_numeric(
        .data$estimatedMinutesWatched
      ),
      .company_view_percentage = dashboard_company_numeric(
        .data$averageViewPercentage
      ),
      .company_subscribers = dashboard_company_numeric(
        .data$subscribersGained
      ),
      .company_duration_seconds = dashboard_company_numeric(
        .data$duration_seconds
      )
    )
  if (length(group_columns) > 0) {
    prepared <- prepared %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_columns)))
  }

  prepared %>%
    dplyr::summarise(
      `Videos / Streams` = dplyr::n_distinct(.data$`Video ID`),
      `Average Views` = dashboard_company_mean(.data$.company_views),
      `Median Views` = dashboard_company_quantile(.data$.company_views, 0.5),
      `Total Views` = sum(.data$.company_views, na.rm = TRUE),
      `Watch Hours` = sum(.data$.company_watch_minutes, na.rm = TRUE) / 60,
      `Average View Duration (sec)` = dashboard_company_safe_divide(
        sum(.data$.company_watch_minutes, na.rm = TRUE) * 60,
        sum(.data$.company_views, na.rm = TRUE)
      ),
      `Views-weighted Average View %` = dashboard_company_weighted_mean(
        .data$.company_view_percentage,
        .data$.company_views
      ),
      `Video-Page Subscriptions` = sum(
        .data$.company_subscribers,
        na.rm = TRUE
      ),
      `Video-Page Subscriptions per Release` = dashboard_company_safe_divide(
        sum(.data$.company_subscribers, na.rm = TRUE),
        dplyr::n_distinct(.data$`Video ID`)
      ),
      `Views per Content Hour` = dashboard_company_safe_divide(
        sum(.data$.company_views, na.rm = TRUE),
        sum(.data$.company_duration_seconds, na.rm = TRUE) / 3600
      ),
      `Video-Page Subscriptions per 1,000 Views` =
        1000 * dashboard_company_safe_divide(
          sum(.data$.company_subscribers, na.rm = TRUE),
          sum(.data$.company_views, na.rm = TRUE)
        ),
      `Collaboration-Tagged Releases` = sum(.data$is_collab %in% TRUE),
      `Collaboration-Tagged Share` = mean(.data$is_collab %in% TRUE),
      `Topic Breadth` = dplyr::n_distinct(
        .data$topic[!is.na(.data$topic) & .data$topic != "Unclassified"]
      ),
      .groups = "drop"
    )
}

dashboard_company_attention_summary <- function(analytics) {
  dashboard_company_performance_summary(analytics) %>%
    dplyr::select(dplyr::all_of(c(
      "Watch Hours",
      "Average View Duration (sec)",
      "Views-weighted Average View %",
      "Views per Content Hour",
      "Video-Page Subscriptions per 1,000 Views"
    )))
}

dashboard_company_portfolio_summary <- function(analytics) {
  summary <- dashboard_company_performance_summary(
    analytics,
    group_columns = "talent_name"
  )
  summary %>%
    dplyr::mutate(
      `Release Share` = .data$`Videos / Streams` / sum(.data$`Videos / Streams`),
      `View Share` = .data$`Total Views` / sum(.data$`Total Views`),
      `Watch-time Share` = .data$`Watch Hours` / sum(.data$`Watch Hours`),
      `Video-Page Subscription Share` = if (
        sum(.data$`Video-Page Subscriptions`) == 0
      ) {
        NA_real_
      } else {
        .data$`Video-Page Subscriptions` /
          sum(.data$`Video-Page Subscriptions`)
      }
    ) %>%
    dplyr::arrange(dplyr::desc(.data$`View Share`))
}

dashboard_company_content_type_summary <- function(analytics) {
  dashboard_company_performance_summary(
    analytics,
    group_columns = c("talent_name", "Content Type")
  ) %>%
    dplyr::group_by(.data$talent_name) %>%
    dplyr::mutate(
      `Talent Release Share` = .data$`Videos / Streams` /
        sum(.data$`Videos / Streams`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$talent_name, dplyr::desc(.data$`Videos / Streams`))
}

dashboard_company_video_explorer <- function(analytics) {
  analytics %>%
    dplyr::transmute(
      Talent = .data$talent_name,
      Title = .data$Title,
      `Video ID` = .data$`Video ID`,
      `Content Type` = .data$`Content Type`,
      `Published Date` = .data$publish_date,
      `Snapshot Date` = .data$snapshot_date,
      Topic = .data$topic,
      `Reference / Tags` = .data$content_reference,
      Collaboration = dplyr::if_else(.data$is_collab, "Yes", "No"),
      Views = dashboard_company_numeric(.data$views),
      `Watch Hours` = dashboard_company_numeric(
        .data$estimatedMinutesWatched
      ) / 60,
      `Average View Duration (sec)` = dashboard_company_numeric(
        .data$averageViewDuration
      ),
      `Average View %` = dashboard_company_numeric(
        .data$averageViewPercentage
      ),
      `Video-Page Subscriptions` = dashboard_company_numeric(
        .data$subscribersGained
      ),
      URL = paste0("https://www.youtube.com/watch?v=", .data$`Video ID`)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Views))
}

dashboard_company_longest_release_streak <- function(publish_dates) {
  dates <- sort(unique(as.Date(publish_dates[!is.na(publish_dates)])))
  if (length(dates) == 0) {
    return(0L)
  }
  streak_groups <- cumsum(c(TRUE, diff(dates) != 1))
  as.integer(max(tabulate(streak_groups)))
}

dashboard_company_fun_statistics <- function(analytics) {
  valid_dates <- analytics %>%
    dplyr::filter(!is.na(.data$publish_date))
  month_counts <- dashboard_company_month_summary(analytics)
  weekday_counts <- valid_dates %>%
    dplyr::count(.data$publish_wday, name = "releases") %>%
    dplyr::arrange(dplyr::desc(.data$releases), .data$publish_wday)
  milestone_views <- dashboard_company_numeric(analytics$views)

  busiest_month <- if (nrow(month_counts) == 0) {
    "Unavailable"
  } else {
    busiest <- month_counts %>%
      dplyr::slice_max(.data$`Content Releases`, n = 1, with_ties = FALSE)
    paste0(
      format(busiest$publish_month[[1]], "%Y-%m"),
      " (",
      busiest$`Content Releases`[[1]],
      " releases)"
    )
  }
  common_weekday <- if (nrow(weekday_counts) == 0) {
    "Unavailable"
  } else {
    paste0(
      as.character(weekday_counts$publish_wday[[1]]),
      " (",
      weekday_counts$releases[[1]],
      " releases)"
    )
  }

  tibble::tibble(
    statistic = c(
      "Longest continuous release streak",
      "Most common release day",
      "Busiest publication month",
      "Classified topic breadth",
      "Releases with at least 10,000 views",
      "Releases with at least 50,000 views",
      "Releases with at least 100,000 views"
    ),
    value = c(
      paste0(
        dashboard_company_longest_release_streak(valid_dates$publish_date),
        " days"
      ),
      common_weekday,
      busiest_month,
      paste0(
        dplyr::n_distinct(
          analytics$topic[
            !is.na(analytics$topic) & analytics$topic != "Unclassified"
          ]
        ),
        " topics"
      ),
      scales::comma(sum(milestone_views >= 10000, na.rm = TRUE)),
      scales::comma(sum(milestone_views >= 50000, na.rm = TRUE)),
      scales::comma(sum(milestone_views >= 100000, na.rm = TRUE))
    )
  )
}

dashboard_company_data_quality <- function(analytics) {
  fields <- c(
    views = "Views",
    estimatedMinutesWatched = "Watch minutes",
    averageViewDuration = "Average view duration",
    averageViewPercentage = "Average view percentage",
    subscribersGained = "Subscribers gained",
    duration_seconds = "Catalogued duration",
    topic = "Topic classification",
    content_reference = "Reference / tags",
    is_collab = "Collaboration flag"
  )
  purrr::imap_dfr(fields, function(label, field) {
    value <- analytics[[field]]
    present <- !is.na(value)
    if (is.character(value)) {
      present <- present & nzchar(trimws(value))
    }
    tibble::tibble(
      Field = label,
      `Rows Available` = sum(present),
      `Contributing Rows` = nrow(analytics),
      `Coverage %` = 100 * mean(present)
    )
  })
}

dashboard_company_talent_summary <- function(analytics) {
  analytics %>%
    dplyr::group_by(.data$talent_name) %>%
    dplyr::summarise(
      `Videos / Streams` = dplyr::n_distinct(.data$`Video ID`),
      `Content Hours` = sum(.data$duration_seconds, na.rm = TRUE) / 3600,
      `Total Views` = sum(suppressWarnings(as.numeric(.data$views)), na.rm = TRUE),
      `Video-Page Subscriptions` = sum(
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

dashboard_company_month_content_type_summary <- function(analytics) {
  analytics %>%
    dplyr::filter(
      !is.na(.data$publish_date),
      !is.na(.data$`Content Type`),
      nzchar(trimws(as.character(.data$`Content Type`)))
    ) %>%
    dplyr::mutate(
      content_type = stringr::str_to_title(
        trimws(as.character(.data$`Content Type`))
      )
    ) %>%
    dplyr::group_by(
      publish_month = lubridate::floor_date(.data$publish_date, unit = "month"),
      .data$content_type
    ) %>%
    dplyr::summarise(
      `Content Releases` = dplyr::n_distinct(.data$`Video ID`),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$publish_month, .data$content_type)
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
      `Content Hours` = sum(.data$duration_seconds, na.rm = TRUE) / 3600,
      `Video-Page Subscriptions` = sum(
        suppressWarnings(as.numeric(.data$subscribersGained)),
        na.rm = TRUE
      ),
      `Total Views` = sum(suppressWarnings(as.numeric(.data$views)), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$`Content Hours`))
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
      `Content Hours` = sum(.data$duration_seconds, na.rm = TRUE) / 3600,
      `Video-Page Subscriptions` = sum(
        suppressWarnings(as.numeric(.data$subscribersGained)),
        na.rm = TRUE
      ),
      `Total Views` = sum(suppressWarnings(as.numeric(.data$views)), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$`Content Hours`))
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
