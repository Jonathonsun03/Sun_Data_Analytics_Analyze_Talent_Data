# Content-strategy plot and table adapters.

dashboard_revenue_content_type_plot <- function(dashboard_data, talent) {
  monetary <- dashboard_data$source_data$monetary
  if (is.null(monetary) || nrow(monetary) == 0 || !("Estimated Revenue" %in% names(monetary))) {
    return(NULL)
  }
  plot_df <- total_metric_content_type_prep(
    monetary,
    metric_col = "Estimated Revenue",
    window_months = 1
  )
  dashboard_ggplotly(total_metric_content_type_plot(
    plot_df,
    talent = talent,
    metric_label = "Revenue",
    subtitle_text = NULL,
    x_axis_label = "Month",
    bar_position = "stack",
    show_counts = TRUE,
    unique_bar_colors = TRUE
  ))
}

dashboard_content_engagement_plot <- function(dashboard_data, talent) {
  analytics <- dashboard_data$source_data$analytics
  if (is.null(analytics) || nrow(analytics) == 0 || !("averageViewPercentage" %in% names(analytics))) {
    return(NULL)
  }
  dashboard_ggplotly(engagement_distribution_content_type(
    analytics %>% dplyr::mutate(avg_view_prop = .data$averageViewPercentage / 100),
    talent,
    metric_col = "avg_view_prop",
    metric_label = "Average View %",
    y_as_percent = TRUE
  ))
}

dashboard_topic_performance_plot <- function(dashboard_data, talent, as_share = FALSE) {
  topic_summary <- dashboard_data$content_strategy$topic_summary
  if (is.null(topic_summary) || nrow(topic_summary) == 0) {
    return(NULL)
  }
  dashboard_ggplotly(topic_performance_plot(
    topic_summary,
    talent = talent,
    as_share = as_share
  ))
}

dashboard_topic_performance_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(
    dashboard_data$content_strategy$topic_summary,
    page_length = page_length,
    scroll_x = TRUE
  )
}

dashboard_tag_performance_plot <- function(dashboard_data, talent, as_share = FALSE) {
  tag_summary <- dashboard_data$content_strategy$tag_summary
  if (is.null(tag_summary) || nrow(tag_summary) == 0) {
    return(NULL)
  }
  dashboard_ggplotly(tag_performance_plot(
    tag_summary,
    talent = talent,
    as_share = as_share
  ))
}

dashboard_tag_performance_table <- function(dashboard_data, page_length = 10) {
  dashboard_datatable(
    dashboard_data$content_strategy$tag_summary,
    page_length = page_length,
    scroll_x = TRUE
  )
}

dashboard_content_video_data <- function(dashboard_data) {
  analytics <- dashboard_data$source_data$analytics
  if (is.null(analytics) || nrow(analytics) == 0 || !("Video ID" %in% names(analytics))) {
    return(NULL)
  }

  first_column <- function(df, candidates) {
    hits <- candidates[candidates %in% names(df)]
    if (length(hits) == 0) NULL else hits[[1]]
  }
  text_values <- function(df, column) {
    if (is.null(column)) rep(NA_character_, nrow(df)) else as.character(df[[column]])
  }
  numeric_values <- function(df, column) {
    if (is.null(column)) {
      rep(NA_real_, nrow(df))
    } else {
      suppressWarnings(as.numeric(df[[column]]))
    }
  }

  title_col <- first_column(analytics, c("Title", "title_raw", "title"))
  content_type_col <- first_column(analytics, c("Content Type", "content_type"))
  publish_date_col <- first_column(analytics, c("Published At", "publish_date", "published_at"))
  topic_col <- first_column(analytics, "topic")
  tags_col <- first_column(analytics, c("canonical_tags", "tags"))
  views_col <- first_column(analytics, "views")
  watch_minutes_col <- first_column(analytics, "estimatedMinutesWatched")
  average_view_percentage_col <- first_column(analytics, "averageViewPercentage")
  average_view_duration_col <- first_column(analytics, "averageViewDuration")
  duration_minutes_col <- first_column(analytics, c("duration_minutes", "DurationMinutes"))
  subscribers_gained_col <- first_column(analytics, "subscribersGained")

  topic_values <- trimws(text_values(analytics, topic_col))
  topic_values[is.na(topic_values) | !nzchar(topic_values)] <- "(unclassified)"
  tag_values <- trimws(text_values(analytics, tags_col))
  tag_values[is.na(tag_values) | !nzchar(tag_values)] <- NA_character_

  video_data <- tibble::tibble(
    `Video ID` = as.character(analytics$`Video ID`),
    Title = text_values(analytics, title_col),
    `Content Type` = text_values(analytics, content_type_col),
    `Publish Date` = bundle_a_as_date(text_values(analytics, publish_date_col)),
    Topic = topic_values,
    Tags = tag_values,
    Views = numeric_values(analytics, views_col),
    `Watch Hours` = numeric_values(analytics, watch_minutes_col) / 60,
    `Average View %` = numeric_values(analytics, average_view_percentage_col),
    `Average View Duration (sec)` = numeric_values(analytics, average_view_duration_col),
    `Duration (min)` = numeric_values(analytics, duration_minutes_col),
    `Subscribers Gained` = numeric_values(analytics, subscribers_gained_col)
  ) %>%
    dplyr::mutate(
      Title = dplyr::if_else(
        is.na(.data$Title) | !nzchar(trimws(.data$Title)),
        paste0("Video ID: ", .data$`Video ID`),
        .data$Title
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Views)) %>%
    dplyr::distinct(.data$`Video ID`, .keep_all = TRUE)

  monetary <- dashboard_data$source_data$monetary
  if (!is.null(monetary) && nrow(monetary) > 0 &&
      all(c("Video ID", "Estimated Revenue") %in% names(monetary))) {
    revenue_by_video <- monetary %>%
      dplyr::transmute(
        `Video ID` = as.character(.data$`Video ID`),
        .revenue = suppressWarnings(as.numeric(.data$`Estimated Revenue`))
      ) %>%
      dplyr::group_by(.data$`Video ID`) %>%
      dplyr::summarise(
        `Estimated Revenue` = if (all(is.na(.data$.revenue))) {
          NA_real_
        } else {
          sum(.data$.revenue, na.rm = TRUE)
        },
        .groups = "drop"
      )
    video_data <- video_data %>%
      dplyr::left_join(revenue_by_video, by = "Video ID")
  } else {
    video_data$`Estimated Revenue` <- NA_real_
  }

  video_data %>%
    dplyr::select(
      "Title",
      "Video ID",
      "Content Type",
      "Publish Date",
      "Topic",
      "Tags",
      "Views",
      "Estimated Revenue",
      "Watch Hours",
      "Average View %",
      "Average View Duration (sec)",
      "Duration (min)",
      "Subscribers Gained"
    )
}

dashboard_content_video_classifications <- function(
  dashboard_data,
  classification = c("topic", "tag")
) {
  classification <- match.arg(classification)
  video_data <- dashboard_content_video_data(dashboard_data)
  if (is.null(video_data) || nrow(video_data) == 0) {
    return(NULL)
  }

  if (identical(classification, "topic")) {
    return(video_data %>%
      dplyr::mutate(.classification_value = trimws(as.character(.data$Topic))) %>%
      dplyr::filter(!is.na(.data$.classification_value), nzchar(.data$.classification_value)) %>%
      dplyr::distinct(.data$.classification_value, .data$`Video ID`, .keep_all = TRUE))
  }

  video_data %>%
    dplyr::mutate(.classification_value = dplyr::coalesce(as.character(.data$Tags), "")) %>%
    tidyr::separate_rows(".classification_value", sep = ",") %>%
    dplyr::mutate(.classification_value = tolower(trimws(.data$.classification_value))) %>%
    dplyr::filter(nzchar(.data$.classification_value)) %>%
    dplyr::distinct(.data$.classification_value, .data$`Video ID`, .keep_all = TRUE)
}

dashboard_content_video_choices <- function(
  dashboard_data,
  classification = c("topic", "tag")
) {
  classification <- match.arg(classification)
  classified <- dashboard_content_video_classifications(dashboard_data, classification)
  if (is.null(classified) || nrow(classified) == 0) {
    return(character())
  }

  counts <- classified %>%
    dplyr::count(.data$.classification_value, name = "video_count") %>%
    dplyr::arrange(dplyr::desc(.data$video_count), .data$.classification_value) %>%
    dplyr::mutate(
      choice_label = paste0(
        .data$.classification_value,
        " (",
        scales::comma(.data$video_count),
        ifelse(.data$video_count == 1, " video)", " videos)")
      )
    )

  stats::setNames(counts$.classification_value, counts$choice_label)
}

dashboard_content_video_table <- function(
  dashboard_data,
  classification = c("topic", "tag"),
  selection,
  page_length = 10
) {
  classification <- match.arg(classification)
  if (is.null(selection) || length(selection) == 0 || !nzchar(selection[[1]])) {
    return(NULL)
  }

  classified <- dashboard_content_video_classifications(dashboard_data, classification)
  if (is.null(classified) || nrow(classified) == 0) {
    return(NULL)
  }

  selection <- as.character(selection[[1]])
  table_data <- classified %>%
    dplyr::filter(.data$.classification_value == selection) %>%
    dplyr::arrange(dplyr::desc(.data$Views), .data$Title) %>%
    dplyr::select(
      "Title",
      "Content Type",
      "Publish Date",
      "Views",
      "Estimated Revenue",
      "Average View %"
    )
  if (nrow(table_data) == 0) {
    return(NULL)
  }

  classification_label <- if (identical(classification, "topic")) "topic" else "tag"
  htmltools::tagList(
    htmltools::div(
      style = "font-size:0.82rem; color:#555; padding:0 0 0.4rem;",
      paste0(
        scales::comma(nrow(table_data)),
        ifelse(nrow(table_data) == 1, " video matches ", " videos match "),
        classification_label,
        " “",
        selection,
        "”. Metrics use the latest snapshot in the selected date range."
      )
    ),
    DTSettings(
      table_data,
      class = "display compact stripe hover",
      scroll_x = FALSE,
      filter = "none",
      options = list(
        pageLength = page_length,
        dom = "Brtip",
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "42%", targets = 0),
          list(width = "11%", targets = 1:5)
        )
      ),
      rownames = FALSE
    )
  )
}

dashboard_collaboration_performance_compact_plot <- function(dashboard_data) {
  collaboration_performance_plotly(dashboard_data$content_strategy$collab_summary)
}
