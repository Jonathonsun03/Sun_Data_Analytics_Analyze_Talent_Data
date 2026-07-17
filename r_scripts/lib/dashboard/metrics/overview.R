# Overview and top-video metric builders.

dashboard_build_overview <- function(analytics, monetary, demo, talent, data_source, data_root) {
  video_id_col <- bundle_a_optional_col(analytics, candidates = c("Video ID", "video_id"))
  date_col <- bundle_a_optional_col(analytics, candidates = c("publish_date", "Published At", "date"))
  publish_dates <- if (is.null(date_col)) as.Date(character()) else bundle_a_as_date(analytics[[date_col]])
  source_location_label <- if (identical(data_source, "unified_db")) {
    "Database path"
  } else {
    "Data root"
  }

  tibble::tibble(
    metric = c(
      "Talent",
      "Data source",
      source_location_label,
      "Analytics rows",
      "Monetary rows",
      "Audience rows",
      "Unique videos",
      "Publish date range"
    ),
    value = c(
      talent,
      data_source,
      data_root,
      scales::comma(nrow(analytics)),
      scales::comma(nrow(monetary)),
      scales::comma(nrow(demo)),
      if (is.null(video_id_col)) "N/A" else scales::comma(dplyr::n_distinct(analytics[[video_id_col]])),
      if (length(stats::na.omit(publish_dates)) == 0) {
        "N/A"
      } else {
        paste0(format(min(publish_dates, na.rm = TRUE), "%Y-%m-%d"), " to ", format(max(publish_dates, na.rm = TRUE), "%Y-%m-%d"))
      }
    )
  )
}

dashboard_build_top_videos <- function(analytics, monetary = NULL, top_n = 20) {
  if (is.null(analytics) || nrow(analytics) == 0 || !("views" %in% names(analytics))) {
    return(NULL)
  }

  revenue_by_video <- if (!is.null(monetary) && nrow(monetary) > 0 &&
      all(c("Video ID", "Estimated Revenue") %in% names(monetary))) {
    monetary %>%
      dplyr::group_by(.data$`Video ID`) %>%
      dplyr::summarise(`Estimated Revenue` = sum(.data$`Estimated Revenue`, na.rm = TRUE), .groups = "drop")
  } else {
    NULL
  }

  out <- analytics %>%
    dplyr::mutate(views = suppressWarnings(as.numeric(.data$views))) %>%
    dplyr::arrange(dplyr::desc(.data$views)) %>%
    dplyr::select(dplyr::any_of(c("Video ID", "Title", "publish_date", "Content Type", "topic", "views"))) %>%
    dplyr::slice_head(n = top_n)

  if (!is.null(revenue_by_video) && "Video ID" %in% names(out)) {
    out <- out %>% dplyr::left_join(revenue_by_video, by = "Video ID")
  }
  out
}
