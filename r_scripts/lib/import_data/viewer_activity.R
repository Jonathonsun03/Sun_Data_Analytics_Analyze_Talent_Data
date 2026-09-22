# Canonical viewer-activity lakehouse loaders.

load_viewer_video_activity <- function(con, talent_code = NULL) {
  talent_code <- if (is.null(talent_code)) NULL else trimws(as.character(talent_code[[1]]))
  filter_sql <- if (is.null(talent_code) || !nzchar(talent_code)) "" else "AND activity.talent_code = ?"
  params <- if (nzchar(filter_sql)) list(talent_code) else list()

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT activity.talent_code, talent.talent_name, activity.channel_id,",
      "activity.video_id, video.title AS video_title, activity.user_id,",
      "activity.latest_username_in_video, activity.stream_at, activity.message_count,",
      "activity.text_message_count, activity.paid_message_count,",
      "activity.membership_event_count, activity.first_message_second,",
      "activity.last_message_second, activity.active_span_seconds,",
      "activity.pipeline_run_id, activity.calculated_at",
      "FROM analysis.viewer_video_activity AS activity",
      "JOIN catalog.videos AS video USING (talent_code, channel_id, video_id)",
      "JOIN catalog.talents AS talent USING (talent_code)",
      "WHERE 1 = 1", filter_sql,
      "ORDER BY activity.talent_code, activity.stream_at, activity.video_id, activity.user_id"
    ),
    params = params
  )
}

load_available_live_videos <- function(con, talent_code = NULL) {
  talent_code <- if (is.null(talent_code)) NULL else trimws(as.character(talent_code[[1]]))
  filter_sql <- if (is.null(talent_code) || !nzchar(talent_code)) "" else "AND video.talent_code = ?"
  params <- if (nzchar(filter_sql)) list(talent_code) else list()

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT video.video_id, video.title AS video_title, video.talent_code,",
      "talent.talent_name, video.channel_id,",
      "COALESCE(video.actual_start_at, video.published_at) AS stream_at",
      "FROM catalog.videos AS video",
      "JOIN catalog.talents AS talent USING (talent_code)",
      "WHERE video.is_available AND video.content_type = 'live'", filter_sql,
      "ORDER BY stream_at DESC NULLS LAST, video.video_id"
    ),
    params = params
  )
}

load_title_classification_contributions <- function(con, talent_code = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to load title classifications.", call. = FALSE)
  }
  talent_code <- if (is.null(talent_code)) NULL else trimws(as.character(talent_code[[1]]))
  filter_sql <- if (is.null(talent_code) || !nzchar(talent_code)) "" else "AND status.talent_code = ?"
  params <- if (nzchar(filter_sql)) list(talent_code) else list()

  classified <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT status.video_id, status.talent_code, status.title AS video_title,",
      "result.confidence, result.classification_json, performance.views AS video_views",
      "FROM classification.title_classification_status AS status",
      "JOIN classification.title_classification_results AS result",
      "ON result.video_id = status.video_id",
      "AND result.talent_code = status.talent_code",
      "AND result.channel_id = status.channel_id",
      "AND result.title_hash = status.title_hash",
      "AND result.title_version_id = status.title_version_id",
      "LEFT JOIN analytics.video_latest_performance AS performance",
      "ON performance.video_id = status.video_id",
      "AND performance.talent_code = status.talent_code",
      "AND performance.channel_id = status.channel_id",
      "WHERE status.is_classified", filter_sql,
      "QUALIFY ROW_NUMBER() OVER (",
      "PARTITION BY status.talent_code, status.video_id",
      "ORDER BY result.created_at DESC NULLS LAST, result.confidence DESC NULLS LAST",
      ") = 1",
      "ORDER BY status.talent_code, status.video_id"
    ),
    params = params
  )
  mappings <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT DISTINCT raw_label, normalized_label",
      "FROM normalization.active_label_mappings",
      "WHERE label_type = 'title_tag'"
    )
  )

  parsed <- lapply(classified$classification_json, function(value) {
    tryCatch(jsonlite::fromJSON(value, simplifyVector = TRUE), error = function(error) NULL)
  })
  topics <- vapply(
    parsed,
    function(value) {
      if (is.null(value) || is.null(value$topic) || !length(value$topic)) return(NA_character_)
      trimws(as.character(value$topic[[1]]))
    },
    character(1)
  )
  topic_edges <- classified %>%
    dplyr::mutate(classification = topics) %>%
    dplyr::filter(!is.na(.data$classification), nzchar(.data$classification))

  tag_rows <- lapply(seq_len(nrow(classified)), function(index) {
    tags <- parsed[[index]]$tags
    if (is.null(tags) || !length(tags)) return(NULL)
    data.frame(
      video_id = classified$video_id[[index]],
      talent_code = classified$talent_code[[index]],
      video_title = classified$video_title[[index]],
      confidence = classified$confidence[[index]],
      video_views = classified$video_views[[index]],
      raw_label = trimws(as.character(tags)),
      stringsAsFactors = FALSE
    )
  })
  tag_edges <- dplyr::bind_rows(tag_rows) %>%
    dplyr::filter(nzchar(.data$raw_label)) %>%
    dplyr::inner_join(mappings, by = "raw_label") %>%
    dplyr::transmute(
      video_id = .data$video_id,
      talent_code = .data$talent_code,
      video_title = .data$video_title,
      confidence = .data$confidence,
      video_views = .data$video_views,
      classification = .data$normalized_label
    ) %>%
    dplyr::distinct(.data$talent_code, .data$video_id, .data$classification, .keep_all = TRUE)

  add_contribution <- function(edges) {
    edges %>%
      dplyr::filter(!is.na(.data$video_views), .data$video_views >= 0) %>%
      dplyr::group_by(.data$talent_code, .data$classification) %>%
      dplyr::mutate(
        classification_views = sum(.data$video_views),
        contribution_percentage = dplyr::if_else(
          .data$classification_views > 0,
          100 * .data$video_views / .data$classification_views,
          NA_real_
        )
      ) %>%
      dplyr::ungroup()
  }

  list(
    topics = add_contribution(topic_edges),
    keywords = add_contribution(tag_edges),
    filter_topics = topic_edges %>%
      dplyr::select("video_id", "classification") %>%
      dplyr::distinct(),
    filter_keywords = tag_edges %>%
      dplyr::select("video_id", "classification") %>%
      dplyr::distinct(),
    audit = list(
      classified_videos = nrow(classified),
      missing_view_videos = classified %>%
        dplyr::filter(is.na(.data$video_views)) %>%
        dplyr::select("video_id", "video_title"),
      missing_topic_videos = classified %>%
        dplyr::filter(is.na(topics) | !nzchar(topics)) %>%
        dplyr::select("video_id", "video_title"),
      missing_normalized_keyword_videos = classified %>%
        dplyr::anti_join(
          tag_edges %>% dplyr::distinct(.data$video_id),
          by = "video_id"
        ) %>%
        dplyr::select("video_id", "video_title")
    )
  )
}

load_viewer_profiles <- function(con, talent_code = NULL) {
  talent_code <- if (is.null(talent_code)) NULL else trimws(as.character(talent_code[[1]]))
  filter_sql <- if (is.null(talent_code) || !nzchar(talent_code)) "" else "WHERE profile.talent_code = ?"
  params <- if (nzchar(filter_sql)) list(talent_code) else list()

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT profile.*, talent.talent_name,",
      "profile.public_subscribed_at AS subscribed_at,",
      "profile.public_subscriber_snapshot_date AS snapshot_date",
      "FROM analysis.viewer_profiles AS profile",
      "JOIN catalog.talents AS talent USING (talent_code)",
      filter_sql,
      "ORDER BY profile.talent_code, profile.user_id"
    ),
    params = params
  )
}
