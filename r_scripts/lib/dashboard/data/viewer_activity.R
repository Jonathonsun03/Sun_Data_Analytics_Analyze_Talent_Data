# Community-dashboard data access and assembly.

dashboard_community_talent_catalog <- function(database_path = NULL) {
  database_path <- dashboard_resolve_database_path(database_path)
  con <- duckdb_connect(db_path = database_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbGetQuery(
    con,
    paste(
      "WITH activity_bounds AS (",
      "  SELECT talent_code,",
      "    MIN(CAST(stream_at AS DATE)) AS earliest_chat_date,",
      "    MAX(CAST(stream_at AS DATE)) AS latest_chat_date,",
      "    COUNT(DISTINCT video_id) AS videos_with_chat",
      "  FROM analysis.viewer_video_activity",
      "  GROUP BY talent_code",
      ")",
      "SELECT talent.talent_code, talent.talent_name,",
      "  bounds.earliest_chat_date, bounds.latest_chat_date,",
      "  bounds.videos_with_chat",
      "FROM catalog.talents AS talent",
      "LEFT JOIN activity_bounds AS bounds USING (talent_code)",
      "WHERE talent.active",
      "ORDER BY talent.talent_name"
    )
  ) %>%
    dplyr::mutate(
      earliest_chat_date = as.Date(.data$earliest_chat_date),
      latest_chat_date = as.Date(.data$latest_chat_date),
      videos_with_chat = as.integer(.data$videos_with_chat)
    )
}

dashboard_load_community_data <- function(
  database_path,
  talent_code,
  start_date = NULL,
  end_date = NULL,
  minimum_messages = 1L,
  overlap_max_videos = 16L
) {
  database_path <- dashboard_resolve_database_path(database_path)
  con <- duckdb_connect(db_path = database_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  activity <- load_viewer_video_activity(
    con,
    talent_code = talent_code,
    start_date = start_date,
    end_date = end_date,
    minimum_messages = minimum_messages
  )
  available_videos <- load_available_live_videos(
    con,
    talent_code = talent_code,
    start_date = start_date,
    end_date = end_date
  )

  if (nrow(activity) == 0L) {
    return(list(
      activity = activity,
      available_videos = available_videos,
      explorer = NULL,
      community_shape = NULL,
      summary = list(
        chatters = 0L,
        available_live_videos = nrow(available_videos),
        videos_with_chat = 0L,
        messages = 0
      )
    ))
  }

  classifications <- load_title_classification_contributions(con, talent_code)
  scoped_video_ids <- unique(available_videos$video_id)
  classifications[c("topics", "keywords", "filter_topics", "filter_keywords")] <- lapply(
    classifications[c("topics", "keywords", "filter_topics", "filter_keywords")],
    function(rows) {
      if (is.null(rows) || nrow(rows) == 0L) return(rows)
      dplyr::filter(rows, .data$video_id %in% scoped_video_ids)
    }
  )

  explorer <- viewer_activity_video_explorer_prep(
    activity,
    video_catalog = available_videos,
    classification_contributions = classifications
  )
  community_shape <- viewer_activity_community_shape_prep(
    activity,
    max_overlap_videos = overlap_max_videos
  )

  list(
    activity = activity,
    available_videos = available_videos,
    explorer = explorer,
    community_shape = community_shape,
    summary = list(
      chatters = nrow(community_shape$profiles),
      available_live_videos = nrow(available_videos),
      videos_with_chat = dplyr::n_distinct(activity$video_id),
      messages = sum(activity$message_count)
    )
  )
}
