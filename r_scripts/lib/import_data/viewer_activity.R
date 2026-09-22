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
