# Reusable numerical summaries for observed viewer chat activity.

viewer_activity_talent_summary <- function(viewer_profiles, viewer_video_activity) {
  viewer_profiles %>%
    dplyr::group_by(.data$talent_code, .data$talent_name) %>%
    dplyr::summarise(
      chatters = dplyr::n(),
      videos_with_chat = dplyr::n_distinct(
        viewer_video_activity$video_id[
          viewer_video_activity$talent_code == dplyr::first(.data$talent_code)
        ]
      ),
      messages = sum(.data$total_messages),
      median_videos_per_chatter = stats::median(.data$videos_chatted_in),
      median_messages_per_chatter = stats::median(.data$total_messages),
      public_subscription_matches = sum(.data$public_subscription_matched),
      public_subscription_coverage = mean(.data$public_subscription_matched),
      chatters_with_membership_events = sum(.data$membership_events > 0),
      .groups = "drop"
    )
}

viewer_activity_video_summary <- function(viewer_video_activity) {
  viewer_video_activity %>%
    dplyr::group_by(
      .data$talent_code, .data$video_id, .data$video_title, .data$stream_at
    ) %>%
    dplyr::summarise(
      chatters = dplyr::n_distinct(.data$user_id),
      messages = sum(.data$message_count),
      paid_messages = sum(.data$paid_message_count),
      membership_events = sum(.data$membership_event_count),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$messages))
}

viewer_activity_video_participants <- function(viewer_video_activity, video_id, limit = 50L) {
  viewer_video_activity %>%
    dplyr::filter(.data$video_id == video_id) %>%
    dplyr::arrange(dplyr::desc(.data$message_count), .data$user_id) %>%
    dplyr::select(
      .data$talent_code, .data$video_id, .data$user_id,
      .data$latest_username_in_video, .data$message_count,
      .data$text_message_count, .data$paid_message_count,
      .data$membership_event_count, .data$first_message_second,
      .data$last_message_second, .data$active_minutes
    ) %>%
    dplyr::slice_head(n = as.integer(limit))
}
