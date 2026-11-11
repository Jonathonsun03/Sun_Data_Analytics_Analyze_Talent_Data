# Aggregate observed chatter attendance by community layer and main topic.

dashboard_community_layer_attendance <- function(activity, profiles, topics) {
  topic_lookup <- topics %>%
    dplyr::select("video_id", topic = "classification") %>%
    dplyr::distinct(.data$video_id, .keep_all = TRUE)

  activity %>%
    dplyr::inner_join(
      profiles %>% dplyr::select("user_id", layer = "segment"),
      by = "user_id"
    ) %>%
    dplyr::left_join(topic_lookup, by = "video_id") %>%
    dplyr::mutate(
      topic = dplyr::coalesce(.data$topic, "Unclassified"),
      video_title = dplyr::coalesce(.data$video_title, .data$video_id)
    ) %>%
    dplyr::group_by(.data$layer, .data$topic, .data$video_id, .data$video_title) %>%
    dplyr::summarise(
      attendance = dplyr::n_distinct(.data$user_id),
      messages = sum(.data$message_count),
      .groups = "drop"
    )
}

dashboard_community_layer_crosstab <- function(attendance) {
  topics <- attendance %>%
    dplyr::group_by(.data$topic) %>%
    dplyr::summarise(total = sum(.data$attendance), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$total), .data$topic) %>%
    dplyr::pull(.data$topic)
  attendance %>%
    dplyr::group_by(.data$layer, .data$topic) %>%
    dplyr::summarise(attendance = sum(.data$attendance), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = "topic", values_from = "attendance", values_fill = 0
    ) %>%
    dplyr::select("layer", dplyr::all_of(topics)) %>%
    dplyr::arrange(match(.data$layer, c("Drop-in", "Returning", "Regular", "Core"))) %>%
    dplyr::rename("Community Layer" = "layer")
}

dashboard_community_layer_flow <- function(attendance, tags,
                                           max_topics = 8L, max_streams = 18L,
                                           max_tags = 12L) {
  rows <- attendance
  if (nrow(rows) == 0L) return(rows)

  top_topics <- rows %>%
    dplyr::group_by(.data$topic) %>%
    dplyr::summarise(total = sum(.data$attendance), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$total), .data$topic) %>%
    dplyr::slice_head(n = max_topics) %>%
    dplyr::pull(.data$topic)
  rows <- rows %>%
    dplyr::mutate(flow_topic = dplyr::if_else(
      .data$topic %in% top_topics, .data$topic, "Other topics"
    ))

  top_streams <- rows %>%
    dplyr::filter(.data$flow_topic != "Other topics") %>%
    dplyr::group_by(.data$video_id) %>%
    dplyr::summarise(total = sum(.data$attendance), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$total), .data$video_id) %>%
    dplyr::slice_head(n = max_streams) %>%
    dplyr::pull(.data$video_id)

  tag_lookup <- tags %>%
    dplyr::select("video_id", tag = "classification") %>%
    dplyr::distinct()
  rows <- rows %>%
    dplyr::mutate(
      flow_stream = dplyr::case_when(
        .data$flow_topic == "Other topics" ~ "Other topics · streams",
        .data$video_id %in% top_streams ~ paste0(.data$video_title, " [", .data$video_id, "]"),
        TRUE ~ paste0(.data$flow_topic, " · other streams")
      )
    ) %>%
    dplyr::left_join(tag_lookup, by = "video_id", relationship = "many-to-many") %>%
    dplyr::mutate(tag = dplyr::coalesce(.data$tag, "Untagged")) %>%
    dplyr::group_by(.data$layer, .data$topic, .data$video_id) %>%
    dplyr::mutate(weight = .data$attendance / dplyr::n()) %>%
    dplyr::ungroup()

  top_tags <- rows %>%
    dplyr::group_by(.data$tag) %>%
    dplyr::summarise(total = sum(.data$weight), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$total), .data$tag) %>%
    dplyr::slice_head(n = max_tags) %>%
    dplyr::pull(.data$tag)
  rows %>%
    dplyr::mutate(flow_tag = dplyr::if_else(
      .data$tag %in% top_tags, .data$tag, "Other tags"
    )) %>%
    dplyr::group_by(.data$layer, .data$flow_topic, .data$flow_tag, .data$flow_stream) %>%
    dplyr::summarise(attendance = sum(.data$weight), .groups = "drop")
}
