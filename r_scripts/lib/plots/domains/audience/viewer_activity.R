# Viewer--video engagement network preparation and rendering.

viewer_activity_bipartite_prep <- function(
  viewer_video_activity,
  video_id = NULL,
  max_videos = 12L,
  max_users = 60L
) {
  max_videos <- suppressWarnings(as.integer(max_videos))
  max_users <- suppressWarnings(as.integer(max_users))
  if (is.na(max_videos) || max_videos < 1L) max_videos <- 12L
  if (is.na(max_users) || max_users < 1L) max_users <- 60L
  video_summary <- viewer_video_activity %>%
    dplyr::group_by(
      .data$talent_code, .data$talent_name, .data$channel_id, .data$video_id,
      .data$video_title, .data$stream_at
    ) %>%
    dplyr::summarise(
      total_messages = sum(.data$message_count),
      chatters = dplyr::n_distinct(.data$user_id),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$total_messages), dplyr::desc(.data$chatters), .data$video_id)
  selected_videos <- if (is.null(video_id)) {
    dplyr::slice_head(video_summary, n = max_videos)
  } else {
    dplyr::filter(video_summary, .data$video_id == video_id)
  }
  selected_keys <- selected_videos %>% dplyr::select(.data$talent_code, .data$channel_id, .data$video_id)
  user_ranking <- viewer_video_activity %>%
    dplyr::semi_join(selected_keys, by = c("talent_code", "channel_id", "video_id")) %>%
    dplyr::group_by(.data$user_id) %>%
    dplyr::summarise(
      current_username = dplyr::last(.data$latest_username_in_video),
      selected_messages = sum(.data$message_count),
      selected_videos = dplyr::n_distinct(.data$video_id),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$selected_messages), dplyr::desc(.data$selected_videos), .data$user_id) %>%
    dplyr::slice_head(n = max_users)
  engagement_edges <- viewer_video_activity %>%
    dplyr::semi_join(selected_keys, by = c("talent_code", "channel_id", "video_id")) %>%
    dplyr::semi_join(user_ranking, by = "user_id") %>%
    dplyr::transmute(
      user_id = .data$user_id, talent_code = .data$talent_code,
      channel_id = .data$channel_id, video_id = .data$video_id,
      message_count = .data$message_count
    )
  position_layer <- function(nodes) {
    nodes %>%
      dplyr::arrange(dplyr::desc(.data$node_weight), .data$node_id) %>%
      dplyr::mutate(y = if (dplyr::n() == 1L) 0 else seq(-1, 1, length.out = dplyr::n()))
  }
  user_nodes <- user_ranking %>%
    dplyr::transmute(node_id = .data$user_id, node_type = "User", label = dplyr::coalesce(.data$current_username, .data$user_id), node_weight = .data$selected_messages, x = 0) %>%
    position_layer()
  displayed_video_weights <- engagement_edges %>%
    dplyr::group_by(.data$talent_code, .data$channel_id, .data$video_id) %>%
    dplyr::summarise(selected_messages = sum(.data$message_count), .groups = "drop")
  video_nodes <- selected_videos %>%
    dplyr::inner_join(displayed_video_weights, by = c("talent_code", "channel_id", "video_id")) %>%
    dplyr::transmute(node_id = .data$video_id, node_type = "Video", label = dplyr::coalesce(.data$video_title, .data$video_id), talent_code = .data$talent_code, channel_id = .data$channel_id, node_weight = .data$selected_messages, x = 1) %>%
    position_layer()
  streamer_nodes <- video_nodes %>%
    dplyr::group_by(.data$talent_code, .data$channel_id) %>%
    dplyr::summarise(node_id = dplyr::first(.data$channel_id), node_type = "Streamer", label = dplyr::first(.data$talent_code), node_weight = sum(.data$node_weight), x = 2, .groups = "drop") %>%
    position_layer()
  user_lookup <- user_nodes %>% dplyr::select(user_id = .data$node_id, x_user = .data$x, y_user = .data$y)
  video_lookup <- video_nodes %>% dplyr::select(video_id = .data$node_id, x_video = .data$x, y_video = .data$y)
  streamer_lookup <- streamer_nodes %>% dplyr::select(channel_id = .data$node_id, x_streamer = .data$x, y_streamer = .data$y)
  list(
    users = user_nodes,
    videos = video_nodes,
    streamers = streamer_nodes,
    engagement_edges = engagement_edges %>% dplyr::inner_join(user_lookup, by = "user_id") %>% dplyr::inner_join(video_lookup, by = "video_id"),
    ownership_edges = video_nodes %>% dplyr::inner_join(streamer_lookup, by = "channel_id"),
    nodes = dplyr::bind_rows(user_nodes, video_nodes %>% dplyr::select(names(user_nodes)), streamer_nodes %>% dplyr::select(names(user_nodes)))
  )
}

viewer_activity_bipartite_ggplot <- function(network, label_count = 15L) {
  if (nrow(network$engagement_edges) == 0L) return(NULL)
  labels <- dplyr::bind_rows(
    dplyr::slice_head(network$users, n = min(as.integer(label_count), nrow(network$users))),
    network$videos, network$streamers
  )
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = network$ownership_edges, ggplot2::aes(x = .data$x, y = .data$y, xend = .data$x_streamer, yend = .data$y_streamer), color = "#CBD5E1", linewidth = 0.35) +
    ggplot2::geom_segment(data = network$engagement_edges, ggplot2::aes(x = .data$x_user, y = .data$y_user, xend = .data$x_video, yend = .data$y_video, linewidth = .data$message_count), color = "#2563EB", alpha = 0.38) +
    ggplot2::geom_point(data = network$nodes, ggplot2::aes(x = .data$x, y = .data$y, size = .data$node_weight, fill = .data$node_type, shape = .data$node_type), color = "#0F172A", alpha = 0.92) +
    ggplot2::geom_text(data = labels, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label), check_overlap = TRUE, size = 3) +
    ggplot2::scale_x_continuous(breaks = c(0, 1, 2), labels = c("Users", "Videos", "Streamers"), limits = c(-0.45, 2.45)) +
    ggplot2::scale_linewidth_continuous(trans = "sqrt", range = c(0.15, 2.8), name = "Messages sent") +
    ggplot2::scale_size_continuous(trans = "sqrt", range = c(2.5, 11), name = "Selected messages") +
    ggplot2::scale_fill_manual(values = c(User = "#93C5FD", Video = "#FCD34D", Streamer = "#C4B5FD")) +
    ggplot2::scale_shape_manual(values = c(User = 21, Video = 22, Streamer = 24)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom") +
    ggplot2::labs(title = "User–video chat engagement network", subtitle = "Blue edge width is messages sent in one video; gray edges identify video ownership.", x = NULL, y = NULL, fill = "Node type", shape = "Node type")
}
