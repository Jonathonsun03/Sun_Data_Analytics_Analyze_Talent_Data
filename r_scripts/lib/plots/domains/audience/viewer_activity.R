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

.viewer_activity_d3_state <- new.env(parent = emptyenv())
.viewer_activity_d3_state$widget_id <- 0L
.viewer_activity_d3_state$published_datasets <- new.env(parent = emptyenv())

.viewer_activity_d3_dependency <- function() {
  htmltools::htmlDependency(
    name = "sun-data-viewer-activity",
    version = "1.8.0",
    src = c(file = normalizePath(here::here("js"), mustWork = TRUE)),
    script = c(
      "vendor/d3.v7.9.0.min.js",
      "lib/viewer_activity_network.js",
      "lib/community_shape.js",
      "lib/video_explorer.js",
      "lib/chatter_relationship_views.js"
    ),
    stylesheet = c(
      "styles/viewer_activity_network.css",
      "styles/community_shape.css"
    )
  )
}

viewer_activity_bipartite_d3 <- function(network, label_count = 15L, height = 680L) {
  if (nrow(network$engagement_edges) == 0L) return(NULL)
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Install the htmltools package to render the D3 network.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Install the jsonlite package to render the D3 network.", call. = FALSE)
  }

  label_count <- suppressWarnings(as.integer(label_count))
  if (is.na(label_count) || label_count < 0L) label_count <- 15L
  height <- suppressWarnings(as.integer(height))
  if (is.na(height) || height < 320L) height <- 680L

  labeled_users <- network$users %>%
    dplyr::slice_head(n = min(label_count, nrow(network$users))) %>%
    dplyr::pull(.data$node_id)
  nodes <- dplyr::bind_rows(
    network$users %>%
      dplyr::transmute(
        id = paste0("user:", .data$node_id),
        type = "User",
        label = .data$label,
        weight = .data$node_weight,
        showLabel = .data$node_id %in% labeled_users
      ),
    network$videos %>%
      dplyr::transmute(
        id = paste0("video:", .data$node_id),
        type = "Video",
        label = .data$label,
        weight = .data$node_weight,
        showLabel = TRUE
      ),
    network$streamers %>%
      dplyr::transmute(
        id = paste0("streamer:", .data$node_id),
        type = "Streamer",
        label = .data$label,
        weight = .data$node_weight,
        showLabel = TRUE
      )
  )
  links <- dplyr::bind_rows(
    network$engagement_edges %>%
      dplyr::transmute(
        source = paste0("user:", .data$user_id),
        target = paste0("video:", .data$video_id),
        type = "engagement",
        weight = .data$message_count
      ),
    network$ownership_edges %>%
      dplyr::transmute(
        source = paste0("video:", .data$node_id),
        target = paste0("streamer:", .data$channel_id),
        type = "ownership",
        weight = .data$node_weight
      )
  )

  payload <- jsonlite::toJSON(
    list(nodes = nodes, links = links),
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null",
    digits = NA
  )
  payload <- gsub("</", "<\\/", payload, fixed = TRUE)
  .viewer_activity_d3_state$widget_id <- .viewer_activity_d3_state$widget_id + 1L
  widget_id <- paste0("viewer-activity-network-", .viewer_activity_d3_state$widget_id)
  data_id <- paste0(widget_id, "-data")

  widget <- htmltools::tagList(
    htmltools::tags$div(id = widget_id),
    htmltools::tags$script(
      id = data_id,
      type = "application/json",
      htmltools::HTML(payload)
    ),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "SunDataNetwork.renderViewerActivityNetwork(document.getElementById(%s), JSON.parse(document.getElementById(%s).textContent), {height: %d});",
      jsonlite::toJSON(widget_id, auto_unbox = TRUE),
      jsonlite::toJSON(data_id, auto_unbox = TRUE),
      height
    )))
  )
  htmltools::browsable(htmltools::attachDependencies(widget, .viewer_activity_d3_dependency()))
}

viewer_activity_community_shape_prep <- function(
  viewer_video_activity,
  max_overlap_videos = 12L
) {
  if (nrow(viewer_video_activity) == 0L) {
    stop("Community-shape preparation requires at least one activity row.", call. = FALSE)
  }
  max_overlap_videos <- suppressWarnings(as.integer(max_overlap_videos))
  if (is.na(max_overlap_videos) || max_overlap_videos < 2L) max_overlap_videos <- 12L

  segment_levels <- c("Drop-in", "Returning", "Regular", "Core")
  breadth_levels <- c("1", "2", "3–4", "5–8", "9+")
  intensity_levels <- c("1–9", "10–49", "50–199", "200–999", "1,000+")

  chatter_profiles <- viewer_video_activity %>%
    dplyr::arrange(.data$user_id, .data$stream_at, .data$video_id) %>%
    dplyr::group_by(.data$user_id) %>%
    dplyr::summarise(
      label = dplyr::last(dplyr::coalesce(.data$latest_username_in_video, .data$user_id)),
      videos = dplyr::n_distinct(.data$video_id),
      messages = sum(.data$message_count),
      first_stream_at = min(.data$stream_at, na.rm = TRUE),
      last_stream_at = max(.data$stream_at, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      segment = dplyr::case_when(
        .data$videos >= 8L ~ "Core",
        .data$videos >= 4L ~ "Regular",
        .data$videos >= 2L ~ "Returning",
        TRUE ~ "Drop-in"
      ),
      breadth_band = dplyr::case_when(
        .data$videos == 1L ~ "1",
        .data$videos == 2L ~ "2",
        .data$videos <= 4L ~ "3–4",
        .data$videos <= 8L ~ "5–8",
        TRUE ~ "9+"
      ),
      intensity_band = dplyr::case_when(
        .data$messages <= 9 ~ "1–9",
        .data$messages <= 49 ~ "10–49",
        .data$messages <= 199 ~ "50–199",
        .data$messages <= 999 ~ "200–999",
        TRUE ~ "1,000+"
      )
    )

  segment_summary <- data.frame(segment = segment_levels) %>%
    dplyr::left_join(
      chatter_profiles %>%
        dplyr::group_by(.data$segment) %>%
        dplyr::summarise(
          chatters = dplyr::n(),
          messages = sum(.data$messages),
          .groups = "drop"
        ),
      by = "segment"
    ) %>%
    dplyr::mutate(
      chatters = dplyr::coalesce(.data$chatters, 0L),
      messages = dplyr::coalesce(.data$messages, 0),
      chatter_share = .data$chatters / sum(.data$chatters),
      message_share = .data$messages / sum(.data$messages),
      order = match(.data$segment, segment_levels)
    )

  landscape <- expand.grid(
    breadth_band = breadth_levels,
    intensity_band = intensity_levels,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      chatter_profiles %>%
        dplyr::count(.data$breadth_band, .data$intensity_band, name = "chatters"),
      by = c("breadth_band", "intensity_band")
    ) %>%
    dplyr::mutate(
      chatters = dplyr::coalesce(.data$chatters, 0L),
      breadth_order = match(.data$breadth_band, breadth_levels),
      intensity_order = match(.data$intensity_band, intensity_levels)
    )

  video_summary <- viewer_video_activity %>%
    dplyr::group_by(.data$video_id, .data$video_title, .data$talent_code) %>%
    dplyr::summarise(
      messages = sum(.data$message_count),
      chatters = dplyr::n_distinct(.data$user_id),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$messages), dplyr::desc(.data$chatters), .data$video_id) %>%
    dplyr::slice_head(n = max_overlap_videos) %>%
    dplyr::mutate(
      label = dplyr::coalesce(.data$video_title, .data$video_id),
      order = dplyr::row_number()
    )
  membership <- viewer_video_activity %>%
    dplyr::semi_join(video_summary, by = "video_id") %>%
    dplyr::distinct(.data$video_id, .data$user_id)
  member_sets <- split(membership$user_id, membership$video_id)
  overlap <- expand.grid(
    row_id = video_summary$video_id,
    column_id = video_summary$video_id,
    stringsAsFactors = FALSE
  )
  overlap$shared_chatters <- vapply(
    seq_len(nrow(overlap)),
    function(index) {
      length(intersect(member_sets[[overlap$row_id[[index]]]], member_sets[[overlap$column_id[[index]]]]))
    },
    integer(1)
  )
  overlap$union_chatters <- vapply(
    seq_len(nrow(overlap)),
    function(index) {
      length(union(member_sets[[overlap$row_id[[index]]]], member_sets[[overlap$column_id[[index]]]]))
    },
    integer(1)
  )
  overlap <- overlap %>%
    dplyr::mutate(
      similarity = dplyr::if_else(
        .data$union_chatters > 0L,
        .data$shared_chatters / .data$union_chatters,
        0
      )
    )

  list(
    profiles = chatter_profiles,
    segments = segment_summary,
    landscape = landscape,
    breadth_levels = breadth_levels,
    intensity_levels = intensity_levels,
    overlap_videos = video_summary %>%
      dplyr::transmute(
        id = .data$video_id,
        label = .data$label,
        messages = .data$messages,
        chatters = .data$chatters,
        order = .data$order
      ),
    overlap = overlap
  )
}

viewer_activity_community_shape_d3 <- function(
  community_shape,
  view = c("segments", "landscape", "overlap"),
  height = NULL
) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Install the htmltools package to render the D3 community charts.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Install the jsonlite package to render the D3 community charts.", call. = FALSE)
  }
  view <- match.arg(view)
  default_heights <- c(segments = 310L, landscape = 430L, overlap = 720L)
  if (is.null(height)) height <- default_heights[[view]]
  height <- suppressWarnings(as.integer(height))
  if (is.na(height) || height < 240L) height <- default_heights[[view]]

  payload_data <- switch(
    view,
    segments = list(rows = community_shape$segments),
    landscape = list(
      cells = community_shape$landscape,
      breadth = community_shape$breadth_levels,
      intensity = community_shape$intensity_levels
    ),
    overlap = list(
      videos = community_shape$overlap_videos,
      cells = community_shape$overlap
    )
  )
  payload <- jsonlite::toJSON(
    payload_data,
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null",
    digits = NA
  )
  payload <- gsub("</", "<\\/", payload, fixed = TRUE)
  .viewer_activity_d3_state$widget_id <- .viewer_activity_d3_state$widget_id + 1L
  widget_id <- paste0("viewer-activity-community-", view, "-", .viewer_activity_d3_state$widget_id)
  data_id <- paste0(widget_id, "-data")
  renderer <- switch(
    view,
    segments = "renderSegments",
    landscape = "renderLandscape",
    overlap = "renderOverlap"
  )

  widget <- htmltools::tagList(
    htmltools::tags$div(id = widget_id, class = "sd-community-chart"),
    htmltools::tags$script(
      id = data_id,
      type = "application/json",
      htmltools::HTML(payload)
    ),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "SunDataCommunity.%s(document.getElementById(%s), JSON.parse(document.getElementById(%s).textContent), {height: %d});",
      renderer,
      jsonlite::toJSON(widget_id, auto_unbox = TRUE),
      jsonlite::toJSON(data_id, auto_unbox = TRUE),
      height
    )))
  )
  htmltools::browsable(htmltools::attachDependencies(widget, .viewer_activity_d3_dependency()))
}

viewer_activity_video_explorer_prep <- function(
  viewer_video_activity,
  video_catalog = NULL,
  classification_contributions = NULL
) {
  if (nrow(viewer_video_activity) == 0L) {
    stop("Video-explorer preparation requires at least one activity row.", call. = FALSE)
  }

  activity_videos <- viewer_video_activity %>%
    dplyr::group_by(
      .data$video_id,
      .data$video_title,
      .data$talent_code,
      .data$talent_name,
      .data$channel_id,
      .data$stream_at
    ) %>%
    dplyr::summarise(
      messages = sum(.data$message_count),
      chatters = dplyr::n_distinct(.data$user_id),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      dplyr::desc(.data$messages),
      dplyr::desc(.data$chatters),
      dplyr::desc(.data$stream_at),
      .data$video_id
    ) %>%
    dplyr::mutate(label = dplyr::coalesce(.data$video_title, .data$video_id))
  videos <- if (is.null(video_catalog)) {
    activity_videos
  } else {
    video_catalog %>%
      dplyr::left_join(
        activity_videos %>%
          dplyr::select("video_id", "messages", "chatters"),
        by = "video_id"
      ) %>%
      dplyr::mutate(
        messages = dplyr::coalesce(.data$messages, 0),
        chatters = dplyr::coalesce(.data$chatters, 0L),
        label = dplyr::coalesce(.data$video_title, .data$video_id)
      ) %>%
      dplyr::arrange(
        dplyr::desc(.data$messages),
        dplyr::desc(.data$chatters),
        dplyr::desc(.data$stream_at),
        .data$video_id
      )
  }
  videos <- videos %>%
    dplyr::mutate(video_index = dplyr::row_number() - 1L)
  multiple_talents <- dplyr::n_distinct(videos$talent_code) > 1L
  videos <- videos %>%
    dplyr::mutate(
      option_label = if (multiple_talents) {
        paste0("[", .data$talent_code, "] ", .data$label)
      } else {
        .data$label
      }
    )
  users <- viewer_video_activity %>%
    dplyr::arrange(.data$user_id, .data$stream_at, .data$video_id) %>%
    dplyr::group_by(.data$user_id) %>%
    dplyr::summarise(
      label = dplyr::last(dplyr::coalesce(.data$latest_username_in_video, .data$user_id)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$user_id) %>%
    dplyr::mutate(user_index = dplyr::row_number() - 1L)
  edge_rows <- viewer_video_activity %>%
    dplyr::transmute(
      video_index = match(.data$video_id, videos$video_id) - 1L,
      user_index = match(.data$user_id, users$user_id) - 1L,
      messages = as.integer(.data$message_count)
    )
  edge_matrix <- as.matrix(edge_rows)
  storage.mode(edge_matrix) <- "integer"

  serialize_classifications <- function(edges) {
    if (is.null(edges) || nrow(edges) == 0L) return(list())
    edges %>%
      dplyr::transmute(
        videoId = .data$video_id,
        label = .data$classification,
        videoTitle = .data$video_title,
        videoViews = .data$video_views,
        classificationViews = .data$classification_views,
        contributionPercentage = .data$contribution_percentage,
        confidence = .data$confidence
      )
  }

  serialize_filters <- function(edges) {
    if (is.null(edges) || nrow(edges) == 0L) return(list())
    edges %>%
      dplyr::semi_join(
        videos %>% dplyr::select("video_id"),
        by = "video_id"
      ) %>%
      dplyr::transmute(
        videoId = .data$video_id,
        label = .data$classification
      )
  }

  list(
    videos = videos %>%
      dplyr::transmute(
        id = .data$video_id,
        label = .data$label,
        optionLabel = .data$option_label,
        talentCode = .data$talent_code,
        talentName = .data$talent_name,
        streamerId = .data$channel_id,
        streamAt = as.character(.data$stream_at),
        messages = .data$messages,
        chatters = .data$chatters
      ),
    users = users %>%
      dplyr::transmute(id = .data$user_id, label = .data$label),
    edges = edge_matrix,
    classifications = list(
      topics = serialize_classifications(classification_contributions$topics),
      keywords = serialize_classifications(classification_contributions$keywords)
    ),
    filters = list(
      topics = serialize_filters(classification_contributions$filter_topics),
      keywords = serialize_filters(classification_contributions$filter_keywords)
    )
  )
}

viewer_activity_video_explorer_d3 <- function(
  explorer_data,
  view = c("network", "overlap", "sankey", "heatmap"),
  dataset_id = "viewer_activity_explorer",
  initial_video_count = NULL,
  initial_video_ids = NULL,
  max_selected = NULL,
  max_users = 100L,
  label_count = 18L,
  height = NULL
) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Install the htmltools package to render the D3 video explorer.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Install the jsonlite package to render the D3 video explorer.", call. = FALSE)
  }
  view <- match.arg(view)
  defaults <- switch(
    view,
    network = list(initial = 24L, selected = 40L, height = 760L),
    overlap = list(initial = 16L, selected = 30L, height = 720L),
    sankey = list(initial = 12L, selected = 20L, height = 820L),
    heatmap = list(initial = 24L, selected = 40L, height = 900L)
  )
  normalize_positive <- function(value, default, minimum = 1L) {
    if (is.null(value)) return(default)
    value <- suppressWarnings(as.integer(value))
    if (is.na(value) || value < minimum) default else value
  }
  initial_video_count <- normalize_positive(initial_video_count, defaults$initial)
  max_selected <- normalize_positive(max_selected, defaults$selected)
  max_users <- normalize_positive(max_users, 100L)
  label_count <- normalize_positive(label_count, 18L)
  height <- normalize_positive(height, defaults$height, minimum = 320L)
  initial_video_ids <- if (is.null(initial_video_ids)) character() else as.character(initial_video_ids)

  .viewer_activity_d3_state$widget_id <- .viewer_activity_d3_state$widget_id + 1L
  widget_id <- paste0("viewer-activity-video-explorer-", view, "-", .viewer_activity_d3_state$widget_id)
  include_data <- !exists(
    dataset_id,
    envir = .viewer_activity_d3_state$published_datasets,
    inherits = FALSE
  )
  data_tags <- htmltools::tagList()
  if (include_data) {
    payload <- jsonlite::toJSON(
      explorer_data,
      dataframe = "rows",
      matrix = "rowmajor",
      auto_unbox = TRUE,
      na = "null",
      digits = NA
    )
    payload <- gsub("</", "<\\/", payload, fixed = TRUE)
    data_id <- paste0(widget_id, "-data")
    data_tags <- htmltools::tagList(
      htmltools::tags$script(
        id = data_id,
        type = "application/json",
        htmltools::HTML(payload)
      ),
      htmltools::tags$script(htmltools::HTML(sprintf(
        "window.SunDataVideoExplorerDatasets = window.SunDataVideoExplorerDatasets || {}; window.SunDataVideoExplorerDatasets[%s] = JSON.parse(document.getElementById(%s).textContent);",
        jsonlite::toJSON(dataset_id, auto_unbox = TRUE),
        jsonlite::toJSON(data_id, auto_unbox = TRUE)
      )))
    )
    assign(
      dataset_id,
      TRUE,
      envir = .viewer_activity_d3_state$published_datasets
    )
  }
  renderer <- switch(
    view,
    network = "renderNetwork",
    overlap = "renderOverlap",
    sankey = "renderSankey",
    heatmap = "renderHeatmap"
  )
  options <- list(
    initialVideoCount = initial_video_count,
    initialVideoIds = initial_video_ids,
    maxSelected = max_selected,
    maxUsers = max_users,
    labelCount = label_count,
    height = height
  )
  widget <- htmltools::tagList(
    data_tags,
    htmltools::tags$div(id = widget_id),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "SunDataVideoExplorer.%s(document.getElementById(%s), window.SunDataVideoExplorerDatasets[%s], %s);",
      renderer,
      jsonlite::toJSON(widget_id, auto_unbox = TRUE),
      jsonlite::toJSON(dataset_id, auto_unbox = TRUE),
      jsonlite::toJSON(options, auto_unbox = TRUE, null = "null")
    )))
  )
  htmltools::browsable(htmltools::attachDependencies(widget, .viewer_activity_d3_dependency()))
}
