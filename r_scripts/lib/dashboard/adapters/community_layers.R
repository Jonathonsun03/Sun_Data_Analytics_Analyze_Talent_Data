# Plotly adapter for aggregate community-layer attendance.

dashboard_community_layer_sankey <- function(flow) {
  if (nrow(flow) == 0L) return(NULL)
  layers <- intersect(c("Drop-in", "Returning", "Regular", "Core"), unique(flow$layer))
  topics <- unique(flow$flow_topic)
  tag_keys <- flow %>%
    dplyr::distinct(.data$flow_topic, .data$flow_tag) %>%
    dplyr::mutate(id = paste(.data$flow_topic, .data$flow_tag, sep = "|"))
  streams <- unique(flow$flow_stream)
  ids <- c(paste0("layer:", layers), paste0("topic:", topics),
           paste0("tag:", tag_keys$id), paste0("stream:", streams))
  labels <- c(layers, topics, tag_keys$flow_tag, streams)
  colors <- c(
    "Drop-in" = "#94a3b8", "Returning" = "#38bdf8",
    "Regular" = "#6366f1", "Core" = "#8b5cf6"
  )
  node_colors <- c(
    unname(colors[layers]), rep("#3478c7", length(topics)),
    rep("#80a8d2", nrow(tag_keys)), rep("#cbd5e1", length(streams))
  )
  layer_links <- flow %>%
    dplyr::group_by(.data$layer, .data$flow_topic) %>%
    dplyr::summarise(value = sum(.data$attendance), .groups = "drop") %>%
    dplyr::transmute(
      source = match(paste0("layer:", .data$layer), ids) - 1L,
      target = match(paste0("topic:", .data$flow_topic), ids) - 1L,
      value = .data$value, color = unname(colors[.data$layer])
    )
  topic_links <- flow %>%
    dplyr::group_by(.data$flow_topic, .data$flow_tag) %>%
    dplyr::summarise(value = sum(.data$attendance), .groups = "drop") %>%
    dplyr::transmute(
      source = match(paste0("topic:", .data$flow_topic), ids) - 1L,
      target = match(paste0("tag:", .data$flow_topic, "|", .data$flow_tag), ids) - 1L,
      value = .data$value, color = "#8bb4df"
    )
  tag_links <- flow %>%
    dplyr::group_by(.data$flow_topic, .data$flow_tag, .data$flow_stream) %>%
    dplyr::summarise(value = sum(.data$attendance), .groups = "drop") %>%
    dplyr::transmute(
      source = match(paste0("tag:", .data$flow_topic, "|", .data$flow_tag), ids) - 1L,
      target = match(paste0("stream:", .data$flow_stream), ids) - 1L,
      value = .data$value, color = "#b7cde4"
    )
  links <- dplyr::bind_rows(layer_links, topic_links, tag_links)

  plotly::plot_ly(
    type = "sankey", arrangement = "snap",
    height = max(640L, 24L * max(nrow(tag_keys), length(streams))),
    node = list(label = labels, color = node_colors, pad = 10, thickness = 12,
                hovertemplate = "%{label}<extra></extra>"),
    link = list(source = links$source, target = links$target,
                value = links$value, color = links$color,
                hovertemplate = "%{source.label} → %{target.label}<br>%{value:,.1f} attendance units<extra></extra>")
  ) %>%
    plotly::layout(
      margin = list(l = 10, r = 20, t = 20, b = 20),
      font = list(family = "Arial, sans-serif", size = 11, color = "#101f39")
    )
}
