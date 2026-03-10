## Bundle B: combined opportunity matrix by content attributes.
## Scope: prep + static/interactive matrix plots + table helper.

bundle_b_attribute_opportunity_prep <- function(
  analytics_df,
  monetary_df,
  id_col = "Video ID",
  views_col = "views",
  revenue_col = "Estimated Revenue",
  engagement_col = "averageViewPercentage",
  content_col = "Content Type",
  topic_col = "topic",
  primary_reference_col = "primary_reference",
  tags_col = "tags",
  top_n_content = 8,
  top_n_labels = 8,
  top_n_tags = 10,
  min_videos = 2
) {
  required_analytics <- c(id_col, views_col, engagement_col)
  if (!all(required_analytics %in% names(analytics_df))) {
    stop(
      "analytics_df must include: ",
      paste(required_analytics, collapse = ", ")
    )
  }
  if (!id_col %in% names(monetary_df) || !revenue_col %in% names(monetary_df)) {
    stop("monetary_df must include: ", id_col, ", ", revenue_col)
  }

  clean_label <- function(topic_value, ref_value) {
    t <- trimws(as.character(topic_value))
    r <- trimws(as.character(ref_value))
    t[is.na(t) | !nzchar(t)] <- ""
    r[is.na(r) | !nzchar(r)] <- ""
    out <- dplyr::if_else(nzchar(r), r, t)
    out[!nzchar(out)] <- "(unclassified)"
    tolower(out)
  }

  analytics_base <- analytics_df %>%
    dplyr::mutate(
      .video_id = as.character(.data[[id_col]]),
      .views = suppressWarnings(as.numeric(.data[[views_col]])),
      .eng = suppressWarnings(as.numeric(.data[[engagement_col]])) / 100,
      .content = if (content_col %in% names(analytics_df)) as.character(.data[[content_col]]) else NA_character_,
      .topic = if (topic_col %in% names(analytics_df)) .data[[topic_col]] else NA_character_,
      .ref = if (primary_reference_col %in% names(analytics_df)) .data[[primary_reference_col]] else NA_character_,
      .label = clean_label(.topic, .ref),
      .tags = if (tags_col %in% names(analytics_df)) as.character(.data[[tags_col]]) else NA_character_
    ) %>%
    dplyr::filter(!is.na(.video_id), nzchar(.video_id), is.finite(.views), is.finite(.eng)) %>%
    dplyr::select(dplyr::all_of(c(".video_id", ".views", ".eng", ".content", ".label", ".tags")))

  revenue_by_video <- monetary_df %>%
    dplyr::transmute(
      .video_id = as.character(.data[[id_col]]),
      .revenue = suppressWarnings(as.numeric(.data[[revenue_col]]))
    ) %>%
    dplyr::filter(!is.na(.video_id), nzchar(.video_id)) %>%
    dplyr::group_by(.data$.video_id) %>%
    dplyr::summarize(.revenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop")

  label_video <- analytics_base %>%
    dplyr::distinct(.data$.video_id, .data$.label, .keep_all = TRUE)

  content_video <- analytics_base %>%
    dplyr::mutate(.content = trimws(as.character(.data$.content))) %>%
    dplyr::mutate(.content = dplyr::if_else(is.na(.data$.content) | !nzchar(.data$.content), "(unclassified)", .data$.content)) %>%
    dplyr::distinct(.data$.video_id, .data$.content, .keep_all = TRUE)

  content_summary <- content_video %>%
    dplyr::group_by(.data$.content) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data$.video_id),
      TotalViews = sum(.data$.views, na.rm = TRUE),
      MedianEngagement = stats::median(.data$.eng, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      content_video %>%
        dplyr::select(dplyr::all_of(c(".video_id", ".content"))) %>%
        dplyr::left_join(revenue_by_video, by = ".video_id") %>%
        dplyr::group_by(.data$.content) %>%
        dplyr::summarize(TotalRevenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop"),
      by = ".content"
    ) %>%
    dplyr::mutate(
      TotalRevenue = tidyr::replace_na(.data$TotalRevenue, 0),
      Attribute = tolower(.data$.content),
      LabelType = "Content type"
    ) %>%
    dplyr::filter(.data$VideoCount >= min_videos) %>%
    dplyr::arrange(dplyr::desc(.data$TotalViews + .data$TotalRevenue)) %>%
    dplyr::slice_head(n = top_n_content)

  label_summary <- label_video %>%
    dplyr::group_by(.data$.label) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data$.video_id),
      TotalViews = sum(.data$.views, na.rm = TRUE),
      MedianEngagement = stats::median(.data$.eng, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      label_video %>%
        dplyr::select(dplyr::all_of(c(".video_id", ".label"))) %>%
        dplyr::left_join(revenue_by_video, by = ".video_id") %>%
        dplyr::group_by(.data$.label) %>%
        dplyr::summarize(TotalRevenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop"),
      by = ".label"
    ) %>%
    dplyr::mutate(
      TotalRevenue = tidyr::replace_na(.data$TotalRevenue, 0),
      Attribute = .data$.label,
      LabelType = "Title label"
    ) %>%
    dplyr::filter(.data$VideoCount >= min_videos) %>%
    dplyr::arrange(dplyr::desc(.data$TotalViews + .data$TotalRevenue)) %>%
    dplyr::slice_head(n = top_n_labels)

  tag_video <- analytics_base %>%
    dplyr::mutate(.tags = ifelse(is.na(.data$.tags), "", .data$.tags)) %>%
    tidyr::separate_rows(".tags", sep = ",") %>%
    dplyr::mutate(.tag = trimws(tolower(.data$.tags))) %>%
    dplyr::filter(nzchar(.data$.tag)) %>%
    dplyr::distinct(.data$.video_id, .data$.tag, .keep_all = TRUE)

  tag_summary <- tag_video %>%
    dplyr::group_by(.data$.tag) %>%
    dplyr::summarize(
      VideoCount = dplyr::n_distinct(.data$.video_id),
      TotalViews = sum(.data$.views, na.rm = TRUE),
      MedianEngagement = stats::median(.data$.eng, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      tag_video %>%
        dplyr::select(dplyr::all_of(c(".video_id", ".tag"))) %>%
        dplyr::left_join(revenue_by_video, by = ".video_id") %>%
        dplyr::group_by(.data$.tag) %>%
        dplyr::summarize(TotalRevenue = sum(.data$.revenue, na.rm = TRUE), .groups = "drop"),
      by = ".tag"
    ) %>%
    dplyr::mutate(
      TotalRevenue = tidyr::replace_na(.data$TotalRevenue, 0),
      Attribute = .data$.tag,
      LabelType = "Tag"
    ) %>%
    dplyr::filter(.data$VideoCount >= min_videos) %>%
    dplyr::arrange(dplyr::desc(.data$TotalViews + .data$TotalRevenue)) %>%
    dplyr::slice_head(n = top_n_tags)

  out <- dplyr::bind_rows(
    content_summary %>%
      dplyr::transmute(
        Attribute = .data$Attribute,
        LabelType = .data$LabelType,
        VideoCount = .data$VideoCount,
        TotalViews = .data$TotalViews,
        TotalRevenue = .data$TotalRevenue,
        MedianEngagement = .data$MedianEngagement
      ),
    label_summary %>%
      dplyr::transmute(
        Attribute = .data$Attribute,
        LabelType = .data$LabelType,
        VideoCount = .data$VideoCount,
        TotalViews = .data$TotalViews,
        TotalRevenue = .data$TotalRevenue,
        MedianEngagement = .data$MedianEngagement
      ),
    tag_summary %>%
      dplyr::transmute(
        Attribute = .data$Attribute,
        LabelType = .data$LabelType,
        VideoCount = .data$VideoCount,
        TotalViews = .data$TotalViews,
        TotalRevenue = .data$TotalRevenue,
        MedianEngagement = .data$MedianEngagement
      )
  )

  if (nrow(out) == 0) {
    return(
      out %>%
        dplyr::mutate(
          Views_Z = numeric(),
          Revenue_Z = numeric(),
          Engagement_Z = numeric(),
          Composite_Score = numeric(),
          Performance_Band = character()
        )
    )
  }

  safe_zscore_local <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    if (length(stats::na.omit(x_num)) <= 1 || isTRUE(all.equal(stats::sd(x_num, na.rm = TRUE), 0))) {
      return(rep(0, length(x_num)))
    }
    as.numeric(scale(x_num))
  }

  out <- out %>%
    dplyr::mutate(
      AvgViewsPerVideo = dplyr::if_else(
        .data$VideoCount > 0,
        .data$TotalViews / .data$VideoCount,
        NA_real_
      ),
      AvgRevenuePerVideo = dplyr::if_else(
        .data$VideoCount > 0,
        .data$TotalRevenue / .data$VideoCount,
        NA_real_
      ),
      Views_Z = safe_zscore_local(.data$AvgViewsPerVideo),
      Revenue_Z = safe_zscore_local(.data$AvgRevenuePerVideo),
      Engagement_Z = safe_zscore_local(.data$MedianEngagement),
      Composite_Score = rowMeans(
        dplyr::across(c("Views_Z", "Revenue_Z", "Engagement_Z")),
        na.rm = TRUE
      )
    )

  if (nrow(out) < 3) {
    return(
      out %>%
        dplyr::mutate(Performance_Band = "Middle") %>%
        dplyr::arrange(dplyr::desc(.data$Composite_Score))
    )
  }

  high_cut <- stats::quantile(out$Composite_Score, probs = 0.67, na.rm = TRUE, names = FALSE)
  low_cut <- stats::quantile(out$Composite_Score, probs = 0.33, na.rm = TRUE, names = FALSE)

  out %>%
    dplyr::mutate(
      Performance_Band = dplyr::case_when(
        .data$Composite_Score >= high_cut ~ "Strength",
        .data$Composite_Score <= low_cut ~ "Weakness / Improve",
        TRUE ~ "Middle"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Composite_Score))
}

bundle_b_attribute_opportunity_matrix_plot <- function(
  attr_df,
  talent,
  add_labels = TRUE,
  use_repel = TRUE
) {
  required_cols <- c(
    "Attribute",
    "VideoCount",
    "AvgViewsPerVideo",
    "AvgRevenuePerVideo",
    "MedianEngagement",
    "Performance_Band"
  )
  if (!all(required_cols %in% names(attr_df))) {
    stop("attr_df must include: ", paste(required_cols, collapse = ", "))
  }

  if (nrow(attr_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No attribute opportunity data available.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = bundle_b_title_text(talent, "Opportunity Matrix by Tags and Title Labels")
        )
    )
  }

  plot_df <- attr_df %>%
    dplyr::filter(
      is.finite(.data$MedianEngagement),
      is.finite(.data$AvgRevenuePerVideo),
      is.finite(.data$AvgViewsPerVideo),
      .data$AvgViewsPerVideo > 0,
      .data$AvgRevenuePerVideo > 0
    ) %>%
    dplyr::mutate(
      HoverText = paste0(
        "Attribute: ", .data$Attribute,
        "<br>Type: ", .data$LabelType,
        "<br>Band: ", .data$Performance_Band,
        "<br>Videos: ", scales::comma(.data$VideoCount),
        "<br>Median Engagement: ", scales::percent(.data$MedianEngagement, accuracy = 0.1),
        "<br>Avg Views/Video: ", scales::comma(.data$AvgViewsPerVideo),
        "<br>Avg Revenue/Video: ", scales::dollar(.data$AvgRevenuePerVideo),
        "<br>Total Views: ", scales::comma(.data$TotalViews),
        "<br>Total Revenue: ", scales::dollar(.data$TotalRevenue)
      )
    )

  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No non-zero revenue points available for matrix.") +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = bundle_b_title_text(talent, "Combined Opportunity Matrix")
        )
    )
  }

  p <- plot_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$MedianEngagement,
        y = .data$AvgRevenuePerVideo,
        size = .data$VideoCount,
        shape = .data$Performance_Band,
        color = .data$Performance_Band,
        text = .data$HoverText
      )
    ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::scale_color_manual(
      values = c(
        "Strength" = sun_data_brand_colors()[["blue"]],
        "Middle" = sun_data_brand_colors()[["steel"]],
        "Weakness / Improve" = sun_data_brand_colors()[["orange"]]
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "Strength" = 17,
        "Middle" = 16,
        "Weakness / Improve" = 15
      )
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_y_continuous(
      labels = scales::label_dollar(scale = 1)
    ) +
    theme_nyt() +
    bundle_b_theme_standard() +
    ggplot2::labs(
      title = bundle_b_title_text(talent, "Combined Opportunity Matrix"),
      subtitle = "Content types, tags, and title labels in one map. Y-axis uses avg revenue per video (linear scale).",
      x = "Median engagement",
      y = "Revenue per video",
      size = "Video count",
      shape = "Performance band"
    ) +
    ggplot2::guides(color = "none")

  if (isTRUE(add_labels)) {
    if (isTRUE(use_repel) && requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_label_repel(
        ggplot2::aes(label = .data$Attribute),
        size = 2.3,
        color = sun_data_brand_colors()[["midnight"]],
        fill = "white",
        label.size = 0.1,
        box.padding = 0.2,
        point.padding = 0.2,
        min.segment.length = 0,
        max.overlaps = Inf,
        seed = 123,
        show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data$Attribute),
        size = 2.8,
        check_overlap = TRUE,
        nudge_x = 0.002,
        show.legend = FALSE
      )
    }
  }

  p
}

bundle_b_attribute_opportunity_plotly_data <- function(attr_df) {
  required_cols <- c(
    "Attribute",
    "LabelType",
    "VideoCount",
    "AvgViewsPerVideo",
    "AvgRevenuePerVideo",
    "MedianEngagement",
    "Performance_Band"
  )
  if (!all(required_cols %in% names(attr_df))) {
    stop("attr_df must include: ", paste(required_cols, collapse = ", "))
  }

  attr_df %>%
    dplyr::filter(
      is.finite(.data$MedianEngagement),
      is.finite(.data$AvgRevenuePerVideo),
      is.finite(.data$AvgViewsPerVideo),
      .data$AvgViewsPerVideo > 0,
      .data$AvgRevenuePerVideo > 0
    ) %>%
    dplyr::mutate(
      Performance_Band = factor(
        .data$Performance_Band,
        levels = c("Strength", "Middle", "Weakness / Improve")
      ),
      SizeProxy = pmax(suppressWarnings(as.numeric(.data$VideoCount)), 3),
      HoverText = paste0(
        "<b>", .data$Attribute, "</b>",
        "<br>Type: ", .data$LabelType,
        "<br>Band: ", .data$Performance_Band,
        "<br>Videos: ", scales::comma(.data$VideoCount),
        "<br>Median Engagement: ", scales::percent(.data$MedianEngagement, accuracy = 0.1),
        "<br>Avg Views/Video: ", scales::comma(.data$AvgViewsPerVideo),
        "<br>Avg Revenue/Video: ", scales::dollar(.data$AvgRevenuePerVideo),
        "<br>Total Views: ", scales::comma(.data$TotalViews),
        "<br>Total Revenue: ", scales::dollar(.data$TotalRevenue)
      )
    )
}

bundle_b_attribute_metric_matrix_plotly <- function(
  plot_df,
  talent,
  x_col,
  y_col,
  title_suffix,
  x_label,
  y_label,
  x_axis_type = c("percent", "dollar", "number"),
  y_axis_type = c("percent", "dollar", "number")
) {
  x_axis_type <- match.arg(x_axis_type)
  y_axis_type <- match.arg(y_axis_type)

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive matrix.")
  }
  if (!(x_col %in% names(plot_df)) || !(y_col %in% names(plot_df))) {
    stop("plot_df must include x and y columns: ", x_col, ", ", y_col)
  }

  keep <- is.finite(plot_df[[x_col]]) & is.finite(plot_df[[y_col]]) &
    (plot_df[[x_col]] > 0) & (plot_df[[y_col]] > 0)
  plot_df <- plot_df[keep, , drop = FALSE]

  if (nrow(plot_df) == 0) {
    return(plotly::plotly_empty(type = "scatter", mode = "markers"))
  }

  axis_layout <- function(axis_type, axis_title) {
    if (identical(axis_type, "percent")) {
      return(list(title = axis_title, tickformat = ".0%"))
    }
    if (identical(axis_type, "dollar")) {
      return(list(
        title = axis_title,
        tickprefix = "$",
        separatethousands = TRUE,
        tickformat = ",.0f"
      ))
    }
    list(title = axis_title, separatethousands = TRUE, tickformat = ",.0f")
  }

  plotly::plot_ly(
    data = plot_df,
    x = plot_df[[x_col]],
    y = plot_df[[y_col]],
    type = "scatter",
    mode = "markers",
    color = ~Performance_Band,
    colors = c(
      "Strength" = sun_data_brand_colors()[["blue"]],
      "Middle" = sun_data_brand_colors()[["steel"]],
      "Weakness / Improve" = sun_data_brand_colors()[["orange"]]
    ),
    symbol = ~Performance_Band,
    symbols = c("triangle-up", "circle", "square"),
    size = ~SizeProxy,
    sizes = c(18, 72),
    marker = list(
      line = list(color = sun_data_brand_colors()[["midnight"]], width = 1.2),
      sizemode = "diameter"
    ),
    text = ~HoverText,
    hovertemplate = "%{text}<extra></extra>"
  ) %>%
    plotly::layout(
      title = list(text = bundle_b_title_text(talent, title_suffix), x = 0.02, xanchor = "left"),
      xaxis = axis_layout(x_axis_type, x_label),
      yaxis = axis_layout(y_axis_type, y_label),
      legend = list(title = list(text = "Performance band"))
    ) %>%
    bundle_b_plotly_layout(margin_l = 90)
}

bundle_b_attribute_opportunity_matrix_plotly <- function(attr_df, talent) {
  plot_df <- bundle_b_attribute_opportunity_plotly_data(attr_df)
  bundle_b_attribute_metric_matrix_plotly(
    plot_df = plot_df,
    talent = talent,
    x_col = "MedianEngagement",
    y_col = "AvgRevenuePerVideo",
    title_suffix = "Combined Opportunity Matrix",
    x_label = "Median engagement",
    y_label = "Average revenue per video",
    x_axis_type = "percent",
    y_axis_type = "dollar"
  )
}

bundle_b_attribute_engagement_views_matrix_plotly <- function(attr_df, talent) {
  plot_df <- bundle_b_attribute_opportunity_plotly_data(attr_df)
  bundle_b_attribute_metric_matrix_plotly(
    plot_df = plot_df,
    talent = talent,
    x_col = "MedianEngagement",
    y_col = "AvgViewsPerVideo",
    title_suffix = "Engagement vs Average Views Matrix",
    x_label = "Median engagement",
    y_label = "Average views per video",
    x_axis_type = "percent",
    y_axis_type = "number"
  )
}

bundle_b_attribute_views_revenue_matrix_plotly <- function(attr_df, talent) {
  plot_df <- bundle_b_attribute_opportunity_plotly_data(attr_df)
  bundle_b_attribute_metric_matrix_plotly(
    plot_df = plot_df,
    talent = talent,
    x_col = "AvgViewsPerVideo",
    y_col = "AvgRevenuePerVideo",
    title_suffix = "Average Views vs Average Revenue Matrix",
    x_label = "Average views per video",
    y_label = "Average revenue per video",
    x_axis_type = "number",
    y_axis_type = "dollar"
  )
}

bundle_b_attribute_opportunity_table <- function(attr_df) {
  required_cols <- c(
    "Attribute",
    "LabelType",
    "VideoCount",
    "AvgViewsPerVideo",
    "AvgRevenuePerVideo",
    "MedianEngagement",
    "TotalViews",
    "TotalRevenue",
    "Composite_Score",
    "Performance_Band"
  )
  if (!all(required_cols %in% names(attr_df))) {
    stop("attr_df must include: ", paste(required_cols, collapse = ", "))
  }

  attr_df %>%
    dplyr::arrange(dplyr::desc(.data$Composite_Score), .data$Attribute) %>%
    dplyr::select(dplyr::all_of(required_cols))
}
