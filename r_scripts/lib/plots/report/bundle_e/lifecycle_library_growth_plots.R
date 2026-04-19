plot_bundle_e_library_growth <- function(daily_df, talent) {
  if (nrow(daily_df) == 0) {
    return(bundle_e_empty_plot("Library Growth Snapshot", talent))
  }

  ggplot2::ggplot(daily_df, ggplot2::aes(x = .data$snapshot_date, y = .data$total_library_views)) +
    ggplot2::geom_line(color = sun_data_brand_colors()[["blue"]], linewidth = 1.0) +
    ggplot2::geom_point(color = sun_data_brand_colors()[["midnight"]], size = 1.6) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Library Growth Snapshot"),
      subtitle = bundle_e_subtitle(talent, "Total observed library views across snapshots."),
      x = "Snapshot date",
      y = "Total library views"
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
}

plot_bundle_e_back_catalog_share <- function(back_catalog_df, talent) {
  if (nrow(back_catalog_df) == 0) {
    return(bundle_e_empty_plot("Back Catalog vs Recent Contribution", talent))
  }

  plot_df <- back_catalog_df %>%
    dplyr::filter(!is.na(.data$view_share), is.finite(.data$view_share)) %>%
    dplyr::mutate(
      catalog_segment = dplyr::case_when(
        .data$library_age_bucket %in% c("0_7_days", "8_30_days") ~ "Recent videos\n(0-30 days)",
        .data$library_age_bucket %in% c("31_60_days", "61_90_days", "91_plus_days") ~ "Back catalog\n(31+ days)",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(.data$catalog_segment)) %>%
    dplyr::group_by(.data$snapshot_date, .data$catalog_segment) %>%
    dplyr::summarise(view_share = sum(.data$view_share, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      catalog_segment = factor(
        .data$catalog_segment,
        levels = c("Recent videos\n(0-30 days)", "Back catalog\n(31+ days)")
      ),
      tooltip_text = paste0(
        "Snapshot date: ", format(.data$snapshot_date, "%Y-%m-%d"),
        "<br>Segment: ", .data$catalog_segment,
        "<br>Share of library views: ", scales::percent(.data$view_share, accuracy = 0.1)
      )
    )

  max_share <- max(plot_df$view_share, na.rm = TRUE)
  y_upper <- if (is.finite(max_share) && max_share > 0) {
    max_share * 1.08
  } else {
    0.01
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$snapshot_date,
      y = .data$view_share,
      fill = .data$catalog_segment,
      group = 1,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_area(alpha = 0.82, show.legend = FALSE) +
    ggplot2::geom_line(color = sun_data_brand_colors()[["midnight"]], linewidth = 0.5, show.legend = FALSE) +
    ggplot2::facet_wrap(~catalog_segment, nrow = 1) +
    ggplot2::scale_fill_manual(
      values = c(
        "Recent videos\n(0-30 days)" = sun_data_brand_colors()[["blue"]],
        "Back catalog\n(31+ days)" = sun_data_brand_colors()[["orange"]]
      )
    ) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Back Catalog vs Recent Contribution"),
      subtitle = bundle_e_subtitle(talent, "Shows whether newer uploads or older videos are carrying more of total library views over time."),
      x = "Snapshot date",
      y = "Percent of total library views"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = c(0, y_upper)
    )
}

plot_bundle_e_latest_views_vs_age <- function(video_summary, talent) {
  if (nrow(video_summary) == 0) {
    return(bundle_e_empty_plot("Latest Views vs Video Age", talent))
  }

  plot_df <- video_summary %>%
    dplyr::filter(
      !is.na(.data$latest_age_days),
      !is.na(.data$latest_views),
      is.finite(.data$latest_age_days),
      is.finite(.data$latest_views)
    )

  if (nrow(plot_df) == 0) {
    return(bundle_e_empty_plot("Latest Views vs Video Age", talent))
  }

  plot_df <- plot_df %>%
    dplyr::mutate(
      content_type_label = dplyr::case_when(
        .data$`Content Type` == "live" ~ "Stream",
        .data$`Content Type` == "short" ~ "Short",
        .data$`Content Type` == "video" ~ "Video",
        TRUE ~ tools::toTitleCase(as.character(.data$`Content Type`))
      ),
      content_type_label = factor(
        .data$content_type_label,
        levels = c("Short", "Video", "Stream"),
        ordered = TRUE
      ),
      tooltip_text = paste0(
        "Title: ", .data$Title,
        "<br>Video age: ", scales::comma(.data$latest_age_days, accuracy = 1), " days",
        "<br>Latest observed views: ", scales::comma(.data$latest_views, accuracy = 1),
        "<br>Content type: ", .data$content_type_label
      )
    ) %>%
    dplyr::filter(!is.na(.data$content_type_label))

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$latest_age_days,
      y = .data$latest_views,
      color = .data$content_type_label,
      shape = .data$content_type_label,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_point(alpha = 0.75, size = 2.6) +
    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      color = sun_data_brand_colors()[["midnight"]],
      linewidth = 0.7,
      inherit.aes = FALSE,
      ggplot2::aes(x = .data$latest_age_days, y = .data$latest_views, group = .data$content_type_label)
    ) +
    ggplot2::facet_wrap(~content_type_label, nrow = 1, scales = "free_y") +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Latest Views vs Video Age"),
      subtitle = bundle_e_subtitle(talent, "Separate video types to compare age-versus-views patterns without one format dominating the scale."),
      x = "Video age at latest observation (days)",
      y = "Latest observed views (square-root adjusted)",
      color = "Content type",
      shape = "Content type"
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "Short" = 17,
        "Video" = 15,
        "Stream" = 16
      )
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Short" = sun_data_brand_colors()[["orange"]],
        "Video" = sun_data_brand_colors()[["blue"]],
        "Stream" = sun_data_brand_colors()[["steel"]]
      )
    ) +
    ggplot2::guides(color = "none", shape = "none") +
    ggplot2::scale_y_continuous(
      trans = "sqrt",
      labels = scales::label_comma()
    )
}

plot_bundle_e_recent_vs_lifetime_rate <- function(video_summary, talent) {
  if (nrow(video_summary) == 0) {
    return(bundle_e_empty_plot("Recent Momentum vs Lifetime Rate", talent))
  }

  plot_df <- video_summary %>%
    dplyr::mutate(
      profile = dplyr::case_when(
        .data$reacceleration_flag ~ "Re-accelerating",
        .data$evergreen_flag ~ "Evergreen",
        .data$front_loaded_flag ~ "Front-loaded",
        TRUE ~ "Other"
      ),
      profile = factor(
        .data$profile,
        levels = c("Evergreen", "Front-loaded", "Re-accelerating", "Other"),
        ordered = TRUE
      ),
      content_type_label = dplyr::case_when(
        .data$`Content Type` == "live" ~ "Stream",
        .data$`Content Type` == "short" ~ "Short",
        .data$`Content Type` == "video" ~ "Video",
        TRUE ~ tools::toTitleCase(as.character(.data$`Content Type`))
      ),
      content_type_label = factor(
        .data$content_type_label,
        levels = c("Short", "Video", "Stream"),
        ordered = TRUE
      ),
      tooltip_text = paste0(
        "Title: ", .data$Title,
        "<br>Lifetime average views/day: ", scales::comma(.data$lifetime_avg_views_per_day, accuracy = 0.1),
        "<br>Recent 30-day average views/day: ", scales::comma(.data$recent_30d_avg_views_per_day, accuracy = 0.1),
        "<br>Latest views: ", scales::comma(.data$latest_views, accuracy = 1),
        "<br>Profile: ", .data$profile,
        "<br>Content type: ", .data$content_type_label
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$lifetime_avg_views_per_day,
      y = .data$recent_30d_avg_views_per_day,
      size = .data$latest_views,
      color = .data$profile,
      shape = .data$content_type_label,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::facet_wrap(~profile, scales = "free") +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Recent Momentum vs Lifetime Rate"),
      subtitle = bundle_e_subtitle(talent, "Split by lifecycle profile so overlap is reduced and each pattern is easier to read."),
      x = "Lifetime average views per day",
      y = "Recent 30-day average views per day",
      size = "Latest views",
      color = "Profile",
      shape = "Content type"
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::scale_shape_manual(
      values = c(
        "Short" = 17,
        "Video" = 15,
        "Stream" = 16
      )
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
}

plot_bundle_e_publish_cohort <- function(cohort_df, talent) {
  if (nrow(cohort_df) == 0) {
    return(bundle_e_empty_plot("Publish Cohort Performance", talent))
  }

  plot_df <- cohort_df %>%
    dplyr::mutate(
      tooltip_text = paste0(
        "Publish cohort: ", format(.data$publish_cohort, "%Y-%m"),
        "<br>Median recent 30-day avg views/day: ",
        scales::comma(.data$median_recent_30d_avg_views_per_day, accuracy = 0.1),
        "<br>Videos in cohort: ", scales::comma(.data$video_count)
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$publish_cohort,
      y = .data$median_recent_30d_avg_views_per_day,
      fill = .data$video_count,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_col(alpha = 0.85, show.legend = FALSE) +
    scale_fill_sun_data_c(variant = "orange") +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Publish Cohort Performance"),
      subtitle = bundle_e_subtitle(talent, "Median recent 30-day pace by publish cohort."),
      x = "Publish cohort",
      y = "Median recent 30-day avg views/day"
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(accuracy = 1))
}

plot_bundle_e_publish_cohort_by_type <- function(cohort_type_df, talent) {
  if (nrow(cohort_type_df) == 0) {
    return(bundle_e_empty_plot("Publish Cohort Performance by Video Type", talent))
  }

  has_label_col <- "content_type_label" %in% names(cohort_type_df)

  plot_df <- cohort_type_df %>%
    dplyr::mutate(
      content_type = tolower(trimws(as.character(.data$content_type))),
      content_type_label = if (has_label_col) {
        as.character(.data$content_type_label)
      } else {
        dplyr::case_when(
          .data$content_type == "short" ~ "Short",
          .data$content_type == "video" ~ "Video",
          .data$content_type == "live" ~ "Stream",
          TRUE ~ tools::toTitleCase(.data$content_type)
        )
      },
      tooltip_text = paste0(
        "Publish cohort: ", format(.data$publish_cohort, "%Y-%m"),
        "<br>Video type: ", .data$content_type_label,
        "<br>Median recent 30-day avg views/day: ",
        scales::comma(.data$median_recent_30d_avg_views_per_day, accuracy = 0.1),
        "<br>Videos in cohort: ", scales::comma(.data$video_count)
      )
    ) %>%
    dplyr::filter(!is.na(.data$content_type), nzchar(.data$content_type))

  preferred_levels <- c("Short", "Video", "Stream")
  other_levels <- sort(setdiff(unique(plot_df$content_type_label), preferred_levels))
  plot_df <- plot_df %>%
    dplyr::mutate(
      content_type_label = factor(
        .data$content_type_label,
        levels = c(preferred_levels[preferred_levels %in% unique(.data$content_type_label)], other_levels),
        ordered = TRUE
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$publish_cohort,
      y = .data$median_recent_30d_avg_views_per_day,
      fill = .data$content_type_label,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_col(alpha = 0.85, show.legend = FALSE) +
    ggplot2::facet_wrap(~content_type_label, nrow = 1) +
    ggplot2::scale_fill_manual(
      values = c(
        "Short" = sun_data_brand_colors()[["orange"]],
        "Video" = sun_data_brand_colors()[["blue"]],
        "Stream" = sun_data_brand_colors()[["steel"]]
      ),
      na.value = sun_data_brand_colors()[["cloud"]]
    ) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Publish Cohort Performance by Video Type"),
      subtitle = bundle_e_subtitle(talent, "The same cohort view, disaggregated so Shorts, Videos, and Streams can be read separately."),
      x = "Publish cohort",
      y = "Median recent 30-day avg views/day"
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(accuracy = 1))
}

plot_bundle_e_type_age_curve_comparison <- function(curve_df, talent, content_type_label = "Live uploads") {
  if (nrow(curve_df) == 0) {
    return(bundle_e_empty_plot(paste(content_type_label, "Age-Normalized Comparison"), talent))
  }

  latest_points <- curve_df %>%
    dplyr::group_by(.data$`Video ID`) %>%
    dplyr::slice_max(order_by = .data$video_age_days, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  plot_df <- curve_df %>%
    dplyr::mutate(
      comparison_role = as.character(.data$comparison_role),
      comparison_role_type = dplyr::case_when(
        grepl("\\+", .data$comparison_role) ~ "Top-performing + newest uploads",
        grepl("newest", .data$comparison_role, ignore.case = TRUE) ~ "Newest uploads",
        TRUE ~ "Top-performing uploads"
      ),
      tooltip_text = paste0(
        "Title: ", .data$Title,
        "<br>Publish date: ", as.character(.data$publish_date),
        "<br>Video age: ", scales::comma(.data$video_age_days, accuracy = 1), " days",
        "<br>Cumulative views: ", scales::comma(.data$views_cumulative, accuracy = 1),
        "<br>Comparison group: ", .data$comparison_role
      )
    )

  role_levels <- plot_df %>%
    dplyr::distinct(.data$comparison_role, .data$comparison_role_type) %>%
    dplyr::arrange(
      match(
        .data$comparison_role_type,
        c("Top-performing uploads", "Top-performing + newest uploads", "Newest uploads")
      ),
      .data$comparison_role
    )

  role_colors <- dplyr::case_when(
    role_levels$comparison_role_type == "Top-performing uploads" ~ sun_data_brand_colors()[["blue"]],
    role_levels$comparison_role_type == "Newest uploads" ~ sun_data_brand_colors()[["orange"]],
    TRUE ~ sun_data_brand_colors()[["steel"]]
  )
  names(role_colors) <- role_levels$comparison_role

  plot_df <- plot_df %>%
    dplyr::mutate(comparison_role = factor(.data$comparison_role, levels = role_levels$comparison_role))

  latest_points <- plot_df %>%
    dplyr::group_by(.data$`Video ID`) %>%
    dplyr::slice_max(order_by = .data$video_age_days, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$video_age_days,
      y = .data$views_cumulative,
      group = .data$`Video ID`,
      color = .data$comparison_role,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.75) +
    ggplot2::geom_point(
      data = latest_points,
      size = 2.2,
      show.legend = FALSE
    ) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text(paste(content_type_label, "Age-Normalized Comparison")),
      subtitle = bundle_e_subtitle(talent, paste0("Compare selected uploads on the same age-normalized frame using days since publish.")),
      x = "Video age (days since publish)",
      y = "Cumulative views",
      color = "Comparison group"
    ) +
    ggplot2::scale_color_manual(values = role_colors) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
}

plot_bundle_e_video_type_longevity <- function(video_type_df, talent) {
  if (nrow(video_type_df) == 0) {
    return(bundle_e_empty_plot("Video Type Longevity", talent))
  }

  plot_df <- video_type_df %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::mutate(group_value = factor(.data$group_value, levels = rev(.data$group_value), ordered = TRUE))

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$group_value,
      y = .data$median_recent_30d_avg_views_per_day,
      fill = .data$group_value
    )
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    scale_fill_sun_data(variant = "brand") +
    ggplot2::guides(fill = "none") +
    ggplot2::coord_flip() +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Video Type Longevity"),
      subtitle = bundle_e_subtitle(talent, "Median recent 30-day pace by live, video, and short uploads."),
      x = "Video type",
      y = "Median recent 30-day avg views/day"
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
}

plot_bundle_e_topic_video_type_longevity <- function(video_summary, talent, top_n = 8, min_videos = 2) {
  if (nrow(video_summary) == 0 || !"topic" %in% names(video_summary)) {
    return(bundle_e_empty_plot("Topic by Video Type Longevity", talent))
  }

  plot_df <- video_summary %>%
    dplyr::mutate(
      topic = trimws(as.character(.data$topic)),
      content_type_label = dplyr::case_when(
        .data$`Content Type` == "live" ~ "Stream",
        .data$`Content Type` == "short" ~ "Short",
        .data$`Content Type` == "video" ~ "Video",
        TRUE ~ tools::toTitleCase(as.character(.data$`Content Type`))
      )
    ) %>%
    dplyr::filter(
      !is.na(.data$topic),
      nzchar(.data$topic),
      !is.na(.data$content_type_label),
      !is.na(.data$recent_30d_avg_views_per_day),
      is.finite(.data$recent_30d_avg_views_per_day)
    ) %>%
    dplyr::group_by(.data$topic, .data$content_type_label) %>%
    dplyr::summarise(
      video_count = dplyr::n_distinct(.data$`Video ID`),
      median_recent_30d_avg_views_per_day = stats::median(.data$recent_30d_avg_views_per_day, na.rm = TRUE),
      median_latest_views = stats::median(.data$latest_views, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$video_count >= min_videos)

  if (nrow(plot_df) == 0) {
    return(bundle_e_empty_plot("Topic by Video Type Longevity", talent, "No topic and video-type groups met the minimum video count."))
  }

  top_topics <- plot_df %>%
    dplyr::group_by(.data$topic) %>%
    dplyr::summarise(
      max_recent_pace = max(.data$median_recent_30d_avg_views_per_day, na.rm = TRUE),
      total_videos = sum(.data$video_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$max_recent_pace), dplyr::desc(.data$total_videos)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data$topic)

  plot_df <- plot_df %>%
    dplyr::filter(.data$topic %in% top_topics) %>%
    dplyr::mutate(
      topic = factor(.data$topic, levels = rev(top_topics), ordered = TRUE),
      content_type_label = factor(.data$content_type_label, levels = c("Short", "Video", "Stream"), ordered = TRUE),
      tooltip_text = paste0(
        "Topic: ", .data$topic,
        "<br>Content type: ", .data$content_type_label,
        "<br>Videos: ", scales::comma(.data$video_count),
        "<br>Median recent 30-day avg views/day: ", scales::comma(.data$median_recent_30d_avg_views_per_day, accuracy = 0.01),
        "<br>Median latest views: ", scales::comma(.data$median_latest_views, accuracy = 1)
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$topic,
      y = .data$median_recent_30d_avg_views_per_day,
      fill = .data$content_type_label,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(width = 0.75, preserve = "single"), alpha = 0.9) +
    ggplot2::coord_flip() +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Topic Longevity by Video Type"),
      subtitle = bundle_e_subtitle(talent, "Median recent 30-day pace by topic, split by Short, Video, and Stream uploads."),
      x = "Topic",
      y = "Median recent 30-day avg views/day",
      fill = "Content type"
    ) +
    ggplot2::scale_fill_manual(values = c(
      Short = sun_data_brand_colors()[["orange"]],
      Video = sun_data_brand_colors()[["blue"]],
      Stream = sun_data_brand_colors()[["steel"]]
    )) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(accuracy = 0.01))
}

plot_bundle_e_attribute_momentum_by_type <- function(
  video_summary,
  talent,
  attribute = c("topic", "tag"),
  top_n = 6,
  min_videos = 3
) {
  attribute <- match.arg(attribute)
  plot_title <- if (attribute == "topic") "Topic Momentum by Video Type" else "Tag Momentum by Video Type"
  attribute_label <- if (attribute == "topic") "Topic" else "Tag"

  if (nrow(video_summary) == 0) {
    return(bundle_e_empty_plot(plot_title, talent))
  }

  base <- video_summary %>%
    dplyr::filter(
      !is.na(.data$lifetime_avg_views_per_day),
      !is.na(.data$recent_30d_avg_views_per_day),
      is.finite(.data$lifetime_avg_views_per_day),
      is.finite(.data$recent_30d_avg_views_per_day)
    ) %>%
    dplyr::mutate(
      content_type_label = dplyr::case_when(
        .data$`Content Type` == "live" ~ "Stream",
        .data$`Content Type` == "short" ~ "Short",
        .data$`Content Type` == "video" ~ "Video",
        TRUE ~ tools::toTitleCase(as.character(.data$`Content Type`))
      ),
      content_type_label = factor(
        .data$content_type_label,
        levels = c("Short", "Video", "Stream"),
        ordered = TRUE
      )
    ) %>%
    dplyr::filter(!is.na(.data$content_type_label))

  if (attribute == "topic") {
    plot_df <- base %>%
      dplyr::mutate(attribute_group = trimws(as.character(.data$topic))) %>%
      dplyr::filter(!is.na(.data$attribute_group), nzchar(.data$attribute_group))
  } else {
    plot_df <- base %>%
      dplyr::mutate(attribute_group = as.character(.data$tags)) %>%
      dplyr::mutate(attribute_group = ifelse(is.na(.data$attribute_group), "", .data$attribute_group)) %>%
      tidyr::separate_rows("attribute_group", sep = ",") %>%
      dplyr::mutate(attribute_group = trimws(tolower(.data$attribute_group))) %>%
      dplyr::filter(nzchar(.data$attribute_group))
  }

  if (nrow(plot_df) == 0) {
    return(bundle_e_empty_plot(plot_title, talent))
  }

  top_groups <- plot_df %>%
    dplyr::group_by(.data$attribute_group) %>%
    dplyr::summarise(
      video_count = dplyr::n_distinct(.data$`Video ID`),
      median_recent_30d_avg_views_per_day = stats::median(.data$recent_30d_avg_views_per_day, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$video_count >= min_videos) %>%
    dplyr::arrange(dplyr::desc(.data$median_recent_30d_avg_views_per_day), dplyr::desc(.data$video_count)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data$attribute_group)

  if (length(top_groups) == 0) {
    return(bundle_e_empty_plot(plot_title, talent, paste0("No ", tolower(attribute_label), " groups met the minimum video count.")))
  }

  plot_df <- plot_df %>%
    dplyr::filter(.data$attribute_group %in% top_groups) %>%
    dplyr::mutate(
      attribute_group = factor(.data$attribute_group, levels = top_groups, ordered = TRUE),
      tooltip_text = paste0(
        "Title: ", .data$Title,
        "<br>", attribute_label, ": ", .data$attribute_group,
        "<br>Content type: ", .data$content_type_label,
        "<br>Lifetime avg views/day: ", scales::comma(.data$lifetime_avg_views_per_day, accuracy = 0.1),
        "<br>Recent 30-day avg views/day: ", scales::comma(.data$recent_30d_avg_views_per_day, accuracy = 0.1),
        "<br>Latest views: ", scales::comma(.data$latest_views, accuracy = 1),
        "<br>Observed window: ", as.character(.data$window_first_observed_date), " to ", as.character(.data$window_last_observed_date),
        "<br>First observed views: ", scales::comma(.data$window_first_observed_views, accuracy = 1),
        "<br>Observed view gain: ", scales::comma(.data$observed_view_gain, accuracy = 1),
        "<br>Observed span: ", scales::comma(.data$observed_span_days, accuracy = 1), " days"
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$lifetime_avg_views_per_day,
      y = .data$recent_30d_avg_views_per_day,
      color = .data$content_type_label,
      shape = .data$content_type_label,
      text = .data$tooltip_text
    )
  ) +
    ggplot2::geom_point(alpha = 0.78, size = 2.5) +
    ggplot2::facet_wrap(~attribute_group, scales = "free", ncol = 2) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text(plot_title),
      subtitle = bundle_e_subtitle(talent, paste0("Each facet is a high-performing ", tolower(attribute_label), "; hover for the observed view window and view gain.")),
      x = "Lifetime average views/day",
      y = "Recent 30-day average views/day",
      color = "Content type",
      shape = "Content type"
    ) +
    ggplot2::scale_shape_manual(values = c(Short = 17, Video = 15, Stream = 16)) +
    ggplot2::scale_color_manual(values = c(
      Short = sun_data_brand_colors()[["orange"]],
      Video = sun_data_brand_colors()[["blue"]],
      Stream = sun_data_brand_colors()[["steel"]]
    )) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
}

build_bundle_e_plot_set <- function(
  library_growth_snapshot,
  back_catalog_contribution,
  video_summary,
  publish_cohort_performance,
  publish_cohort_performance_by_type,
  video_type_longevity,
  short_window_diagnostics = tibble::tibble(),
  short_window_video_scores = tibble::tibble(),
  talent
) {
  list(
    library_growth_snapshot = plot_bundle_e_library_growth(library_growth_snapshot, talent),
    back_catalog_vs_recent_contribution = plot_bundle_e_back_catalog_share(back_catalog_contribution, talent),
    latest_views_vs_video_age = plot_bundle_e_latest_views_vs_age(video_summary, talent),
    recent_momentum_vs_lifetime_rate = plot_bundle_e_recent_vs_lifetime_rate(video_summary, talent),
    publish_cohort_performance = plot_bundle_e_publish_cohort(publish_cohort_performance, talent),
    publish_cohort_performance_by_type = plot_bundle_e_publish_cohort_by_type(publish_cohort_performance_by_type, talent),
    video_type_longevity = plot_bundle_e_video_type_longevity(video_type_longevity, talent),
    short_stability_window = plot_bundle_e_short_window_diagnostics(short_window_diagnostics, talent),
    top_shorts_within_window = plot_bundle_e_short_window_leaders(short_window_video_scores, talent)
  )
}
