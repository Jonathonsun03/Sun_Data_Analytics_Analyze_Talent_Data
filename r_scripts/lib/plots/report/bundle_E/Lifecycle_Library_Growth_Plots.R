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
      library_age_bucket = factor(
        .data$library_age_bucket,
        levels = c("0_7_days", "8_30_days", "31_60_days", "61_90_days", "91_plus_days"),
        ordered = TRUE
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$snapshot_date, y = .data$view_share, fill = .data$library_age_bucket)) +
    ggplot2::geom_area(alpha = 0.9) +
    scale_fill_sun_data(variant = "brand") +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Back Catalog vs Recent Contribution"),
      subtitle = bundle_e_subtitle(talent, "Share of total library views by video age bucket."),
      x = "Snapshot date",
      y = "Share of library views",
      fill = "Video age bucket"
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))
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

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$latest_age_days,
      y = .data$latest_views,
      color = .data$`Content Type`
    )
  ) +
    ggplot2::geom_point(alpha = 0.75, size = 2.4) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Latest Views vs Video Age"),
      subtitle = bundle_e_subtitle(talent, "Older videos should not automatically dominate if the library is broadly durable."),
      x = "Video age at latest observation (days)",
      y = "Latest observed views",
      color = "Content type"
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
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
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$lifetime_avg_views_per_day,
      y = .data$recent_30d_avg_views_per_day,
      size = .data$latest_views,
      color = .data$profile
    )
  ) +
    ggplot2::geom_point(alpha = 0.7) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Recent Momentum vs Lifetime Rate"),
      subtitle = bundle_e_subtitle(talent, "Compares long-run average pace with most recent 30-day pace."),
      x = "Lifetime average views per day",
      y = "Recent 30-day average views per day",
      size = "Latest views",
      color = "Profile"
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
}

plot_bundle_e_publish_cohort <- function(cohort_df, talent) {
  if (nrow(cohort_df) == 0) {
    return(bundle_e_empty_plot("Publish Cohort Performance", talent))
  }

  ggplot2::ggplot(
    cohort_df,
    ggplot2::aes(
      x = .data$publish_cohort,
      y = .data$median_recent_30d_avg_views_per_day
    )
  ) +
    ggplot2::geom_col(fill = sun_data_brand_colors()[["orange"]], alpha = 0.8) +
    theme_nyt() +
    ggplot2::labs(
      title = bundle_e_title_text("Publish Cohort Performance"),
      subtitle = bundle_e_subtitle(talent, "Median recent 30-day pace by publish cohort."),
      x = "Publish cohort",
      y = "Median recent 30-day avg views/day"
    ) +
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

build_bundle_e_plot_set <- function(
  library_growth_snapshot,
  back_catalog_contribution,
  video_summary,
  publish_cohort_performance,
  video_type_longevity,
  talent
) {
  list(
    library_growth_snapshot = plot_bundle_e_library_growth(library_growth_snapshot, talent),
    back_catalog_vs_recent_contribution = plot_bundle_e_back_catalog_share(back_catalog_contribution, talent),
    latest_views_vs_video_age = plot_bundle_e_latest_views_vs_age(video_summary, talent),
    recent_momentum_vs_lifetime_rate = plot_bundle_e_recent_vs_lifetime_rate(video_summary, talent),
    publish_cohort_performance = plot_bundle_e_publish_cohort(publish_cohort_performance, talent),
    video_type_longevity = plot_bundle_e_video_type_longevity(video_type_longevity, talent)
  )
}
