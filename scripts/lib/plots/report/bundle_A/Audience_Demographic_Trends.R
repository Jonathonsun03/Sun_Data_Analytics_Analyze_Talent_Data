audience_age_gender_trends_prep <- function(
  demo_df,
  date_col = NULL,
  age_col = "Viewer Age",
  gender_col = "Viewer Gender",
  share_col = "Viewer Percentage",
  video_id_col = "Video ID",
  freq = c("month", "week"),
  include_no_data = FALSE
) {
  freq <- match.arg(freq)

  if (is.null(date_col)) {
    date_col <- bundle_a_pick_col(
      demo_df,
      c("publish_date", "Published At", "date"),
      label = "demographics date column"
    )
  }
  if (!age_col %in% names(demo_df)) {
    stop("Missing age column: ", age_col)
  }
  if (!gender_col %in% names(demo_df)) {
    stop("Missing gender column: ", gender_col)
  }
  if (!share_col %in% names(demo_df)) {
    stop("Missing share column: ", share_col)
  }
  if (!video_id_col %in% names(demo_df)) {
    video_id_col <- NULL
  }

  unit <- if (freq == "month") "month" else "week"

  out <- demo_df %>%
    dplyr::mutate(
      .date = bundle_a_as_date(.data[[date_col]]),
      .period = lubridate::floor_date(.data$.date, unit = unit),
      .age = as.character(.data[[age_col]]),
      .gender = as.character(.data[[gender_col]]),
      .share = bundle_a_percent_to_prop(.data[[share_col]])
    ) %>%
    dplyr::filter(!is.na(.data$.period), !is.na(.data$.age), !is.na(.data$.gender), !is.na(.data$.share))

  if (!isTRUE(include_no_data)) {
    out <- out %>%
      dplyr::filter(
        !grepl("^\\(no data\\)$", trimws(.data$.age), ignore.case = TRUE),
        !grepl("^\\(no data\\)$", trimws(.data$.gender), ignore.case = TRUE)
      )
  }

  if (!is.null(video_id_col)) {
    out %>%
      dplyr::group_by(.data$.period, .data$.gender, .data$.age) %>%
      dplyr::summarize(
        share = mean(.data$.share, na.rm = TRUE),
        videos = dplyr::n_distinct(.data[[video_id_col]]),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$.period, .data$.gender, .data$.age)
  } else {
    out %>%
      dplyr::group_by(.data$.period, .data$.gender, .data$.age) %>%
      dplyr::summarize(
        share = mean(.data$.share, na.rm = TRUE),
        videos = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$.period, .data$.gender, .data$.age)
  }
}

audience_age_gender_trends_plot <- function(
  plot_df,
  talent,
  smooth = FALSE,
  show_points = TRUE
) {
  if (!all(c(".period", ".gender", ".age", "share") %in% names(plot_df))) {
    stop("plot_df must contain: .period, .gender, .age, share")
  }

  p <- plot_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$.period,
        y = .data$share,
        color = .data$.age,
        group = .data$.age
      )
    ) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.8) +
    ggplot2::facet_wrap(~.gender, ncol = 1, scales = "free_y") +
    scale_color_sun_data(variant = "brand") +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Audience Age Mix Over Time by Gender"),
      subtitle = bundle_a_date_range_subtitle(plot_df$.period),
      x = "Period",
      y = "Average viewer share",
      color = "Viewer Age"
    )

  if (isTRUE(show_points)) {
    p <- p + ggplot2::geom_point(size = 1.1, alpha = 0.65)
  }
  if (isTRUE(smooth)) {
    p <- p + ggplot2::geom_smooth(se = FALSE, linewidth = 0.9, alpha = 0.4)
  }

  p
}

audience_age_gender_trends <- function(
  demo_df,
  talent,
  date_col = NULL,
  age_col = "Viewer Age",
  gender_col = "Viewer Gender",
  share_col = "Viewer Percentage",
  video_id_col = "Video ID",
  freq = c("month", "week"),
  include_no_data = FALSE,
  smooth = FALSE,
  show_points = TRUE
) {
  plot_df <- audience_age_gender_trends_prep(
    demo_df = demo_df,
    date_col = date_col,
    age_col = age_col,
    gender_col = gender_col,
    share_col = share_col,
    video_id_col = video_id_col,
    freq = freq,
    include_no_data = include_no_data
  )
  audience_age_gender_trends_plot(
    plot_df = plot_df,
    talent = talent,
    smooth = smooth,
    show_points = show_points
  )
}

audience_age_gender_trends_with_data <- function(
  demo_df,
  talent,
  date_col = NULL,
  age_col = "Viewer Age",
  gender_col = "Viewer Gender",
  share_col = "Viewer Percentage",
  video_id_col = "Video ID",
  freq = c("month", "week"),
  include_no_data = FALSE,
  smooth = FALSE,
  show_points = TRUE
) {
  plot_df <- audience_age_gender_trends_prep(
    demo_df = demo_df,
    date_col = date_col,
    age_col = age_col,
    gender_col = gender_col,
    share_col = share_col,
    video_id_col = video_id_col,
    freq = freq,
    include_no_data = include_no_data
  )
  list(
    data = plot_df,
    plot = audience_age_gender_trends_plot(
      plot_df = plot_df,
      talent = talent,
      smooth = smooth,
      show_points = show_points
    )
  )
}

audience_core_segment_stability_prep <- function(
  demo_df,
  date_col = NULL,
  age_col = "Viewer Age",
  gender_col = "Viewer Gender",
  share_col = "Viewer Percentage",
  video_id_col = "Video ID",
  freq = c("month", "week"),
  include_no_data = FALSE
) {
  base <- audience_age_gender_trends_prep(
    demo_df = demo_df,
    date_col = date_col,
    age_col = age_col,
    gender_col = gender_col,
    share_col = share_col,
    video_id_col = video_id_col,
    freq = freq,
    include_no_data = include_no_data
  ) %>%
    dplyr::mutate(segment = paste(.data$.gender, .data$.age, sep = " | "))

  top_segment <- base %>%
    dplyr::group_by(.data$segment) %>%
    dplyr::summarize(avg_share = mean(.data$share, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$avg_share)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(.data$segment)

  if (length(top_segment) == 0 || is.na(top_segment[[1]])) {
    stop("Unable to determine top audience segment.")
  }

  base %>%
    dplyr::group_by(.data$.period) %>%
    dplyr::summarize(
      top_segment = top_segment[[1]],
      top_share = .data$share[match(top_segment[[1]], .data$segment)],
      hhi = sum(.data$share^2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$.period)
}

audience_core_segment_stability_plot <- function(stability_df, talent, show_hhi = FALSE) {
  if (!all(c(".period", "top_segment", "top_share", "hhi") %in% names(stability_df))) {
    stop("stability_df must contain: .period, top_segment, top_share, hhi")
  }

  top_seg <- unique(stability_df$top_segment)[1]
  max_hhi <- suppressWarnings(max(stability_df$hhi, na.rm = TRUE))
  hhi_scaled <- if (is.finite(max_hhi) && max_hhi > 0) {
    stability_df$hhi / max_hhi
  } else {
    rep(NA_real_, nrow(stability_df))
  }

  plot_df <- stability_df %>%
    dplyr::transmute(
      .period = .data$.period,
      value = .data$top_share,
      series = paste0("Core audience share (", top_seg, ")")
    ) %>%
    dplyr::bind_rows(
      stability_df %>%
        dplyr::transmute(
          .period = .data$.period,
          value = pmax(0, 1 - .data$top_share),
          series = "Potential audience share (all other segments)"
        )
    )

  if (isTRUE(show_hhi)) {
    plot_df <- plot_df %>%
      dplyr::bind_rows(
        stability_df %>%
          dplyr::transmute(
            .period = .data$.period,
            value = hhi_scaled,
            series = "Audience concentration index (HHI, scaled)"
          )
      )
  }

  brand <- sun_data_brand_colors()
  color_map <- c(
    setNames(brand[["blue"]], paste0("Core audience share (", top_seg, ")")),
    "Potential audience share (all other segments)" = brand[["orange"]],
    "Audience concentration index (HHI, scaled)" = brand[["midnight"]]
  )

  plot_df <- plot_df %>%
    dplyr::arrange(.data$.period) %>%
    dplyr::mutate(
      series = factor(
        .data$series,
        levels = c(
          paste0("Core audience share (", top_seg, ")"),
          "Potential audience share (all other segments)",
          "Audience concentration index (HHI, scaled)"
        )
      ),
      text = paste0(
        "Period: ", format(.data$.period, "%b %Y"),
        "<br>Series: ", .data$series,
        "<br>Value: ", scales::percent(.data$value, accuracy = 0.1)
      )
    )

  plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.period, y = .data$value, color = .data$series, group = .data$series, text = .data$text)) +
    ggplot2::geom_line(linewidth = 1.0) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1)
    ) +
    ggplot2::scale_color_manual(values = color_map) +
    theme_nyt() +
    ggplot2::labs(
      title = paste0(talent, " - Core vs Potential Audience Stability"),
      subtitle = bundle_a_date_range_subtitle(stability_df$.period),
      x = "Period",
      y = "Audience share",
      color = NULL,
      caption = "Potential audience share = 100% - core audience share."
    )
}
