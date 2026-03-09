load_title_classifications <- function(
  path = file.path(
    "classification", "output", "title_classifications",
    "classification_export_gpt-5-mini_from_duckdb.csv"
  ),
  talent = NULL,
  latest_per_video = TRUE
) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package `readr` is required.")
  }

  titles <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

  required <- c("video_id", "talent_name")
  missing <- setdiff(required, names(titles))
  if (length(missing) > 0) {
    stop("Missing required title-classification columns: ", paste(missing, collapse = ", "))
  }

  if (!is.null(talent) && length(talent) > 0) {
    pattern <- paste(as.character(talent), collapse = "|")
    titles <- dplyr::filter(
      titles,
      grepl(pattern, .data$talent_name, ignore.case = TRUE)
    )
  }

  if (isTRUE(latest_per_video)) {
    if ("created_at" %in% names(titles)) {
      has_conf <- "confidence" %in% names(titles)
      titles <- titles %>%
        dplyr::mutate(
          .created_at = suppressWarnings(as.POSIXct(.data$created_at, tz = "UTC"))
        ) %>%
        dplyr::arrange(
          dplyr::desc(.data$.created_at),
          if (has_conf) dplyr::desc(.data$confidence) else dplyr::desc(.data$video_id)
        ) %>%
        dplyr::distinct(.data$video_id, .keep_all = TRUE) %>%
        dplyr::select(-dplyr::any_of(".created_at"))
    } else {
      titles <- dplyr::distinct(titles, .data$video_id, .keep_all = TRUE)
    }
  }

  logical_cols <- names(titles)[vapply(titles, is.logical, logical(1))]
  base_cols <- c(
    "video_id",
    "talent_name",
    "confidence",
    "title_raw",
    "content_type",
    "published_at",
    "topic",
    "tags",
    "primary_reference"
  )

  keep <- unique(c(intersect(base_cols, names(titles)), logical_cols))

  titles %>%
    dplyr::select(dplyr::all_of(keep)) %>%
    dplyr::rename(`Video ID` = video_id)
}

attach_title_classifications <- function(df, titles, by = "Video ID") {
  if (is.character(titles) && length(titles) == 1L) {
    titles <- load_title_classifications(path = titles)
  }

  if (!is.data.frame(titles)) {
    stop("`titles` must be a data frame or a CSV path.")
  }

  if (!(by %in% names(df))) {
    stop("Join key missing in `df`: ", by)
  }
  if (!(by %in% names(titles)) && by == "Video ID" && "video_id" %in% names(titles)) {
    titles <- dplyr::rename(titles, `Video ID` = video_id)
  }
  if (!(by %in% names(titles))) {
    stop("Join key missing in `titles`: ", by)
  }

  common <- intersect(names(df), names(titles))
  to_drop_from_df <- setdiff(common, by)

  df %>%
    dplyr::select(-dplyr::any_of(to_drop_from_df)) %>%
    dplyr::left_join(titles, by = by)
}

dedupe_latest_rows <- function(
  df,
  key_cols = "Video ID",
  sort_cols = c("confidence", "published_at", "created_at")
) {
  missing_keys <- setdiff(key_cols, names(df))
  if (length(missing_keys) > 0) {
    stop("Missing key column(s): ", paste(missing_keys, collapse = ", "))
  }

  existing_sort <- sort_cols[sort_cols %in% names(df)]

  out <- df
  if (length(existing_sort) > 0) {
    out <- out %>%
      dplyr::arrange(
        dplyr::across(dplyr::all_of(existing_sort), ~ dplyr::desc(.x))
      )
  }

  out %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()
}

video_analytics_prep_with_titles <- function(
  files,
  titles,
  talent = NULL,
  talent_index = NULL,
  by = "Video ID"
) {
  base <- video_analytics_prep(
    files = files,
    talent = talent,
    talent_index = talent_index
  )
  attach_title_classifications(base, titles = titles, by = by)
}

video_monetary_prep_with_titles <- function(
  files,
  titles,
  talent = NULL,
  talent_index = NULL,
  by = "Video ID"
) {
  base <- video_monetary_prep(
    files = files,
    talent = talent,
    talent_index = talent_index
  )
  attach_title_classifications(base, titles = titles, by = by)
}

video_demographic_prep_with_titles <- function(
  files,
  titles,
  talent = NULL,
  talent_index = NULL,
  by = "Video ID"
) {
  base <- video_demographic_prep(
    files = files,
    talent = talent,
    talent_index = talent_index
  )
  attach_title_classifications(base, titles = titles, by = by)
}

video_geographic_prep_with_titles <- function(
  files,
  titles,
  talent = NULL,
  talent_index = NULL,
  by = "Video ID"
) {
  base <- video_geographic_prep(
    files = files,
    talent = talent,
    talent_index = talent_index
  )
  attach_title_classifications(base, titles = titles, by = by)
}

video_preps_with_titles <- function(
  files,
  titles,
  talent = NULL,
  talent_index = NULL,
  by = "Video ID",
  dedupe = FALSE,
  key_cols = "Video ID",
  sort_cols = c("confidence", "published_at"),
  dedupe_sets = c("analytics", "monetary", "demo", "geo")
) {
  out <- list(
    analytics = video_analytics_prep_with_titles(
      files = files,
      titles = titles,
      talent = talent,
      talent_index = talent_index,
      by = by
    ),
    monetary = video_monetary_prep_with_titles(
      files = files,
      titles = titles,
      talent = talent,
      talent_index = talent_index,
      by = by
    ),
    demo = video_demographic_prep_with_titles(
      files = files,
      titles = titles,
      talent = talent,
      talent_index = talent_index,
      by = by
    ),
    geo = video_geographic_prep_with_titles(
      files = files,
      titles = titles,
      talent = talent,
      talent_index = talent_index,
      by = by
    )
  )

  if (isTRUE(dedupe)) {
    target_sets <- intersect(names(out), dedupe_sets)
    for (nm in target_sets) {
      out[[nm]] <- dedupe_latest_rows(
        out[[nm]],
        key_cols = key_cols,
        sort_cols = sort_cols
      )
    }
  }

  out
}
