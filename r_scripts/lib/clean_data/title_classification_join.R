default_title_classifications_export_dir <- function() {
  override <- trimws(Sys.getenv("TITLE_CLASSIFICATIONS_DIR", unset = ""))
  if (nzchar(override)) {
    return(override)
  }

  if (!exists("get_datalake_root", mode = "function")) {
    repo_root <- Sys.getenv("TALENT_REPO_ROOT", unset = "")
    datalake_root_path <- if (nzchar(repo_root)) {
      file.path(repo_root, "r_scripts", "lib", "utils", "datalake_root.r")
    } else {
      file.path("r_scripts", "lib", "utils", "datalake_root.r")
    }
    if (file.exists(datalake_root_path)) {
      source(datalake_root_path)
    }
  }

  if (exists("get_datalake_root", mode = "function")) {
    datalake_root <- normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)
    return(file.path(dirname(datalake_root), "Processed", "Title_classification"))
  }

  file.path("classification", "output", "title_classifications")
}

current_title_classifications_path <- function(
  export_dir = default_title_classifications_export_dir()
) {
  file.path(export_dir, "current", "classification_export_current.csv")
}

resolve_latest_title_classifications_path <- function(
  export_dir = default_title_classifications_export_dir()
) {
  current_path <- current_title_classifications_path(export_dir)
  if (file.exists(current_path)) {
    return(current_path)
  }

  archive_dir <- file.path(export_dir, "archived")
  if (dir.exists(archive_dir)) {
    export_dir <- archive_dir
  }

  if (!dir.exists(export_dir)) {
    stop("Title-classification export directory does not exist: ", export_dir)
  }

  candidates <- list.files(
    export_dir,
    pattern = "^classification_export_.+\\.csv$",
    full.names = TRUE
  )
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop("No title-classification export CSVs found in: ", export_dir)
  }

  info <- file.info(candidates)
  candidates <- candidates[order(info$mtime, basename(candidates), decreasing = TRUE)]
  candidates[[1]]
}

load_title_classifications <- function(
  path = NULL,
  talent = NULL,
  latest_per_video = TRUE
) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package `readr` is required.")
  }

  override <- trimws(Sys.getenv("TITLE_CLASSIFICATIONS_PATH", unset = ""))
  if (!nzchar(override)) {
    bundle_overrides <- c(
      trimws(Sys.getenv("BUNDLE_A_TITLE_CLASSIFICATIONS_PATH", unset = "")),
      trimws(Sys.getenv("BUNDLE_B_TITLE_CLASSIFICATIONS_PATH", unset = "")),
      trimws(Sys.getenv("BUNDLE_E_TITLE_CLASSIFICATIONS_PATH", unset = ""))
    )
    bundle_overrides <- bundle_overrides[nzchar(bundle_overrides)]
    if (length(bundle_overrides) > 0) {
      override <- bundle_overrides[[1]]
    }
  }
  if (nzchar(override)) {
    path <- override
  }
  if (is.null(path) || !nzchar(trimws(path))) {
    path <- resolve_latest_title_classifications_path()
  }

  titles <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

  required <- c("video_id", "talent_name")
  missing <- setdiff(required, names(titles))
  if (length(missing) > 0) {
    stop("Missing required title-classification columns: ", paste(missing, collapse = ", "))
  }

  if (!is.null(talent) && length(talent) > 0) {
    normalize_talent_key <- function(x) {
      x <- enc2utf8(as.character(x))
      x <- tolower(x)
      gsub("[^a-z0-9]+", "", x)
    }

    talent_keys <- unique(normalize_talent_key(talent))
    titles <- titles %>%
      dplyr::mutate(.talent_key = normalize_talent_key(.data$talent_name))

    exact_match <- dplyr::filter(titles, .data$.talent_key %in% talent_keys)
    if (nrow(exact_match) > 0) {
      titles <- exact_match
    } else if (length(talent_keys) == 1 && nzchar(talent_keys[[1]])) {
      q <- talent_keys[[1]]
      talent_key_values <- titles$.talent_key
      fuzzy_keep <- grepl(q, talent_key_values, fixed = TRUE) |
        vapply(
          talent_key_values,
          function(k) nzchar(k) && grepl(k, q, fixed = TRUE),
          logical(1)
        )
      fuzzy_match <- titles[fuzzy_keep %in% TRUE, , drop = FALSE]
      if (nrow(fuzzy_match) > 0) {
        titles <- fuzzy_match
      } else {
        warning(
          "No title classifications matched talent filter `", as.character(talent[[1]]),
          "`. Falling back to all titles and joining by Video ID."
        )
      }
    } else {
      warning(
        "No title classifications matched talent filter. ",
        "Falling back to all titles and joining by Video ID."
      )
    }

    titles <- dplyr::select(titles, -dplyr::any_of(".talent_key"))
  }

  if (isTRUE(latest_per_video)) {
    if ("created_at" %in% names(titles)) {
      has_conf <- "confidence" %in% names(titles)
      parse_title_created_at <- function(x) {
        x <- trimws(as.character(x))
        x[!nzchar(x)] <- NA_character_
        out <- rep(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"), length(x))
        formats <- c(
          "%Y-%m-%d %H:%M:%OS",
          "%Y-%m-%dT%H:%M:%OS",
          "%Y-%m-%dT%H:%M:%OSZ",
          "%Y-%m-%d",
          "%m/%d/%Y %H:%M:%OS",
          "%m/%d/%Y"
        )

        for (fmt in formats) {
          missing <- is.na(out) & !is.na(x)
          if (!any(missing)) {
            break
          }
          parsed <- suppressWarnings(as.POSIXct(x[missing], format = fmt, tz = "UTC"))
          out[missing] <- parsed
        }

        out
      }
      titles <- titles %>%
        dplyr::mutate(
          .created_at = parse_title_created_at(.data$created_at)
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

  # Prefer most recent snapshot row when available, then user-provided sort keys.
  snapshot_cols <- c("date")
  existing_sort <- unique(c(snapshot_cols[snapshot_cols %in% names(df)], sort_cols[sort_cols %in% names(df)]))

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
  dedupe_sets = c("analytics", "monetary", "demo", "geo"),
  standardize_content_type = TRUE,
  content_type_keep_diagnostics = TRUE,
  content_type_live_min_seconds = 20 * 60,
  content_type_short_max_seconds = 70
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

  if (isTRUE(standardize_content_type)) {
    if (!exists("apply_content_type_filter", mode = "function")) {
      warning(
        "Content-type utility `apply_content_type_filter()` is not loaded; ",
        "skipping standardized content-type filtering."
      )
    } else {
      for (nm in names(out)) {
        out[[nm]] <- apply_content_type_filter(
          out[[nm]],
          output_col = "Content Type",
          keep_diagnostics = content_type_keep_diagnostics,
          live_min_seconds = content_type_live_min_seconds,
          short_max_seconds = content_type_short_max_seconds
        )
      }
    }
  }

  out
}
