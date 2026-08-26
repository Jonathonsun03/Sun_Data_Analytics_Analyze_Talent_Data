# Company membership and company-level analytics loading helpers.

dashboard_company_memberships <- function(
  talent_catalog,
  mapping_path = here::here("config", "dashboard", "company_talents.csv")
) {
  if (!file.exists(mapping_path)) {
    stop("Company talent mapping not found: ", mapping_path, call. = FALSE)
  }

  memberships <- readr::read_csv(
    mapping_path,
    show_col_types = FALSE,
    progress = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  required <- c("company_code", "company_name", "talent_code")
  missing <- setdiff(required, names(memberships))
  if (length(missing) > 0) {
    stop(
      "Company talent mapping is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  memberships <- memberships %>%
    dplyr::transmute(
      company_code = trimws(.data$company_code),
      company_name = trimws(.data$company_name),
      talent_code = trimws(.data$talent_code)
    )
  if (any(!nzchar(memberships$company_code)) ||
      any(!nzchar(memberships$company_name)) ||
      any(!nzchar(memberships$talent_code))) {
    stop("Company talent mapping values cannot be blank.", call. = FALSE)
  }
  if (anyDuplicated(memberships[c("company_code", "talent_code")])) {
    stop("Company/talent pairs must be unique in the mapping.", call. = FALSE)
  }

  unknown_codes <- setdiff(memberships$talent_code, talent_catalog$talent_code)
  if (length(unknown_codes) > 0) {
    stop(
      "Company talent mapping contains unknown active talent codes: ",
      paste(unknown_codes, collapse = ", "),
      call. = FALSE
    )
  }

  memberships %>%
    dplyr::left_join(talent_catalog, by = "talent_code") %>%
    dplyr::arrange(.data$company_name, .data$talent_name)
}

dashboard_company_parse_classifications <- function(classifications) {
  if (nrow(classifications) == 0) {
    return(
      classifications %>%
        dplyr::select(-dplyr::any_of("classification_json")) %>%
        dplyr::rename(`Video ID` = "video_id")
    )
  }

  details <- lapply(classifications$classification_json, function(value) {
    if (is.na(value) || !nzchar(trimws(value))) {
      return(list())
    }
    tryCatch(
      jsonlite::fromJSON(value, simplifyVector = TRUE),
      error = function(e) list()
    )
  })
  scalar_value <- function(value, field) {
    item <- value[[field]]
    if (is.null(item) || length(item) == 0) {
      NA_character_
    } else {
      as.character(item[[1]])
    }
  }
  classifications$topic <- vapply(details, scalar_value, character(1), field = "topic")
  classifications$tags <- vapply(
    details,
    function(value) {
      tags <- value$tags
      if (is.null(tags) || length(tags) == 0) {
        NA_character_
      } else {
        paste(tags, collapse = ", ")
      }
    },
    character(1)
  )
  classifications$primary_reference <- vapply(
    details,
    scalar_value,
    character(1),
    field = "primary_reference"
  )

  classifications %>%
    dplyr::select(-dplyr::any_of("classification_json")) %>%
    dplyr::rename(`Video ID` = "video_id")
}

dashboard_load_company_analytics <- function(
  database_path,
  talent_codes,
  start_date = NULL,
  end_date = NULL
) {
  talent_codes <- unique(trimws(as.character(talent_codes)))
  talent_codes <- talent_codes[nzchar(talent_codes)]
  if (length(talent_codes) == 0) {
    stop("Select at least one talent.", call. = FALSE)
  }
  if (any(!grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,79}$", talent_codes))) {
    stop("Talent codes contain an invalid value.", call. = FALSE)
  }

  start_date <- dashboard_parse_optional_date(start_date, "start_date")
  end_date <- dashboard_parse_optional_date(end_date, "end_date")
  if (!is.na(start_date) && !is.na(end_date) && start_date > end_date) {
    stop("`start_date` cannot be after `end_date`.", call. = FALSE)
  }

  database_path <- dashboard_resolve_database_path(database_path)
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = database_path,
    read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  placeholders <- paste(rep("?", length(talent_codes)), collapse = ", ")
  requested_talents <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT talent_code, talent_name",
      " FROM catalog.talents",
      " WHERE active AND talent_code IN (", placeholders, ")",
      " ORDER BY talent_name"
    ),
    params = as.list(talent_codes)
  )
  unknown_codes <- setdiff(talent_codes, requested_talents$talent_code)
  if (length(unknown_codes) > 0) {
    stop(
      "Unknown or inactive talent codes: ",
      paste(unknown_codes, collapse = ", "),
      call. = FALSE
    )
  }

  date_conditions <- character()
  analytics_params <- as.list(talent_codes)
  if (!is.na(start_date)) {
    date_conditions <- c(date_conditions, "a.snapshot_date >= ?")
    analytics_params <- c(analytics_params, list(start_date))
  }
  if (!is.na(end_date)) {
    date_conditions <- c(date_conditions, "a.snapshot_date <= ?")
    analytics_params <- c(analytics_params, list(end_date))
  }
  date_sql <- if (length(date_conditions) == 0) {
    ""
  } else {
    paste0(" AND ", paste(date_conditions, collapse = " AND "))
  }

  analytics_sql <- paste0(
    "WITH filtered AS (",
    " SELECT a.* FROM clean.video_analytics_snapshots AS a",
    " WHERE a.talent_code IN (", placeholders, ")", date_sql,
    "), latest_by_talent AS (",
    " SELECT talent_code, MAX(snapshot_date) AS snapshot_date",
    " FROM filtered GROUP BY talent_code",
    ") SELECT",
    " a.video_id AS \"Video ID\",",
    " a.channel_id AS \"Channel ID\",",
    " c.channel_name AS \"Channel Name\",",
    " v.title AS \"Title\",",
    " v.published_at AS \"Published At\",",
    " a.snapshot_date AS date,",
    " v.content_type AS \"Content Type\",",
    " a.views,",
    " a.estimated_minutes_watched AS estimatedMinutesWatched,",
    " a.average_view_duration AS averageViewDuration,",
    " a.average_view_percentage AS averageViewPercentage,",
    " a.subscribers_gained AS subscribersGained,",
    " a.subscribers_lost AS subscribersLost,",
    " v.duration_seconds AS DurationSeconds,",
    " v.duration_iso AS DurationISO,",
    " a.talent_code,",
    " t.talent_name AS catalog_talent_name",
    " FROM filtered AS a",
    " JOIN latest_by_talent AS latest",
    "   ON latest.talent_code = a.talent_code",
    "  AND latest.snapshot_date = a.snapshot_date",
    " JOIN catalog.videos AS v",
    "   ON v.video_id = a.video_id",
    "  AND v.channel_id = a.channel_id",
    "  AND v.talent_code = a.talent_code",
    " JOIN catalog.channels AS c",
    "   ON c.channel_id = a.channel_id",
    "  AND c.talent_code = a.talent_code",
    " JOIN catalog.talents AS t ON t.talent_code = a.talent_code",
    " ORDER BY a.talent_code, a.video_id"
  )
  analytics_raw <- DBI::dbGetQuery(con, analytics_sql, params = analytics_params)
  if (nrow(analytics_raw) == 0) {
    stop("No analytics snapshots are available for the selected filters.", call. = FALSE)
  }

  classification_sql <- paste0(
    "WITH ranked AS (",
    " SELECT r.*, ROW_NUMBER() OVER (",
    "   PARTITION BY r.talent_code, r.video_id",
    "   ORDER BY r.created_at DESC NULLS LAST, r.confidence DESC NULLS LAST",
    " ) AS row_number",
    " FROM classification.title_classification_results AS r",
    " WHERE r.talent_code IN (", placeholders, ")",
    ") SELECT",
    " r.video_id, t.talent_name, r.confidence,",
    " v.title AS title_raw, v.content_type, v.published_at,",
    " r.classification_json, r.collaborative_energy",
    " FROM ranked AS r",
    " JOIN catalog.videos AS v USING (video_id, talent_code, channel_id)",
    " JOIN catalog.talents AS t USING (talent_code)",
    " WHERE r.row_number = 1",
    " ORDER BY r.talent_code, r.video_id"
  )
  classifications <- DBI::dbGetQuery(
    con,
    classification_sql,
    params = as.list(talent_codes)
  ) %>%
    dashboard_company_parse_classifications()

  video_identity <- analytics_raw %>%
    dplyr::distinct(
      .data$`Video ID`,
      .data$talent_code,
      .data$catalog_talent_name
    )
  analytics_input <- analytics_raw %>%
    dplyr::select(-dplyr::all_of(c("talent_code", "catalog_talent_name")))
  prepared <- video_analytics_prep_with_titles(
    files = list(video_analytics = analytics_input),
    titles = classifications,
    talent = "selected company talents"
  ) %>%
    dedupe_latest_rows(key_cols = "Video ID") %>%
    dplyr::left_join(video_identity, by = "Video ID")

  if (!("topic" %in% names(prepared))) {
    prepared$topic <- "Unclassified"
  }
  if (!("primary_reference" %in% names(prepared))) {
    prepared$primary_reference <- NA_character_
  }
  if (!("tags" %in% names(prepared))) {
    prepared$tags <- NA_character_
  }
  if (!("collaborative_energy" %in% names(prepared))) {
    prepared$collaborative_energy <- FALSE
  }

  prepared <- prepared %>%
    dplyr::mutate(
      talent_name = dplyr::coalesce(
        dplyr::na_if(trimws(as.character(.data$talent_name)), ""),
        .data$catalog_talent_name
      ),
      snapshot_date = as.Date(.data$date),
      topic = dplyr::coalesce(
        dplyr::na_if(trimws(as.character(.data$topic)), ""),
        "Unclassified"
      ),
      content_reference = dplyr::coalesce(
        dplyr::na_if(trimws(as.character(.data$primary_reference)), ""),
        dplyr::na_if(trimws(as.character(.data$tags)), ""),
        "No reference"
      ),
      is_collab = !is.na(.data$collaborative_energy) & .data$collaborative_energy
    ) %>%
    dplyr::select(-dplyr::any_of("catalog_talent_name"))

  available_coverage <- prepared %>%
    dplyr::distinct(.data$talent_code, .data$talent_name, .data$snapshot_date) %>%
    dplyr::arrange(.data$talent_name)
  coverage <- requested_talents %>%
    dplyr::left_join(
      available_coverage %>%
        dplyr::select(-dplyr::all_of("talent_name")),
      by = "talent_code"
    ) %>%
    dplyr::arrange(.data$talent_name)

  list(
    analytics = prepared,
    coverage = coverage,
    unavailable_talent_codes = coverage$talent_code[is.na(coverage$snapshot_date)]
  )
}
