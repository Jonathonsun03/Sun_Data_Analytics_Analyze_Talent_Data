# Audience geography normalization and metric builders.

dashboard_iso3166_lookup <- function() {
  iso_path <- "/usr/share/iso-codes/json/iso_3166-1.json"
  if (file.exists(iso_path) && requireNamespace("jsonlite", quietly = TRUE)) {
    iso <- jsonlite::fromJSON(iso_path)[["3166-1"]]
    return(iso %>%
      dplyr::transmute(
        country_code = toupper(.data$alpha_2),
        country_iso3 = toupper(.data$alpha_3),
        country_name = .data$name
      ))
  }

  # Fallback for common YouTube geography codes if the system ISO table is unavailable.
  tibble::tribble(
    ~country_code, ~country_iso3, ~country_name,
    "AR", "ARG", "Argentina",
    "AU", "AUS", "Australia",
    "BR", "BRA", "Brazil",
    "CA", "CAN", "Canada",
    "DE", "DEU", "Germany",
    "ES", "ESP", "Spain",
    "FR", "FRA", "France",
    "GB", "GBR", "United Kingdom",
    "ID", "IDN", "Indonesia",
    "IN", "IND", "India",
    "IT", "ITA", "Italy",
    "JP", "JPN", "Japan",
    "KR", "KOR", "Korea, Republic of",
    "MX", "MEX", "Mexico",
    "MY", "MYS", "Malaysia",
    "NL", "NLD", "Netherlands",
    "PH", "PHL", "Philippines",
    "SE", "SWE", "Sweden",
    "SG", "SGP", "Singapore",
    "TH", "THA", "Thailand",
    "TW", "TWN", "Taiwan, Province of China",
    "UA", "UKR", "Ukraine",
    "US", "USA", "United States",
    "VN", "VNM", "Viet Nam",
    "ZA", "ZAF", "South Africa"
  )
}

dashboard_select_geography_metric <- function(geo_df) {
  metric_candidates <- c(
    "views",
    "Views",
    "Est Views (calc)",
    "Estimated Views",
    "estimated_views",
    "Viewer Percentage",
    "viewer_percentage",
    "Audience Share",
    "audience_share"
  )
  metric_col <- metric_candidates[metric_candidates %in% names(geo_df)][1]
  if (is.na(metric_col)) {
    return(NULL)
  }

  metric_label <- dplyr::case_when(
    metric_col %in% c("views", "Views") ~ "Views",
    metric_col %in% c("Est Views (calc)", "Estimated Views", "estimated_views") ~ "Estimated Cumulative Views",
    metric_col %in% c("Viewer Percentage", "viewer_percentage") ~ "Viewer Percentage",
    metric_col %in% c("Audience Share", "audience_share") ~ "Audience Share",
    TRUE ~ metric_col
  )
  metric_agg <- if (metric_label %in% c("Viewer Percentage", "Audience Share")) "mean" else "sum"

  list(column = metric_col, label = metric_label, aggregation = metric_agg)
}

dashboard_build_audience_geography <- function(geo_df) {
  if (is.null(geo_df) || nrow(geo_df) == 0 || !("Country" %in% names(geo_df))) {
    return(NULL)
  }

  date_col <- c("snapshot_date", "date", "Date", "Report Date", "report_date")
  date_col <- date_col[date_col %in% names(geo_df)][1]
  if (is.na(date_col)) {
    return(NULL)
  }

  metric <- dashboard_select_geography_metric(geo_df)
  if (is.null(metric)) {
    return(NULL)
  }

  cleaned <- geo_df %>%
    dplyr::transmute(
      snapshot_date = suppressWarnings(as.Date(.data[[date_col]])),
      country_code = toupper(trimws(as.character(.data$Country))),
      metric_value = suppressWarnings(as.numeric(.data[[metric$column]]))
    ) %>%
    dplyr::mutate(
      country_code = dplyr::na_if(.data$country_code, ""),
      country_code = dplyr::na_if(.data$country_code, "(NO DATA)")
    ) %>%
    dplyr::filter(!is.na(.data$snapshot_date), !is.na(.data$country_code), !is.na(.data$metric_value))

  if (nrow(cleaned) == 0) {
    return(NULL)
  }

  grouped <- cleaned %>%
    dplyr::group_by(.data$snapshot_date, .data$country_code)

  summary_df <- if (identical(metric$aggregation, "mean")) {
    grouped %>% dplyr::summarise(metric_value = mean(.data$metric_value, na.rm = TRUE), .groups = "drop")
  } else {
    grouped %>% dplyr::summarise(metric_value = sum(.data$metric_value, na.rm = TRUE), .groups = "drop")
  }

  summary_df <- summary_df %>%
    dplyr::left_join(dashboard_iso3166_lookup(), by = "country_code") %>%
    dplyr::mutate(
      snapshot_date_label = format(.data$snapshot_date, "%Y-%m-%d"),
      metric_label = metric$label,
      metric_display = if (metric$label %in% c("Viewer Percentage", "Audience Share")) {
        scales::percent(.data$metric_value, accuracy = 0.1)
      } else {
        scales::comma(round(.data$metric_value))
      }
    ) %>%
    dplyr::arrange(.data$snapshot_date, dplyr::desc(.data$metric_value))

  unmatched_codes <- summary_df %>%
    dplyr::filter(is.na(.data$country_iso3)) %>%
    dplyr::distinct(.data$country_code) %>%
    dplyr::pull("country_code")

  attr(summary_df, "metric_label") <- metric$label
  attr(summary_df, "metric_source_column") <- metric$column
  attr(summary_df, "metric_aggregation") <- metric$aggregation
  attr(summary_df, "unmatched_country_count") <- length(unmatched_codes)
  attr(summary_df, "unmatched_country_codes") <- unmatched_codes
  summary_df
}
