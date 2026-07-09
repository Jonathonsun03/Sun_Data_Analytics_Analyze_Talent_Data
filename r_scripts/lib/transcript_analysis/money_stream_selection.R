default_text_playback_fx_rates <- function() {
  tibble::tribble(
    ~paid_currency, ~usd_per_unit,
    "USD", 1,
    "US$", 1,
    "$", 1,
    "JPY", 0.0067,
    "¥", 0.0067,
    "EUR", 1.08,
    "€", 1.08,
    "GBP", 1.27,
    "£", 1.27,
    "CAD", 0.74,
    "CA$", 0.74,
    "AUD", 0.66,
    "A$", 0.66,
    "NZD", 0.61,
    "NZ$", 0.61,
    "MXN", 0.055,
    "MX$", 0.055,
    "BRL", 0.20,
    "R$", 0.20,
    "PHP", 0.017,
    "₱", 0.017,
    "SGD", 0.74,
    "SGD$", 0.74,
    "HKD", 0.13,
    "HK$", 0.13,
    "TWD", 0.031,
    "NT$", 0.031,
    "KRW", 0.00073,
    "₩", 0.00073,
    "VND", 0.000039,
    "₫", 0.000039,
    "IDR", 0.000061,
    "PLN", 0.25,
    "CHF", 1.10,
    "SEK", 0.091,
    "NOK", 0.095,
    "DKK", 0.15,
    "BGN", 0.55,
    "ARS", 0.0011,
    "CLP", 0.0011,
    "COP", 0.00025,
    "PEN", 0.27,
    "MYR", 0.21,
    "THB", 0.027
  )
}

normalize_text_playback_paid_currency <- function(x) {
  x <- trimws(as.character(x))
  x <- trimws(gsub("\u00A0", " ", x, fixed = TRUE))
  x[!nzchar(x)] <- NA_character_
  upper <- toupper(x)

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    upper %in% c("USD", "US$", "US DOLLAR", "US DOLLARS") | x == "$" ~ "USD",
    upper %in% c("JPY", "YEN", "JP¥") | x == "¥" ~ "JPY",
    upper %in% c("EUR", "EURO", "EUROS") | x == "€" ~ "EUR",
    upper %in% c("GBP", "POUND", "POUNDS", "POUND STERLING") | x == "£" ~ "GBP",
    upper %in% c("CAD", "CA$", "C$") ~ "CAD",
    upper %in% c("AUD", "A$", "AU$") ~ "AUD",
    upper %in% c("NZD", "NZ$") ~ "NZD",
    upper %in% c("MXN", "MX$") ~ "MXN",
    upper %in% c("BRL", "R$") ~ "BRL",
    upper %in% c("PHP") | x == "₱" ~ "PHP",
    upper %in% c("SGD", "SGD$", "S$") ~ "SGD",
    upper %in% c("HKD", "HK$") ~ "HKD",
    upper %in% c("TWD", "NT$") ~ "TWD",
    upper %in% c("KRW") | x == "₩" ~ "KRW",
    upper %in% c("VND") | x == "₫" ~ "VND",
    upper %in% c(
      "IDR", "PLN", "CHF", "SEK", "NOK", "DKK", "BGN",
      "ARS", "CLP", "COP", "PEN", "MYR", "THB"
    ) ~ upper,
    TRUE ~ upper
  )
}

normalize_text_playback_paid_amounts <- function(df, fx_rates = default_text_playback_fx_rates()) {
  if (!("paid_amount_value" %in% names(df))) {
    df$paid_amount_value <- NA_real_
  }
  if (!("paid_amount_text" %in% names(df))) {
    df$paid_amount_text <- NA_character_
  }
  if (!("paid_currency" %in% names(df))) {
    df$paid_currency <- NA_character_
  }

  rates <- fx_rates %>%
    dplyr::mutate(paid_currency_norm = normalize_text_playback_paid_currency(.data$paid_currency)) %>%
    dplyr::filter(!is.na(.data$paid_currency_norm), !is.na(.data$usd_per_unit)) %>%
    dplyr::distinct(.data$paid_currency_norm, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(c("paid_currency_norm", "usd_per_unit")))

  df %>%
    dplyr::mutate(
      paid_amount_native = dplyr::coalesce(
        suppressWarnings(as.numeric(.data$paid_amount_value)),
        readr::parse_number(as.character(.data$paid_amount_text))
      ),
      paid_currency_norm = normalize_text_playback_paid_currency(.data$paid_currency)
    ) %>%
    dplyr::left_join(rates, by = "paid_currency_norm") %>%
    dplyr::mutate(
      paid_amount_usd = dplyr::if_else(
        !is.na(.data$paid_amount_native) & !is.na(.data$usd_per_unit),
        .data$paid_amount_native * .data$usd_per_unit,
        NA_real_
      )
    )
}

summarize_text_playback_money_file <- function(path, fx_rates = default_text_playback_fx_rates()) {
  empty_summary <- function(error = NA_character_) {
    tibble::tibble(
      source_path = normalizePath(path, winslash = "/", mustWork = FALSE),
      source_file = basename(path),
      `Video ID` = text_playback_extract_video_id(path),
      transcript_rows = 0L,
      paid_event_count = 0L,
      total_paid_amount_native = 0,
      total_paid_amount_usd = 0,
      has_paid_value = FALSE,
      unmapped_paid_currency_count = 0L,
      paid_currencies = NA_character_,
      read_error = error
    )
  }

  df <- tryCatch(
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress = FALSE
    ),
    error = function(e) e
  )

  if (inherits(df, "error")) {
    return(empty_summary(conditionMessage(df)))
  }

  paid <- normalize_text_playback_paid_amounts(df, fx_rates = fx_rates)
  paid_positive <- paid %>%
    dplyr::filter(!is.na(.data$paid_amount_native), .data$paid_amount_native > 0)

  if (nrow(paid_positive) == 0L) {
    return(empty_summary())
  }

  currencies <- paid_positive %>%
    dplyr::filter(!is.na(.data$paid_currency_norm), nzchar(.data$paid_currency_norm)) %>%
    dplyr::distinct(.data$paid_currency_norm) %>%
    dplyr::arrange(.data$paid_currency_norm) %>%
    dplyr::pull(.data$paid_currency_norm)

  tibble::tibble(
    source_path = normalizePath(path, winslash = "/", mustWork = FALSE),
    source_file = basename(path),
    `Video ID` = text_playback_extract_video_id(path),
    transcript_rows = nrow(df),
    paid_event_count = nrow(paid_positive),
    total_paid_amount_native = sum(paid_positive$paid_amount_native, na.rm = TRUE),
    total_paid_amount_usd = sum(paid_positive$paid_amount_usd, na.rm = TRUE),
    has_paid_value = TRUE,
    unmapped_paid_currency_count = sum(
      is.na(paid_positive$usd_per_unit) &
        !is.na(paid_positive$paid_currency_norm) &
        nzchar(paid_positive$paid_currency_norm)
    ),
    paid_currencies = if (length(currencies) == 0L) NA_character_ else paste(currencies, collapse = ", "),
    read_error = NA_character_
  )
}

summarize_text_playback_money <- function(
  talent = NULL,
  text_playback_path = NULL,
  titles = NULL,
  talent_data_root = NULL,
  content_type = NULL,
  fx_rates = default_text_playback_fx_rates(),
  paid_only = FALSE
) {
  streams <- list_text_playback_streams(
    talent = talent,
    text_playback_path = text_playback_path,
    titles = titles,
    talent_data_root = talent_data_root,
    content_type = content_type
  )

  money <- purrr::map_dfr(
    streams$source_path,
    summarize_text_playback_money_file,
    fx_rates = fx_rates
  )

  out <- streams %>%
    dplyr::left_join(
      money %>%
        dplyr::select(
          dplyr::any_of(c(
            "source_path",
            "transcript_rows",
            "paid_event_count",
            "total_paid_amount_native",
            "total_paid_amount_usd",
            "has_paid_value",
            "unmapped_paid_currency_count",
            "paid_currencies",
            "read_error"
          ))
        ),
      by = "source_path"
    ) %>%
    dplyr::mutate(
      has_paid_value = dplyr::coalesce(.data$has_paid_value, FALSE),
      paid_event_count = dplyr::coalesce(.data$paid_event_count, 0L),
      total_paid_amount_native = dplyr::coalesce(.data$total_paid_amount_native, 0),
      total_paid_amount_usd = dplyr::coalesce(.data$total_paid_amount_usd, 0)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$total_paid_amount_usd), dplyr::desc(.data$paid_event_count))

  if (isTRUE(paid_only)) {
    out <- out %>% dplyr::filter(.data$has_paid_value)
  }

  out
}

summarize_all_text_playback_money <- function(
  talent_data_root = NULL,
  content_type = NULL,
  fx_rates = default_text_playback_fx_rates(),
  paid_only = TRUE
) {
  talents <- list_text_playback_talents(talent_data_root = talent_data_root)

  purrr::map_dfr(seq_len(nrow(talents)), function(i) {
    talent <- talents$talent_name[[i]]
    tryCatch(
      summarize_text_playback_money(
        talent = talent,
        text_playback_path = talents$text_playback_path[[i]],
        talent_data_root = talent_data_root,
        content_type = content_type,
        fx_rates = fx_rates,
        paid_only = paid_only
      ) %>%
        dplyr::mutate(talent_name = dplyr::coalesce(.data$talent_name, talent), .before = 1),
      error = function(e) {
        tibble::tibble(
          talent_name = talent,
          source_path = NA_character_,
          has_paid_value = FALSE,
          read_error = conditionMessage(e)
        )
      }
    )
  })
}

select_average_giving_streams <- function(
  streams,
  amount_col = "average_paid_message_amount",
  paid_event_col = "paid_event_count",
  min_paid_events = 5,
  quantile_probs = c(0.25, 0.5, 0.75)
) {
  required <- c(amount_col, paid_event_col)
  missing <- setdiff(required, names(streams))
  if (length(missing) > 0L) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  metrics <- streams %>%
    dplyr::filter(
      is.finite(.data[[amount_col]]),
      .data[[paid_event_col]] >= min_paid_events
    )

  qs <- stats::quantile(metrics[[amount_col]], probs = quantile_probs, na.rm = TRUE)
  quantile_tbl <- tibble::tibble(
    quantile = c("Q1", "Median", "Q3"),
    average_paid_message_amount = as.numeric(qs),
    percentile = quantile_probs
  )

  candidates <- metrics %>%
    dplyr::mutate(
      average_giving_band = dplyr::case_when(
        .data[[amount_col]] >= qs[[1]] & .data[[amount_col]] <= qs[[3]] ~ "middle_50_percent",
        TRUE ~ "outside_middle_50_percent"
      ),
      distance_from_median_paid_amount = abs(.data[[amount_col]] - qs[[2]])
    ) %>%
    dplyr::filter(.data$average_giving_band == "middle_50_percent") %>%
    dplyr::arrange(.data$distance_from_median_paid_amount, dplyr::desc(.data[[paid_event_col]]))

  list(
    metrics = metrics,
    quantiles = qs,
    quantile_table = quantile_tbl,
    candidates = candidates
  )
}

filter_paid_money_streams <- function(money_streams) {
  money_streams %>%
    dplyr::filter(.data$has_paid_value) %>%
    dplyr::arrange(dplyr::desc(.data$total_paid_amount_usd), dplyr::desc(.data$paid_event_count))
}

add_average_paid_message_amount <- function(streams) {
  streams %>%
    dplyr::mutate(
      average_paid_message_amount = .data$total_paid_amount_usd / .data$paid_event_count
    )
}

filter_paid_stream_metric_rows <- function(streams, min_paid_events = 5) {
  streams %>%
    dplyr::filter(
      is.finite(.data$average_paid_message_amount),
      .data$paid_event_count >= min_paid_events
    )
}

build_paid_stream_metrics <- function(money_streams, min_paid_events = 5) {
  money_streams %>%
    filter_paid_money_streams() %>%
    add_average_paid_message_amount() %>%
    filter_paid_stream_metric_rows(min_paid_events = min_paid_events)
}

label_paid_stream_quantiles <- function(stream_metrics, quantiles) {
  stream_metrics %>%
    dplyr::mutate(
      giving_quantile_label = dplyr::case_when(
        .data$average_paid_message_amount < quantiles[[1]] ~ "low_giving",
        .data$average_paid_message_amount <= quantiles[[3]] ~ "average_giving",
        TRUE ~ "high_giving"
      ),
      distance_from_median_paid_amount = abs(.data$average_paid_message_amount - quantiles[[2]])
    ) %>%
    dplyr::arrange(.data$giving_quantile_label, .data$distance_from_median_paid_amount)
}

summarize_money_stream_selection <- function(
  money_streams,
  stream_metrics = NULL,
  talent = NA_character_
) {
  if (is.null(stream_metrics)) {
    eligible_transcript_count <- NA_integer_
  } else {
    eligible_transcript_count <- nrow(stream_metrics)
  }

  money_streams %>%
    dplyr::summarise(
      talent = talent,
      transcript_count = dplyr::n(),
      paid_transcript_count = sum(.data$has_paid_value),
      eligible_transcript_count = eligible_transcript_count,
      total_paid_events = sum(.data$paid_event_count, na.rm = TRUE),
      total_paid_amount_usd = sum(.data$total_paid_amount_usd, na.rm = TRUE),
      transcript_count_with_unmapped_currency = sum(
        .data$unmapped_paid_currency_count > 0,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

format_average_giving_candidates <- function(candidates, candidate_n = 25) {
  candidates %>%
    dplyr::select(
      dplyr::any_of(c(
        "talent_name",
        "title_raw",
        "published_date",
        "paid_event_count",
        "total_paid_amount_usd",
        "average_paid_message_amount",
        "distance_from_median_paid_amount",
        "paid_currencies",
        "source_path"
      ))
    ) %>%
    dplyr::slice_head(n = candidate_n)
}

plot_average_paid_message_distribution <- function(
  stream_metrics,
  quantile_table,
  quantiles,
  talent = NULL,
  min_paid_events = 5,
  histogram_binwidth = 0.5
) {
  title_suffix <- if (!is.null(talent) && nzchar(as.character(talent))) {
    paste0(": ", talent)
  } else {
    ""
  }

  ggplot2::ggplot(stream_metrics, ggplot2::aes(x = .data$average_paid_message_amount)) +
    ggplot2::geom_histogram(binwidth = histogram_binwidth) +
    ggplot2::geom_vline(
      data = quantile_table,
      ggplot2::aes(xintercept = .data$average_paid_message_amount, linetype = .data$quantile),
      linewidth = 0.8
    ) +
    ggplot2::annotate(
      "rect",
      xmin = quantiles[[1]],
      xmax = quantiles[[3]],
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.12
    ) +
    ggplot2::labs(
      title = paste0("Average Paid Message Amount by Stream", title_suffix),
      subtitle = paste0(
        "Shaded region is average_giving/middle 50%. ",
        "Minimum paid events: ", min_paid_events
      ),
      x = "Average paid message amount (USD estimate)",
      y = "Stream count",
      linetype = "Quantile"
    )
}

build_money_chat_quantile_outputs <- function(
  money_streams,
  talent = NA_character_,
  min_paid_events = 5,
  candidate_n = 25,
  histogram_binwidth = 0.5
) {
  paid_streams <- filter_paid_money_streams(money_streams)
  stream_metrics <- build_paid_stream_metrics(money_streams, min_paid_events = min_paid_events)

  if (nrow(stream_metrics) == 0L) {
    return(list(
      talent = talent,
      money_streams = money_streams,
      paid_streams = paid_streams,
      stream_metrics = stream_metrics,
      summary = summarize_money_stream_selection(money_streams, stream_metrics, talent = talent),
      quantile_table = tibble::tibble(),
      labeled_streams = tibble::tibble(),
      average_giving_candidate_streams = tibble::tibble(),
      average_giving_candidates_print = tibble::tibble(),
      plot = NULL
    ))
  }

  average_giving_selection <- select_average_giving_streams(
    stream_metrics,
    min_paid_events = min_paid_events
  )

  quantiles <- average_giving_selection$quantiles
  quantile_table <- average_giving_selection$quantile_table
  average_giving_candidate_streams <- average_giving_selection$candidates
  labeled_streams <- label_paid_stream_quantiles(stream_metrics, quantiles)
  average_giving_candidates_print <- format_average_giving_candidates(
    average_giving_candidate_streams,
    candidate_n = candidate_n
  )
  money_plot <- plot_average_paid_message_distribution(
    stream_metrics = stream_metrics,
    quantile_table = quantile_table,
    quantiles = quantiles,
    talent = talent,
    min_paid_events = min_paid_events,
    histogram_binwidth = histogram_binwidth
  )

  list(
    talent = talent,
    money_streams = money_streams,
    paid_streams = paid_streams,
    stream_metrics = stream_metrics,
    summary = summarize_money_stream_selection(money_streams, stream_metrics, talent = talent),
    quantiles = quantiles,
    quantile_table = quantile_table,
    labeled_streams = labeled_streams,
    average_giving_candidate_streams = average_giving_candidate_streams,
    average_giving_candidates_print = average_giving_candidates_print,
    plot = money_plot
  )
}
