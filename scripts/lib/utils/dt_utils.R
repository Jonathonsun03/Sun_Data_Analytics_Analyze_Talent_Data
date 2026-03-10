dt_resolve_columns <- function(df, cols = NULL, label = "columns") {
  if (is.null(cols)) {
    return(character())
  }

  cols <- as.character(cols)
  hits <- cols[cols %in% names(df)]

  if (!length(hits)) {
    return(character())
  }

  hits
}

dt_detect_currency_cols <- function(
    df,
    currency_cols = NULL,
    pattern = "(revenue|usd|dollar|amount|cost|price|earnings|income|salary|wage|profit|arpu|rpm|cpm)") {
  if (!is.null(currency_cols)) {
    return(dt_resolve_columns(df, currency_cols, label = "currency columns"))
  }

  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!length(numeric_cols)) {
    return(character())
  }

  hit <- grepl(pattern, numeric_cols, ignore.case = TRUE)
  numeric_cols[hit]
}

dt_export_buttons <- function() {
  export_all <- list(modifier = list(page = "all"))

  list(
    list(extend = "copyHtml5", text = "Copy", exportOptions = export_all),
    list(extend = "csvHtml5", text = "CSV", exportOptions = export_all),
    list(extend = "excelHtml5", text = "Excel", exportOptions = export_all)
  )
}

dt_default_options <- function(scroll_x = FALSE, page_length = 10, options = list()) {
  base <- list(
    scrollX = isTRUE(scroll_x),
    dom = "Bfrtip",
    buttons = dt_export_buttons(),
    pageLength = page_length,
    lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All"))
  )

  utils::modifyList(base, options)
}

dt_display_label <- function(x) {
  raw <- as.character(x)
  # Preserve uppercase letters while stripping punctuation so CamelCase keys map correctly.
  key <- tolower(gsub("[^A-Za-z0-9]+", "", raw))

  exact_map <- c(
    "title" = "Title",
    "publishdate" = "Publish Date",
    "contenttype" = "Content Type",
    "estimatedminuteswatched" = "Est Minutes Watched",
    "averageviewduration" = "Avg View Duration",
    "averageviewpercentage" = "Avg View %",
    "estimatedrevenue" = "Est Revenue",
    "windowlabel" = "Period",
    "metric" = "Metric",
    "totalrevenue" = "Total Revenue",
    "revenuegeneratingstreamcount" = "Revenue Stream Count",
    "revenueperstream" = "Revenue per Stream",
    "weekendgroup" = "Day Type",
    "dayofweek" = "Day of Week",
    "collabgroup" = "Collaboration Type",
    "topicgroup" = "Topic",
    "taggroup" = "Tag",
    "videocountviews" = "Video Count (Views)",
    "videocountrevenue" = "Video Count (Revenue)",
    "videocount" = "Video Count",
    "metricscovered" = "Metrics Used",
    "totalviews" = "Total Views",
    "averageviewspervideo" = "Avg Views per Video",
    "averagerevenuepervideo" = "Avg Revenue per Video",
    "viewsmedianpercentile" = "Views Pctl",
    "revenuemedianpercentile" = "Revenue Pctl",
    "engagementmedianpercentile" = "Engage Pctl",
    "avgmedianpercentile" = "Avg Pctl",
    "performanceband" = "Band",
    "avgviewsliftvsnoncollab" = "Avg Views Lift vs Non-Collab",
    "avgrevenueliftvsnoncollab" = "Avg Revenue Lift vs Non-Collab",
    "value" = "Value"
  )

  if (key %in% names(exact_map)) {
    return(unname(exact_map[[key]]))
  }

  label <- raw
  label <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", label, perl = TRUE)
  label <- gsub("[_.]+", " ", label)
  label <- gsub("\\s+", " ", label)
  label <- trimws(label)

  label <- gsub("\\bAverage\\b", "Avg", label, ignore.case = TRUE)
  label <- gsub("\\bEstimated\\b", "Est", label, ignore.case = TRUE)
  label <- gsub("\\bPercentage\\b", "%", label, ignore.case = TRUE)
  label <- gsub("\\bMinutes\\b", "Min", label, ignore.case = TRUE)

  words <- strsplit(label, "\\s+")[[1]]
  words <- vapply(
    words,
    function(w) {
      lw <- tolower(w)
      if (lw %in% c("id", "cpm", "rpm", "usd")) {
        return(toupper(w))
      }
      if (w %in% c("%")) {
        return(w)
      }
      paste0(toupper(substr(w, 1, 1)), tolower(substr(w, 2, nchar(w))))
    },
    character(1)
  )

  paste(words, collapse = " ")
}

dt_pretty_colnames <- function(df) {
  unname(vapply(names(df), dt_display_label, character(1)))
}

DTSettings <- function(
    df,
    class = "display stripe hover",
    scroll_x = FALSE,
    currency_cols = NULL,
    percent_cols = NULL,
    digits = 2,
    filter = "top",
    options = list(),
    ...) {
  currency_cols <- dt_detect_currency_cols(df, currency_cols = currency_cols)
  percent_cols <- dt_resolve_columns(df, percent_cols, label = "percent columns")
  currency_cols <- setdiff(currency_cols, percent_cols)

  decimal_cols <- if (exists("detect_decimals", mode = "function")) {
    detect_decimals(df)
  } else {
    names(df)[vapply(df, is.numeric, logical(1))]
  }

  round_cols <- setdiff(decimal_cols, c(currency_cols, percent_cols))

  dot_args <- list(...)
  if (is.null(dot_args$colnames)) {
    dot_args$colnames <- dt_pretty_colnames(df)
  }

  table <- do.call(
    DT::datatable,
    c(
      list(
        data = df,
        class = class,
        filter = filter,
        extensions = "Buttons",
        options = dt_default_options(
          scroll_x = scroll_x,
          options = options
        )
      ),
      dot_args
    )
  )

  if (length(currency_cols)) {
    table <- DT::formatCurrency(
      table,
      columns = currency_cols,
      currency = "$",
      digits = digits,
      interval = 3,
      mark = ",",
      dec.mark = "."
    )
  }

  if (length(percent_cols)) {
    table <- DT::formatPercentage(
      table,
      columns = percent_cols,
      digits = digits
    )
  }

  if (length(round_cols)) {
    table <- DT::formatRound(
      table,
      columns = round_cols,
      digits = digits
    )
  }

  table
}
