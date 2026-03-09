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

  table <- DT::datatable(
    df,
    class = class,
    filter = filter,
    extensions = "Buttons",
    options = dt_default_options(
      scroll_x = scroll_x,
      options = options
    ),
    ...
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
