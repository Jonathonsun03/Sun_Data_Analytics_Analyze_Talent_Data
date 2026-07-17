# Shared dashboard table, empty-state, and filter-context UI.

dashboard_datatable <- function(df, caption = NULL, page_length = 10, scroll_x = TRUE) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  DTSettings(
    df,
    class = "display stripe hover",
    scroll_x = scroll_x,
    options = list(pageLength = page_length),
    filter = "none"
  )
}

dashboard_static_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  htmltools::tags$table(
    class = "table table-sm table-striped dashboard-static-table",
    style = "width:100%; font-size:0.86rem; margin:0;",
    htmltools::tags$thead(
      htmltools::tags$tr(
        lapply(names(df), function(nm) {
          htmltools::tags$th(style = "vertical-align:top;", nm)
        })
      )
    ),
    htmltools::tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        htmltools::tags$tr(
          lapply(names(df), function(nm) {
            htmltools::tags$td(style = "vertical-align:top;", as.character(df[[nm]][[i]]))
          })
        )
      })
    )
  )
}

dashboard_overview_table <- function(dashboard_data) {
  dashboard_datatable(dashboard_data$overview, page_length = nrow(dashboard_data$overview), scroll_x = TRUE)
}

dashboard_empty_state <- function(message) {
  htmltools::div(
    class = "dashboard-empty-state",
    style = "padding: 1rem; color: #555;",
    message
  )
}

dashboard_filter_context_card <- function(filter_summary) {
  if (is.null(filter_summary) || nrow(filter_summary) == 0) {
    return(dashboard_empty_state("No active filter summary is available."))
  }

  htmltools::div(
    class = "dashboard-filter-context",
    style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(9rem,1fr)); gap:0.5rem 1rem; padding:0.65rem 0.9rem;",
    lapply(seq_len(nrow(filter_summary)), function(i) {
      htmltools::div(
        style = "min-width:0;",
        htmltools::tags$div(
          style = "font-size:0.72rem; color:#666; text-transform:uppercase; letter-spacing:0.04em;",
          filter_summary$setting[[i]]
        ),
        htmltools::tags$div(
          style = "font-size:0.92rem; font-weight:600; overflow-wrap:anywhere;",
          filter_summary$value[[i]]
        )
      )
    })
  )
}
