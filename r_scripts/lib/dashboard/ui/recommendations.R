# Recommendation card UI.

dashboard_recommendation_cards <- function(recommendations, domain = NULL, limit = NULL) {
  recs <- recommendations
  if (is.list(recommendations) && !is.data.frame(recommendations)) {
    recs <- if (is.null(domain)) recommendations$all else recommendations[[domain]]
  } else if (!is.null(domain) && "domain" %in% names(recs)) {
    recs <- recs %>% dplyr::filter(.data$domain == .env$domain)
  }

  if (is.null(recs) || nrow(recs) == 0) {
    return(dashboard_empty_state("Not enough evidence for a recommendation in this area for the active filters."))
  }
  if (!is.null(limit) && is.finite(limit)) {
    recs <- recs %>% dplyr::slice_head(n = limit)
  }

  badge_color <- function(confidence) {
    switch(
      confidence,
      strong = "#1f7a4d",
      moderate = "#2f6f9f",
      weak = "#8a6d1d",
      insufficient = "#777777",
      "#777777"
    )
  }
  priority_label <- function(priority) {
    switch(
      priority,
      high = "High priority",
      medium = "Medium priority",
      low = "Low priority",
      "Priority"
    )
  }

  cards <- lapply(seq_len(nrow(recs)), function(i) {
    rec <- recs[i, , drop = FALSE]
    confidence <- as.character(rec$confidence[[1]])
    htmltools::tags$article(
      class = "dashboard-recommendation-card",
      style = paste(
        "border:1px solid #d9d9d9; border-radius:6px; padding:0.9rem 1rem;",
        "background:#fff; display:flex; flex-direction:column; gap:0.55rem;"
      ),
      htmltools::div(
        style = "display:flex; justify-content:space-between; gap:0.75rem; align-items:flex-start;",
        htmltools::tags$h3(
          style = "font-size:1rem; line-height:1.25; margin:0;",
          as.character(rec$title[[1]])
        ),
        htmltools::tags$span(
          style = paste0(
            "background:", badge_color(confidence), "; color:white; border-radius:999px;",
            "font-size:0.72rem; line-height:1; padding:0.32rem 0.5rem; text-transform:uppercase;",
            "letter-spacing:0.04em; white-space:nowrap;"
          ),
          confidence
        )
      ),
      htmltools::tags$div(
        style = "font-size:0.78rem; color:#555; text-transform:uppercase; letter-spacing:0.04em;",
        paste(priority_label(as.character(rec$priority[[1]])), "|", as.character(rec$domain[[1]]))
      ),
      htmltools::tags$p(
        style = "margin:0; color:#333;",
        htmltools::tags$strong("Finding: "),
        as.character(rec$finding[[1]])
      ),
      htmltools::tags$p(
        style = "margin:0; color:#333;",
        htmltools::tags$strong("Recommendation: "),
        as.character(rec$recommendation[[1]])
      ),
      htmltools::tags$p(
        style = "margin:0; color:#333;",
        htmltools::tags$strong("Evidence: "),
        as.character(rec$evidence[[1]])
      ),
      htmltools::tags$p(
        style = "margin:0; color:#666; font-size:0.9rem;",
        htmltools::tags$strong("Caveat: "),
        as.character(rec$caveat[[1]])
      )
    )
  })

  htmltools::div(
    class = "dashboard-recommendation-grid",
    style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(19rem,1fr)); gap:0.85rem; padding:0.25rem;",
    cards
  )
}

dashboard_recommendation_caveat_cards <- function(recommendation_story) {
  caveats <- recommendation_story$caveats
  if (is.null(caveats) || nrow(caveats) == 0) {
    return(dashboard_empty_state("No additional caveats were generated for the active filters."))
  }

  caveats <- caveats %>% dplyr::slice_head(n = 6)
  htmltools::tags$ul(
    style = "margin:0; padding:0.75rem 1.25rem; color:#444;",
    lapply(seq_len(nrow(caveats)), function(i) {
      htmltools::tags$li(
        style = "margin-bottom:0.45rem;",
        paste0(caveats$title[[i]], ": ", caveats$caveat[[i]])
      )
    })
  )
}
