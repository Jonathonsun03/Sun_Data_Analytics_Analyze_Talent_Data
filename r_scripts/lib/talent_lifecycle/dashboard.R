# Internal dashboard adapter: read-only catalog lookup, atomic control-file edits.
source(here::here("r_scripts", "lib", "talent_lifecycle", "policy.R"))

talent_control_catalog <- function(database_path) {
  con <- duckdb_connect(db_path = database_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbGetQuery(con, paste(
    "SELECT talent.talent_code, talent.talent_name, channel.channel_id, channel.channel_name",
    "FROM catalog.talents AS talent LEFT JOIN catalog.channels AS channel USING (talent_code)",
    "ORDER BY talent.talent_code, channel.channel_id"
  ))
}

talent_control_current <- function(code) {
  for (record in talent_control_records()) {
    if (identical(record$talent_code, code)) return(record)
  }
  list(talent_code = code, revision = 0L, retrieval_enabled = TRUE,
       processing_enabled = TRUE, updated_at = "No saved change", updated_by = "")
}

talent_control_save <- function(catalog, code, action, revision, actor) {
  if (!identical(Sys.getenv("TALENT_CONTROL_UI_ENABLED"), "true")) {
    stop("Talent control editing is not enabled for this dashboard")
  }
  rows <- catalog[catalog$talent_code == code, , drop = FALSE]
  if (nrow(rows) == 0L) stop("Talent is not in the catalog")
  args <- c(
    here::here("py_scripts", "run", "talent_control.py"), action,
    "--talent-code", code, "--actor", actor,
    "--expected-revision", as.character(revision)
  )
  for (id in unique(stats::na.omit(rows$channel_id))) args <- c(args, "--channel-id", id)
  aliases <- unique(stats::na.omit(c(rows$talent_name, rows$channel_name)))
  # Collectors also use filesystem-safe channel names for raw-data directories.
  aliases <- unique(c(aliases, gsub('[<>:"/\\\\|?*]', "_", aliases)))
  for (alias in aliases) args <- c(args, "--alias", alias)
  python <- Sys.getenv("TALENT_CONTROL_PYTHON", unset = here::here(".venv", "bin", "python"))
  output <- system2(python, shQuote(args), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) stop(paste(output, collapse = "\n"))
  invisible(output)
}

talent_control_ui <- function() {
  shiny::tagList(
    shiny::uiOutput("lifecycle_selector"),
    shiny::verbatimTextOutput("lifecycle_status"),
    shiny::uiOutput("lifecycle_actions"),
    shiny::tags$p("Archive stops retrieval only. Pause stops downstream processing at safe checkpoints. Existing data is retained.")
  )
}

talent_control_status_text <- function(state) {
  paste(
    paste("Retrieval:", if (state$retrieval_enabled) "enabled" else "archived"),
    paste("Processing:", if (state$processing_enabled) "enabled" else "pause requested / future work blocked"),
    "Running workers finish their current operation before observing a pause.",
    paste("Last change:", state$updated_at, state$updated_by),
    sep = "\n"
  )
}

talent_control_server <- function(input, output, session, database_path) {
  catalog <- shiny::reactive({
    input$refresh_dashboard
    talent_control_catalog(database_path)
  })
  changed <- shiny::reactiveVal(0L)
  current <- shiny::reactive({
    changed()
    shiny::invalidateLater(5000, session)
    shiny::req(input$lifecycle_talent)
    talent_control_current(input$lifecycle_talent)
  })
  output$lifecycle_selector <- shiny::renderUI({
    rows <- unique(catalog()[c("talent_code", "talent_name")])
    shiny::selectInput("lifecycle_talent", "Talent", stats::setNames(
      rows$talent_code, paste(rows$talent_name, rows$talent_code, sep = " — ")
    ))
  })
  output$lifecycle_status <- shiny::renderText({
    if (!nzchar(Sys.getenv("TALENT_CONTROL_ROOT"))) {
      return("Shared controls are not configured. Existing worker behavior is unchanged.")
    }
    talent_control_status_text(current())
  })
  output$lifecycle_actions <- shiny::renderUI({
    if (!identical(Sys.getenv("TALENT_CONTROL_UI_ENABLED"), "true") ||
        !nzchar(Sys.getenv("TALENT_CONTROL_ROOT"))) {
      return(shiny::tags$p("Editing is disabled until the shared control mount is configured."))
    }
    state <- current()
    shiny::tagList(
      if (state$retrieval_enabled) shiny::actionButton("lifecycle_archive", "Archive retrieval")
      else shiny::actionButton("lifecycle_restore", "Restore retrieval"),
      if (state$processing_enabled) shiny::actionButton("lifecycle_pause", "Pause processing")
      else shiny::actionButton("lifecycle_resume", "Resume processing")
    )
  })
  for (action in c("archive", "restore", "pause", "resume")) local({
    selected_action <- action
    shiny::observeEvent(input[[paste0("lifecycle_", selected_action)]], {
      tryCatch({
        state <- current()
        actor <- session$request$HTTP_CF_ACCESS_AUTHENTICATED_USER_EMAIL
        if (is.null(actor) || !nzchar(actor)) actor <- Sys.info()[["user"]]
        talent_control_save(catalog(), state$talent_code, selected_action, state$revision, actor)
        changed(changed() + 1L)
        shiny::showNotification("Talent control saved.", type = "message")
      }, error = function(error) {
        shiny::showNotification(conditionMessage(error), type = "error", duration = NULL)
      })
    }, ignoreInit = TRUE)
  })
}
