# Exercise the dashboard-to-writer adapter against a temporary control directory.
source("r_scripts/lib/talent_lifecycle/dashboard.R")

local({
  root <- tempfile("dashboard-controls-")
  dir.create(root)
  settings <- c("TALENT_CONTROL_ROOT", "TALENT_CONTROL_UI_ENABLED", "TALENT_CONTROL_PYTHON")
  previous <- Sys.getenv(settings, unset = NA_character_)
  on.exit({
    for (name in settings) {
      if (is.na(previous[[name]])) Sys.unsetenv(name)
      else do.call(Sys.setenv, stats::setNames(list(previous[[name]]), name))
    }
    unlink(root, recursive = TRUE)
  })
  Sys.setenv(TALENT_CONTROL_ROOT = root, TALENT_CONTROL_UI_ENABLED = "true",
             TALENT_CONTROL_PYTHON = Sys.which("python3"))
  catalog <- data.frame(talent_code = "TES1", talent_name = "Quoted ' Talent",
                        channel_id = "UC-test", channel_name = "Test Talent")
  talent_control_save(catalog, "TES1", "archive", 0L, "test-operator")
  state <- talent_control_current("TES1")
  stopifnot(!state$retrieval_enabled, state$processing_enabled, state$revision == 1L)
  stopifnot(!talent_work_allowed("UC-test", "retrieval"))
  talent_control_save(catalog, "TES1", "pause", 1L, "test-operator")
  stopifnot(!talent_work_allowed("Quoted ' Talent"))
  Sys.setenv(TALENT_CONTROL_UI_ENABLED = "false")
  rejected <- tryCatch({
    talent_control_save(catalog, "TES1", "resume", 2L, "test-operator")
    FALSE
  }, error = function(error) TRUE)
  stopifnot(rejected, !talent_control_current("TES1")$processing_enabled)
})
message("Talent lifecycle dashboard adapter tests passed.")
