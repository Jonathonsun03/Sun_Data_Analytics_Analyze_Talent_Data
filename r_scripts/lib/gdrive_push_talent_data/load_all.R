gdrive_talent_load_all <- function(root = NULL) {
  if (is.null(root) || !nzchar(root)) {
    root <- file.path("r_scripts", "lib", "gdrive_push_talent_data")
  }

  files <- list.files(root, pattern = "\\.R$", full.names = TRUE)
  files <- files[basename(files) != "load_all.R"]

  load_order <- c(
    "utils.R",
    "config.R",
    "permissions.R",
    "local_tree.R",
    "manifest.R",
    "drive_helpers.R",
    "folder_tree.R",
    "client_structure.R",
    "delivery_files.R",
    "sharing.R",
    "plan.R",
    "sync.R",
    "talents_sheet.R",
    "reports_sheet.R",
    "report_schedule_sheet.R"
  )

  ordered <- file.path(root, load_order)
  ordered <- ordered[file.exists(ordered)]
  remaining <- setdiff(sort(files), ordered)

  for (f in c(ordered, remaining)) {
    source(f)
  }

  invisible(c(ordered, remaining))
}
