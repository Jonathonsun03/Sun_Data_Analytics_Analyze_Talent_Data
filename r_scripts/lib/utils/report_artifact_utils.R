report_export_artifacts <- function(
  plots,
  tables,
  ai_inputs = NULL,
  out_root,
  manifest_file,
  ai_inputs_file,
  plot_subdir = "figures",
  table_subdir = "tables",
  plot_width = 11,
  plot_height = 7,
  plot_dpi = 150
) {
  plots_dir <- file.path(out_root, plot_subdir)
  tables_dir <- file.path(out_root, table_subdir)
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(plots_dir, pattern = "\\.png$", full.names = TRUE))
  unlink(list.files(tables_dir, pattern = "\\.csv$", full.names = TRUE))

  safe_name <- function(x) {
    gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
  }

  for (nm in names(plots)) {
    p <- plots[[nm]]
    if (inherits(p, "ggplot")) {
      ggplot2::ggsave(
        filename = file.path(plots_dir, paste0(safe_name(nm), ".png")),
        plot = p,
        width = plot_width,
        height = plot_height,
        dpi = plot_dpi
      )
    }
  }

  for (nm in names(tables)) {
    tbl <- tables[[nm]]
    if (is.data.frame(tbl)) {
      readr::write_csv(tbl, file.path(tables_dir, paste0(safe_name(nm), ".csv")))
    }
  }

  ai_inputs_path <- NA_character_
  if (!is.null(ai_inputs)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package `jsonlite` is required to export report AI inputs.")
    }
    ai_inputs_path <- file.path(out_root, ai_inputs_file)
    jsonlite::write_json(
      ai_inputs,
      path = ai_inputs_path,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
  }

  manifest <- list(
    talent = if (!is.null(ai_inputs$metadata$talent)) ai_inputs$metadata$talent else NA_character_,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    out_root = normalizePath(out_root, winslash = "/", mustWork = FALSE),
    figures_dir = normalizePath(plots_dir, winslash = "/", mustWork = FALSE),
    tables_dir = normalizePath(tables_dir, winslash = "/", mustWork = FALSE),
    ai_inputs_json = if (is.na(ai_inputs_path)) NA_character_ else normalizePath(ai_inputs_path, winslash = "/", mustWork = FALSE),
    figure_files = unname(vapply(
      names(plots),
      function(nm) file.path(normalizePath(plots_dir, winslash = "/", mustWork = FALSE), paste0(safe_name(nm), ".png")),
      character(1)
    )),
    table_files = unname(vapply(
      names(tables),
      function(nm) file.path(normalizePath(tables_dir, winslash = "/", mustWork = FALSE), paste0(safe_name(nm), ".csv")),
      character(1)
    ))
  )

  manifest_path <- file.path(out_root, manifest_file)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to export report artifact manifest.")
  }
  jsonlite::write_json(
    manifest,
    path = manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  invisible(list(
    out_root = normalizePath(out_root, winslash = "/", mustWork = FALSE),
    plots_dir = normalizePath(plots_dir, winslash = "/", mustWork = FALSE),
    tables_dir = normalizePath(tables_dir, winslash = "/", mustWork = FALSE),
    ai_inputs_path = if (is.na(ai_inputs_path)) NA_character_ else normalizePath(ai_inputs_path, winslash = "/", mustWork = FALSE),
    manifest_path = normalizePath(manifest_path, winslash = "/", mustWork = FALSE)
  ))
}

report_resolve_artifact_root <- function(
  talent_root,
  bundle_name,
  report_subdir = "reports",
  artifact_subdir = "artifacts",
  env_var = ""
) {
  if (nzchar(env_var)) {
    override <- trimws(Sys.getenv(env_var, unset = ""))
    if (nzchar(override)) {
      return(normalizePath(override, winslash = "/", mustWork = FALSE))
    }
  }

  datalake_root <- normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)
  talent_folder <- safe_basename(talent_root)
  normalizePath(
    file.path(datalake_root, talent_folder, report_subdir, bundle_name, artifact_subdir),
    winslash = "/",
    mustWork = FALSE
  )
}
