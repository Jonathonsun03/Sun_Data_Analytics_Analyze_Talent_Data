gdrive_talent_report_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

gdrive_talent_report_key_compact <- function(x) {
  gsub("[^a-z0-9]+", "", tolower(as.character(x)))
}

gdrive_talent_report_title <- function(bundle_key) {
  suffix <- sub("^bundle_", "", bundle_key)
  paste("Bundle", toupper(suffix))
}

gdrive_talent_read_report_desc <- function(path) {
  if (!file.exists(path)) {
    return("")
  }

  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

gdrive_talent_plain_report_text <- function(text) {
  text <- gsub("```[\\s\\S]*?```", " ", text, perl = TRUE)
  text <- gsub("`([^`]+)`", "\\1", text, perl = TRUE)
  text <- gsub("^\\s{0,3}#{1,6}\\s*", "", text, perl = TRUE)
  text <- gsub("\\*\\*|__", "", text)
  text <- gsub("^\\s*[-*+]\\s*", "", text, perl = TRUE)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

gdrive_talent_extract_report_description <- function(text, max_chars = 320) {
  if (!nzchar(text)) {
    return("")
  }

  lines <- readLines(textConnection(text), warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  best_for_idx <- grep("best for clients asking|who this report is for|what this bundle answers", lines, ignore.case = TRUE)
  if (length(best_for_idx) > 0) {
    start <- best_for_idx[[1]] + 1L
    candidates <- lines[start:min(length(lines), start + 6L)]
    candidate_labels <- tolower(gdrive_talent_plain_report_text(candidates))
    candidates <- candidates[!grepl("^#{1,6}\\s+", candidates)]
    candidates <- candidates[!grepl("^(included analyses|key questions|delivered outputs|strategic use)\\b", candidate_labels)]
    candidates <- candidates[nzchar(candidates)]
    if (length(candidates) > 0) {
      n_keep <- if (grepl("^[\"“]", gdrive_talent_plain_report_text(candidates[[1]]))) 1L else min(2L, length(candidates))
      desc <- gdrive_talent_plain_report_text(paste(candidates[seq_len(n_keep)], collapse = " "))
      return(substr(desc, 1L, max_chars))
    }
  }

  candidates <- lines[
    !grepl("^#{1,6}\\s+", lines) &
      !grepl("^\\*?Bundle [A-Z]", lines, ignore.case = TRUE)
  ]
  candidates <- candidates[nzchar(candidates)]
  desc <- if (length(candidates) > 0) {
    gdrive_talent_plain_report_text(paste(candidates[seq_len(min(2L, length(candidates)))], collapse = " "))
  } else {
    gdrive_talent_plain_report_text(text)
  }

  substr(desc, 1L, max_chars)
}

gdrive_talent_report_template_inventory <- function(repo_root) {
  template_root <- file.path(repo_root, "templates", "reports")
  if (!dir.exists(template_root)) {
    return(gdrive_talent_empty_df(c(
      "template_key",
      "template_dir",
      "template_rmd",
      "template_html_count",
      "template_plot_count",
      "template_table_count"
    )))
  }

  dirs <- list.dirs(template_root, full.names = TRUE, recursive = FALSE)
  rows <- lapply(dirs, function(dir) {
    files <- list.files(dir, full.names = TRUE, recursive = TRUE)
    data.frame(
      template_key = gdrive_talent_report_key_compact(basename(dir)),
      template_dir = normalizePath(dir, winslash = "/", mustWork = FALSE),
      template_rmd = paste(
        normalizePath(files[grepl("\\.Rmd$", files, ignore.case = TRUE)], winslash = "/", mustWork = FALSE),
        collapse = "; "
      ),
      template_html_count = sum(grepl("\\.html?$", files, ignore.case = TRUE)),
      template_plot_count = sum(grepl("\\.(png|jpg|jpeg|svg)$", files, ignore.case = TRUE)),
      template_table_count = sum(grepl("\\.csv$", files, ignore.case = TRUE)),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

gdrive_talent_report_render_counts <- function(datalake_root, bundle_key) {
  if (is.null(datalake_root) || !nzchar(datalake_root) || !dir.exists(datalake_root)) {
    return(list(rendered_report_count = 0L, latest_rendered_report = "", latest_rendered_at = ""))
  }

  bundle_suffix <- sub("^bundle_", "", bundle_key)
  report_dirs <- c(
    file.path(datalake_root, "*", "reports", paste0("bundle_", bundle_suffix)),
    file.path(datalake_root, "*", "reports", paste0("bundle_", toupper(bundle_suffix)))
  )
  files <- unique(unlist(lapply(report_dirs, function(pattern) {
    Sys.glob(file.path(pattern, "*"))
  }), use.names = FALSE))
  files <- files[file.exists(files)]
  files <- files[grepl("\\.(html?|pdf|docx)$", files, ignore.case = TRUE)]

  if (length(files) == 0) {
    return(list(rendered_report_count = 0L, latest_rendered_report = "", latest_rendered_at = ""))
  }

  info <- file.info(files)
  latest_idx <- which.max(info$mtime)
  list(
    rendered_report_count = length(files),
    latest_rendered_report = normalizePath(files[[latest_idx]], winslash = "/", mustWork = FALSE),
    latest_rendered_at = format(info$mtime[[latest_idx]], "%Y-%m-%d %H:%M:%S %z")
  )
}

gdrive_talent_report_status <- function(has_rmd, html_count, plot_count, table_count) {
  if (!isTRUE(has_rmd)) {
    return("not started")
  }

  if (html_count > 0 && (plot_count > 0 || table_count > 0)) {
    return("completed")
  }

  "in progress"
}

gdrive_talent_report_status_reason <- function(has_rmd, html_count, plot_count, table_count) {
  if (!isTRUE(has_rmd)) {
    return("No report Rmd found under templates/reports.")
  }

  if (html_count > 0 && (plot_count > 0 || table_count > 0)) {
    return("Template Rmd, test HTML, and template artifacts are present.")
  }

  "Template Rmd exists, but test HTML or template artifacts are missing."
}

gdrive_talent_build_reports_table <- function(
  repo_root = gdrive_talent_repo_root(),
  datalake_root = NULL,
  include_render_counts = TRUE
) {
  gdrive_talent_source_repo_helpers(repo_root)

  if (is.null(datalake_root) || !nzchar(datalake_root)) {
    datalake_root <- get_datalake_root()
  }

  desc_paths <- list.files(
    file.path(repo_root, "r_scripts", "run"),
    pattern = "(?i)desc\\.md$",
    full.names = TRUE,
    recursive = TRUE
  )
  desc_paths <- desc_paths[grepl("(?i)/bundle[_A-Za-z0-9]+/", normalizePath(desc_paths, winslash = "/", mustWork = FALSE))]
  if (length(desc_paths) == 0) {
    return(gdrive_talent_empty_df(c(
      "active",
      "report_id",
      "report_name",
      "status",
      "brief_description",
      "desc_path",
      "template_dir",
      "template_rmd",
      "rendered_report_count",
      "latest_rendered_report",
      "latest_rendered_at",
      "status_reason",
      "updated_at",
      "notes"
    )))
  }

  templates <- gdrive_talent_report_template_inventory(repo_root)
  rows <- vector("list", length(desc_paths))
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")

  for (i in seq_along(desc_paths)) {
    desc_path <- normalizePath(desc_paths[[i]], winslash = "/", mustWork = FALSE)
    run_dir <- basename(dirname(desc_path))
    report_id <- gdrive_talent_report_key(run_dir)
    compact_key <- gdrive_talent_report_key_compact(report_id)
    template_match <- which(templates$template_key == compact_key)
    template <- if (length(template_match) > 0) templates[template_match[[1]], , drop = FALSE] else NULL

    has_rmd <- !is.null(template) && nzchar(template$template_rmd[[1]])
    html_count <- if (is.null(template)) 0L else template$template_html_count[[1]]
    plot_count <- if (is.null(template)) 0L else template$template_plot_count[[1]]
    table_count <- if (is.null(template)) 0L else template$template_table_count[[1]]
    render_counts <- if (isTRUE(include_render_counts)) {
      gdrive_talent_report_render_counts(datalake_root, report_id)
    } else {
      list(rendered_report_count = 0L, latest_rendered_report = "", latest_rendered_at = "")
    }

    rows[[i]] <- data.frame(
      active = TRUE,
      report_id = report_id,
      report_name = gdrive_talent_report_title(report_id),
      status = gdrive_talent_report_status(has_rmd, html_count, plot_count, table_count),
      brief_description = gdrive_talent_extract_report_description(gdrive_talent_read_report_desc(desc_path)),
      desc_path = desc_path,
      template_dir = if (is.null(template)) "" else template$template_dir[[1]],
      template_rmd = if (is.null(template)) "" else template$template_rmd[[1]],
      rendered_report_count = render_counts$rendered_report_count,
      latest_rendered_report = render_counts$latest_rendered_report,
      latest_rendered_at = render_counts$latest_rendered_at,
      status_reason = gdrive_talent_report_status_reason(has_rmd, html_count, plot_count, table_count),
      updated_at = now,
      notes = "",
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$report_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

gdrive_talent_existing_report_key <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  report_id <- if ("report_id" %in% names(x)) gdrive_talent_chr(x$report_id) else rep("", nrow(x))
  report_name <- if ("report_name" %in% names(x)) gdrive_talent_chr(x$report_name) else rep("", nrow(x))
  fallback <- gdrive_talent_report_key(report_name)
  ifelse(nzchar(report_id), report_id, fallback)
}

gdrive_talent_merge_existing_reports <- function(
  table,
  existing,
  preserve_cols = c("active", "notes")
) {
  if (is.null(existing) || nrow(existing) == 0 || length(preserve_cols) == 0) {
    return(table)
  }

  existing <- as.data.frame(existing, stringsAsFactors = FALSE)
  preserve_cols <- intersect(preserve_cols, intersect(names(table), names(existing)))
  if (length(preserve_cols) == 0) {
    return(table)
  }

  table_key <- gdrive_talent_existing_report_key(table)
  existing_key <- gdrive_talent_existing_report_key(existing)

  for (col in preserve_cols) {
    for (i in seq_len(nrow(table))) {
      match_idx <- which(existing_key == table_key[[i]])
      if (length(match_idx) == 0) next

      value <- existing[[col]][[match_idx[[1]]]]
      if (is.null(value) || length(value) == 0 || is.na(value)) next

      if (identical(col, "active")) {
        table[[col]][[i]] <- gdrive_talent_bool(value)
      } else if (nzchar(trimws(as.character(value)))) {
        table[[col]][[i]] <- as.character(value)
      }
    }
  }

  table
}

gdrive_talent_write_reports_sheet <- function(
  sheet_id,
  worksheet = "reports",
  repo_root = gdrive_talent_repo_root(),
  datalake_root = NULL,
  include_render_counts = TRUE,
  preserve_manual_columns = TRUE,
  manual_columns = c("active", "notes"),
  create_named_ranges = TRUE,
  auth = FALSE,
  google_email = Sys.getenv("TALENT_GDRIVE_EMAIL", unset = ""),
  auth_cache = Sys.getenv("TALENT_GDRIVE_AUTH_CACHE", unset = "~/.cache/gargle")
) {
  gdrive_talent_assert_packages("googlesheets4")
  gdrive_talent_source_repo_helpers(repo_root)

  if (isTRUE(auth)) {
    gdrive_talent_auth_sheets(
      google_email = google_email,
      auth_cache = auth_cache,
      repo_root = repo_root
    )
  }

  table <- gdrive_talent_build_reports_table(
    repo_root = repo_root,
    datalake_root = datalake_root,
    include_render_counts = include_render_counts
  )

  normalized_sheet_id <- gdrive_talent_normalize_sheet_id(sheet_id)
  if (isTRUE(preserve_manual_columns)) {
    existing <- tryCatch(
      googlesheets4::read_sheet(
        ss = normalized_sheet_id,
        sheet = worksheet,
        col_types = "c"
      ),
      error = function(e) NULL
    )
    table <- gdrive_talent_merge_existing_reports(
      table = table,
      existing = existing,
      preserve_cols = manual_columns
    )
  }

  googlesheets4::sheet_write(
    data = table,
    ss = normalized_sheet_id,
    sheet = worksheet
  )

  if (isTRUE(create_named_ranges)) {
    gdrive_talent_upsert_named_range(
      sheet_id = normalized_sheet_id,
      worksheet = worksheet,
      named_range = "Report_id",
      column_name = "report_id",
      data = table,
      include_header = FALSE
    )
    gdrive_talent_upsert_named_range(
      sheet_id = normalized_sheet_id,
      worksheet = worksheet,
      named_range = "Report_name",
      column_name = "report_name",
      data = table,
      include_header = FALSE
    )
  }

  invisible(table)
}
