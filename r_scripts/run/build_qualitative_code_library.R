#!/usr/bin/env Rscript

source("r_scripts/lib/ENA_prep/shared/ena_precoding_paths.R")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(trimws(x))) {
    return(y)
  }
  x
}

read_csv_base <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
}

value <- function(row, names, default = "") {
  for (name in names) {
    if (name %in% names(row)) {
      val <- row[[name]]
      if (!is.na(val) && nzchar(trimws(as.character(val)))) {
        return(trimws(as.character(val)))
      }
    }
  }
  default
}

make_code_column <- function(code_id) {
  code_id <- trimws(as.character(code_id))
  if (!nzchar(code_id)) {
    return("")
  }
  paste0("code_", code_id)
}

infer_analytic_object <- function(source_group) {
  if (source_group %in% c("chat_current", "chat_shared_baseline", "talent_chat_open_coding")) {
    return("chat")
  }
  "streamer"
}

infer_row_source_scope <- function(source_group) {
  if (infer_analytic_object(source_group) == "chat") {
    return("chat")
  }
  "subtitle"
}

normalize_rows <- function(df, source_group, source_label, source_path) {
  if (is.null(df) || nrow(df) == 0L) {
    return(data.frame())
  }

  rows <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]

    primary_id <- value(row, c("primary_code_id", "Primary Code ID", "shared_code_id"))
    primary_code <- value(row, c("primary_code", "Primary Code", "shared_code_name"))
    secondary_id <- value(row, c("secondary_code_id", "Secondary Code ID", "code_id"))
    secondary_code <- value(row, c("secondary_code", "Secondary Code", "code_name"))
    code_id <- value(row, c("code_id", "Code ID"))

    if (!nzchar(code_id)) {
      code_id <- if (nzchar(secondary_id)) secondary_id else primary_id
    }

    code_column <- value(row, c("code_column", "Code Column"))
    if (!nzchar(code_column)) {
      code_column <- make_code_column(code_id)
    }

    data.frame(
      selected_for_processing = FALSE,
      source_group = source_group,
      source_label = source_label,
      source_path = normalizePath(source_path, winslash = "/", mustWork = FALSE),
      code_id = code_id,
      code_column = code_column,
      primary_code_id = primary_id,
      primary_code = primary_code,
      secondary_code_id = secondary_id,
      secondary_code = secondary_code,
      code_name = value(row, c("code_name", "shared_code_name"), secondary_code %||% primary_code),
      analytic_object = infer_analytic_object(source_group),
      row_source_scope = infer_row_source_scope(source_group),
      definition = value(row, c("definition", "Definition")),
      inclusion_criteria = value(row, c("inclusion_criteria", "Inclusion Criteria")),
      exclusion_criteria = value(row, c("exclusion_criteria", "Exclusion Criteria")),
      interactional_function = value(row, c("interactional_function", "common_interactional_function")),
      examples_from_text = value(row, c("examples_from_text", "Examples from text", "example_quote")),
      talents_observed = value(row, c("talents_observed", "talents_observed", "talent")),
      status = value(row, c("status"), "active"),
      notes = value(row, c("notes", "Notes")),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out <- out[nzchar(out$code_id) | nzchar(out$definition), , drop = FALSE]
  row.names(out) <- NULL
  out
}

discover_sources <- function(talent_data_root = NULL) {
  talent_data_root <- ena_precoding_talent_data_root(talent_data_root)
  processed_root <- ena_precoding_processed_root(talent_data_root)

  sources <- data.frame(
    source_group = character(),
    source_label = character(),
    source_path = character(),
    stringsAsFactors = FALSE
  )

  add_source <- function(source_group, source_label, source_path) {
    if (file.exists(source_path)) {
      sources <<- rbind(
        sources,
        data.frame(
          source_group = source_group,
          source_label = source_label,
          source_path = source_path,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  add_source(
    "streamer_current",
    "Current streamer/personality qualitative codebook",
    file.path(processed_root, "Qualitative Codebook", "current", "personality_qualitative_code_log.csv")
  )
  add_source(
    "streamer_compiled_current",
    "Compiled streamer codebook copy used by qualitative prep",
    file.path(processed_root, "qualitative_prep", "_shared", "codebooks", "personality_current.csv")
  )
  add_source(
    "chat_current",
    "Current cumulative chat qualitative codebook",
    file.path(processed_root, "Chat Qualitative Codebook", "current", "chat_personality_qualitative_code_log.csv")
  )
  add_source(
    "chat_shared_baseline",
    "Current shared chat behavior baseline codebook",
    file.path(processed_root, "chat_shared_interactions", "current", "chat_shared_behavior_codebook.csv")
  )

  talent_codebooks <- list.files(
    talent_data_root,
    pattern = "^chat_open_codebook\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  talent_codebooks <- talent_codebooks[
    grepl("/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_codebook[.]csv$", talent_codebooks)
  ]

  for (path in sort(talent_codebooks)) {
    root_norm <- normalizePath(talent_data_root, winslash = "/", mustWork = FALSE)
    path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
    talent_name <- if (startsWith(path_norm, paste0(root_norm, "/"))) {
      substring(path_norm, nchar(root_norm) + 2L)
    } else {
      path_norm
    }
    talent_name <- strsplit(talent_name, "/", fixed = TRUE)[[1]][1]
    add_source(
      "talent_chat_open_coding",
      paste0("Talent chat open-coding codebook: ", talent_name),
      path
    )
  }

  sources
}

write_markdown_summary <- function(path, library_df, source_df) {
  lines <- c(
    "# Qualitative Code Library",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "This inventory normalizes the currently available streamer and chat qualitative codebooks into one reviewable table.",
    "",
    "## Sources",
    ""
  )

  if (nrow(source_df) == 0L) {
    lines <- c(lines, "- No source codebooks found.")
  } else {
    lines <- c(
      lines,
      paste0("- ", source_df$source_group, ": ", source_df$source_path)
    )
  }

  counts <- aggregate(
    code_id ~ source_group + analytic_object + row_source_scope,
    data = library_df,
    FUN = length
  )
  names(counts)[names(counts) == "code_id"] <- "code_count"

  lines <- c(lines, "", "## Counts", "")
  if (nrow(counts) == 0L) {
    lines <- c(lines, "- No codes found.")
  } else {
    lines <- c(lines, "| source_group | analytic_object | row_source_scope | code_count |")
    lines <- c(lines, "| --- | --- | --- | --- |")
    for (i in seq_len(nrow(counts))) {
      lines <- c(
        lines,
        paste0(
          "| ", counts$source_group[[i]],
          " | ", counts$analytic_object[[i]],
          " | ", counts$row_source_scope[[i]],
          " | ", counts$code_count[[i]],
          " |"
        )
      )
    }
  }

  lines <- c(
    lines,
    "",
    "## Review Workflow",
    "",
    "Use `qualitative_code_library.csv` as the review table. Set `selected_for_processing` to TRUE for codes you want included in the combined coding pass, then use that edited selection file as the next pipeline input."
  )

  writeLines(lines, path, useBytes = TRUE)
}

parse_args <- function(args) {
  out <- list(
    talent_data_root = NULL,
    output_dir = NULL
  )
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--talent-data-root") {
      out$talent_data_root <- args[[i + 1L]]
      i <- i + 2L
    } else if (arg == "--output-dir") {
      out$output_dir <- args[[i + 1L]]
      i <- i + 2L
    } else if (arg %in% c("-h", "--help")) {
      cat(
        "Usage: Rscript r_scripts/run/build_qualitative_code_library.R [--talent-data-root PATH] [--output-dir PATH]\n",
        sep = ""
      )
      quit(save = "no", status = 0)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
  }
  out
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  talent_data_root <- ena_precoding_talent_data_root(args$talent_data_root)
  processed_root <- ena_precoding_processed_root(talent_data_root)
  output_dir <- args$output_dir %||% file.path(
    processed_root,
    "qualitative_code_library",
    "current"
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  sources <- discover_sources(talent_data_root)
  normalized <- lapply(seq_len(nrow(sources)), function(i) {
    df <- read_csv_base(sources$source_path[[i]])
    normalize_rows(
      df = df,
      source_group = sources$source_group[[i]],
      source_label = sources$source_label[[i]],
      source_path = sources$source_path[[i]]
    )
  })
  library_df <- do.call(rbind, normalized)

  if (is.null(library_df) || nrow(library_df) == 0L) {
    library_df <- data.frame(
      selected_for_processing = logical(),
      source_group = character(),
      source_label = character(),
      source_path = character(),
      code_id = character(),
      code_column = character(),
      primary_code_id = character(),
      primary_code = character(),
      secondary_code_id = character(),
      secondary_code = character(),
      code_name = character(),
      analytic_object = character(),
      row_source_scope = character(),
      definition = character(),
      inclusion_criteria = character(),
      exclusion_criteria = character(),
      interactional_function = character(),
      examples_from_text = character(),
      talents_observed = character(),
      status = character(),
      notes = character(),
      stringsAsFactors = FALSE
    )
  }

  library_df <- library_df[order(
    library_df$analytic_object,
    library_df$source_group,
    library_df$primary_code_id,
    library_df$secondary_code_id,
    library_df$code_id
  ), , drop = FALSE]
  row.names(library_df) <- NULL

  library_path <- file.path(output_dir, "qualitative_code_library.csv")
  sources_path <- file.path(output_dir, "qualitative_code_library_sources.csv")
  summary_path <- file.path(output_dir, "qualitative_code_library.md")

  write.csv(library_df, library_path, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
  write.csv(sources, sources_path, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
  write_markdown_summary(summary_path, library_df, sources)

  cat("Qualitative code library written:\n")
  cat("- ", normalizePath(library_path, winslash = "/", mustWork = FALSE), "\n", sep = "")
  cat("- ", normalizePath(summary_path, winslash = "/", mustWork = FALSE), "\n", sep = "")
  cat("- ", normalizePath(sources_path, winslash = "/", mustWork = FALSE), "\n", sep = "")
  cat("Codes discovered: ", nrow(library_df), "\n", sep = "")
}

main()
