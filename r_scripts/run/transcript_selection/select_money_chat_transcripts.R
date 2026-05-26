usage <- function() {
  cat(
    paste(
      "Usage:",
      "  bin/linux/transcript_selection/select_money_chat_transcripts.sh [options]",
      "",
      "Options:",
      "  --talent VALUE            Talent query, comma-separated talents, or all. Default: TALENT_QUERY env or Nova",
      "  --content-type VALUE      Title content type filter. Default: live",
      "  --min-paid-events N       Minimum paid events required for average-giving selection. Default: 5",
      "  --candidate-n N           Number of average-giving candidates to keep per talent. Default: 25",
      "  --histogram-binwidth N    Histogram bin width. Default: 0.5",
      "  --out-dir PATH            Output directory. Default: processed DataLake selected_transcripts folder",
      "  --run-id VALUE            Output subfolder name. Default: timestamped run id",
      "  --no-plots                Do not write histogram PNGs",
      "  --help                    Show this help",
      "",
      sep = "\n"
    )
  )
}

parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- list(
    talent = Sys.getenv("TALENT_QUERY", unset = "Nova"),
    content_type = "live",
    min_paid_events = 5L,
    candidate_n = 25L,
    histogram_binwidth = 0.5,
    out_dir = NA_character_,
    run_id = NA_character_,
    write_plots = TRUE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg %in% c("--help", "-h")) {
      usage()
      quit(save = "no", status = 0)
    }

    if (arg == "--no-plots") {
      opts$write_plots <- FALSE
      i <- i + 1L
      next
    }

    if (!startsWith(arg, "--")) {
      stop("Unexpected argument: ", arg, call. = FALSE)
    }

    if (i == length(args)) {
      stop("Missing value for argument: ", arg, call. = FALSE)
    }

    value <- args[[i + 1L]]
    if (arg == "--talent") opts$talent <- value
    else if (arg == "--content-type") opts$content_type <- value
    else if (arg == "--min-paid-events") opts$min_paid_events <- as.integer(value)
    else if (arg == "--candidate-n") opts$candidate_n <- as.integer(value)
    else if (arg == "--histogram-binwidth") opts$histogram_binwidth <- as.numeric(value)
    else if (arg == "--out-dir") opts$out_dir <- value
    else if (arg == "--run-id") opts$run_id <- value
    else stop("Unknown argument: ", arg, call. = FALSE)

    i <- i + 2L
  }

  opts
}

opts <- parse_args()

library(here)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)

source(here::here("r_scripts", "lib", "utils", "datalake_root.r"))
source(here::here("r_scripts", "lib", "clean_data", "CleanData.R"))
source(here::here("r_scripts", "lib", "import_data", "text_playback_streams.R"))
source(here::here("r_scripts", "lib", "transcript_analysis", "money_stream_selection.R"))

processed_sun_data_root <- function() {
  talent_root <- normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE)
  sub("/Talent_data/?$", "/Processed", talent_root)
}

default_selected_transcript_dir <- function() {
  file.path(
    processed_sun_data_root(),
    "qualitative_coding",
    "selected_transcripts"
  )
}

safe_file_slug <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "unknown")
}

parse_talent_input <- function(talent_input) {
  talent_input <- trimws(talent_input)
  if (!nzchar(talent_input)) {
    stop("Talent input is empty.", call. = FALSE)
  }

  if (tolower(talent_input) %in% c("all", "*")) {
    return(list_text_playback_talents()$talent_name)
  }

  trimws(strsplit(talent_input, ",", fixed = TRUE)[[1]])
}

write_csv_if_present <- function(df, path) {
  readr::write_csv(df, path, na = "")
  invisible(path)
}

run_one_talent <- function(
  talent,
  content_type,
  min_paid_events,
  candidate_n,
  histogram_binwidth,
  plot_dir = NULL
) {
  message("Analyzing talent: ", talent)

  money_streams <- summarize_text_playback_money(
    talent = talent,
    content_type = content_type,
    paid_only = FALSE
  )

  outputs <- build_money_chat_quantile_outputs(
    money_streams = money_streams,
    talent = talent,
    min_paid_events = min_paid_events,
    candidate_n = candidate_n,
    histogram_binwidth = histogram_binwidth
  )

  if (!is.null(plot_dir) && !is.null(outputs$plot)) {
    ggplot2::ggsave(
      filename = file.path(plot_dir, paste0(safe_file_slug(talent), "_average_paid_message_histogram.png")),
      plot = outputs$plot,
      width = 10,
      height = 7,
      dpi = 150
    )
  }

  outputs
}

run_selection <- function(opts) {
  selected_talents <- parse_talent_input(opts$talent)
  base_out_dir <- if (is.na(opts$out_dir) || !nzchar(opts$out_dir)) {
    default_selected_transcript_dir()
  } else {
    opts$out_dir
  }
  run_id <- if (is.na(opts$run_id) || !nzchar(opts$run_id)) {
    paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_money_chat_quantile_selection")
  } else {
    opts$run_id
  }

  output_dir <- file.path(base_out_dir, run_id)
  plot_dir <- file.path(output_dir, "plots")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(opts$write_plots)) {
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  }

  message("Writing transcript selection outputs to: ", output_dir)

  results <- selected_talents %>%
    rlang::set_names() %>%
    purrr::map(function(talent) {
      run_one_talent(
        talent = talent,
        content_type = opts$content_type,
        min_paid_events = opts$min_paid_events,
        candidate_n = opts$candidate_n,
        histogram_binwidth = opts$histogram_binwidth,
        plot_dir = if (isTRUE(opts$write_plots)) plot_dir else NULL
      )
    })

  summary_tbl <- purrr::map_dfr(results, "summary")
  quantile_tbl <- purrr::imap_dfr(results, function(result, talent) {
    result$quantile_table %>%
      dplyr::mutate(talent = talent, .before = 1)
  })
  labeled_streams <- purrr::map_dfr(results, "labeled_streams")
  selected_candidates <- purrr::map_dfr(results, "average_giving_candidates_print")

  manifest <- tibble::tibble(
    run_id = run_id,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    talent_input = opts$talent,
    talent_count = length(selected_talents),
    talents = paste(selected_talents, collapse = ", "),
    content_type = opts$content_type,
    min_paid_events = opts$min_paid_events,
    candidate_n = opts$candidate_n,
    histogram_binwidth = opts$histogram_binwidth,
    write_plots = opts$write_plots,
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  )

  write_csv_if_present(manifest, file.path(output_dir, "run_manifest.csv"))
  write_csv_if_present(summary_tbl, file.path(output_dir, "money_chat_summary.csv"))
  write_csv_if_present(quantile_tbl, file.path(output_dir, "average_paid_message_quantiles.csv"))
  write_csv_if_present(labeled_streams, file.path(output_dir, "labeled_streams.csv"))
  write_csv_if_present(selected_candidates, file.path(output_dir, "selected_transcripts.csv"))

  print(summary_tbl)
  print(quantile_tbl)
  print(selected_candidates, n = opts$candidate_n)

  invisible(list(
    output_dir = output_dir,
    manifest = manifest,
    summary = summary_tbl,
    quantiles = quantile_tbl,
    labeled_streams = labeled_streams,
    selected_transcripts = selected_candidates,
    results = results
  ))
}

run_selection(opts)
