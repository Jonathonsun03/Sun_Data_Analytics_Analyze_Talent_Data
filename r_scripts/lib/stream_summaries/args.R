ss_parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- list(
    talent_project = "Leia Memoria【Variance Project】",
    model = "gpt-4.1-mini",
    db_filename = "stream_summaries.duckdb",
    prompt_path = "",
    input_subdir = "text_playback",
    output_subdir = "stream_summary",
    force_resummarize = FALSE,
    limit = 0L,
    one_stream = ""
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--force-resummarize")) {
      opts$force_resummarize <- TRUE
      i <- i + 1L
      next
    }
    if (arg %in% c("-h", "--help")) {
      opts$help <- TRUE
      i <- i + 1L
      next
    }
    if (i == length(args)) {
      stop("Missing value for argument: ", arg)
    }
    val <- args[[i + 1L]]
    if (arg == "--talent-project") {
      opts$talent_project <- val
    } else if (arg == "--model") {
      opts$model <- val
    } else if (arg == "--db-filename") {
      opts$db_filename <- val
    } else if (arg == "--prompt-path") {
      opts$prompt_path <- val
    } else if (arg == "--input-subdir") {
      opts$input_subdir <- val
    } else if (arg == "--output-subdir") {
      opts$output_subdir <- val
    } else if (arg == "--limit") {
      opts$limit <- as.integer(val)
    } else if (arg == "--one-stream") {
      opts$one_stream <- val
    } else {
      stop("Unknown argument: ", arg)
    }
    i <- i + 2L
  }

  opts
}

ss_print_help <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript r_scripts/run/Text_Replay_Analysis/Text_replay_analysis_openAI [options]",
      "",
      "Options:",
      "  --talent-project NAME        Talent project folder name",
      "  --model NAME                 OpenAI model (default: gpt-4.1-mini)",
      "  --db-filename NAME           DuckDB file under datalake root (default: stream_summaries.duckdb)",
      "  --prompt-path PATH           Prompt markdown file with ---SYSTEM---/---USER--- (required for legacy runner)",
      "  --input-subdir NAME          Input subdir under talent project (default: text_playback)",
      "  --output-subdir NAME         Output subdir under talent project (default: stream_summary)",
      "  --one-stream VALUE           Process only one stream file (basename or full path match)",
      "  --limit N                    Limit pending files for smoke tests (0 = no limit)",
      "  --force-resummarize          Ignore pending filter and rerun selected files",
      "  -h, --help                   Show this help",
      sep = "\n"
    )
  )
}
