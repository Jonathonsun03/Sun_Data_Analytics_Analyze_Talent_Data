ss_build_snippet <- function(df) {
  required_cols <- c("timecode", "speaker", "text")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  df %>%
    dplyr::filter(!is.na(text), text != "") %>%
    dplyr::mutate(line = paste0("[", timecode, "] ", speaker, ": ", text)) %>%
    dplyr::pull(line) %>%
    paste(collapse = "\n")
}

ss_make_output_path <- function(summary_root, source_title, prompt_md5) {
  safe_title <- ss_safe_title(source_title)
  file.path(
    summary_root,
    paste0(safe_title, "_summary_", substr(prompt_md5, 1, 8), ".md")
  )
}

ss_summarize_one <- function(
    source_file,
    source_title,
    system_prompt,
    user_prompt,
    model_name,
    prompt_path,
    prompt_md5,
    output_path
) {
  tryCatch(
    {
      file <- read.csv(source_file)
      snippet <- ss_build_snippet(file)
      messages <- chatgpt_make_messages(
        system_prompt = system_prompt,
        user_prompt = user_prompt,
        data_text = snippet
      )
      response <- chatgpt_send_chat(
        messages = messages,
        model = model_name
      )
      text_out <- chatgpt_extract_text(response)

      md_lines <- c(
        paste0("# Summary: ", source_title),
        "",
        paste0("- Source file: `", source_file, "`"),
        paste0("- Model: `", model_name, "`"),
        paste0("- Prompt md5: `", prompt_md5, "`"),
        paste0("- Prompt path: `", prompt_path, "`"),
        paste0("- Generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        if (!is.null(response$usage)) paste0("- Prompt tokens: ", response$usage$prompt_tokens) else NULL,
        if (!is.null(response$usage)) paste0("- Completion tokens: ", response$usage$completion_tokens) else NULL,
        if (!is.null(response$usage)) paste0("- Total tokens: ", response$usage$total_tokens) else NULL,
        "",
        "## Output",
        "",
        text_out
      )
      writeLines(md_lines, con = output_path, useBytes = TRUE)

      list(
        status = "success",
        error_message = NA_character_,
        prompt_tokens = if (!is.null(response$usage)) response$usage$prompt_tokens else NA_real_,
        completion_tokens = if (!is.null(response$usage)) response$usage$completion_tokens else NA_real_,
        total_tokens = if (!is.null(response$usage)) response$usage$total_tokens else NA_real_
      )
    },
    error = function(e) {
      list(
        status = "error",
        error_message = conditionMessage(e),
        prompt_tokens = NA_real_,
        completion_tokens = NA_real_,
        total_tokens = NA_real_
      )
    }
  )
}

ss_run <- function(opts) {
  datalake_root <- get_datalake_root()
  db_filename <- if (!is.null(opts$db_filename) && nzchar(opts$db_filename)) opts$db_filename else "stream_summaries.duckdb"
  db_path <- file.path(datalake_root, db_filename)
  input_dir <- here::here(datalake_root, opts$talent_project, opts$input_subdir)
  summary_root <- here::here(datalake_root, opts$talent_project, opts$output_subdir)
  dir.create(summary_root, recursive = TRUE, showWarnings = FALSE)

  if (is.null(opts$prompt_path) || !nzchar(opts$prompt_path)) {
    stop(
      paste(
        "Legacy stream-summary runner requires --prompt-path.",
        "Prompt specs for maintained Codex workflows now live under prompts/, organized to mirror bin/linux/codex_prompts/."
      )
    )
  }
  if (!file.exists(opts$prompt_path)) {
    stop("Prompt file not found: ", opts$prompt_path)
  }

  prompt_parts <- ss_load_prompt_parts(opts$prompt_path)
  con <- duckdb_connect(datalake_root = datalake_root, db_path = db_path)
  on.exit(
    tryCatch(DBI::dbDisconnect(con, shutdown = FALSE), error = function(e) NULL),
    add = TRUE
  )

  ss_init_db(con)
  to_stage <- ss_stage_sources(con, input_dir)
  pending <- ss_get_pending_sources(
    con = con,
    prompt_md5 = prompt_parts$prompt_md5,
    model_name = opts$model,
    force_resummarize = opts$force_resummarize
  )

  if (nzchar(opts$one_stream)) {
    target <- opts$one_stream
    pending <- pending %>%
      dplyr::filter(
        source_file == target |
          basename(source_file) == target |
          tools::file_path_sans_ext(basename(source_file)) == target
      )
  }

  if (!is.null(opts$limit) && as.integer(opts$limit) > 0L) {
    pending <- dplyr::slice_head(pending, n = as.integer(opts$limit))
  }

  message("Total streams found: ", nrow(to_stage))
  message("Pending streams: ", nrow(pending))
  if (nrow(pending) == 0) {
    message("No pending streams to summarize for this prompt/model.")
    return(invisible(list(total = nrow(to_stage), pending = 0L, success = 0L, failed = 0L)))
  }

  ok <- 0L
  fail <- 0L
  for (i in seq_len(nrow(pending))) {
    row <- pending[i, , drop = FALSE]
    source_file <- row$source_file[[1]]
    source_title <- row$source_title[[1]]
    output_path <- ss_make_output_path(summary_root, source_title, prompt_parts$prompt_md5)

    message("[", i, "/", nrow(pending), "] Summarizing: ", basename(source_file))
    result <- ss_summarize_one(
      source_file = source_file,
      source_title = source_title,
      system_prompt = prompt_parts$system_prompt,
      user_prompt = prompt_parts$user_prompt,
      model_name = opts$model,
      prompt_path = opts$prompt_path,
      prompt_md5 = prompt_parts$prompt_md5,
      output_path = output_path
    )

    ss_record_result(
      con = con,
      talent_project = opts$talent_project,
      row = row,
      prompt_path = opts$prompt_path,
      prompt_md5 = prompt_parts$prompt_md5,
      model_name = opts$model,
      output_path = output_path,
      result = result
    )

    if (identical(result$status, "success")) {
      ok <- ok + 1L
      message("saved_markdown=", output_path)
    } else {
      fail <- fail + 1L
      message("failed: ", basename(source_file), " :: ", result$error_message)
    }
  }

  message("Run complete: success=", ok, " failed=", fail)
  invisible(list(total = nrow(to_stage), pending = nrow(pending), success = ok, failed = fail))
}
