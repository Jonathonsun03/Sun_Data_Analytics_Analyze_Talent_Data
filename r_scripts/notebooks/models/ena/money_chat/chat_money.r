library(here)
library(readr)
library(dplyr)
library(purrr)
library(rENA)
library(ggplot2)


# Setup -------------------------------------------------------------------

source(here::here("r_scripts", "lib", "utils", "source_dir.r"))
source(here::here("r_scripts", "lib", "utils", "datalake_root.r"))

source(here::here("r_scripts", "lib", "clean_data", "CleanData.R"))
source(here::here("r_scripts", "lib", "import_data", "text_playback_streams.R"))
source(here::here("r_scripts", "lib", "transcript_analysis", "money_stream_selection.R"))

# Inputs ------------------------------------------------------------------

# Set this directly for interactive use, or set TALENT_QUERY before running.
# Use a comma-separated value for several talents, e.g. "Nova,Avaritia".
# Use "all" intentionally to scan every talent with text_playback files.
Talent <- Sys.getenv("TALENT_QUERY", unset = "Leia")
content_type_filter <- "live"
min_paid_events_for_average_sample <- 5

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


# Runner ------------------------------------------------------------------

run_money_chat_quantile_analysis <- function(
  talent,
  content_type = content_type_filter,
  min_paid_events = min_paid_events_for_average_sample,
  candidate_n = 25,
  histogram_binwidth = 0.5
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

  if (nrow(outputs$stream_metrics) == 0L) {
    warning("No paid streams met the minimum paid-event threshold for: ", talent)
    return(outputs)
  }

  print(outputs$summary)
  print(outputs$quantile_table)
  print(
    outputs$labeled_streams %>%
      count(giving_quantile_label, sort = TRUE)
  )
  print(outputs$average_giving_candidates_print, n = candidate_n)
  print(outputs$plot)

  outputs
}


# Run ---------------------------------------------------------------------

selected_talents <- parse_talent_input(Talent)

# Manual step-through for one talent:
# selected_talent <- selected_talents[[1]]
# money_streams <- summarize_text_playback_money(
#   talent = selected_talent,
#   content_type = content_type_filter,
#   paid_only = FALSE
# )
# paid_streams <- filter_paid_money_streams(money_streams)
# stream_metrics <- build_paid_stream_metrics(
#   money_streams,
#   min_paid_events = min_paid_events_for_average_sample
# )
# average_giving_selection <- select_average_giving_streams(
#   stream_metrics,
#   min_paid_events = min_paid_events_for_average_sample
# )
# labeled_streams <- label_paid_stream_quantiles(
#   stream_metrics,
#   average_giving_selection$quantiles
# )
# average_giving_candidate_streams <- average_giving_selection$candidates
# money_summary <- summarize_money_stream_selection(
#   money_streams,
#   stream_metrics,
#   talent = selected_talent
# )
# money_plot <- plot_average_paid_message_distribution(
#   stream_metrics,
#   average_giving_selection$quantile_table,
#   average_giving_selection$quantiles,
#   talent = selected_talent,
#   min_paid_events = min_paid_events_for_average_sample
# )

money_chat_results <- selected_talents %>%
  set_names() %>%
  map(run_money_chat_quantile_analysis)

money_chat_summary <- money_chat_results %>%
  map_dfr("summary")

average_giving_candidate_streams <- money_chat_results %>%
  map_dfr(~ .x$average_giving_candidate_streams)

labeled_streams <- money_chat_results %>%
  map_dfr(~ .x$labeled_streams)

money_chat_summary
