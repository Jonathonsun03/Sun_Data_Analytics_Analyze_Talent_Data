# transcript_analysis

Reusable analysis helpers for transcript-level selection, summarization, and sampling.

## Money Stream Selection

Defined in `money_stream_selection.R`.

Use these helpers with the text playback import helpers:

```r
source(here::here("r_scripts", "lib", "clean_data", "CleanData.R"))
source(here::here("r_scripts", "lib", "import_data", "text_playback_streams.R"))
source(here::here("r_scripts", "lib", "transcript_analysis", "money_stream_selection.R"))
```

Identify transcripts with positive paid chat value:

```r
nova_money_streams <- summarize_text_playback_money(
  talent = "Nova",
  content_type = "live",
  paid_only = TRUE
)

nova_money_streams %>%
  dplyr::select(title_raw, paid_event_count, total_paid_amount_usd, paid_currencies, source_path)
```

Select middle-50% average-giving transcript candidates:

```r
nova_paid_stream_metrics <- nova_money_streams %>%
  dplyr::mutate(average_paid_message_amount = total_paid_amount_usd / paid_event_count)

average_giving_selection <- select_average_giving_streams(
  nova_paid_stream_metrics,
  min_paid_events = 5
)

average_giving_selection$quantile_table
average_giving_selection$candidates
```

To scan every talent with a `text_playback` folder, run this intentionally because it reads every transcript CSV:

```r
all_paid_streams <- summarize_all_text_playback_money(
  content_type = "live",
  paid_only = TRUE
)
```
