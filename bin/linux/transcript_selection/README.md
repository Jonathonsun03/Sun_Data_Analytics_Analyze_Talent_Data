# Transcript Selection

Use this wrapper to select money-chat transcript candidates for later qualitative coding.

The default output location is:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/qualitative_coding/selected_transcripts/<run_id>/
```

This keeps the files out of raw `Talent_data` because the outputs are derived selection metadata. The selected transcript table keeps `source_path`, so later qualitative batch tooling can read the original transcript without copying or changing raw files.

## Examples

```bash
bin/linux/transcript_selection/select_money_chat_transcripts.sh --talent Nova
```

```bash
bin/linux/transcript_selection/select_money_chat_transcripts.sh \
  --talent "Nova,Avaritia" \
  --min-paid-events 5 \
  --candidate-n 25
```

```bash
bin/linux/transcript_selection/select_money_chat_transcripts.sh \
  --talent all \
  --content-type live \
  --out-dir /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/qualitative_coding/selected_transcripts
```

## Outputs

- `selected_transcripts.csv`: the middle-50-percent average-giving candidate streams, ranked closest to median first.
- `labeled_streams.csv`: all eligible paid streams labeled `low_giving`, `average_giving`, or `high_giving`.
- `average_paid_message_quantiles.csv`: Q1, median, and Q3 per talent.
- `money_chat_summary.csv`: per-talent transcript and paid-message summary.
- `run_manifest.csv`: parameters and output path for the run.
- `plots/`: optional histogram PNGs with quantile lines.
