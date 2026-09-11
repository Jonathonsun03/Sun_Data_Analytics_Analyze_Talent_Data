# Subtitle backfill library

Backfill functions are separated by responsibility:

| File | Responsibility |
| --- | --- |
| `subtitle_backfill_tracks.R` | Lists, loads, and checks source tracks. |
| `subtitle_backfill_checkpoints.R` | Builds, loads, validates, and saves checkpoints. |
| `subtitle_backfill_database.R` | Opens short write connections, retries locks, and records runs. |
| `subtitle_backfill_inference.R` | Runs or reuses punctuation for each block. |
| `subtitle_backfill_reconstruction.R` | Validates and publishes one reconstructed video. |
| `subtitle_backfill_runtime.R` | Loads runner settings and waits on collection. |
| `subtitle_backfill_batch.R` | Decides whether one candidate video needs reconstruction. |

The executable entrypoint is
`r_scripts/run/Subtitle_clean/backfill_subtitle_sentences.R`. The public Bash
wrapper is `bin/linux/subtitles/run_subtitle_sentence_backfill.sh`.

The per-video flow is:

1. `subtitle_backfill_batch.R` loads the source track, current status, and
   database checkpoints through `subtitle_backfill_tracks.R`.
2. `subtitle_backfill_inference.R` reuses checkpoints or requests missing
   punctuation blocks, saving each successful response through
   `subtitle_backfill_checkpoints.R`.
3. `subtitle_backfill_database.R` opens a short write connection so the
   reconstruction can validate and publish through
   `r_scripts/lib/duckdb/subtitle_sentence_publish.R`.

The shared DuckDB table definitions remain in
`r_scripts/lib/duckdb/subtitle_sentence_schema.R` because they define canonical
storage rather than backfill execution.
