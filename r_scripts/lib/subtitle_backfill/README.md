# Subtitle backfill library

Backfill functions are separated by responsibility:

| File | Responsibility |
| --- | --- |
| `subtitle_backfill_tracks.R` | Lists noncurrent tracks, loads sources, and rechecks checksums. |
| `subtitle_backfill_checkpoints.R` | Builds, loads, validates, and saves checkpoints. |
| `subtitle_backfill_database.R` | Opens short write connections, retries locks, and records runs. |
| `subtitle_backfill_inference.R` | Runs or reuses punctuation for each block. |
| `subtitle_backfill_speakers.R` | Uses Qwen 3B to infer missing sentence boundaries inside explicit turns. |
| `subtitle_backfill_reconstruction.R` | Validates and publishes one reconstructed video. |
| `subtitle_backfill_runtime.R` | Loads runner settings and waits on collection. |
| `subtitle_backfill_batch.R` | Decides whether one candidate video needs reconstruction. |

The executable entrypoint is
`r_scripts/run/Subtitle_clean/backfill_subtitle_sentences.R`. The public Bash
wrapper is `bin/linux/subtitles/run_subtitle_sentence_backfill.sh`. Executable
batches with pending work perform the inference API readiness and conditional
wake-on-LAN check once before entering the per-video loop.

Each track actually entered by a batch is recorded in
`ops.subtitle_backfill_attempts` before processing starts and updated with its
terminal status, metrics, publication run ID, and error. This is the canonical
batch-membership source for operational reporting; the aggregate
`ops.pipeline_runs.error_summary` remains a concise run summary.

The per-video flow is:

1. `subtitle_backfill_tracks.R` computes source checksums in the initial DuckDB
   query and excludes tracks already current for that checksum and pipeline.
2. `subtitle_backfill_batch.R` loads each selected source track, rechecks its
   current status, and loads database checkpoints.
3. `subtitle_backfill_inference.R` reuses checkpoints or requests missing
   punctuation blocks, saving each successful response through
   `subtitle_backfill_checkpoints.R`.
4. `subtitle_backfill_database.R` opens a short write connection so the
   reconstruction can validate and publish through
   `r_scripts/lib/duckdb/subtitle_sentence_publish.R`.

After punctuation reconstruction, Qwen 3B evaluates adjacent sentences only
inside the same source-derived turn. Explicit `>>` boundaries are always
preserved. The cumulative result is published as `inferred_speaker_turn_id`;
`speaker_turn_id` and `speaker_change` retain the source-caption interpretation.

The shared DuckDB table definitions remain in
`r_scripts/lib/duckdb/subtitle_sentence_schema.R` because they define canonical
storage rather than backfill execution.
