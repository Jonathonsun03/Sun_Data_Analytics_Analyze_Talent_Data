# Subtitles Runner

## Canonical DuckDB sentence backfill

The maintained full-track sentence backfill reads canonical raw rows from
`text.subtitle_units`, closes DuckDB, and runs all inference for that video.
Each successful block is saved immediately to one persistent RDS checkpoint file
per video. After inference, the runner opens DuckDB, rechecks the source checksum,
and transactionally publishes completed checkpoints and sentence rows to
`text.subtitle_sentence_units` with `source_scope = 'full_track'`.

Inventory the work without model calls or database writes:

```bash
bin/linux/subtitles/run_subtitle_sentence_backfill.sh --dry-run
```

Run a one-video pilot:

```bash
bin/linux/subtitles/run_subtitle_sentence_backfill.sh \
  --execute \
  --video-id VIDEO_ID
```

Start the complete backfill in a detached tmux session:

```bash
bin/linux/subtitles/start_subtitle_sentence_backfill_tmux.sh
```

The launcher prints the tmux session name and persistent DataLake log path.
Use:

```bash
tmux attach -t subtitle-sentence-backfill
```

Detach without stopping the process with `Ctrl+B`, then `D`. Completed tracks
are checksum- and pipeline-version-aware and are skipped on another run.
Completed blocks for an interrupted track are also reused, so restarting the
same command resumes rather than repeating successful FullStop requests.

Failed blocks receive up to three attempts per run by default. Only successful
responses are saved locally; restarting retries unfinished blocks. `--retry-failed`
still resets exhausted legacy failed checkpoints already stored in DuckDB.
Use `--force` only when intentionally rebuilding already-current tracks.

The checkpoint directory defaults to
`<Talent DataLake root>/Processed/subtitle_backfill_checkpoints/`, derived from the
configured lakehouse path. Override it with `SUBTITLE_BACKFILL_CHECKPOINT_DIR` if
needed; use a persistent directory outside the staging tree cleared by refresh.
Each video has one hash-named `.rds` file containing its existing checkpoint rows
and deterministic source/block keys. Writes replace that file atomically. Files
remain after publication for recovery; DuckDB remains the authoritative store.

No DuckDB reads or writes occur inside the per-video inference loop. The final
save retries database lock conflicts up to 12 times with a fixed five-second
sleep, using the existing helper. Other errors fail immediately. If retries are
exhausted, keep the checkpoint file and rerun the same command: successful model
calls are reused. Source changes prevent stale publication. The existing run-log
updates still use short connections at the start and end of the overall run.

DuckDB permits only one process that can write at a time. Close Quarto previews,
interactive R sessions, or dashboards that hold the talent lakehouse open
before starting the backfill.

## `run_subtitle_clean.sh`

Wrapper script for:

- `r_scripts/run/Subtitle_clean/run_subtitle_pipeline.R`

This runs subtitle cleaning for selected talents and writes outputs to each talent's `Subtitles/Processed` + `Subtitles/RData`, plus summary files in:

- `<Talent>/Subtitles/Sentence_Units/<video>_subtitles.parquet`
- `<Processed>/Talent_Data/subtitle_analysis/subtitle_summary.csv`
- `<Processed>/Talent_Data/subtitle_analysis/subtitle_quotes_sample.csv`
- `<Processed>/Talent_Data/subtitle_analysis/subtitle_ena_units.csv`
- `<Processed>/Talent_Data/subtitle_analysis/subtitle_ena_units.txt` (optional)

### Basic usage

Run for all talents:

```bash
bin/linux/subtitles/run_subtitle_clean.sh
```

Run for one talent query:

```bash
bin/linux/subtitles/run_subtitle_clean.sh --talent-query "Avaritia"
```

### Options

- `--talent-query VALUE`
  - Selector passed to `select_talent()` (default: `all`)
- `--datalake-root PATH`
  - Overrides `TALENT_DATALAKE_ROOT`
- `TALENT_PROCESSED_ROOT` (env var)
  - Optional override for processed output root. If not set, defaults to:
  - `dirname(TALENT_DATALAKE_ROOT)/Processed/Talent_Data`
- `--quotes-per-talent N`
  - Sets `SUBTITLE_QUOTES_PER_TALENT` (default: `3`)
- `--context-rows N`
  - Sets `SUBTITLE_CONTEXT_ROWS` (default: `10`)
- `--top-k-sheets N`
  - Sets `SUBTITLE_TOP_K_SHEETS` (default: `1`)
- `--pause-gap-sec N`
  - Sets `SUBTITLE_PAUSE_GAP_SEC` used to insert pause units (default: `2.0`)
- `--n-cores N`
  - Sets `SUBTITLE_N_CORES` for parallel processing across talents / ENA builds (default: `1`)
- `--write-ena-txt`
  - Sets `SUBTITLE_WRITE_ENA_TXT=true` and writes a human-readable ENA text file
- `--reclean`
  - Sets `SUBTITLE_RECLEAN=true` and reprocesses existing subtitle files (`skip_existing = FALSE`)
- `--ena-as-final`
  - Sets `SUBTITLE_ENA_AS_FINAL=true` and writes ENA rows back to each video's
  - `<Talent>/Subtitles/Processed/<video>_subtitles.csv` as the final per-video format
- `SUBTITLE_PUNCTUATION_ENABLED` (env var)
  - Enables the sentence reconstruction stage (default: `true`)
- `SUBTITLE_PUNCTUATION_URL` (env var)
  - Punctuation endpoint (default: `http://192.168.1.173:8000/v1/punctuate`)
- `SUBTITLE_PUNCTUATION_TIMEOUT_SEC` (env var)
  - Per-block HTTP timeout in seconds (default: `120`)
- `SUBTITLE_BLOCK_TARGET_WORDS` / `SUBTITLE_BLOCK_MAX_WORDS` (env vars)
  - Approximate punctuation block size (defaults: `175` / `200`)
- `SUBTITLE_PUNCTUATION_ALLOW_UNKNOWN_LANGUAGE` (env var)
  - Treat legacy cleaned files without language metadata as English (default: `true`)
- `-h`, `--help`
  - Show help

### Example with explicit datalake root

```bash
bin/linux/subtitles/run_subtitle_clean.sh \
  --datalake-root /mnt/datalake/Datalake/Sun_Data_Analytics/Talent_data \
  --talent-query all \
  --pause-gap-sec 2.0 \
  --n-cores 4 \
  --write-ena-txt
```

Force full reclean (overwrite processed subtitle CSVs):

```bash
bin/linux/subtitles/run_subtitle_clean.sh --reclean
```

Force ENA as final per-video output format:

```bash
bin/linux/subtitles/run_subtitle_clean.sh --reclean --ena-as-final
```

### Weekly run (cron example)

Run every Sunday at 02:15:

```cron
15 2 * * 0 cd /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data && bin/linux/subtitles/run_subtitle_clean.sh >> /tmp/subtitle_clean.log 2>&1
```

### Notes

- The pipeline uses `skip_existing = TRUE`, so existing processed subtitle files are skipped and only new files are cleaned.
- If a processed file is deleted, that source file will be reprocessed on the next run.
- Use `--reclean` when you want to overwrite/rebuild all existing processed subtitle outputs.
- Sentence reconstruction reads the cleaned caption rows and writes separate Parquet files; it does not replace the cleaned CSVs.
- FullStop is only called for English tracks. Legacy cleaned CSVs do not contain language metadata, so unknown language is treated as English unless `SUBTITLE_PUNCTUATION_ALLOW_UNKNOWN_LANGUAGE=false`.

### Run sentence reconstruction for one cleaned file

The sentence stage can run independently without downloading or cleaning subtitles again:

```bash
Rscript --vanilla r_scripts/run/Subtitle_clean/run_sentence_reconstruction.R \
  "/path/to/Subtitles/Processed/video_subtitles.csv" \
  "/path/to/Subtitles/Sentence_Units/video_subtitles.parquet" \
  en \
  "Talent Name"
```
