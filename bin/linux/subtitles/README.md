# Subtitles Runner

## `run_subtitle_clean.sh`

Wrapper script for:

- `scripts/run/Subtitle_clean/Subtitle_clean.R`

This runs subtitle cleaning for selected talents and writes outputs to each talent's `Subtitles/Processed` + `Subtitles/RData`, plus summary files in:

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
