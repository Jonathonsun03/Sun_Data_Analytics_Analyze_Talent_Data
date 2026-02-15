# Text Replay Runner

## `run_text_replay.sh`

Wrapper script for:

- `scripts/run/text_replay/render_text_replay.R`

This builds full text replay CSVs by combining subtitle + chat files per stream.

Outputs are written per talent to:

- `<Talent>/text_playback/<stream_name>.csv`

`<stream_name>` comes from the subtitle filename stem (without `_subtitles.csv`).

## Basic usage

Run all talents:

```bash
bin/linux/test_stream_replay/run_text_replay.sh
```

Run one talent:

```bash
bin/linux/test_stream_replay/run_text_replay.sh --talent-query "Avaritia"
```

Run with parallel workers:

```bash
bin/linux/test_stream_replay/run_text_replay.sh --talent-query "Avaritia" --n-cores 8
```

## Options

- `--talent-query VALUE`
  - Selector passed to `select_talent()` (default: `all`)
- `--datalake-root PATH`
  - Overrides `TALENT_DATALAKE_ROOT`
- `--n-cores N`
  - Sets `TEXT_REPLAY_N_CORES` for parallel processing (default: `1`)
- `-h`, `--help`
  - Show help

## How It Works

1. Select talents from datalake using `TALENT_QUERY`.
2. For each talent:
   - Read chat files from `Chat/Original/*_chat.csv`
   - Read subtitle files from `Subtitles/Processed/*_subtitles.csv`
   - Match chat/subtitles by `video_id` parsed from filenames
3. Normalize subtitle schema and build a merged replay timeline.
4. Write one replay CSV per stream into `<Talent>/text_playback`.

## Environment variables used by the wrapper

- `TALENT_QUERY`
- `TEXT_REPLAY_N_CORES`
- `TALENT_DATALAKE_ROOT` (optional)

## `run_replay_pipeline.sh`

Replay-only orchestrator that runs both stages in order:

1. `bin/linux/subtitles/run_subtitle_clean.sh`
2. `bin/linux/test_stream_replay/run_text_replay.sh`

Use this when your cron should produce replay outputs end-to-end in one run.

Example:

```bash
bin/linux/test_stream_replay/run_replay_pipeline.sh --talent-query "Avaritia" --n-cores 8
```

Force subtitle reclean before replay:

```bash
bin/linux/test_stream_replay/run_replay_pipeline.sh --talent-query "Avaritia" --n-cores 8 --reclean
```
