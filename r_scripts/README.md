# r_scripts

This directory contains the repository's maintained R code.

## Layout

- `r_scripts/lib/`
  - shared R helpers for data loading, cleaning, DuckDB, prompt assembly, report rendering, and stream summarization
  - reusable plot implementations are organized by analytical domain under `r_scripts/lib/plots/domains/`
- `r_scripts/run/`
  - runnable R entrypoints grouped by workflow

## Key workflows

- Title classification
  - entrypoints live under `r_scripts/run/title_classification/`
  - shared DuckDB and prompt helpers live under `r_scripts/lib/duckdb/` and `r_scripts/lib/stream_classification/`
- Subtitle cleaning and text replay
  - entrypoints live under `r_scripts/run/Subtitle_clean/` and `r_scripts/run/text_replay/`
- R-based stream summarization
  - the maintained runner is `r_scripts/run/Text_Replay_Analysis/Text_replay_analysis_openAI`
  - shared summarization helpers live under `r_scripts/lib/stream_summaries/`
- Report bundles
  - renderers live under `r_scripts/run/bundles/`

## Python split

Python stream-summary analysis runners no longer live here.

- maintained Python entrypoints now live under `py_scripts/run/stream_summaries/`
- reusable Python helpers belong under `py_scripts/lib/`

## Conventions

- Put new reusable R code in `r_scripts/lib/`.
- Put new runnable R entrypoints in `r_scripts/run/`.
- Resolve repository-relative paths from the `.git` root rather than relying on RStudio or the current working directory.
- Keep Python code in `py_scripts/`, not under `r_scripts/`.
