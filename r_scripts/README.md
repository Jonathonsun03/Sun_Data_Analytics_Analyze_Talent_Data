# r_scripts

This directory contains the repository's maintained R code.

## Layout

- `r_scripts/lib/`
  - shared R helpers for data loading, cleaning, DuckDB, prompt assembly, report rendering, and stream summarization
  - reusable plot implementations are organized by analytical domain under `r_scripts/lib/plots/domains/`
- `r_scripts/notebooks/`
  - interactive analyses, model development notebooks, reports, and dashboards
  - model notebooks are grouped by analytical family under `r_scripts/notebooks/models/`
- `r_scripts/run/`
  - automation-oriented R entrypoints grouped by workflow

## Key workflows

- Title classification
  - entrypoints live under `r_scripts/run/title_classification/`
  - shared schema, storage, and prompt helpers live under `r_scripts/lib/title_classification/`
  - production reads and writes the unified DuckDB returned by `talent_lakehouse_db_path()`
- Subtitle cleaning and text replay
  - entrypoints live under `r_scripts/run/Subtitle_clean/` and `r_scripts/run/text_replay/`
  - `run_sentence_reconstruction.R` can punctuate an already-cleaned subtitle CSV and write separate sentence-level Parquet output
- R-based stream summarization
  - the maintained runner is `r_scripts/run/Text_Replay_Analysis/Text_replay_analysis_openAI`
  - shared summarization helpers live under `r_scripts/lib/stream_summaries/`
- Report bundles
  - renderers live under `r_scripts/run/bundles/`
- Qualitative coding
  - canonical schema, publishing, and analysis interfaces are documented below

## Python split

Python stream-summary analysis runners no longer live here.

- maintained Python entrypoints now live under `py_scripts/run/stream_summaries/`
- reusable Python helpers belong under `py_scripts/lib/`

## Conventions

- Put new reusable R code in `r_scripts/lib/`.
- Put interactive analyses and model notebooks in `r_scripts/notebooks/`.
- Put new runnable R entrypoints in `r_scripts/run/`.
- Do not create a second notebook tree under `r_scripts/run/`.
- Resolve repository-relative paths from the `.git` root rather than relying on RStudio or the current working directory.
- Keep Python code in `py_scripts/`, not under `r_scripts/`.

## Canonical qualitative lakehouse

Qualitative coding is stored in the unified talent lakehouse returned by
`talent_lakehouse_db_path()`. There are three persistent qualitative tables:

| Relation | Responsibility |
| --- | --- |
| `qualitative.transcripts` | Selected semantic units, exact coded-text snapshots, sequence, `video_id`, `talent_code`, and source-record lineage |
| `qualitative.codebooks` | Versioned code definitions, checksums, column names, and wide-view metadata |
| `qualitative.coding` | Pipeline/run status, review metadata, and the generic `code_id -> BOOLEAN` map |

Dataset membership is carried by `dataset_id` on the transcript rows. The
coding key is `(transcript_line_id, pipeline_run_id, codebook_id)`, allowing the
same semantic unit and run identifier to be used safely with multiple
codebooks.

`qualitative.coding` supports every codebook. Each codebook gets a semantic wide
view whose `code_*` columns are logical values; it does not get another physical
coding table. For example, `chat_monetary_growth_v1` resolves to
`qualitative.coding_chat_monetary_growth_v1`.

The qualitative schema deliberately does not duplicate titles, talent names,
video analytics, or chat payment fields. Loaders retrieve those fields from the
existing relations:

| Existing relation | Reused data |
| --- | --- |
| `catalog.videos` and `catalog.talents` | Video and talent dimensions |
| `text.chat_messages` | Chat identity, message type, and payment fields |
| `text.subtitle_units` | Subtitle identity and raw transcript lineage |
| `analytics.video_latest_performance` | Video-performance measures |
| `ops.source_files` and `ops.pipeline_runs` | File provenance and execution history |

The text in `qualitative.transcripts` is the immutable snapshot of the semantic
unit presented to the coder. `source_record_keys`, `alignment_status`, and
`text_sha256` connect that snapshot to the canonical raw chat/subtitle records.

### Publish a dataset

Use the maintained entrypoint:

```bash
Rscript --vanilla r_scripts/run/publish_qualitative_coding.R
```

Validate a new source/codebook combination before writing:

```bash
QUALITATIVE_PUBLISH_DRY_RUN=true \
  Rscript --vanilla r_scripts/run/publish_qualitative_coding.R
```

The runner accepts `QUALITATIVE_*` environment variables for the database,
coded-export directory, dataset ID, codebook path/ID/name/version, and generated
wide-view name. Publishing is transactional and idempotent.

### Load data for analysis

Source `r_scripts/lib/import_data/qualitative_transcripts.R` and use
`load_qualitative_transcripts_wide()`. The loader resolves the correct
codebook-specific view and returns the `code_*` columns in the wide logical
format required by ENA and the existing model notebooks.

Before adding a table, publisher, or loader, inspect these shared files:

- `r_scripts/lib/duckdb/qualitative_schema.R`
- `r_scripts/lib/duckdb/qualitative_publish.R`
- `r_scripts/lib/import_data/qualitative_transcripts.R`

Extend those interfaces instead of introducing project-specific coding tables,
parallel run logs, or notebook-local imports of already-published coded CSVs.
