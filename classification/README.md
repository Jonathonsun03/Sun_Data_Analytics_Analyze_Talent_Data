# Classification Pipeline (DuckDB)

## What’s New
The old single-script flow was split into two entry points and small helpers.

Entry points:
- `classification/run/01_ingest_titles.R`
- `classification/run/02_classify_pending_titles.R`

Helpers:
- `scripts/lib/duckdb/db_connect.R`
- `scripts/lib/duckdb/db_schema.R`
- `scripts/lib/duckdb/ingest_videos.R`
- `scripts/lib/duckdb/pending.R`

Utilities:
- `scripts/lib/utils/talent_select.R` now includes `talent_slugify()` and `ensure_talent_id()`.

## Current Flow
1. Build the `Classification` dataframe:
   - Columns: `Video ID`, `Title`, `Content Type`, `Published At`
2. Set `Talent` to the talent selector used by `select_talent()`.
3. Run `01_ingest_titles.R` to upsert rows into DuckDB.
4. Run `02_classify_pending_titles.R` to compute pending titles.

DuckDB file:
- `get_datalake_root()` + `classifications.duckdb`

## Next Steps (To Finish)
1. Decide classification key:
   - `video_id` (default): no reclass on title edits
   - `title_hash`: reclass when title changes
2. Add GPT classification step in `02_classify_pending_titles.R`:
   - Build prompt + JSON schema
   - Call OpenAI API via `scripts/lib/ChatGPT`
   - Insert into `classifications` table
3. Add prompt files under `classification/prompts/`
4. Optionally add a small batch size + retry logic

## Notes
`run_classification.r` now only points you to the two entry scripts.
