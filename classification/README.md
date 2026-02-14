# Classification Pipeline (DuckDB)

## Why The Folders Are Split
- `classification/` holds domain-specific classification assets and orchestration: run scripts, prompt assets, profile config, and classification-only helper functions.
- `scripts/lib/` holds shared infrastructure used by multiple pipelines: DuckDB setup, talent selection utilities, and generic ChatGPT wrappers.

This keeps reusable code centralized while allowing classification logic to evolve quickly in `classification/` without breaking shared libs.

## Current Layout
Entry points:
- `scripts/run/Title_classification/01_ingest_titles.R`
- `scripts/run/Title_classification/02_classify_pending_titles.R`
- `scripts/run/Title_classification/build_talent_profile.R`

Shared helpers:
- `scripts/lib/duckdb/db_connect.R`
- `scripts/lib/duckdb/db_schema.R`
- `scripts/lib/duckdb/ingest_videos.R`
- `scripts/lib/duckdb/pending.R`
- `scripts/lib/stream_classification/talent_rules.R`
- `scripts/lib/stream_classification/prompt_builder.R`
- `scripts/lib/utils/talent_select.R`
- `scripts/lib/ChatGPT/*`

Prompt architecture:
- Base prompt/schema: `classification/prompts/base/system.txt`, `classification/prompts/base/user_template.txt`, `classification/prompts/base/schema_v2.json`
- Talent overlays: `classification/prompts/talents/<profile>/overlay.txt`
- Profile config and matching: `classification/config/talent_profiles.json`

## Current Flow
1. Build the `Classification` dataframe:
   - Columns: `Video ID`, `Title`, `Content Type`, `Published At`
2. Set `Talent` to the talent selector used by `select_talent()`.
3. Run `scripts/run/Title_classification/01_ingest_titles.R` to upsert rows into DuckDB.
4. Run `scripts/run/Title_classification/02_classify_pending_titles.R` to classify pending titles:
   - select talent profile from `talent_profiles.json`
   - combine base prompt + talent overlay
   - validate model JSON against schema
   - upsert results into `classifications`

DuckDB file:
- `get_datalake_root()` + `classifications.duckdb`

## Profile Logic
- One schema and one base prompt are shared across talents.
- Each talent gets a small overlay with structural hints.
- Matching is done by normalized talent name pattern.
- If no matcher hits, the `default` profile is used.

This avoids duplicating full prompts while still honoring talent-specific title conventions.

## Notes
`run_classification.r` now only points you to the two entry scripts.

## Add New Talent Quickly
Use the R profile builder to generate a talent description JSON from titles.

Command:
`Rscript scripts/run/Title_classification/build_talent_profile.R --csv <path/to/titles.csv> --talent "<Talent Name>" --talent-col talent --title-col title --content-type-col content_type --write-overlay --update-master-config`

All talents in one pass:
`Rscript scripts/run/Title_classification/build_talent_profile.R --csv notes/titles.csv --all-talents --talent-col talent --title-col "Title" --content-type-col "Content Type" --write-overlay --update-master-config`

Optional canonical-name mapping file:
`--talent-map notes/talent_name_map.csv`
with columns: `source_talent,canonical_talent`

GPT-assisted discovery (optional, richer profile inference):
`Rscript scripts/run/Title_classification/build_talent_profile.R --csv <path/to/titles.csv> --talent "<Talent Name>" --talent-col talent --title-col title --content-type-col content_type --write-overlay --update-master-config --use-gpt --sample-size 250 --model gpt-5-mini`

What it generates:
- `classification/config/talents/<talent_slug>.json`
- `classification/prompts/talents/<talent_slug>/overlay.txt`
- Appends matcher/profile entry to `classification/config/talent_profiles.json`

GPT discovery prompt assets:
- `classification/prompts/discovery/system.txt`
- `classification/prompts/discovery/user_template.txt`
- `classification/prompts/discovery/schema.json`

Sync `talent_profiles.json` from current prompt folders:
`Rscript scripts/run/Title_classification/sync_talent_profiles.R`

Example:
`from classification.python.prompt_bundle import load_prompt_bundle`
