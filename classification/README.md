# Classification Pipeline (DuckDB)

## Why The Folders Are Split
- `classification/` holds domain-specific classification assets and orchestration: run scripts, prompt assets, profile config, and classification-only helper functions.
- `r_scripts/lib/` holds shared infrastructure used by multiple pipelines: DuckDB setup, talent selection utilities, and generic ChatGPT wrappers.

This keeps reusable code centralized while allowing classification logic to evolve quickly in `classification/` without breaking shared libs.

## Current Layout
Entry points:
- `r_scripts/run/Title_classification/title_classification/01_ingest_titles.R`
- `r_scripts/run/Title_classification/title_classification/02_classify_pending_titles.R`
- `r_scripts/run/Title_classification/title_classification/05_export_results_csv.R`
- `r_scripts/run/Title_classification/talent_profile/build_talent_profile.R`
- `r_scripts/run/Title_classification/talent_profile/sync_talent_profiles.R`

Shared helpers:
- `r_scripts/lib/duckdb/db_connect.R`
- `r_scripts/lib/duckdb/db_schema.R`
- `r_scripts/lib/duckdb/ingest_videos.R`
- `r_scripts/lib/duckdb/pending.R`
- `r_scripts/lib/stream_classification/talent_rules.R`
- `r_scripts/lib/stream_classification/prompt_builder.R`
- `r_scripts/lib/utils/talent_select.R`
- `r_scripts/lib/ChatGPT/*`

Prompt architecture:
- Base prompt assets: `classification/prompts/base/system.txt`, `classification/prompts/base/instructions.txt`, `classification/prompts/base/output_schema.json`
- Modular definitions: `classification/prompts/definitions/*.txt`
- Talent overlays: `classification/prompts/talents/<profile>/overlay.txt`
- Profile config and matching: `classification/config/talent_profiles.json`

## Current Flow
1. Build the `Classification` dataframe:
   - Columns: `Video ID`, `Title`, `Content Type`, `Published At`
2. Set `Talent` to the talent selector used by `select_talent()`.
3. Run `r_scripts/run/Title_classification/title_classification/01_ingest_titles.R` to upsert rows into DuckDB.
4. Run `r_scripts/run/Title_classification/title_classification/02_classify_pending_titles.R` to classify pending titles:
   - select talent profile from `talent_profiles.json`
   - compile prompt deterministically from:
     - base `system.txt`
     - base `instructions.txt`
     - sorted `definitions/*.txt`
     - talent `overlay.txt`
     - output schema
   - extend schema at runtime with one boolean field per definition file
   - validate model JSON against the extended schema
   - ensure DuckDB `classifications` has matching boolean columns
   - upsert results into `classifications`

DuckDB file:
- `get_datalake_root()` + `classifications.duckdb`

## Profile Logic
- One schema and one base system/instructions set are shared across talents.
- Each talent gets a small overlay with structural hints.
- Primary taxonomy definitions are modular files and compiled into one runtime user prompt.
- Matching is done by normalized talent name pattern.
- If no matcher hits, the `default` profile is used.

This avoids duplicating full prompts while still honoring talent-specific title conventions.

## Schema and Versioning
- `classification/config/talent_profiles.json` tracks current versions (`taxonomy_version` and `prompt_version`).
- In `v3`, each `classification/prompts/definitions/*.txt` file is converted into a required boolean output field in `classification`.
- The same fields are persisted in DuckDB as boolean columns on `classifications`.
- `talent_profile` is also persisted per classified row.

## Notes
`run_classification.r` now only points you to the two entry scripts.

## Add A New Talent
Use the profile builder when a new talent needs a classification overlay and matcher entry.

Recommended workflow:

1. Prepare a titles CSV.
   - Required columns are equivalent to: talent, title, and content type.
   - A publish date column is helpful and should be included when available.
2. Run the profile builder for one talent:
   - `Rscript r_scripts/run/Title_classification/talent_profile/build_talent_profile.R --csv <path/to/titles.csv> --talent "<Talent Name>" --talent-col talent --title-col title --content-type-col content_type --write-overlay --update-master-config`
3. Review the generated profile assets:
   - `classification/config/talents/<talent_slug>.json`
   - `classification/prompts/talents/<talent_slug>/overlay.txt`
   - `classification/config/talent_profiles.json`
4. If prompt folders were changed manually or you want to rebuild the matcher file from disk, run:
   - `Rscript r_scripts/run/Title_classification/talent_profile/sync_talent_profiles.R`
5. Run the local self-test before classifying production rows:
   - `Rscript r_scripts/run/Title_classification/title_classification/03_self_test_classification.R`

All talents in one pass:
- `Rscript r_scripts/run/Title_classification/talent_profile/build_talent_profile.R --csv notes/titles.csv --all-talents --talent-col talent --title-col "Title" --content-type-col "Content Type" --write-overlay --update-master-config`

Optional canonical-name mapping file:
- `--talent-map notes/talent_name_map.csv`
- Expected columns: `source_talent,canonical_talent`

GPT-assisted discovery is optional when you want a richer first-pass overlay:
- `Rscript r_scripts/run/Title_classification/talent_profile/build_talent_profile.R --csv <path/to/titles.csv> --talent "<Talent Name>" --talent-col talent --title-col title --content-type-col content_type --write-overlay --update-master-config --use-gpt --sample-size 250 --model gpt-5-mini`

GPT discovery prompt assets:
- `classification/prompts/discovery/system.txt`
- `classification/prompts/discovery/user_template.txt`
- `classification/prompts/discovery/schema.json`

After the profile exists, the normal pipeline is:
1. Ingest titles into DuckDB.
2. Run pending-title classification.
3. Export refreshed classification CSVs for downstream joins and reports.

## Demo Or Synthetic Talents
If you are building a demo talent for client-facing sample reports, do not append those rows to the production title-classification export by default.

Preferred approach:
- Keep the synthetic talent's title classifications in a separate CSV.
- Point Bundle report renders at that isolated file with `--titles-path`.
- This keeps demo-only labels out of `classification/output/title_classifications/*.csv`.

The current synthetic demo dataset generator writes its isolated classification file to:
- `<datalake_root>/<demo_talent>/reports/demo_inputs/demo_title_classifications.csv`

There is no active `classification/python/` runtime scaffold anymore. The maintained prompt-bundle loader lives in `r_scripts/lib/stream_classification/`.
