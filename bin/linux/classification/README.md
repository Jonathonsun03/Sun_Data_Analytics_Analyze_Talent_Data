# Classification Shell Wrappers

## `run_title_classification.sh`
Wrapper for:
`scripts/run/Title_classification/title_classification/07_run_weekly_classification.R`

This runs the title classification pipeline from a CSV by talent:
- ingests rows into DuckDB `videos`
- classifies only pending rows by default (idempotent)
- supports force rerun for prompt tests

### Run (weekly default)
From repo root:
`bin/linux/classification/run_title_classification.sh`

### Smoke test (5 rows per talent)
`bin/linux/classification/run_title_classification.sh --limit-per-talent 5`

### Force rerun (prompt testing)
`bin/linux/classification/run_title_classification.sh --limit-per-talent 5 --force-reclassify`

### Single talent
`bin/linux/classification/run_title_classification.sh --talent "Leia_Memoria_Variance_Project"`

## `run_title_classification_weekly.sh`
Pipeline wrapper that runs:
- `run_title_classification.sh`
- `scripts/run/Title_classification/title_classification/05_export_results_csv.R`

Exports to:
`classification/output/title_classifications`

### Run (classification + export)
`bin/linux/classification/run_title_classification_weekly.sh`

### Smoke test (5 rows per talent + export)
`bin/linux/classification/run_title_classification_weekly.sh --limit-per-talent 5`

## `run_talent_profile_builder.sh`
Wrapper for:
`scripts/run/Title_classification/build_talent_profile.R`

This runs the talent profile builder, which creates/updates:
- `classification/config/talents/<talent_slug>.json`
- `classification/prompts/talents/<talent_slug>/overlay.txt`
- `classification/config/talent_profiles.json` (when `--update-master-config` is used)

### Run
From repo root:
`bin/linux/classification/run_talent_profile_builder.sh --csv notes/titles.csv --all-talents --talent-col talent --title-col "Title" --content-type-col "Content Type" --write-overlay --update-master-config`

### Single Talent
`bin/linux/classification/run_talent_profile_builder.sh --csv notes/titles.csv --talent "Terberri_Solaris_Ch" --talent-col talent --title-col "Title" --content-type-col "Content Type" --write-overlay --update-master-config`

### GPT Discovery Mode (Optional)
`OPENAI_MODEL=gpt-5-mini bin/linux/classification/run_talent_profile_builder.sh --csv notes/titles.csv --all-talents --talent-col talent --title-col "Title" --content-type-col "Content Type" --write-overlay --update-master-config --use-gpt --sample-size 250`
