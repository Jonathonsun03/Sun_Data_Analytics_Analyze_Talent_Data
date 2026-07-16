# Classification Shell Wrappers

## `run_title_classification_batch.sh`
Preferred runner for production title classification.

This creates OpenAI Batch API jobs using the current compiled prompt bundle from
`classification/config/talent_profiles.json` and `classification/prompts/`.
Batch jobs are asynchronous and cheaper than synchronous API calls.

### Build pending batch
`bin/linux/classification/run_title_classification_batch.sh --run-id "title_v7_$(date +%Y-%m-%d_%H-%M-%S)" -- --batch-size 25`

### Build and submit a full reclassification with current definitions
`bin/linux/classification/run_title_classification_batch.sh --run-id "title_v7_full_$(date +%Y-%m-%d_%H-%M-%S)" --execute -- --batch-size 25 --force-reclassify`

Batch JSON artifacts are written under:
`/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/classification/title_classification/batch_runs/`

### Check and retrieve completed output
`bin/linux/classification/run_title_classification_batch.sh --mode check --run-dir "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/classification/title_classification/batch_runs/<run_id>" --retrieve-output`

### Apply retrieved output and refresh current/archive CSVs
`bin/linux/classification/run_title_classification_batch.sh --mode apply --run-dir "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/classification/title_classification/batch_runs/<run_id>"`

Windows users can call:
`bin\windows\classification\run_title_classification_batch.bat`

## `run_title_classification.sh`
Legacy synchronous runner.

Wrapper for:
`r_scripts/run/title_classification/07_run_weekly_classification.R`

This runs the title classification pipeline from a CSV by talent:
- ingests rows into DuckDB `videos`
- classifies only pending rows by default (idempotent)
- supports force rerun for prompt tests

### Run (weekly default)
From repo root:
`bin/linux/classification/run_title_classification.sh`

### Smoke test (5 rows per talent)
`bin/linux/classification/run_title_classification.sh --limit-per-talent 5`

### Slow API run
`bin/linux/classification/run_title_classification.sh --timeout-seconds 300 --batch-size 10`

### Force rerun (prompt testing)
`bin/linux/classification/run_title_classification.sh --limit-per-talent 5 --force-reclassify`

### Single talent
`bin/linux/classification/run_title_classification.sh --talent "Leia_Memoria_Variance_Project"`

## `run_title_classification_weekly.sh`
Compatibility wrapper around the Batch API runner.

Exports to:
Current export:
`/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Title_classification/current/classification_export_current.csv`

Archived timestamped exports:
`/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Title_classification/archived/`

Set `TITLE_CLASSIFICATIONS_DIR` to override the export folder for a one-off run.

### Run (classification + export)
`bin/linux/classification/run_title_classification_weekly.sh`

### Smoke test (5 rows per talent + export)
`bin/linux/classification/run_title_classification_weekly.sh --limit-per-talent 5`

## `run_title_classification_scheduled.sh`
Durable scheduled OpenAI Batch API lifecycle for production title classification.

This is the preferred scheduled entrypoint. It is idempotent and keeps a pending
state pointer in `classifications.duckdb`:

`/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/classifications.duckdb`

Table:
`title_classification_scheduled_state`

Lifecycle:

1. Refresh title rows from DataLake video analytics into `notes/titles.csv`.
2. Detect unclassified or newly changed videos from DuckDB using the current
   taxonomy, prompt version, and model.
3. Build a timestamped run directory under
   `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/classification/title_classification/batch_runs/`.
4. Write `batch_input.jsonl` with one video per Batch API request.
5. Use stable `custom_id` values based on talent, `video_id`, and title hash.
6. Upload the JSONL file and create an OpenAI Batch job.
7. Save `batch_id`, `input_file_id`, status metadata, manifest, input JSONL,
   and submit response in the run directory and DuckDB state table.
8. On later runs, check the existing batch before building anything new.
9. Retrieve `batch_output.jsonl` and `batch_errors.jsonl` when available.
10. Parse output by `custom_id`, validate the existing title-classification
    schema, apply successful rows to DuckDB, and refresh the current/archive CSV
    exports.
11. If any request fails or is missing, create a retry run containing only those
    failed/missing `custom_id`s.
12. Clear pending state only after a run has been applied and no retry is needed.

Logs are written to
`/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/classification/title_classification_scheduled/`
and each run also gets `logs/scheduled.log` inside its run directory.

### Run the scheduled lifecycle once
`bin/linux/classification/run_title_classification_scheduled.sh`

### Recommended cron entry
Do not overwrite the existing crontab automatically. Add this manually with
`crontab -e` if cron is preferred:

`30 9 * * 2 cd /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data && bin/linux/classification/run_title_classification_scheduled.sh >> /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/classification/title_classification_scheduled/cron.log 2>&1`

Because Batch API jobs are asynchronous, running the scheduled wrapper more than
once per week is useful. A daily check can retrieve/apply a batch after it
finishes without submitting duplicates:

`30 9 * * * cd /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data && bin/linux/classification/run_title_classification_scheduled.sh >> /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/classification/title_classification_scheduled/cron.log 2>&1`

## `run_talent_profile_builder.sh`
Wrapper for:
`r_scripts/run/title_classification/talent_profile/build_talent_profile.R`

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
