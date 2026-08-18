# Title Classification Wrappers

Title classification uses the unified talent lakehouse returned by
`talent_lakehouse_db_path()`.

Canonical storage:

- `catalog.videos`: current title, title hash, talent, and video metadata.
- `classification.title_versions`: prompt, definitions, and output schema stored together by version.
- `catalog.talent_profiles`: reusable versioned profiles; title guidance is stored under the `title_classification` context.
- `classification.title_classification_results`: one result payload with title/version/profile/run lineage.
- `classification.title_classification_status`: queryable `classified`, `changed_title`, or `new_or_unclassified` status for the active version.
- `ops.pipeline_runs` and `ops.source_files`: execution and source provenance.

## Publish maintained assets

Validate the repository prompt source without writing:

```bash
Rscript r_scripts/run/title_classification/publish_title_version.R
```

Publish it as the active version:

```bash
Rscript r_scripts/run/title_classification/publish_title_version.R --execute
```

Build a reusable profile from current lakehouse titles (dry-run by default):

```bash
bin/linux/classification/run_talent_profile_builder.sh --talent TER4
bin/linux/classification/run_talent_profile_builder.sh --talent TER4 --execute
```

## Batch lifecycle

Build requests only for new, changed, or missing titles:

```bash
bin/linux/classification/run_title_classification_batch.sh \
  --run-id "title_$(date +%Y-%m-%d_%H-%M-%S)" \
  -- --batch-size 25
```

Submit, check, preview, and apply:

```bash
bin/linux/classification/run_title_classification_batch.sh --mode submit --run-dir PATH --execute
bin/linux/classification/run_title_classification_batch.sh --mode check --run-dir PATH --retrieve-output
bin/linux/classification/run_title_classification_batch.sh --mode preview --run-dir PATH
bin/linux/classification/run_title_classification_batch.sh --mode apply --run-dir PATH
```

Preview mode is read-only with respect to DuckDB. It validates and flattens the
nested Batch API response, prints a concise title/topic/confidence table, and
writes `batch_response_preview.csv` in the run directory for spreadsheet review.

`--force-reclassify` is explicit opt-in. Without it, the pending key is the
current `(video_id, title_hash, title_version_id)`. Changing a title therefore
requeues that video, while activating a new title version naturally queues all
current videos.

## Backfill and weekly automation

The general backfill wrapper uses the same incremental selection logic. A dry
run builds a reviewable batch without contacting OpenAI:

```bash
bin/linux/classification/run_title_classification_weekly.sh \
  --model gpt-5.6-terra \
  --batch-size 25
```

Add `--execute` to start or advance the durable lifecycle:

```bash
bin/linux/classification/run_title_classification_weekly.sh \
  --model gpt-5.6-terra \
  --batch-size 25 \
  --execute
```

The lifecycle builds all currently pending titles, submits one OpenAI Batch
job, stores its state in DuckDB, retrieves and applies completed output,
refreshes both CSV exports, and submits a retry batch for failed or missing
requests. After the final apply—or immediately when no titles are pending—it
publishes the current deterministic and guarded RapidFuzz tag dictionary for
dashboard use. It is safe to invoke repeatedly: when a run is active it advances
that run, and when no run is active it selects only new, changed, or missing
titles.

Preview or publish tag normalization directly:

```bash
.venv/bin/python py_scripts/run/publish_tag_normalization.py
.venv/bin/python py_scripts/run/publish_tag_normalization.py --execute
```

Raw tags remain unchanged in `classification_json`; dashboard aggregation uses
the active canonical mapping from the unified lakehouse.

Advance an active run without ever starting another batch:

```bash
bin/linux/classification/run_title_classification_scheduled.sh --check-only
```

Scheduled state is stored in
`classification.title_classification_scheduled_state` in the unified lakehouse.

Checked-in user-systemd units provide the production cadence:

- `sun-data-title-classification-weekly.timer` starts Mondays at 03:00 UTC;
- `sun-data-title-classification-check.timer` checks an active batch hourly,
  with a randomized delay of up to five minutes.

Install or refresh the units with:

```bash
mkdir -p "$HOME/.config/systemd/user"
cp config/systemd/sun-data-title-classification-{weekly,check}.{service,timer} \
  "$HOME/.config/systemd/user/"
systemctl --user daemon-reload
systemctl --user enable --now \
  sun-data-title-classification-weekly.timer \
  sun-data-title-classification-check.timer
```

Inspect the schedule and recent work with:

```bash
systemctl --user list-timers 'sun-data-title-classification-*'
journalctl --user -u sun-data-title-classification-weekly.service \
  -u sun-data-title-classification-check.service
```

## Read-only pipeline audit

Render the pipeline explainer and live database checks without building,
submitting, or applying a batch:

```bash
quarto render r_scripts/notebooks/tests/title_classification_pipeline.qmd
```

If the project renv autoloader stalls before rendering begins, run the same
read-only audit with `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` for that command.

Limit row previews to one talent code with:

```bash
quarto render r_scripts/notebooks/tests/title_classification_pipeline.qmd \
  -P talent_code:TER4
```

## CSV export

The apply step refreshes both locations under the DataLake:

- `Processed/Title_classification/current/classification_export_current.csv`
- `Processed/Title_classification/archived/classification_export_<timestamp>.csv`

Run the exporter directly with:

```bash
Rscript r_scripts/run/title_classification/05_export_results_csv.R
```

The export keeps the latest active-version result for every classified video.
`classification_status`, `classified_title_hash`, and `current_title_hash` make
changed titles explicit without silently dropping their prior result. All CSV
text is written as UTF-8.
