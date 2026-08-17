# Sun Data Analytics: Talent Data Analysis

This repository is the main code workspace for ingesting, organizing,
classifying, analyzing, and reporting on talent data. Maintained R and Python
code, SQL queries, prompt sources, notebooks, and operating wrappers live here.
Raw data, database files, generated CSVs, rendered dashboards, and reports live
in the DataLake rather than in the repository.

## Architecture at a Glance

```text
DataLake inputs
    |
    v
Unified talent DuckDB
    |-- catalog        talents, videos, reusable talent profiles
    |-- analytics      current and historical performance data
    |-- text           chat messages and subtitle units
    |-- classification title versions, results, status, scheduler state
    |-- qualitative    transcripts, codebooks, and coding results
    `-- ops            source-file and pipeline-run provenance
    |
    +--> R/Python analysis and classification workflows
    +--> Quarto dashboards and reports
    `--> current and archived DataLake exports
```

The canonical database is resolved at runtime with
`talent_lakehouse_db_path()` from
`r_scripts/lib/duckdb/db_connect.R`. Code should use that resolver rather than
hard-coding a machine- or container-specific DuckDB path.

Core design rules:

- DuckDB is the analytical system of record; CSV files are exports or source
  artifacts, not parallel application state.
- Raw DataLake data is read-only. Transformations write derived data to
  processed, staging, or output locations.
- Reusable R code belongs in `r_scripts/lib/`; R entrypoints belong in
  `r_scripts/run/`.
- Reusable Python code belongs in `py_scripts/lib/`; Python entrypoints belong
  in `py_scripts/run/`.
- Shared SQL belongs in `sql_queries/`, and maintained prompt sources belong in
  `prompts/`.
- Pipeline executions and source provenance reuse `ops.pipeline_runs` and
  `ops.source_files`.

## Repository Layout

| Path | Purpose |
| --- | --- |
| `bin/linux/` | Shell entrypoints for classification, dashboards, reports, transcript selection, subtitles, and related workflows |
| `bin/windows/` | Windows-specific operating wrappers |
| `config/` | Checked-in runtime and service configuration templates |
| `notes/` | Architecture, research, product, and scripting notes; not canonical pipeline state |
| `prompts/` | Maintained prompt specifications and archived prompt-family references |
| `py_scripts/lib/` | Shared Python modules |
| `py_scripts/run/` | Runnable Python pipeline and synchronization entrypoints |
| `py_scripts/notebooks/` | Python analysis notebooks |
| `py_scripts/tests/` | Python tests |
| `r_scripts/lib/` | Shared R loaders, database helpers, cleaning, plotting, and workflow libraries |
| `r_scripts/run/` | Runnable R pipeline and report entrypoints |
| `r_scripts/notebooks/` | Quarto/R analyses, dashboards, models, and reports |
| `r_scripts/tests/` | R tests |
| `sql_queries/` | Reusable, language-neutral SQL organized by database and subject |
| `state/` | Small local workflow state that has not been moved into a canonical database |

More detailed conventions are documented in
[`r_scripts/README.md`](r_scripts/README.md),
[`py_scripts/README.md`](py_scripts/README.md),
[`prompts/README.md`](prompts/README.md), and
[`sql_queries/README.md`](sql_queries/README.md).

## Environment Setup

Run commands from the repository root.

### R

The project uses `renv` and the checked-in `renv.lock`:

```bash
Rscript --vanilla setup_R_env.R
```

In a restricted container, retain the project library while disabling renv's
optional system-library sandbox:

```bash
export RENV_CONFIG_SANDBOX_ENABLED=FALSE
Rscript --vanilla setup_R_env.R
```

### Python

Create a project-local virtual environment:

```bash
python3 -m venv .venv
.venv/bin/python -m pip install --upgrade pip
.venv/bin/python -m pip install -r py_scripts/requirements.txt
```

Repository wrappers load non-secret defaults from `.env` where supported.
Credentials and tokens must not be committed.

## Unified Talent Lakehouse

The unified DuckDB prevents each analysis workflow from building a separate
copy of the same talent and video dimensions.

| Schema | Main responsibility |
| --- | --- |
| `catalog` | Talent and video dimensions plus reusable talent profiles |
| `analytics` | Video analytics and latest-performance relations |
| `text` | Canonical chat and subtitle records |
| `classification` | Versioned title classification inputs, results, status, and scheduled state |
| `qualitative` | Selected transcript snapshots, versioned codebooks, and coding results |
| `ops` | Shared source-file and pipeline execution provenance |

Titles, talent names, video metadata, analytics, and payment fields should be
joined from their canonical relations rather than copied into project-specific
tables.

## Title Classification

Title classification is integrated with the rest of the repository. There is
no separate top-level classification code repository or file-based production
state.

### Maintained components

| Component | Location |
| --- | --- |
| Prompt, definitions, and output-schema source | `prompts/title_classification/` |
| Shared R schema, storage, prompt, and profile helpers | `r_scripts/lib/title_classification/` |
| R publishing, batch-building, applying, preview, and export entrypoints | `r_scripts/run/title_classification/` |
| Shell lifecycle wrappers | `bin/linux/classification/` |
| Reusable current-status query | `sql_queries/unified_db/classification/current_title_status.sql` |

Prompt files in the repository are maintained source assets. A published title
version stores the compiled prompt, instructions, content rules, definitions,
and output schema together as one immutable database snapshot.

### Canonical relations

| Relation | Grain and purpose |
| --- | --- |
| `classification.title_versions` | One complete prompt/definition/schema snapshot per title version |
| `catalog.talent_profiles` | Reusable, versioned profiles with generic identity and contextual guidance |
| `classification.title_classification_results` | Classification result with title-version, profile, and pipeline-run lineage |
| `classification.title_classification_status` | One current video per active version with classified/pending status |
| `classification.title_classification_scheduled_state` | Scheduled batch lifecycle state |
| `ops.pipeline_runs` | Execution provenance shared with other pipelines |
| `ops.source_files` | Source-asset provenance shared with other pipelines |

Talent profiles are not exclusive to title classification. Their general
identity and reusable information live in `catalog.talent_profiles`; title
guidance is stored only as the `title_classification` context within a profile.

### Incremental selection rules

Without an explicit override, the pending key is:

```text
(video_id, current title_hash, title_version_id)
```

This produces three queryable states:

| Status | Meaning | Selected by the next normal batch? |
| --- | --- | --- |
| `classified` | The active version has a result for the video's current title hash | No |
| `changed_title` | The video has an active-version result, but its current title hash has changed | Yes |
| `new_or_unclassified` | No result exists for the current video/title/version combination | Yes |

Consequently:

- unchanged titles already classified by the active version are skipped;
- newly discovered titles are queued;
- changed titles are requeued;
- activating a new title version naturally queues every current video; and
- `--force-reclassify` remains an explicit opt-in that queues everything in
  scope.

The model name is result metadata and is intentionally not part of the pending
key.

### Publish a title version

Validate the maintained repository assets without writing:

```bash
Rscript r_scripts/run/title_classification/publish_title_version.R
```

Publish the validated bundle and make it active:

```bash
Rscript r_scripts/run/title_classification/publish_title_version.R --execute
```

### Build or update reusable talent profiles

Profile building is a dry run unless `--execute` is supplied:

```bash
bin/linux/classification/run_talent_profile_builder.sh --talent TER4
bin/linux/classification/run_talent_profile_builder.sh --talent TER4 --execute
```

The builder reads current titles from `catalog.videos` and publishes profiles
to `catalog.talent_profiles`. It does not use Google Drive or create maintained
profile overlays in the repository.

### Classify pending titles

Build a reviewable Batch API run containing only pending titles:

```bash
bin/linux/classification/run_title_classification_batch.sh \
  --run-id "title_$(date +%Y-%m-%d_%H-%M-%S)" \
  -- --batch-size 25
```

The build step does not submit an API request. Review the generated run folder,
then submit explicitly:

```bash
bin/linux/classification/run_title_classification_batch.sh \
  --mode submit \
  --run-dir PATH \
  --execute
```

Check the remote batch and retrieve completed output:

```bash
bin/linux/classification/run_title_classification_batch.sh \
  --mode check \
  --run-dir PATH \
  --retrieve-output
```

Validate and flatten the retrieved response for review without writing to
DuckDB:

```bash
bin/linux/classification/run_title_classification_batch.sh \
  --mode preview \
  --run-dir PATH
```

This prints a concise table and writes `batch_response_preview.csv` beside the
batch manifest and JSONL files.

Validate and apply the responses transactionally, then refresh the current and
archived CSV exports:

```bash
bin/linux/classification/run_title_classification_batch.sh \
  --mode apply \
  --run-dir PATH
```

### Backfill and weekly pipeline

Use the weekly wrapper as the general incremental backfill entrypoint. Without
`--execute`, it only builds local review artifacts:

```bash
bin/linux/classification/run_title_classification_weekly.sh \
  --model gpt-5.6-terra \
  --batch-size 25
```

With `--execute`, the same command starts or advances the durable lifecycle:

```bash
bin/linux/classification/run_title_classification_weekly.sh \
  --model gpt-5.6-terra \
  --batch-size 25 \
  --execute
```

The lifecycle selects all current pending titles, submits the OpenAI Batch job,
stores active state in the unified DuckDB, retrieves and applies completed
responses, refreshes the current and archived CSV exports, and creates a retry
batch for failed or missing requests. Running it again while a batch is active
advances that batch instead of duplicating it. Running it after completion
selects only titles newly added or changed since the previous run.

The checked-in user-systemd timers start the incremental pipeline every Monday
at 03:00 UTC and check an active batch hourly:

```bash
mkdir -p "$HOME/.config/systemd/user"
cp config/systemd/sun-data-title-classification-{weekly,check}.{service,timer} \
  "$HOME/.config/systemd/user/"
systemctl --user daemon-reload
systemctl --user enable --now \
  sun-data-title-classification-weekly.timer \
  sun-data-title-classification-check.timer
```

The hourly service calls the scheduled runner with `--check-only`, so it never
starts a second batch between weekly runs.

Useful build filters after `--` include `--talent NAME_OR_CODE`,
`--title-version-id ID`, `--limit-per-talent N`, `--batch-size N`,
`--model NAME`, and `--force-reclassify`.

Preview the active version and current results without changing data:

```bash
Rscript r_scripts/run/title_classification/04_preview_results.R --limit 20
```

Run the local selection and schema self-test:

```bash
Rscript --vanilla r_scripts/run/title_classification/03_self_test_classification.R
```

Render the read-only pipeline explainer and live lakehouse audit:

```bash
quarto render r_scripts/notebooks/tests/title_classification_pipeline.qmd
```

The notebook documents the R/Python split and checks schema availability,
version state, incremental status, pending titles, profile coverage, result
lineage, recent runs, scheduler state, and the current CSV export. Pass
`-P talent_code:TER4` to narrow row-level previews to one talent.

### CSV exports

CSV remains a supported delivery format, but exports are generated from the
latest unified DuckDB rather than used as classification state.

```bash
Rscript r_scripts/run/title_classification/05_export_results_csv.R
```

Default DataLake outputs:

```text
Processed/Title_classification/current/classification_export_current.csv
Processed/Title_classification/archived/classification_export_<timestamp>.csv
```

The export retains the latest active-version result for each classified video.
It includes `classification_status`, `is_classified`,
`classified_title_hash`, and `current_title_hash`, so a previous result remains
auditable when the current title has changed. CSV text and timestamps are
normalized to UTF-8 and UTC-compatible values.

### Migration verification snapshot

The refactor was validated against the production lakehouse on 2026-08-16.
These counts are a dated migration checkpoint, not live documentation:

| State | Videos |
| --- | ---: |
| Current available videos | 2,966 |
| `classified` for active `title-v7` | 2,454 |
| `changed_title` for active `title-v7` | 252 |
| `new_or_unclassified` for active `title-v7` | 260 |
| Total pending under normal incremental rules | 512 |

The v7 CSV export contained 2,706 latest results: 2,454 matching the current
title hash and 252 retained prior results whose titles have since changed.
Historical `title-v3` through `title-v6` definitions were migrated as
metadata-only versions; `title-v7` is the complete active version. Existing
historical classification rows remain available for lineage. The migration
retained 5,027 historical result rows and published seven reusable talent
profiles; older results without a recoverable versioned profile intentionally
retain a null `profile_id`.

A full pre-refactor DuckDB backup and the legacy v7 CSV were preserved in the
DataLake before migration. The old standalone `classifications.duckdb` was left
in place as a recovery artifact, but maintained classification code no longer
reads or writes it.

Preserved migration artifacts, expressed relative to their corresponding
DataLake roots, are:

```text
Talent_data/Data_lakehouse/backups/title_classification_refactor_20260816/
  talent_lakehouse_pre_refactor.duckdb
Processed/Title_classification/archived/
  classification_export_gpt-5-mini_v7_v7_legacy.csv
```

The migration was verified with R parse checks, the classification self-test,
a temporary end-to-end database build/apply/export run, production dry-run
batch selection, talent filtering, scheduled-state operations, Python compile
checks, shell syntax checks, reusable status-query validation, and CSV loading
through an existing report consumer. No OpenAI/API batch was submitted during
the refactor.

### Retired classification paths

The following are intentionally no longer part of the maintained workflow:

- the top-level `classification/` folder;
- repository-local final classification products and profile overlays;
- `notes/titles.csv` as a classification input or synchronization target;
- the standalone classification DuckDB as active state;
- Google Drive talent-profile lookup or upload code; and
- the dependent Google Drive spreadsheet report scheduler.

Unrelated dashboard authorization and Cloudflare/D1 synchronization are
separate systems and were not removed by this refactor.

See [`bin/linux/classification/README.md`](bin/linux/classification/README.md)
and
[`prompts/title_classification/README.md`](prompts/title_classification/README.md)
for focused operational and prompt-maintenance details.

## Qualitative Coding

The canonical qualitative system also lives in the unified talent lakehouse:

| Relation | Responsibility |
| --- | --- |
| `qualitative.transcripts` | Selected semantic units, exact coded-text snapshots, sequence, and source-record lineage |
| `qualitative.codebooks` | Versioned code definitions, checksums, column names, and wide-view metadata |
| `qualitative.coding` | Run status, review metadata, and generic `code_id -> BOOLEAN` results |

Raw transcript truth remains in `text.chat_messages` and
`text.subtitle_units`. The qualitative transcript text is an auditable snapshot
of the exact semantic unit presented for coding, with source keys, alignment
status, and checksum retained.

Publish a dataset transactionally with:

```bash
Rscript --vanilla r_scripts/run/publish_qualitative_coding.R
```

Dry-run a new source/codebook combination first:

```bash
QUALITATIVE_PUBLISH_DRY_RUN=true \
  Rscript --vanilla r_scripts/run/publish_qualitative_coding.R
```

Analysis code should use `load_qualitative_transcripts_wide()` from
`r_scripts/lib/import_data/qualitative_transcripts.R`. A new codebook creates a
new `codebook_id` and generated wide view, not another physical coding table.

## Dashboards and Reports

Quarto dashboard and report sources live under `r_scripts/notebooks/`.
Operating wrappers resolve DataLake inputs and route rendered outputs back to
the appropriate talent report directory.

Render one static talent dashboard:

```bash
bin/linux/render_dashboards/run_talent_dashboard.sh \
  --talent "Avaritia Hawthorne"
```

Serve the interactive talent dashboard:

```bash
quarto serve r_scripts/notebooks/dashboards/talent_dashboard/dashboard.qmd \
  --host 0.0.0.0 \
  --port 3838
```

Serve the read-only title-classification operations dashboard:

```bash
quarto serve r_scripts/notebooks/dashboards/data_admin/dashboard.qmd \
  --host 0.0.0.0 \
  --port 3839
```

This mobile-friendly admin dashboard shows the latest batch, compact run
history, included talents and tags, current coverage, and
searchable/downloadable classification rows. Keep it behind an authenticated
internal reverse proxy when hosted.

Report bundle wrappers live under `bin/linux/render_reports/`. For example:

```bash
bin/linux/render_reports/run_bundle_A_report.sh \
  --talent "Terberri Solaris Ch" \
  --window-days 90 \
  --input-source datalake
```

See
[`bin/linux/render_dashboards/README.md`](bin/linux/render_dashboards/README.md)
and [`bin/linux/render_reports/README.md`](bin/linux/render_reports/README.md)
for rendering options and output layouts.

## Cloudflare and Dashboard Access

Cloudflare D1 is the source of truth for dashboard client identities and
permissions. DuckDB remains the analytical source of truth for exact talent
codes and catalog metadata. The maintained synchronization copies talent
catalog metadata to D1; it does not grant product or talent access.

Preview the synchronization:

```bash
.venv/bin/python py_scripts/run/sync_d1_talent_catalog.py
```

Apply after reviewing the preview:

```bash
.venv/bin/python py_scripts/run/sync_d1_talent_catalog.py --apply
```

The older private Google Sheet permission importer is retained only for
controlled migrations and is not authoritative or scheduled. Details are in
[`py_scripts/README.md`](py_scripts/README.md).

## Documentation Index

- [`AGENTS.md`](AGENTS.md): repository safety, placement, and architecture rules
- [`notes/folder_architecture/README.md`](notes/folder_architecture/README.md): structural decision log
- [`prompts/README.md`](prompts/README.md): prompt organization
- [`r_scripts/README.md`](r_scripts/README.md): R layout and qualitative lakehouse interfaces
- [`py_scripts/README.md`](py_scripts/README.md): Python layout and synchronization workflows
- [`sql_queries/README.md`](sql_queries/README.md): shared SQL organization and conventions
- [`bin/linux/classification/README.md`](bin/linux/classification/README.md): title-classification operations
- [`bin/linux/render_dashboards/README.md`](bin/linux/render_dashboards/README.md): dashboard rendering
- [`bin/linux/render_reports/README.md`](bin/linux/render_reports/README.md): report bundle rendering

When adding or changing a workflow, update the nearest subsystem README and
keep this file focused on repository-wide architecture, primary entrypoints,
and canonical storage.
