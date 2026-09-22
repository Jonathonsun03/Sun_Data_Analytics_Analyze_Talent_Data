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
  - `r_scripts/notebooks/tests/transcript_cleaning/pipeline_overview.qmd` demonstrates the maintained raw-DuckDB-to-FullStop-to-derived-DuckDB path, including an opt-in transactional sample publication and read-back
  - shared sentence schema and publication helpers live under `r_scripts/lib/duckdb/subtitle_sentence_*.R`; the backfill subsystem lives under `r_scripts/lib/subtitle_backfill/`
  - `subtitle_backfill_tracks.R` owns track reads and current-result checks; `subtitle_backfill_checkpoints.R` owns checkpoint construction and storage; `subtitle_backfill_database.R` owns short write connections and run records
  - `subtitle_backfill_inference.R` handles the block loop; `subtitle_backfill_reconstruction.R` validates and publishes one video; `subtitle_backfill_runtime.R` loads runner settings and waits on collection; `subtitle_backfill_batch.R` processes one candidate track
  - `r_scripts/run/Subtitle_clean/backfill_subtitle_sentences.R` is the canonical resumable full-track DuckDB backfill entrypoint, wrapped by `bin/linux/subtitles/run_subtitle_sentence_backfill.sh`
  - backfill reads one video's source/status/checkpoints and disconnects before inference; successful blocks are saved to one persistent RDS file per video under `<Talent DataLake root>/Processed/subtitle_backfill_checkpoints/` (override: `SUBTITLE_BACKFILL_CHECKPOINT_DIR`)
  - completed videos are source-checksum-validated and written with their checkpoints in the existing transaction; database lock conflicts use fixed-delay retries, while local results survive publication failures
  - the initial track query excludes output already current for the exact source checksum and pipeline version; each selected track is rechecked before work begins
  - `--max-new-videos` limits tracks requiring work; `--max-videos` limits the filtered candidate list (or all candidates with `--force`)
  - the runner waits before DuckDB reads and completed-video publication while `<Talent DataLake root>/Logs/collection-active` exists
- R-based stream summarization
  - the maintained runner is `r_scripts/run/Text_Replay_Analysis/Text_replay_analysis_openAI`
  - shared summarization helpers live under `r_scripts/lib/stream_summaries/`
- Report bundles
  - renderers live under `r_scripts/run/bundles/`
- Qualitative coding
  - canonical schema, publishing, and analysis interfaces are documented below
- Viewer activity and cross-talent chatter profiles
  - `r_scripts/run/publish_viewer_activity.R` refreshes the derived
    `analysis.viewer_video_activity`, `analysis.viewer_profiles`,
    `analysis.global_viewer_profiles`, and optional
    `analysis.company_viewer_profiles` relations from canonical chat data
  - the refresh is atomic, records its execution in `ops.pipeline_runs`, and
    defaults to a read-only dry run
  - global profiles use the stable YouTube `user_id` across every talent;
    optional company membership comes from `config/dashboard/company_talents.csv`
  - `r_scripts/notebooks/tests/sna_overall/overall_chatter_profiles.qmd`
    audits the stored metadata and builds a bounded all-talent co-participation
    network without persisting definition-specific viewer edges

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

## Inference machine power lifecycle

`with_inference_machine()` in `lib/utils/inference_machine.R` keeps one lazy
power scope around an entire batch. The targets are deliberately separate:

| Target | Default | Purpose |
| --- | --- | --- |
| Proxmox `pve-nlp` | `root@192.168.1.161` | Wake-on-LAN, SSH readiness, optional CT start, guarded shutdown |
| CT 106 `llm-inference-new` | `jonathon@192.168.1.173` | Container SSH readiness |
| Inference API | `http://192.168.1.173:8000` | Health and model requests |

Configuration defaults are supplied by `inference_machine_config()`:

- `INFERENCE_MACHINE_HOST=192.168.1.161`
- `INFERENCE_MACHINE_SSH_USER=root`
- `INFERENCE_MACHINE_MAC=10:7B:44:93:28:E2`
- `INFERENCE_CONTAINER_HOST=192.168.1.173`
- `INFERENCE_CONTAINER_SSH_USER=jonathon`
- `INFERENCE_CONTAINER_ID=106`
- `INFERENCE_MANAGE_CONTAINER=false`: existing deployment is expected to start
  the CT; set `true` to check `pct status 106` on Proxmox and run `pct start 106`
  only when stopped. Unknown/error status fails startup.
- `NLP_INFERENCE_URL=http://192.168.1.173:8000`
- `SUBTITLE_PUNCTUATION_URL`: optional full endpoint override; otherwise derived
  from `NLP_INFERENCE_URL` with `/v1/punctuate`.
- `INFERENCE_MACHINE_BOOT_TIMEOUT_SEC=180`: timeout for each readiness stage.
- `INFERENCE_MACHINE_LOCK_DIR=/tmp/sun-data-inference-machine.lock`
- `INFERENCE_MACHINE_SHUTDOWN_GUARD_COMMAND`: unset by default; see below.

Install `wakeonlan` and configure noninteractive SSH authentication/known hosts
for both SSH targets in the calling environment. The helper sends Wake-on-LAN
to the physical host, waits for authenticated Proxmox SSH, optionally starts the
CT, waits for authenticated container SSH, then waits for API `/health` to
return HTTP 200 and `status: ok`. Startup failure leaves the host powered on.
There is no host-side API proxy in this repository; requests targeting the
physical host are rejected with a corrective error.

```r
source(here::here("r_scripts", "lib", "utils", "inference_machine.R"))
with_inference_machine({
  ensure_inference_machine(inference_punctuation_url())
  # Run the complete batch here.
})
```

The three subtitle entrypoints, sentence reconstruction, and `summarize_chat()`
already scope their batches. Nested requests share one wake cycle. Empty scopes,
dry runs without model calls, and cloud calls do not wake this host. Ollama's
separate existing default is preserved; an explicit container target uses the
same lifecycle scope. The host lock serializes cooperating jobs from one caller;
other machines and applications require server-side coordination.

### Workload inspection and fail-safe shutdown

Live inspection on 2026-09-10 confirmed the API title `PCT 106 NLP Inference`
(version 1.0.0). Its OpenAPI schema documents `/health`, `/models`, and eight
model POST routes, including `/v1/punctuate`. It documents no status/busy/queue/
jobs/task endpoint. `/health` returns `{"status":"ok","service":"nlp-inference"}`;
this proves readiness, not idleness. SSH authentication to both targets was
rejected, so server source, undocumented routes, and host process mechanisms
could not be inspected. No workload mechanism was found in this repo or the
nearby scraper inference client. No activity endpoint has been invented.

Until a reliable mechanism is verified, normal completion and errors leave the
host powered on. An unset guard does not prevent processing. The old
`INFERENCE_MACHINE_IDLE_CHECK_COMMAND` and `INFERENCE_MACHINE_SHUTDOWN_COMMAND`
settings are no longer used: a separate idle check followed by shutdown has a
race with new requests.

A verified `INFERENCE_MACHINE_SHUTDOWN_GUARD_COMMAND` must run on Proxmox and
atomically reserve/drain admission, inspect all active and queued work (including
other producers and batches between requests), and execute its single argument
`shutdown -h now` only when confirmed safe. Return 0 only after accepting
shutdown; return 75 without shutdown if busy or inconclusive. Any other exit,
SSH failure, or timeout must leave the host on. This contract requires a real
server-side coordinator; setting a generic health/process/CPU check is unsafe.
No remote guard is installed by this change.

Busy/inconclusive leaves the host on and releases the caller lock so another
managed batch can finish and run its own guard. Guard failures retain the lock
for inspection. Confirmed shutdown waits for physical-host SSH to disappear
and allows 15 seconds for power-down before releasing the lock. Forced process
termination cannot guarantee cleanup; inspect running jobs before removing a
stale lock. The helper does not install an unattended idle monitor.

The host-side guard implementation is now available at
`py_scripts/run/inference_shutdown_guard.py`; see the
[Python guard documentation](../py_scripts/README.md#proxmox-inference-shutdown-guard)
for its coordinator contract, tests, and deployment layout. It remains disabled
until service coverage and batch reservations are verified; the example config
intentionally refuses shutdown. No production server or power state was changed.
