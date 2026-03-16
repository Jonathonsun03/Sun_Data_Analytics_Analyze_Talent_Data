# Scripts

This directory contains the project's runnable analysis workflows in `scripts/run/` and the reusable code they depend on in `scripts/lib/`.

The overall design is:

1. Resolve a data root and talent selection.
2. Load raw or staged talent files.
3. Clean and normalize them into analysis-ready tables.
4. Run either:
   - title classification,
   - chat/subtitle replay construction,
   - stream summarization / personality analysis,
   - report rendering.
5. Write outputs back to the datalake, DuckDB, report folders, or classification exports depending on the workflow.

## Structure

### `scripts/lib/`

Shared functions live here.

- `utils/`
  - Root and path helpers such as `get_datalake_root()`, `get_staging_root()`, and `select_talent()`.
  - Report rendering helpers and small table / formatting utilities.
  - This is the main environment-resolution layer used across the repo.
- `import_data/`
  - `TalentFiles()` loads per-talent CSV files from a talent folder and groups them by file type.
  - This is the entrypoint used by report and prep scripts to assemble talent datasets.
- `clean_data/`
  - Normalizes analytics, chat, subtitles, and video-level datasets.
  - Joins title classifications back onto analytics / monetary / demographic data.
  - Contains the text replay builders that merge cleaned subtitles and cleaned chat into a single chronological stream.
- `duckdb/`
  - Manages the local DuckDB used for title ingestion and title classification tracking.
  - `db_schema.R` creates `videos` and `classifications`.
  - `pending.R` is the dedupe / pending-work layer for classification reruns.
- `stream_classification/`
  - Builds modular title-classification prompts.
  - Resolves talent-specific overlays and extends the classification schema with definition-driven boolean fields.
  - `talent_profile/` builds JSON talent profiles and optional overlay text from title histories.
- `stream_summaries/`
  - Shared logic for summarizing `text_playback` CSVs into markdown summaries.
  - Handles prompt loading, pending-file tracking, DuckDB bookkeeping, and output naming.
- `ChatGPT/`
  - Thin R wrapper around OpenAI chat-completions usage: auth, prompt loading, message building, request sending, and response helpers.
- `Local_LLMs/`
  - Ollama-based local generation helpers.
- `plots/`, `report_tables/`, `ENA_prep/`
  - Plot builders, report-table builders, and ENA utilities used by the bundle reports and subtitle workflows.

### `scripts/run/`

Runnable entrypoints live here.

- `Subtitle_clean/`
  - Cleans subtitle files for one or more talents.
  - Builds ENA-ready subtitle units and sample quote outputs.
  - Controlled primarily through environment variables such as `TALENT_QUERY`, `SUBTITLE_N_CORES`, and `SUBTITLE_RECLEAN`.
- `text_replay/`
  - Builds `text_playback/*.csv` by matching subtitle files to chat files on video ID.
  - This creates the unified timeline used downstream by stream summarization and personality workflows.
- `Text_Replay_Analysis/`
  - Runs LLM-based stream summarization from `text_playback`.
  - `Text_replay_analysis_openAI` is the maintained runner and delegates to `scripts/lib/stream_summaries/`.
- `stream_summaries/`
  - Python runners for personality-profile generation from stream-summary assets and replay data.
  - Prompt specs for this area now live under `prompts/stream_summaries/`.
- `Title_classification/`
  - The title-ingestion and LLM title-classification pipeline.
  - Also contains talent-profile generation for classification overlays.
- `bundle_A/`, `bundle_B/`
  - RMarkdown render entrypoints plus data-prep helpers for report bundles.
- `bundle_C/`, `bundle_D/`
  - Currently description-only bundle notes, not full rendering pipelines yet.
- `Title_analysis/`
  - Ad hoc title export / inspection scripts used to generate or inspect title datasets.
- `check_server.r`
  - Currently empty / placeholder.

## Main Workflows

### 1. Title Classification Pipeline

This is the most structured database-backed workflow in `scripts/`.

Key logic:

1. `scripts/run/Title_classification/title_classification/01_ingest_titles.R`
   - Expects a `Classification` data frame and `Talent` value already in memory.
   - Resolves the talent, creates or reuses a DuckDB talent ID, normalizes titles, and upserts videos into DuckDB.
2. `scripts/run/Title_classification/title_classification/02_classify_pending_titles.R`
   - Loads the prompt bundle for the resolved talent.
   - Builds a full prompt from base instructions, definitions, and any talent overlay.
   - Uses DuckDB pending logic to avoid reclassifying already-classified rows unless forced.
   - Calls OpenAI, validates returned JSON against the expected schema, and stores results plus boolean definition columns in DuckDB.
3. `scripts/run/Title_classification/title_classification/05_export_results_csv.R`
   - Exports the latest stored classifications out of DuckDB and flattens key JSON fields for downstream use.
4. `scripts/run/Title_classification/title_classification/07_run_weekly_classification.R`
   - Convenience wrapper for reading a source CSV and chaining ingest + classify across talents.

Supporting scripts:

- `03_self_test_classification.R` verifies prompt/schema/db compatibility.
- `04_preview_results.R` inspects the classification table for a chosen model.
- `06_compare_exports.R` compares two CSV exports row-by-row.
- `run_classification.r` is intentionally deprecated and only points callers to the split scripts.

Talent-profile support:

- `scripts/run/Title_classification/talent_profile/build_talent_profile.R`
  - Builds canonical talent JSON profiles from title histories.
  - Can optionally call GPT discovery prompts to enrich the profile.
  - Can write both config JSON and prompt overlay text used by the title-classification prompt compiler.
- `scripts/run/Title_classification/talent_profile/sync_talent_profiles.R`
  - Sync-oriented helper for the profile configuration area.

### 2. Subtitle Cleaning and Text Replay

These scripts turn raw chat and subtitle files into aligned text timelines.

`scripts/run/Subtitle_clean/Subtitle_clean.R`

- Resolves selected talents from the datalake.
- Cleans subtitle files through `clean_subtitles/`.
- Builds ENA unitized outputs for subtitle analysis.
- Writes cleaned outputs and summary diagnostics.
- Supports multicore execution and optional recleaning.

`scripts/run/text_replay/replay_analysis.R`

- Reads subtitle files from `Subtitles/Processed` and chat files from `Chat/Original`.
- Extracts video IDs from filenames.
- Matches subtitle and chat files per video.
- Normalizes subtitle and chat timestamps.
- Produces unified `text_playback/*.csv` files for each matched stream.

This `text_playback` output is the core bridge between raw stream text and later LLM workflows.

`scripts/run/text_replay/render_text_replay.R`

- Exploratory inspection script for a single talent's generated `text_playback` files.
- Useful for verifying that paid messages and merged timelines look correct.

### 3. Stream Summaries and Personality Work

This workflow sits on top of `text_playback`.

`scripts/lib/stream_summaries/` provides the shared summarization engine:

- `args.R` parses runner arguments.
- `prompt.R` loads markdown prompt files with `---SYSTEM---` / `---USER---` sections.
- `db.R` stages source files and tracks completed summary runs.
- `process.R` converts a replay CSV into transcript text, sends it to the model, writes markdown, and records metadata.

Primary runner:

- `scripts/run/Text_Replay_Analysis/Text_replay_analysis_openAI`
  - Loads the ChatGPT helper module and `stream_summaries` helpers.
  - Summarizes pending `text_playback` files into markdown outputs.
  - Uses DuckDB to prevent redundant reruns for the same prompt/model combination.

Personality analysis:

- `scripts/run/stream_summaries/run_personality_profile.py`
- `scripts/run/stream_summaries/run_personality_profile_v2_open_coding.py`

These Python runners:

- Read stream-summary markdown, replay CSVs, chat logs, and money timestamp files.
- Generate per-talent personality outputs under `stream_summaries/overall_themes/`.
- Are currently self-contained runners rather than thin wrappers over a shared Python library.

The intended organization for future work in this area is:

- runner entrypoints in `scripts/run/stream_summaries/`
- reusable helpers in `scripts/lib/stream_summaries/`
- prompt specs in `prompts/stream_summaries/`

### 4. Report Bundles

Bundle rendering is the reporting layer built on top of staged/datalake inputs plus title classifications.

`scripts/run/bundle_A/render_bundle_A.R`

- Finds the repo root robustly.
- Accepts talent selection, output, and date-window arguments.
- Calls a shared render helper to render one report per talent from `templates/reports/Bundle_A/Bundle_A.Rmd`.

`scripts/run/bundle_A/import_data.r`

- Loads talent files from staging or datalake.
- Loads title classifications from CSV export.
- Builds cleaned analytics / monetary / demographic / geographic tables with title enrichment.
- Produces plot-ready and table-ready data for Bundle A sections.

Bundle A logic answers:

- trend direction,
- content-type performance,
- audience composition shifts,
- timing effects,
- collaboration lift,
- topic/tag performance.

`scripts/run/bundle_B/render_bundle_B.R`

- Same rendering pattern as Bundle A, but points to the Bundle B RMarkdown template.

`scripts/run/bundle_B/import_data.r`

- Reuses much of Bundle A's prep stack.
- Adds Bundle B-specific opportunity, strength/weakness, and attribute matrix logic.

Bundle B logic is more strategy-oriented:

- format consistency,
- priority ranking,
- opportunity matrix by content attributes,
- revenue efficiency,
- collaboration and schedule lift.

Bundle C and Bundle D:

- `scripts/run/bundle_C/Bundle_C_Desc.md`
- `scripts/run/bundle_D/Bundle_D_Desc.md`

These are concept notes for future report bundles. They are not yet implemented as full render pipelines in `scripts/run/`.

## Utility and Analysis Scripts

These scripts are useful but are not the main production pipelines:

- `scripts/run/Title_analysis/Export_titles.r`
  - Pulls title inventories from staged talent analytics and writes a combined CSV to `notes/titles.csv`.
- `scripts/run/Title_analysis/Examine_titles.r`
  - Ad hoc local inspection of title-classification outputs.
- `scripts/run/Title_classification/title_analysis.R`
  - Manual data viewer for classification exports.
- `scripts/run/text_replay/render_text_replay.R`
  - Manual replay inspection.

Treat these as analyst utilities rather than the primary automation surface.

## Data and State

Most scripts assume one of two roots:

- staging root via `get_staging_root()`
- datalake root via `get_datalake_root()`

Selection is usually driven by:

- explicit CLI args for report renderers
- environment variables for cleaning / replay jobs
- helper defaults in `scripts/lib/utils/`

Persistent state is stored in a few places:

- DuckDB for title classification and stream-summary bookkeeping
- classification export CSVs for report enrichment
- datalake talent folders for cleaned subtitles, replay files, summary markdown, and personality outputs

## Conventions

- Put new reusable code in `scripts/lib/`.
- Put new runnable entrypoints in `scripts/run/`.
- Prefer extending existing helper modules before adding another standalone script.
- If a workflow writes durable outputs, it should be explicit about whether it reads from staging or datalake.
- Stream-summary personality work should stay under the `stream_summaries` area, not `notes/`.

## Recommended Reading Order

If you need to understand the codebase quickly, read in this order:

1. `scripts/lib/utils/`
2. `scripts/lib/import_data/`
3. `scripts/lib/clean_data/CleanData.R`
4. `scripts/lib/duckdb/`
5. `scripts/lib/stream_classification/`
6. `scripts/lib/stream_summaries/`
7. the relevant `scripts/run/...` entrypoint for the workflow you want to change
