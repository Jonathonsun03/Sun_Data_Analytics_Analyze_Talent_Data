# Incremental ENA Data Infrastructure Inventory

Date: 2026-05-10

Scope: repository and datalake inspection for an incremental ENA-ready pipeline. This is an inventory only; no pipeline rewrite is proposed here.

## Current Data Estate

Canonical talent data root is resolved by `r_scripts/lib/utils/datalake_root.r`:

- Linux default: `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`
- Overrides: `TALENT_DATALAKE_ROOT`, then `CHAT_OUTPUT_ROOT`
- Processed aggregate root used by some workflows: `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data`

Observed talent folders under the datalake:

- `Avaritia Hawthorne 【Variance Project】`
- `Katya Sable 【Variance Project】`
- `Leia Memoria【Variance Project】`
- `Nova Aokami Ch`
- `Rius Isonder Ch`
- `Terberri Solaris Ch`
- Plus demo/aggregate folders excluded by selection helpers, e.g. `Northstar Story Lab Demo`, `VarianceProject`, `subtitle_analysis`

Observed materialized data groups:

| Area | Location | Observed files |
| --- | --- | ---: |
| raw video analytics snapshots | `<talent>/raw_data/video_analytics/*.csv` | 482 |
| raw video monetary snapshots | `<talent>/raw_data/video_monetary/*.csv` | 450 |
| raw video demographics snapshots | `<talent>/raw_data/video_demographics/*.csv` | 482 |
| raw video geography snapshots | `<talent>/raw_data/video_geography/*.csv` | 482 |
| raw subscriber snapshots | `<talent>/raw_data/public_subscribers/*.csv` | 481 |
| raw daily subscriber snapshots | `<talent>/raw_data/subs_daily/*.csv` | 6 |
| raw chat logs | `<talent>/Chat/Original/*_chat.csv` | 1,582 |
| raw subtitle logs | `<talent>/Subtitles/Original/*.csv` | 3,108 |
| cleaned subtitles | `<talent>/Subtitles/Processed/*.csv` | 1,551 |
| text replay, chat plus subtitles | `<talent>/text_playback/*.csv` | 1,016 |
| per-stream Codex summaries | `<talent>/stream_summaries/stream_summary_codex/*.md` | 808 |
| overall theme / money outputs | `<talent>/stream_summaries/overall_themes/*` | 9 |
| report outputs | `<talent>/reports/<bundle>/...` | present for bundles A, B, E |
| processed subtitle analysis | `Processed/Talent_Data/subtitle_analysis/*.csv` | 3 |
| processed shared/personality outputs | `Processed/Talent_Data/shared_interactions`, `Processed/Talent_Data/Qualitative Codebook` | present |

Text replay counts by talent: Nova 274, Rius 268, Leia 144, Terberri 125, Avaritia 110, Katya 95.

Per-stream summary counts by talent: Nova 274, Leia 142, Rius 109, Avaritia 109, Katya 92, Terberri 82.

No Parquet files were observed under the talent datalake at the inspected depth. DuckDB files present at the talent root:

- `classifications.duckdb`
- `stream_summaries.duckdb`

## Scripts And Conventions By Area

### 1. YouTube Stream Metadata

Data:

- Raw snapshots live under `<talent>/raw_data/video_analytics`, `video_monetary`, `video_demographics`, `video_geography`, `public_subscribers`, and `subs_daily`.
- Root-level aggregate title file exists: `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/all_stream_titles.csv`.
- Repo-local reference file exists: `notes/titles.csv`.

Key scripts:

- `r_scripts/lib/import_data/talent_files.R`
  - `TalentFiles()` reads root-level legacy CSVs and `raw_data/<type>/*.csv`.
  - Infers type keys from `raw_data/<type>/...` or filename prefixes.
  - Adds snapshot `date` from filenames.
- `r_scripts/lib/clean_data/video_prep.R`
  - `video_analytics_prep()`, `video_monetary_prep()`, `video_demographic_prep()`, `video_geographic_prep()`.
- `r_scripts/lib/clean_data/analytics_core.R`
  - `prepare_analytics()` keeps `Video ID`, `Channel ID`, `Channel Name`, `Title`, `Published At`, `date`, `Content Type`, views, watch time, average view duration, average view percentage, subscribers gained/lost, optional revenue/CPM/duration.
  - Dedupes latest row by `Video ID`, `Channel ID`, `Channel Name`.
- `r_scripts/lib/clean_data/title_classification_join.R`
  - Joins classification exports to video analytics by `Video ID`.

Current IDs:

- Video key: YouTube `Video ID`.
- Channel key: `Channel ID`, with `Channel Name`.
- Talent identity in raw/reporting context: talent folder name.
- Published time: `Published At`.

### 2. Subtitles / Transcripts

Data:

- Raw subtitles: `<talent>/Subtitles/Original/*.csv`.
- Cleaned subtitles: `<talent>/Subtitles/Processed/*.csv`.
- Per-talent RData caches: `<talent>/Subtitles/RData/processed_subtitles_<talent>.RData`.
- Aggregate subtitle analysis: `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/subtitle_analysis/subtitle_summary.csv`, `subtitle_quotes_sample.csv`, `subtitle_ena_units.csv`.

Key scripts:

- Shell entrypoint: `bin/linux/subtitles/run_subtitle_clean.sh`.
- Runner: `r_scripts/run/Subtitle_clean/Subtitle_clean.R`.
- Cleaners:
  - `r_scripts/lib/clean_data/clean_subtitles/Clean_subtitles.R`
  - `r_scripts/lib/clean_data/clean_subtitles/prep_subtitle_time.R`
  - `r_scripts/lib/clean_data/clean_subtitles/normalize_subs_for_replay.r`
  - `r_scripts/lib/clean_data/clean_subtitles/subtitle_units.R`

Current cleaned subtitle schema:

- `start_sec`, `stop_sec`
- `start_time`, `stop_time`
- `VideoID`
- `VideoTitle`
- `Text`

Existing subtitle ENA unitization:

- `build_ena_units_from_clean_df()` groups subtitle text into sentence-like `TEXT` units and inserts `PAUSE` units when gaps exceed `SUBTITLE_PAUSE_GAP_SEC`.
- Unit output columns: `unit_id`, `unit_type`, `start_sec`, `end_sec`, `start`, `end`, `text`.
- Optional `--ena-as-final` writes ENA unit rows back to each `<talent>/Subtitles/Processed/<video>_subtitles.csv`; use carefully because it changes the meaning of "Processed" from cleaned subtitles to unitized ENA rows.

Current IDs:

- Video key: `VideoID` in subtitle files, equivalent to YouTube `Video ID`.
- Transcript row identity: no stable persistent transcript row ID; row order plus `VideoID`, `start_sec`, `stop_sec`, and `Text` currently identify rows.
- ENA unit ID: local sequential `unit_id`, regenerated per output and not globally stable.

### 3. Live Chat Data

Data:

- Raw chat logs: `<talent>/Chat/Original/*_chat.csv`.
- Chat is not currently written to a cleaned canonical table; it is normalized at replay/synthesis time.

Key scripts:

- `r_scripts/lib/clean_data/clean_chat/prep_chat_time.R`
  - Adds `time_in_seconds`, `t_posix`, `t_hms`, 1/5/10-second bins, and `timecode`.
- `r_scripts/run/text_replay/render_text_replay.R`
  - Reads chat CSVs with `data.table::fread()`.
  - Requires matching `*_chat.csv` and `*_subtitles.csv` video IDs.
  - Adds paid-message columns if missing.

Current chat fields used by downstream scripts:

- `username`, `user_id`, `message`, `timestamp`, `message_type`, `video_id`
- `time_in_seconds`, `timecode`
- `paid_amount_text`, `paid_amount_value`, `paid_currency`

Current IDs:

- Video key: `video_id`.
- Chat row identity: no stable persistent message ID is used. Downstream scripts infer identity from file path, file line, `video_id`, rounded seconds, speaker, source, and normalized text.
- Speaker/user identity: `username` and `user_id`.

### 4. Text Replay

Data:

- Combined chat/subtitle timeline: `<talent>/text_playback/*.csv`.

Key scripts:

- Shell entrypoint: `bin/linux/test_stream_replay/run_text_replay.sh`.
- Runner: `r_scripts/run/text_replay/render_text_replay.R`.
- Merger: `r_scripts/lib/clean_data/text_stream_replay/build_stream_replay.r`.
- Segment helper: `r_scripts/lib/clean_data/text_stream_replay/segment_text_replay.r`.

Current text replay schema:

- `video_id`
- `sec`
- `source`: `chat` or `subtitle`
- `speaker`
- `text`
- `message_type`
- `paid_amount_text`, `paid_amount_value`, `paid_currency`
- `timecode`
- `replay_line`

The replay builder chooses the chat time basis that best overlaps the subtitle range, with guardrails for timezone-origin offsets. This is reusable for ENA because it already aligns chat and subtitles onto one per-video timeline.

### 5. Stream Summaries

Data:

- Maintained per-stream summaries: `<talent>/stream_summaries/stream_summary_codex/<stream_stem>_summary.md`.
- Maintained prompt: `prompts/summaries/stream_summary_codex.md`.
- Codex logs: `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/summaries/stream_summary_codex/`.
- Legacy database: `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/stream_summaries.duckdb`.

Key scripts:

- Maintained shell runner: `bin/linux/codex_prompts/summaries/stream_summary_codex.sh`.
- Legacy shell runner: `bin/linux/stream_summary/run_stream_summary.sh`, which points to a missing/stale-looking R target `r_scripts/run/Text_Replay_Analysis/Text_replay_analysis_openAI`.

Incremental convention:

- The maintained Codex prompt is file-based and skips summaries whose matching output file already exists and is non-empty.
- Input: `<talent>/text_playback/*.csv`.
- Optional verification sources: `<talent>/Chat/Original/*_chat.csv`, `<talent>/Subtitles/Processed/*_subtitles.csv`.

DuckDB status:

- `stream_summaries.duckdb` contains table `stream_summaries` with 15 rows.
- `classifications.duckdb` also contains a `stream_summaries` table with the same schema but 0 rows.
- Stream summary DB schema keys include `talent_project`, `source_file`, `prompt_md5`, `model`, `output_path`, token counts, `status`, `processed_at`, with a unique tuple over `source_file`, `prompt_md5`, and `model`.

Current IDs:

- Video ID is parsed from filenames using the final 11-character YouTube ID before `_summary.md`, `_chat.csv`, or `.csv`.
- Summary provenance includes prompt version/path in the markdown body.
- No canonical `summary_id` table exists for the maintained markdown summaries.

### 6. Coding / Classification Outputs

#### Title Classification

Data:

- DuckDB: `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/classifications.duckdb`.
- CSV exports: `classification/output/title_classifications/classification_export_*.csv`.
- Latest observed export: `classification_export_gpt-5-mini_v7_v7.csv`; inspected `classification_export_gpt-5-mini_from_duckdb.csv` has 1,107 rows and 27 columns.

DuckDB tables in `classifications.duckdb`:

- `talents`: 6 rows.
- `videos`: 2,261 rows.
- `classifications`: 2,321 rows.
- `stream_summaries`: 0 rows.

Key scripts:

- `r_scripts/run/Title_classification/title_classification/01_ingest_titles.R`
- `r_scripts/run/Title_classification/title_classification/02_classify_pending_titles.R`
- `r_scripts/run/Title_classification/title_classification/05_export_results_csv.R`
- `r_scripts/run/Title_classification/title_classification/07_run_weekly_classification.R`
- Shell wrappers in `bin/linux/classification/`.
- Prompt/config inputs under `classification/prompts/` and `classification/config/`.

Current title classification IDs:

- `talent_id`: generated by `ensure_talent_id()` as `0001_<talent_slug>`, stored in DuckDB `talents`.
- `talent_num`: integer primary key in `talents`.
- `talent_name`: talent folder/name.
- `talent_slug`: lowercase slug.
- `video_id`: YouTube video ID.
- `title_hash`: `xxhash64(paste(talent_id, normalize_title(title), sep = "||"))`.
- Classification uniqueness: `UNIQUE(video_id, taxonomy_version, prompt_version, model)`.
- Video uniqueness: `UNIQUE(video_id, talent_id)`.
- Code columns: boolean fields added from prompt definitions, currently `collaborative_energy`, `community_milestones`, `interactive_entertainment`, `meme_viral`, `monetization`, `narrative_serialization`, `performance_artistry`, `personality_conversation`.
- Other classification fields parsed from JSON: `topic`, `language`, `tags`, `primary_reference`, `referenced_entities`, `confidence`.

#### Summary / Personality / Monetary Qualitative Coding

Data:

- Overall themes: `<talent>/stream_summaries/overall_themes/overall_themes_codex.md`.
- Money timestamps: `<talent>/stream_summaries/overall_themes/money_timestamps.csv`.
- Personality open coding and evidence under `<talent>/stream_summaries/overall_themes/personality_*`.
- Shared/personality aggregate outputs under `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions` and `Qualitative Codebook`.

Key scripts:

- `py_scripts/run/stream_summaries/summary_classification/summary_classification_incremental.py`
- `py_scripts/run/stream_summaries/monetary_analysis/monetary_summary_classification_incremental.py`
- `py_scripts/run/stream_summaries/personality/personality_profile_v3_incremental_open_coding.py`
- `py_scripts/run/stream_summaries/personality/build_personality_qualitative_code_log.py`
- `py_scripts/run/stream_summaries/personality/personality_profile_synthesis.py`
- `py_scripts/run/stream_summaries/personality/build_shared_behavior_baseline.py`
- Shell wrappers under `bin/linux/codex_prompts/`.

Current qualitative IDs:

- Video IDs parsed from filenames.
- Evidence rows often include `video_id`, time/timecode, `source`, `speaker`, `quote`, `file_path`, and `file_line`.
- Monetary event groups use generated `event_group_id`.
- Personality qualitative code log uses `Primary Code ID` and `Secondary Code ID`.
- Shared behavior outputs use `shared_behavior_name` plus matrix/evidence rows.
- There is no central code registry shared across title classifications, monetary codes, personality codes, and future ENA codes.

### 7. Report Generation

Data:

- Report outputs live under `<talent>/reports/bundle_A`, `bundle_B`, `bundle_E`.
- Report interpretation inputs/outputs are written in bundle-specific `artifacts/` and `interpretations/` trees.

Key scripts:

- Bundle A:
  - `bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh`
  - `bin/linux/render_reports/bundle_A/run_bundle_A_artifacts.sh`
  - `bin/linux/render_reports/bundle_A/run_bundle_A_interpretations.sh`
  - `bin/linux/render_reports/bundle_A/run_bundle_A_render_only.sh`
  - `r_scripts/run/bundle_A/import_data.r`
  - `r_scripts/run/bundle_A/render_bundle_A.R`
  - `templates/reports/Bundle_A/Bundle_A.Rmd`
- Bundle B:
  - analogous scripts under `bin/linux/render_reports/bundle_B/`
  - `r_scripts/run/bundle_B/import_data.r`, `render_bundle_B.R`
  - `templates/reports/Bundle_B/Bundle_B.Rmd`
- Bundle E:
  - `r_scripts/run/bundle_e/import_data.R`
  - `r_scripts/run/bundle_e/bundle_e_plots.R`
  - `r_scripts/run/bundle_e/render_bundle_e.R`
  - `templates/reports/bundle_e/bundle_e.Rmd`

Conventions:

- Reports can use `--input-source staging|datalake`.
- Talent routing uses `list_talents()`, `select_talent()`, and `talent_slugify()`.
- Title classification joins are reused in report preps.
- Prompt-driven interpretation files live under `prompts/reports/...` and datalake `interpretations/...`.

### 8. Existing ENA Code

Reusable ENA helpers:

- `r_scripts/lib/ENA_prep/ENA_prep_data.R`
  - `ENA_setup()`
  - `ENA_accumulate_from_prep()`
  - `ENA_lineweight()`
  - `ENA_lineweight_tbl()`
  - `prepare_ena_lineweight_tbl()`
  - `ENA_points()`
  - `add_ena_point_groups()`
- `r_scripts/lib/ENA_prep/ENA_2d Quadrant.R`
  - ENA grid/cell plotting helpers.
- `r_scripts/lib/ENA_prep/ENA_timeseries_plots.R`
  - SVD/code loading time-series helpers.
- `r_scripts/lib/clean_data/clean_subtitles/subtitle_units.R`
  - Subtitle sentence/pause unitization.

These helpers are analysis-facing, not an incremental data infrastructure. They assume an ENA-ready data frame already exists with units, conversation columns, code columns, and metadata.

## Reusable Building Blocks For Modular ENA

Strong reuse candidates:

- Talent/path resolution:
  - `get_datalake_root()`, `get_staging_root()`, `list_talents()`, `select_talent()`, `talent_slugify()`.
- Raw metadata ingestion:
  - `TalentFiles()`, `video_*_prep()`, `prepare_analytics()`, `attach_title_classifications()`.
- Stable video/talent registry:
  - Existing DuckDB `talents` and `videos` tables in `classifications.duckdb`.
- Incremental classification pattern:
  - `get_pending_titles()` and uniqueness by version tuple.
- Subtitle normalization:
  - `SplitTimeStamp()`, `normalize_subs_for_replay()`, `build_ena_units_from_clean_df()`.
- Chat time normalization:
  - `prep_chat_time()`.
- Cross-source alignment:
  - `build_stream_replay()` already produces a per-video combined timeline.
- File-based incremental behavior:
  - Summary workflow skip-if-output-exists convention.
- Qualitative evidence conventions:
  - Python scripts already preserve `video_id`, `timecode`, `source`, `speaker`, quote/text, file path, and file line.
- ENA analysis helpers:
  - `ENA_setup()`, accumulation, lineweight, plotting, and time-series helpers can consume a finalized ENA table.

## Missing For An Incremental ENA-Ready System

The missing layer is not ENA plotting; it is a canonical, incrementally updated event/unit/code store.

Needed pieces:

- Canonical event IDs:
  - `event_id` for each raw or normalized text event.
  - Stable hash based on `talent_id`, `video_id`, source, source file fingerprint, source row number or native message ID if present, time, speaker/user, and normalized text.
- Canonical transcript/chat unit IDs:
  - Stable `unit_id` not regenerated by row order.
  - Explicit `unit_scope`: raw subtitle row, raw chat row, replay event, sentence unit, pause unit, windowed ENA conversation unit.
- Source manifests:
  - File inventory with path, size, mtime, checksum, row count, schema fingerprint, talent, video ID, source type.
  - Needed to distinguish unchanged files from changed files and support true incremental rebuilds.
- Unified ENA schema:
  - A table such as `ena_events` with `talent_id`, `talent_name`, `video_id`, `event_id`, `unit_id`, `conversation_id`, `source`, `speaker`, `user_id`, `sec`, `timecode`, `text`, paid-message fields, and source provenance.
  - A table such as `ena_codes` or `ena_event_codes` with `event_id` or `unit_id`, `code_id`, `code_value`, `coder`, `coding_method`, `model`, `prompt_version`, `taxonomy_version`, `confidence`, `created_at`.
  - A table such as `ena_conversations` defining windowing choices for rENA, e.g. by stream, segment, time window, speaker turn, or sliding window.
- Central code registry:
  - Shared `code_id`, `code_name`, `code_family`, definition, inclusion/exclusion criteria, taxonomy version.
  - Needed because current title boolean columns, monetary labels, personality code IDs, shared behavior names, and future ENA codes live in separate systems.
- Incremental dependency tracking:
  - Track which derived outputs depend on which source file hashes and code/taxonomy versions.
  - Rebuild only affected videos or windows when chat/subtitle/summary/code inputs change.
- Canonical storage format:
  - Current durable storage is mixed CSV, markdown, RData, and DuckDB.
  - No Parquet dataset layout exists.
  - Choose DuckDB tables plus optional partitioned Parquet exports for ENA analysis datasets.
- Chat cleaning output:
  - Chat is normalized at runtime, but no canonical cleaned chat table/file is materialized.
- Replay incrementality:
  - `render_text_replay.R` rewrites matching replay files; it does not check source file hashes or skip unchanged pairs.
- Summary-to-ENA bridge:
  - Stream summaries have useful relationship descriptors, but there is no structured extraction into ENA codes with stable code IDs.
- Quality checks:
  - Need validations for one-to-one/many-to-one coverage across `videos`, chat, subtitles, replay, summaries, and code rows.
  - Need missingness flags for subtitle-only, chat-only, no-paid-message, time alignment failures, and duplicate video IDs across talents.

## Suggested Next Design Direction

For the next step, design the ENA infrastructure as a thin incremental layer over existing assets:

1. Reuse DuckDB talent/video tables as the identity spine.
2. Add a source manifest table for chat, subtitles, replay, summaries, and classification exports.
3. Build canonical event/unit tables from `text_playback` first, because it already aligns chat and subtitles.
4. Add code registry and event-code tables that can ingest existing title, monetary, personality, and future ENA-specific codes.
5. Export analysis-ready ENA frames for rENA from DuckDB, rather than making rENA helper scripts responsible for pipeline state.

