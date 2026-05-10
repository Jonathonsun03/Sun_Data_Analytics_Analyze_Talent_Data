# Small Incremental ENA Extract Architecture

Date: 2026-05-10

Purpose: propose the smallest practical architecture for generating ENA-ready extracts without building one massive CSV. This design reuses current datalake outputs and existing R/Python helpers wherever possible.

## Guiding Choice

Use DuckDB as the normalized working store and generate narrow, run-specific ENA extracts on demand.

Do not create a single all-purpose CSV containing every stream, segment, code, and metadata field. Instead:

1. Keep stream, segment, code, and manifest data normalized.
2. Store code applications in long form.
3. Pivot to wide only for a specific ENA run.
4. Save reproducible run metadata beside each extract.

## Minimal Storage Location

Recommended database:

`/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/qualitative_ena.duckdb`

Recommended extract output root:

`/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/ena_extracts/`

This keeps the infrastructure near existing talent data while keeping generated ENA outputs in the processed area.

## Source Inputs To Reuse First

Initial source priority:

- `<talent>/text_playback/*.csv`
  - Best first source because it already aligns chat and subtitles into one stream timeline.
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/classifications.duckdb`
  - Reuse `talents`, `videos`, and `classifications` as the identity and title-code source.
- `<talent>/stream_summaries/overall_themes/money_timestamps.csv`
  - Optional source for monetary event/code rows where present.
- `Processed/Talent_Data/shared_interactions/current/*.csv`
  - Optional source for shared/personality code families where present.
- Existing ENA helper scripts:
  - `r_scripts/lib/ENA_prep/ENA_prep_data.R`
  - `r_scripts/lib/clean_data/text_stream_replay/build_stream_replay.r`
  - `r_scripts/lib/clean_data/clean_subtitles/subtitle_units.R`

Defer raw chat/subtitle reprocessing until there is a clear need. Text replay is enough for the first ENA-ready data layer.

## Tables

### `streams`

One row per talent-video stream that has usable text or metadata.

Suggested columns:

| Column | Type | Notes |
| --- | --- | --- |
| `stream_id` | TEXT | Stable key: `talent_id || ':' || video_id`. |
| `talent_id` | TEXT | Reuse from `classifications.duckdb.talents`. |
| `talent_name` | TEXT | Talent folder/display name. |
| `video_id` | TEXT | YouTube video ID. |
| `title_raw` | TEXT | From `videos.title_raw` or filename fallback. |
| `content_type` | TEXT | From `videos.content_type` or classification export. |
| `published_at` | TIMESTAMP | From `videos.published_at` when available. |
| `text_playback_path` | TEXT | Current replay source path. |
| `summary_path` | TEXT | Current per-stream summary path if present. |
| `duration_sec` | DOUBLE | Max `sec` from replay, or null if not computed. |
| `first_seen_at` | TIMESTAMP | First manifest registration. |
| `last_seen_at` | TIMESTAMP | Latest manifest registration. |
| `active` | BOOLEAN | Allows soft-retiring missing streams. |

Minimal uniqueness:

- `UNIQUE(talent_id, video_id)`
- `UNIQUE(stream_id)`

How to populate:

- Start from existing `classifications.duckdb.videos`.
- Add streams found in `text_playback` even if the video is missing from `videos`.
- Resolve `talent_id` through existing `talents`; if absent, create using the same `ensure_talent_id()` convention.

### `segments`

One row per ENA unit or event segment. For the first version, one text replay row can be one segment. Later, sentence units or sliding windows can be added without replacing the base table.

Suggested columns:

| Column | Type | Notes |
| --- | --- | --- |
| `segment_id` | TEXT | Stable hash of stream/source/row/time/text. |
| `stream_id` | TEXT | Links to `streams`. |
| `talent_id` | TEXT | Denormalized for filtering. |
| `video_id` | TEXT | Denormalized for filtering. |
| `segment_index` | INTEGER | Row order within stream. |
| `segment_type` | TEXT | `replay_event`, `subtitle_sentence`, `pause`, `window`. |
| `source` | TEXT | `chat`, `subtitle`, `summary`, etc. |
| `speaker` | TEXT | Replay speaker. |
| `user_id` | TEXT | Available for raw chat later; null for current replay source. |
| `start_sec` | DOUBLE | From replay `sec`. |
| `end_sec` | DOUBLE | Null or same as start for event rows. |
| `timecode` | TEXT | Existing replay timecode. |
| `text` | TEXT | Source text used for coding. |
| `message_type` | TEXT | Existing replay message type. |
| `paid_amount_text` | TEXT | Existing replay paid field. |
| `paid_amount_value` | DOUBLE | Existing replay paid field. |
| `paid_currency` | TEXT | Existing replay paid field. |
| `source_path` | TEXT | CSV path used to build the segment. |
| `source_row_number` | INTEGER | CSV row number including or excluding header, but document which. |
| `source_hash` | TEXT | Source file hash from manifest. |
| `created_at` | TIMESTAMP | Insert timestamp. |

Minimal uniqueness:

- `UNIQUE(segment_id)`
- `UNIQUE(stream_id, segment_type, source_path, source_row_number, source_hash)`

Small first implementation:

- Use `text_playback` rows directly.
- Create `segment_id = sha256(stream_id, source_path, source_row_number, sec, source, speaker, normalized text)`.
- Keep sentence/pause unitization as a later optional segment type, reusing `build_ena_units_from_clean_df()` only when needed.

### `codebook`

One row per code definition available for ENA extraction.

Suggested columns:

| Column | Type | Notes |
| --- | --- | --- |
| `code_id` | TEXT | Stable slug, e.g. `title:monetization`, `money:support_ritual`, `personality:A1`. |
| `code_name` | TEXT | Human-readable label. |
| `code_family` | TEXT | `title_classification`, `monetary`, `personality`, `shared_behavior`, `ena_manual`. |
| `code_layer` | TEXT | Optional layer such as `stream`, `segment`, `event_group`, `talent`. |
| `definition` | TEXT | Definition or short description. |
| `inclusion_criteria` | TEXT | Optional. |
| `exclusion_criteria` | TEXT | Optional. |
| `taxonomy_version` | TEXT | Existing title taxonomy version or local ENA version. |
| `source_system` | TEXT | `classification_duckdb`, `money_timestamps`, `shared_interactions`, etc. |
| `active` | BOOLEAN | Default true. |
| `created_at` | TIMESTAMP | Insert timestamp. |

Initial rows:

- Title boolean columns from `classifications.duckdb.classifications`:
  - `collaborative_energy`
  - `community_milestones`
  - `interactive_entertainment`
  - `meme_viral`
  - `monetization`
  - `narrative_serialization`
  - `performance_artistry`
  - `personality_conversation`
- Monetary codes from `money_timestamps.csv` can be added only for talents where those files exist.
- Personality/shared codes can be added after the title-code path works.

### `code_applications`

Long-form code observations. This is the key table that prevents one massive, sparse CSV.

Suggested columns:

| Column | Type | Notes |
| --- | --- | --- |
| `application_id` | TEXT | Stable hash of target/code/version/source. |
| `code_id` | TEXT | Links to `codebook`. |
| `target_type` | TEXT | `stream`, `segment`, `event_group`, `talent`. |
| `target_id` | TEXT | Usually `stream_id` or `segment_id`. |
| `stream_id` | TEXT | Always populate when applicable. |
| `segment_id` | TEXT | Populate for segment-level codes. |
| `talent_id` | TEXT | Denormalized for filtering. |
| `video_id` | TEXT | Denormalized for filtering. |
| `value_bool` | BOOLEAN | For binary code applications. |
| `value_numeric` | DOUBLE | For weights/confidence/intensity. |
| `value_text` | TEXT | Optional label or extracted category. |
| `confidence` | DOUBLE | Optional model confidence. |
| `coder` | TEXT | `gpt-5-mini`, `codex_rule`, `manual`, etc. |
| `coding_method` | TEXT | `title_classification`, `regex_seed`, `summary_extraction`, etc. |
| `model` | TEXT | Existing model if AI-generated. |
| `prompt_version` | TEXT | Existing prompt version where applicable. |
| `taxonomy_version` | TEXT | Existing taxonomy version where applicable. |
| `source_path` | TEXT | Source file or DB label. |
| `source_row_id` | TEXT | Row number, event group ID, or DB key. |
| `created_at` | TIMESTAMP | Insert timestamp. |

Minimal uniqueness:

- `UNIQUE(code_id, target_type, target_id, coder, coding_method, prompt_version, taxonomy_version)`

Initial applications:

- Stream-level title classifications:
  - For each true boolean code in `classifications`, create one row with `target_type = 'stream'`.
  - `target_id = stream_id`.
- Segment-level paid-message indicators from text replay:
  - Create `code_id = 'event:paid_message_visible'` when `message_type == 'paid_message'` or paid amount fields are non-empty.
  - `target_type = 'segment'`, `target_id = segment_id`.
- Optional later:
  - Summary/personality/monetary codes from existing Python outputs.

### `processing_manifest`

One row per source file or source table snapshot considered by the prep pipeline.

Suggested columns:

| Column | Type | Notes |
| --- | --- | --- |
| `manifest_id` | TEXT | Stable hash of source kind/path/hash. |
| `source_kind` | TEXT | `text_playback`, `classification_duckdb`, `money_timestamps`, `shared_interactions`, etc. |
| `source_path` | TEXT | File path or DB path/table. |
| `source_table` | TEXT | For DB-backed sources. |
| `talent_id` | TEXT | Nullable for global DB/table sources. |
| `talent_name` | TEXT | Nullable. |
| `video_id` | TEXT | Nullable for stream files. |
| `stream_id` | TEXT | Nullable. |
| `file_size_bytes` | BIGINT | File sources. |
| `mtime` | TIMESTAMP | File sources. |
| `content_hash` | TEXT | Prefer sha256 for files; table snapshot hash for DB sources. |
| `schema_hash` | TEXT | Hash of column names/types where cheap. |
| `row_count` | BIGINT | Source row count. |
| `status` | TEXT | `discovered`, `processed`, `skipped_unchanged`, `failed`. |
| `error_message` | TEXT | Nullable. |
| `processed_at` | TIMESTAMP | Latest processing timestamp. |

Minimal behavior:

- If `source_path + content_hash + schema_hash` is unchanged, skip rebuilding dependent `segments`.
- If a source changes, delete and rebuild only rows tied to that `source_path` and previous manifest hash.

## Reproducible ENA Run Metadata

Add a small table for each extract request.

### `ena_runs`

Suggested columns:

| Column | Type | Notes |
| --- | --- | --- |
| `ena_run_id` | TEXT | Stable run slug plus timestamp or hash. |
| `created_at` | TIMESTAMP | Run timestamp. |
| `created_by` | TEXT | User/system. |
| `purpose` | TEXT | Short free-text reason. |
| `db_path` | TEXT | Source DuckDB path. |
| `extract_path` | TEXT | Output CSV/RDS/Parquet path. |
| `metadata_path` | TEXT | JSON metadata path. |
| `talent_filter` | TEXT | JSON array or `all`. |
| `video_filter` | TEXT | JSON array or null. |
| `date_start` | DATE | Nullable. |
| `date_end` | DATE | Nullable. |
| `segment_type` | TEXT | e.g. `replay_event`. |
| `unit_col` | TEXT | rENA unit column. |
| `conversation_cols` | TEXT | JSON array. |
| `metadata_cols` | TEXT | JSON array. |
| `code_ids` | TEXT | JSON array of included codes. |
| `codebook_version` | TEXT | Version/filter used. |
| `window_size_back` | INTEGER | rENA accumulation setting if used. |
| `source_manifest_hash` | TEXT | Hash of manifest rows used. |
| `git_commit` | TEXT | Current repo commit if available. |
| `git_dirty` | BOOLEAN | Whether repo had uncommitted changes. |
| `r_version` | TEXT | `R.version.string`. |
| `package_versions` | TEXT | JSON object for `duckdb`, `DBI`, `dplyr`, `tidyr`, `rENA`, etc. |
| `row_count` | BIGINT | Extract rows. |
| `status` | TEXT | `created`, `failed`. |
| `error_message` | TEXT | Nullable. |

Each ENA extract should also write a sidecar JSON:

`<extract_dir>/<ena_run_id>_metadata.json`

The JSON should mirror `ena_runs` and include the exact SQL query or query parameters used to produce the extract.

## R Extract Function

Add one R helper in the existing ENA prep area:

`r_scripts/lib/ENA_prep/ena_extracts.R`

Proposed function:

```r
generate_ena_wide_extract <- function(
  db_path = "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/qualitative_ena.duckdb",
  output_root = "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/ena_extracts",
  purpose,
  talent_ids = NULL,
  video_ids = NULL,
  date_start = NULL,
  date_end = NULL,
  segment_type = "replay_event",
  target_type = c("segment", "stream"),
  code_ids = NULL,
  unit_col = "stream_id",
  conversation_cols = c("stream_id", "segment_index"),
  metadata_cols = c("talent_name", "video_id", "source", "speaker", "start_sec", "timecode"),
  value_mode = c("boolean", "numeric"),
  window_size_back = 4,
  write_csv = TRUE,
  write_rds = TRUE
) {
  # 1. Open DuckDB.
  # 2. Filter segments/streams and long-form code_applications.
  # 3. Pivot selected code_ids wide using tidyr::pivot_wider().
  # 4. Fill missing code columns with FALSE/0.
  # 5. Write run-specific extract, not a global mega CSV.
  # 6. Write metadata JSON and insert one ena_runs row.
  # 7. Return list(data = extract_df, run = run_metadata, paths = output_paths).
}
```

Expected extract shape:

- One row per selected segment when `target_type = "segment"`.
- One row per selected stream when `target_type = "stream"`.
- Required rENA inputs are present:
  - unit column, such as `stream_id`, `talent_id`, or `speaker`
  - conversation columns, such as `stream_id` and `segment_index`
  - code columns, one boolean/numeric column per `code_id`
  - metadata columns

Example extract columns:

```text
stream_id
segment_id
talent_id
talent_name
video_id
segment_index
source
speaker
start_sec
timecode
code_title_monetization
code_title_interactive_entertainment
code_event_paid_message_visible
code_personality_reassurance
```

The function can then hand the result to existing helpers:

```r
ena_cols <- ENA_setup(
  df = extract$data,
  units_col = "stream_id",
  conversation_cols = c("stream_id", "segment_index"),
  codes_cols = tidyselect::starts_with("code_"),
  metadata_cols = c("talent_name", "video_id", "source", "speaker", "start_sec")
)

ena_set <- ENA_accumulate_from_prep(
  ena_cols,
  window.size.back = extract$run$window_size_back
)
```

## Minimal Incremental Build Flow

### Step 1. Initialize Tables

Create tables if missing:

- `streams`
- `segments`
- `codebook`
- `code_applications`
- `processing_manifest`
- `ena_runs`

Keep schema creation in a small R helper, for example:

`r_scripts/lib/ENA_prep/ena_db_schema.R`

This mirrors the existing style of `r_scripts/lib/duckdb/db_schema.R`.

### Step 2. Register Sources

Scan only these first:

- `<talent>/text_playback/*.csv`
- `classifications.duckdb` tables `talents`, `videos`, `classifications`

Update `processing_manifest`. Skip unchanged text replay files.

### Step 3. Upsert Streams

For every replay file:

- Parse `video_id` from filename or first CSV row.
- Resolve `talent_name` from folder.
- Resolve `talent_id` from existing DuckDB talent table.
- Upsert `streams`.

### Step 4. Upsert Segments

For changed replay files:

- Read replay CSV.
- Create stable `segment_id`.
- Insert one row per replay row into `segments`.
- Delete/rebuild only the changed file's old segment rows.

### Step 5. Seed Codebook

Start with a tiny codebook:

- eight title-classification boolean codes
- `event:paid_message_visible`

Do not import every qualitative code family until the first ENA extract works.

### Step 6. Upsert Code Applications

Initial applications:

- Stream-level title codes from `classifications.duckdb.classifications`.
- Segment-level paid-message visible code from `segments`.

Later applications:

- monetary event groups
- shared/personality evidence rows
- summary-derived codes

### Step 7. Generate ENA Extract

Call `generate_ena_wide_extract()` with explicit filters and code IDs.

Outputs:

- `<output_root>/<ena_run_id>/<ena_run_id>_ena_wide.csv`
- `<output_root>/<ena_run_id>/<ena_run_id>_ena_wide.rds`
- `<output_root>/<ena_run_id>/<ena_run_id>_metadata.json`
- one `ena_runs` row

## Why This Is Small Enough

This avoids overengineering because:

- It does not replace existing subtitle, replay, summary, classification, or report workflows.
- It does not require raw chat/subtitle re-ingestion for v1.
- It does not require Parquet unless extracts become too large.
- It stores codes long-form and pivots only when an ENA run needs a wide table.
- It starts with the two most reliable code sources: title classifications and paid-message flags.
- It preserves reproducibility through run metadata without building a full orchestration framework.

## Explicit Non-Goals For V1

- No universal raw event warehouse.
- No migration of all existing markdown summaries into structured tables.
- No attempt to normalize every personality/monetary output immediately.
- No global all-talents all-codes wide CSV.
- No change to existing report-generation bundles.
- No destructive rewrite of `<talent>/Subtitles/Processed` or `<talent>/text_playback`.

## Recommended First Implementation Slice

Implement only:

1. `ena_db_schema.R`
2. `ena_register_text_playback.R` or a function in `ena_ingest.R`
3. `ena_seed_title_codes.R`
4. `ena_extracts.R` with `generate_ena_wide_extract()`

Run the first extract for one talent, one segment type, and a small code set:

- `segment_type = "replay_event"`
- `target_type = "segment"`
- codes:
  - `event:paid_message_visible`
  - `title:monetization`
  - `title:interactive_entertainment`
  - `title:personality_conversation`

That gives a real ENA-ready wide extract while keeping the durable infrastructure normalized and incremental.

