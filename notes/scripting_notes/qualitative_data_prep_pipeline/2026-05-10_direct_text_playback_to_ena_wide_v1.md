# Direct Text Playback To ENA Wide V1

Date: 2026-05-10

Status: current first implementation direction.

This note revises the earlier ENA extract architecture. The first implementation should not start by building a broad abstract warehouse. It should start with a direct converter from existing line-level `text_playback` CSV files plus long-form qualitative code assignments into ENA-ready wide extracts.

## Why This Revision

The previous architecture proposed normalized tables such as `streams`, `segments`, `codebook`, `code_applications`, `processing_manifest`, and `ena_runs`. That shape is still useful later, but it is too much structure for the first working pass.

The smallest useful v1 is:

1. Treat each `text_playback` CSV as the base line-level table.
2. Add stable IDs and talent metadata.
3. Read the qualitative codebook from the existing personality code log.
4. Read or store line-code assignments in long form.
5. Pivot those assignments wide.
6. Join them back to the copied line table.
7. Write `ena_wide.csv` and `ena_wide.rds`.

## Inputs

### Base Line Files

Use existing replay files:

`/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<Talent>/text_playback/*.csv`

Each input row is one ENA candidate line. Preserve these original columns exactly when present:

- `video_id`
- `sec`
- `source`
- `speaker`
- `text`
- `message_type`
- `paid_amount_text`
- `paid_amount_value`
- `paid_currency`
- `timecode`
- `replay_line`

Do not collapse all replay files into a single permanent mega-CSV. The converter may bind rows in memory for one run, but durable outputs should be run-specific.

### Qualitative Codebook

Read the existing qualitative codebook from:

`/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log.csv`

Relevant source columns:

- `Primary Code ID`
- `Primary Code`
- `Secondary Code ID`
- `Secondary Code`
- `Definition`
- `Date added`
- `Examples from text`

For ENA, derive a compact `code_id` from the primary and secondary code IDs, for example:

`personality_<primary_id>_<secondary_id>`

Example: `personality_A1_Ritualized_supporter_acknowledgment` can be simplified to `code_personality_a1` or `code_personality_a1_ritualized_supporter_acknowledgment` when pivoted. The implementation should make code column names syntactic and stable.

### Line-Code Assignments

Use a long-form assignment file or table with exactly these required fields:

| Column | Meaning |
| --- | --- |
| `line_id` | Stable line ID from the copied replay line table. |
| `code_id` | Code ID derived from the qualitative codebook or another approved code source. |
| `value` | Code value. For v1, use `1` or `TRUE` for applied codes. |
| `coder` | Human, script, or model label. |
| `coded_at` | Timestamp when the assignment was created. |
| `notes` | Optional note or evidence comment. |

Recommended first file location:

`/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/ena_extracts/code_assignments/line_code_assignments.csv`

This can later move into DuckDB, but v1 should work from CSV so coding can begin without database plumbing.

## Stable Columns Added To Replay Lines

For every line read from `text_playback`, add:

| Column | Rule |
| --- | --- |
| `talent_id` | Reuse existing DuckDB `talents.talent_id` if available; otherwise deterministic slug fallback. |
| `talent_name` | Talent folder name. |
| `stream_id` | Stable key, preferably `<talent_id>:<video_id>`. |
| `line_index` | 1-based row number within the source `text_playback` CSV after reading. |
| `line_id` | Stable hash key for the line. |

Recommended `line_id` ingredients:

```text
talent_id
video_id
source text_playback file basename
line_index
sec
source
speaker
normalized text
```

Use a hash so IDs are compact. In R, `digest::digest(..., algo = "xxhash64")` is consistent with existing title-hash usage, while `sha256` is also fine if already available. The important property is that the same source line produces the same `line_id` across reruns.

## Direct Converter Output

The converter should produce a run-specific folder:

`/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/ena_extracts/<run_id>/`

Required outputs:

- `ena_wide.csv`
- `ena_wide.rds`
- `run_metadata.json`

Optional diagnostic outputs:

- `line_table.csv`
- `codebook_used.csv`
- `line_code_assignments_used.csv`
- `missing_code_assignments.csv`

## ENA Wide Shape

The output starts as the copied replay line spreadsheet plus stable columns:

```text
talent_id
talent_name
stream_id
line_index
line_id
video_id
sec
source
speaker
text
message_type
paid_amount_text
paid_amount_value
paid_currency
timecode
replay_line
```

Then add one `code_*` column per qualitative code after pivoting assignments wider.

Example:

```text
code_personality_a1
code_personality_b1
code_personality_c1
```

Missing assignments should become `0` or `FALSE`, depending on the chosen output mode. For rENA, numeric `0/1` columns are usually the least surprising.

## Processing Steps

### 1. Read Replay Lines

For each selected talent and each selected `text_playback` CSV:

- Read the CSV.
- Validate the expected original columns.
- Preserve original columns.
- Add `talent_id`, `talent_name`, `stream_id`, `line_index`, `line_id`.

If an original column is missing, add it as `NA` and report it in `run_metadata.json`.

### 2. Read Qualitative Codebook

Read:

`Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log.csv`

Create a codebook frame with:

- `code_id`
- `code_column`
- `primary_code_id`
- `primary_code`
- `secondary_code_id`
- `secondary_code`
- `definition`

This codebook is not a warehouse table in v1. It is a run input and should be copied into the run folder for reproducibility.

### 3. Read Long-Form Assignments

Read assignments with:

```text
line_id, code_id, value, coder, coded_at, notes
```

Validate:

- every `line_id` exists in the copied replay line table, or write unmatched rows to diagnostics
- every `code_id` exists in the codebook, or write unknown codes to diagnostics
- duplicate `line_id + code_id` assignments are resolved deterministically

For duplicates, v1 should use a conservative rule:

- if any assignment has truthy `value`, output `1`
- concatenate distinct `coder` and `notes` only in diagnostics, not in `ena_wide.csv`

### 4. Pivot Codes Wide

Join assignments to the codebook to get `code_column`.

Pivot:

- rows: `line_id`
- columns: `code_column`
- values: `value`
- fill: `0`

### 5. Join Back To Lines

Left join wide code columns onto the copied replay line table by `line_id`.

Fill all missing `code_*` columns with `0`.

Keep the line table order stable:

1. `talent_name`
2. `video_id`
3. `line_index`

### 6. Write ENA Extracts

Write:

- `ena_wide.csv`
- `ena_wide.rds`

The `.csv` supports inspection and external use. The `.rds` preserves R types for direct rENA workflows.

### 7. Write Run Metadata

Write `run_metadata.json` with:

- `run_id`
- `created_at`
- `created_by`
- selected talents
- selected replay files
- source file sizes, mtimes, and hashes where available
- qualitative codebook path and hash
- assignment path and hash
- row counts:
  - replay lines read
  - codebook rows read
  - assignment rows read
  - assignments matched
  - assignments unmatched
  - final ENA rows
  - final code columns
- repo git commit if available
- repo dirty status if available
- R version
- package versions for `readr`, `dplyr`, `tidyr`, `digest`, `jsonlite`

## Proposed R Function

Put the first implementation in the existing ENA prep area:

`r_scripts/lib/ENA_prep/ena_text_playback_extract.R`

Function:

```r
generate_text_playback_ena_wide <- function(
  talent_query = "all",
  text_playback_root = get_datalake_root(),
  codebook_path = file.path(
    dirname(get_datalake_root()),
    "Processed",
    "Talent_Data",
    "Qualitative Codebook",
    "current",
    "personality_qualitative_code_log.csv"
  ),
  assignments_path,
  output_root = file.path(
    dirname(get_datalake_root()),
    "Processed",
    "Talent_Data",
    "ena_extracts"
  ),
  run_id = NULL,
  value_fill = 0
) {
  # 1. Resolve talents using existing select_talent().
  # 2. Read each <talent>/text_playback/*.csv.
  # 3. Preserve original replay columns.
  # 4. Add talent_id, talent_name, stream_id, line_index, line_id.
  # 5. Read personality_qualitative_code_log.csv as the codebook.
  # 6. Read line_code_assignments.csv in long format.
  # 7. Pivot line-code assignments to code_* columns.
  # 8. Join codes back to replay lines.
  # 9. Write ena_wide.csv, ena_wide.rds, and run_metadata.json.
  # 10. Return list(data = ena_wide, metadata = metadata, output_dir = output_dir).
}
```

This function should source and reuse:

- `r_scripts/lib/utils/datalake_root.r`
- `r_scripts/lib/utils/talent_select.R`
- existing talent ID logic from `r_scripts/lib/utils/talent_select.R` where practical

It should not require initializing a new DuckDB database.

## Relationship To Earlier Notes

This v1 direct converter supersedes the first implementation slice in `2026-05-10_small_incremental_ena_extract_architecture.md`.

Keep the older normalized design as a later growth path only:

- `streams` becomes useful once multiple segment definitions are needed.
- `segments` becomes useful once line-level replay rows are not enough.
- `code_applications` becomes useful once assignments should be queried across runs or coders.
- `processing_manifest` becomes useful once incremental refresh logic matters.
- `ena_runs` becomes useful once run metadata should be searchable.

For now, the durable system is a folder of reproducible ENA runs, each built directly from:

```text
text_playback CSVs
+ personality_qualitative_code_log.csv
+ line_code_assignments.csv
=> ena_wide.csv
=> ena_wide.rds
```

