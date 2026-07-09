# ENA Prep Library Layout

This folder is organized by the current qualitative ENA pipeline modules.

## `shared/`

Reusable helpers used across modules:

- path resolution
- talent/source discovery
- text playback schema validation
- qualitative codebook normalization
- stable line ID construction
- legacy pre-coding queue helpers

## `module_1_build_coding_sheet/`

Builds the line-level qualitative coding spreadsheet.

Primary file:

- `build_qualitative_coding_sheet.R`

Legacy compatibility file:

- `ena_precoding_run.R`

## `module_2_code_batches/`

Placeholder/skeleton for later qualitative coding in batches.

Primary file:

- `code_qualitative_batch.R`

## `module_3_load_duckdb/`

Loads coded or partially coded sheets into DuckDB for querying.

Primary file:

- `load_coded_sheet_to_duckdb.R`

## `analysis_helpers/`

Existing rENA analysis and plotting helpers. These are not part of the first
three pipeline modules, but they remain available for downstream ENA analysis.

