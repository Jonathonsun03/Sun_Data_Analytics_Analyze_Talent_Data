# Qualitative Data Prep Pipeline Notes

This folder records the design trail for the qualitative data prep and ENA extract pipeline.

## Current status

As of 2026-07-26, the implemented canonical path is the shared qualitative
schema in the unified talent lakehouse:

- schema: `r_scripts/lib/duckdb/qualitative_schema.R`
- transactional publisher: `r_scripts/lib/duckdb/qualitative_publish.R`
- runnable entrypoint: `r_scripts/run/publish_qualitative_coding.R`
- analysis loader: `r_scripts/lib/import_data/qualitative_transcripts.R`

The ordered notes below are retained as design history. Where they describe
CSV/RDS-only workflows, codebook-specific load tables, or advise postponing the
shared lakehouse, they are superseded by `AGENTS.md` and
`r_scripts/README.md`. Do not implement a parallel path from an older note.

Read in this order:

1. Order 01: `2026-05-10_incremental_ena_data_infrastructure_inventory.md`
   - Inventory of existing data, scripts, IDs, and reusable components.
2. Order 02: `2026-05-10_small_incremental_ena_extract_architecture.md`
   - Earlier normalized DuckDB-centered architecture proposal.
3. Order 03: `2026-05-10_direct_text_playback_to_ena_wide_v1.md`
   - Current v1 direction: direct converter from existing `text_playback` line CSVs plus long-form line-code assignments to `ena_wide.csv` and `ena_wide.rds`.
4. Order 04: `2026-05-10_pipeline`
   - Short direct v1 workflow outline.
5. Order 05: `2026-05-10_proposed_file_architecture`
   - Early file layout sketch.
6. Order 06: `2026-05-10_order_06_qualitative_ena_implementation_log.md`
   - Implementation log for today's Module 1/2/3 code, lean `qc/talents/...` task workflow, wrappers, migration/archive work, and smoke-test status.

Historical implementation direction:

- Order 06 describes the earlier implemented shape.
- Module 2 is now a lean transcript-level Codex task-packet workflow under `qc/talents/<talent_slug>/runs/<run_id>/`: build task, let Codex code `coded.csv`, validate output, merge into a new updated coding sheet.
- The Avaritia smoke run now lives at `qc/talents/avaritia_hawthorne_variance_project/runs/smoke`; the old `qualitative_coding/coding_runs/coding_sheet_avaritia_smoke` folder was moved to `qc/_archive/legacy_runs/`.
- Order 03 explains the conceptual pivot toward direct line-level `text_playback` conversion.
- These notes may still explain how older coding sheets were produced, but
  published analysis should use the canonical qualitative lakehouse interfaces.
