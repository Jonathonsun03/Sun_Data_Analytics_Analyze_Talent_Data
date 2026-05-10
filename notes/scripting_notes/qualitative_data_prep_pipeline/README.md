# Qualitative Data Prep Pipeline Notes

This folder records the design trail for the qualitative data prep and ENA extract pipeline.

Read in this order:

1. `2026-05-10_incremental_ena_data_infrastructure_inventory.md`
   - Inventory of existing data, scripts, IDs, and reusable components.
2. `2026-05-10_small_incremental_ena_extract_architecture.md`
   - Earlier normalized DuckDB-centered architecture proposal.
3. `2026-05-10_direct_text_playback_to_ena_wide_v1.md`
   - Current v1 direction: direct converter from existing `text_playback` line CSVs plus long-form line-code assignments to `ena_wide.csv` and `ena_wide.rds`.

Current implementation direction:

- Start with `2026-05-10_direct_text_playback_to_ena_wide_v1.md`.
- Do not build a broad abstract warehouse first.
- Use the normalized architecture note as a later growth path if the direct converter becomes too hard to manage with run folders and CSV/RDS outputs alone.

Note:

- `2026-05-10_proposed_file_architecture` is currently empty and appears to be a placeholder.

