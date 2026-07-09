# Talent Research Instructions

Use this location as the canonical destination for talent-level qualitative notes:

- Root: `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/notes/research_notes`
- Per-talent folder pattern: `/notes/research_notes/<talent_slug>/`

Each talent folder should contain:
- `findings.md`
- `references.md`
- `definitions.md`

## Repro Workflow

1. Analyze all `*_summary.md` files in:
   `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<TALENT_NAME>/stream_summary_codex`
2. Build `findings.md`, `references.md`, and `definitions.md`.
3. Save final files to:
   `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/notes/research_notes/<talent_slug>/`
4. Confirm files exist in `research_notes`.
5. Remove temporary datalake `notes` folder only after destination verification.

## Notes

- `findings.md` should reference the local file as `definitions.md` (not `notes/definitions.md`).
- Keep source paths and evidence counts in `references.md` for reproducibility.
