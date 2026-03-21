# 2026-03-20: Bundle B Customized Text Pipeline

## Goal

Align Bundle B to the same customized-text workflow already implemented for Bundle A so each Bundle B report can:

- export prompt-ready evidence to the datalake
- generate chart-level and section-level interpretation text with `codex exec`
- add a report-level executive summary and conclusion
- run a full editorial rewrite that overwrites the generated text before final render

## Prompt structure

Bundle B prompts live under:

- `prompts/reports/bundle_b/`

The prompt tree follows the report structure:

- `01_format_level_retention_consistency/`
- `02_category_strengths_and_weaknesses/`
- `03_content_attributes_matter_for_engagement_outcomes/`
- `04_scheduling_effects_matter_for_publishing_decisions/`
- `05_turn_findings_into_action/`

Two new folders were added to match the Bundle A pattern:

- `06_report_bookends/`
  - `01_executive_summary/`
  - `02_conclusion/`
- `07_editorial_review/`
  - `01_full_report_editorial_review/`
  - `02_full_report_editorial_rewrite/`

The output style remains one short narrative paragraph per client-facing prompt, with the internal editorial-review prompt reserved for QA bullets if needed.

## Artifact and datalake routing

Bundle B already had datalake artifact export in place through:

- `r_scripts/run/bundle_B/import_data.r`

Artifacts continue to write under the talent Bundle B folder:

- `<datalake_root>/<talent>/reports/bundle_B/artifacts/figures/`
- `<datalake_root>/<talent>/reports/bundle_B/artifacts/tables/`
- `bundle_b_ai_inputs.json`
- `bundle_b_artifact_manifest.json`

This keeps Bundle B aligned with the same per-talent folder strategy used by Bundle A.

## Interpretation generation

The interpretation runner is:

- `bin/linux/render_reports/bundle_B/run_bundle_B_interpretations.sh`

It was upgraded so it now:

- maps Bundle B prompts to the correct exported tables and figures
- includes report continuity context in each prompt input
- supports the new report bookend prompts
- keeps the narrative style consistent with Bundle A
- skips internal editorial-review prompts by default unless explicitly targeted with `--prompt-filter`

Interpretation outputs are written to:

- `<datalake_root>/<talent>/reports/bundle_B/interpretations/<section>/<prompt>/output.md`

## Editorial rewrite

A new runner was added:

- `bin/linux/render_reports/bundle_B/run_bundle_B_editorial_rewrite.sh`

This stage:

- collects all client-facing Bundle B `output.md` files
- builds a single JSON payload of the current report paragraphs
- runs the full-report rewrite prompt with `codex exec`
- validates the returned JSON
- overwrites the existing report-facing `output.md` files before render

The editorial rewrite work files live under:

- `<datalake_root>/<talent>/reports/bundle_B/interpretations/07_editorial_review/02_full_report_editorial_rewrite/`

## Report integration

The Bundle B template already supported interpretation inclusion via:

- `bundle_b_read_interpretation()`
- `bundle_b_emit_interpretation()`

The template was updated in:

- `templates/reports/Bundle_B/Bundle_B.Rmd`

New client-facing placements:

- executive summary near the top of the report
- conclusion at the end of the report

The editorial review/rewrite content is not printed directly into the final HTML.

## Shell wrappers

Bundle B now follows the same wrapper pattern as Bundle A:

- `bin/linux/render_reports/run_bundle_B_report.sh`
  - main entrypoint
- `bin/linux/render_reports/bundle_B/run_bundle_B_artifacts.sh`
- `bin/linux/render_reports/bundle_B/run_bundle_B_interpretations.sh`
- `bin/linux/render_reports/bundle_B/run_bundle_B_editorial_rewrite.sh`
- `bin/linux/render_reports/bundle_B/run_bundle_B_render_only.sh`
- `bin/linux/render_reports/bundle_B/run_bundle_B_full_pipeline.sh`

The full pipeline order is now:

1. artifacts
2. interpretations
3. editorial rewrite
4. render

## Validation run

Validated on:

- `Avaritia Hawthorne 【Variance Project】`

Explicit date range used for validation:

- `2025-12-21` to `2026-03-20`

Validation steps completed:

1. Generated the new Bundle B report bookends with:
   - `run_bundle_B_interpretations.sh --prompt-filter "06_report_bookends"`
2. Ran the new editorial rewrite stage with:
   - `run_bundle_B_editorial_rewrite.sh`
3. Rendered the final report with:
   - `run_bundle_B_render_only.sh --start-date 2025-12-21 --end-date 2026-03-20`

Validation outcome:

- executive summary and conclusion were generated successfully
- the editorial rewrite overwrote 20 report-facing paragraphs
- final HTML rendered successfully to the talent Bundle B folder

Rendered output:

- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Avaritia Hawthorne 【Variance Project】/reports/bundle_B/Bundle_B_range_2025-12-21_to_2026-03-20_avaritia_hawthorne_variance_project.html`

## Current state

Bundle B now matches Bundle A’s customized-text workflow closely enough that future report bundles can reuse the same implementation pattern:

- prompt tree aligned to report structure
- datalake artifacts as evidence layer
- `codex exec` interpretation generation
- report bookends
- final editorial overwrite before HTML render
