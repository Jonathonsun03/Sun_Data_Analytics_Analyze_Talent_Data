# Bundle E Customized Text Pipeline Implementation Prompt

Implement Bundle E so it follows the same customized-text workflow and quality bar now used for Bundle A and Bundle B.

## Goal

Build Bundle E so each report can:

1. export the underlying figures and tables used by the report into the talent's datalake Bundle E folder
2. generate section-level and chart/table-level interpretation text from those exported artifacts using `codex exec`
3. generate a report-level executive summary and conclusion
4. run a final editorial rewrite pass that overwrites all client-facing interpretation text before the final report render
5. render the final Bundle E HTML report with the generated text included in the correct places

The result should feel architecturally consistent with Bundle A and Bundle B rather than like a separate one-off system.

## Repo and output rules

- Read raw and intermediate data from `/mnt/datalake/`
- Write rendered artifacts, interpretations, logs, and final reports to `/mnt/datalake/`
- Keep Bundle E prompts in this repository under `prompts/reports/bundle_e/`
- Do not create new scripts in the datalake
- Do not create files in the repository root
- Reuse existing project utilities where possible

## Target folder layout

Prompts:

- `prompts/reports/bundle_e/`

Per-talent datalake output:

- `<datalake_root>/<talent>/reports/bundle_E/`
- `<datalake_root>/<talent>/reports/bundle_E/artifacts/figures/`
- `<datalake_root>/<talent>/reports/bundle_E/artifacts/tables/`
- `<datalake_root>/<talent>/reports/bundle_E/artifacts/bundle_e_ai_inputs.json`
- `<datalake_root>/<talent>/reports/bundle_E/artifacts/bundle_e_artifact_manifest.json`
- `<datalake_root>/<talent>/reports/bundle_E/interpretations/<section>/<prompt>/input.md`
- `<datalake_root>/<talent>/reports/bundle_E/interpretations/<section>/<prompt>/output.md`

Keep the final HTML, artifacts, and interpretations together under the same Bundle E folder for each talent.

## Required implementation pattern

### 1. Inspect Bundle E before changing anything

Read and understand:

- the Bundle E R Markdown template
- the Bundle E import/artifact-building script
- the Bundle E render script
- the current shell wrappers for Bundle E
- any existing Bundle E description or notes

Do not assume Bundle E has the same sections as Bundle A or B. Derive the prompt structure from the actual Bundle E report headings and subsections.

### 2. Build the Bundle E prompt tree from the report structure

Create a prompt hierarchy under:

- `prompts/reports/bundle_e/`

The top-level folders should mirror Bundle E's actual major report sections.

Within each section:

- add `00_section_synthesis/` when a section-level bridge paragraph is useful
- add one prompt folder per chart or table that should receive custom interpretation text

Also add:

- `06_report_bookends/`
  - `01_executive_summary/`
  - `02_conclusion/`
- `07_editorial_review/`
  - `01_full_report_editorial_review/`
  - `02_full_report_editorial_rewrite/`

If Bundle E has fewer or more major sections than Bundle B, keep the numbering logical, but preserve the same bookend/editorial concept.

### 3. Match the writing style used in the current system

All client-facing Bundle E prompts should produce:

- markdown only
- one short narrative paragraph
- no bullets
- no headings
- no labeled sections

The paragraph style should:

- lead with interpretation
- explain why it matters
- end with a natural implication, recommendation, or next-step bridge
- sound report-ready and executive-friendly
- avoid repetitive phrasing
- avoid overly rigid templates
- avoid restating the full date range in every paragraph

The executive summary and conclusion should each be one short report-ready paragraph.

The editorial-review prompt may use flat bullets for internal QA.
The editorial-rewrite prompt must return JSON keyed by relative paragraph path so the rewrite stage can overwrite the existing `output.md` files.

### 4. Export Bundle E evidence to the datalake

Update the Bundle E data-prep / import stage so it exports the true evidence layer for interpretation generation:

- static figures as PNG files
- supporting tables as CSV files
- a `bundle_e_ai_inputs.json`
- a `bundle_e_artifact_manifest.json`

Important:

- The interpretation step should rely primarily on the prepared tables, not on interactive plotly objects or rendered HTML.
- The exported artifacts must line up with the same filtered date window used by the final report render.
- Use the shared datalake helpers and keep the pathing consistent with the Bundle A and Bundle B implementations.

### 5. Wire interpretation generation with `codex exec`

Create or update:

- `bin/linux/render_reports/bundle_E/run_bundle_E_interpretations.sh`

It should:

- resolve the correct datalake talent folder
- find the Bundle E prompt folders
- map each prompt folder to the relevant artifact keys
- build `input.md` files in the datalake interpretation tree
- call `codex exec`
- write one `output.md` per prompt

It should also:

- support `--talent`, `--talents`, `--talents-file`, and `--all`
- support `--prompt-filter` and `--max-prompts`
- support `--window-days`, `--start-date`, `--end-date`, `--input-source`, `--input-root`, `--datalake-root`, and `--staging-root`
- include continuity context so each paragraph reads like part of one report
- skip internal editorial prompts by default unless explicitly targeted

### 6. Add the editorial rewrite stage

Create:

- `bin/linux/render_reports/bundle_E/run_bundle_E_editorial_rewrite.sh`

It should:

- collect all client-facing Bundle E `output.md` files
- include executive summary and conclusion
- optionally include editorial QA notes if they exist
- build a single JSON payload of the current report paragraphs
- call `codex exec` once for the full-report rewrite
- validate the returned JSON
- overwrite the client-facing `output.md` files in place

This stage should happen after interpretation generation and before final HTML render.

### 7. Update the Bundle E template to include generated text

Update the Bundle E Rmd so it:

- reads interpretation files from `params$render_output_dir/interpretations/...`
- prints section synthesis paragraphs in the right places
- prints chart/table interpretations below the relevant visuals
- prints the executive summary near the top
- prints the conclusion near the end

Do not print the internal editorial-review notes into the client-facing HTML.

### 8. Organize Bundle E shell wrappers the same way

Place Bundle E scripts under:

- `bin/linux/render_reports/bundle_E/`

Expected wrappers:

- `run_bundle_E_artifacts.sh`
- `run_bundle_E_interpretations.sh`
- `run_bundle_E_editorial_rewrite.sh`
- `run_bundle_E_render_only.sh`
- `run_bundle_E_full_pipeline.sh`

And keep:

- `bin/linux/render_reports/run_bundle_E_report.sh`

as the main wrapper entrypoint.

The full pipeline order should be:

1. artifacts
2. interpretations
3. editorial rewrite
4. render

### 9. Keep interface consistency

The Bundle E wrappers should expose the same kinds of flags used by Bundle A and Bundle B, including:

- `--talent`
- `--talents`
- `--talents-file`
- `--all`
- `--window-days`
- `--start-date`
- `--end-date`
- `--input-source`
- `--input-root`
- `--datalake-root`
- `--staging-root`
- `--prompt-filter`
- `--max-prompts`
- `--dry-run`
- `--skip-interpretation`
- `--skip-editorial-rewrite`

### 10. Test on one talent first

Validate the full Bundle E workflow on one talent before scaling up.

Minimum validation:

1. confirm artifacts export to the correct datalake Bundle E folder
2. confirm at least one chart-level prompt generates correctly
3. confirm executive summary and conclusion generate correctly
4. confirm the editorial rewrite overwrites the report-facing paragraphs
5. confirm the final HTML renders successfully and includes the generated text

Use exact dates in any validation notes so the evidence window is unambiguous.

### 11. Document the work in scripting notes

Create a scripting note under:

- `notes/scripting_notes/`

Document:

- the Bundle E prompt structure
- how artifacts are exported
- how `codex exec` is used for interpretation generation
- how the editorial rewrite works
- where outputs are written in the datalake
- how the shell wrappers are organized
- what talent/date-range was used for validation

Also update:

- `notes/scripting_notes/README.md`
- the Bundle E description note if one exists
- `bin/linux/render_reports/README.md`

## Tone and quality bar

Bundle E should match the same overall text quality standard now expected for Bundle A and Bundle B:

- concise
- narrative
- non-redundant
- analytically grounded
- practical
- cohesive from section to section

The final report should not feel like a stack of isolated prompt outputs. It should read like a single edited report.

## Deliverable expectation

When the implementation is complete, Bundle E should be runnable from a single command in the same pattern as the other bundles, for example:

```bash
bin/linux/render_reports/run_bundle_E_report.sh \
  --talent "<Talent Name>" \
  --window-days 90 \
  --input-source datalake
```

If any Bundle E sections require deviations from Bundle A / Bundle B because of different report logic, preserve the shared architecture but document the differences clearly.
