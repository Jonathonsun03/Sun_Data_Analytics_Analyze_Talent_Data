# Bundle A Customized Text Pipeline

## Purpose

This note documents how customized interpretation text was added to Bundle A so the same pattern can be reused for Bundle B and future report bundles.

The core design goal was:

- keep the report visuals interactive for the HTML report,
- but generate interpretation text from the underlying exported tables and figures rather than asking the model to interpret `plotly` or `DT` widgets directly.

## Final architecture

The Bundle A customized-text flow now has three stages:

1. export report evidence,
2. generate interpretation text,
3. render the final report with that text included.

In practical terms:

- `import_data.r` builds and exports the evidence layer,
- `codex exec` reads the exported evidence and writes interpretation markdown,
- `Bundle_A.Rmd` reads the interpretation markdown and inserts it into the final report.

## Files and responsibilities

### Prompt definitions

- `prompts/reports/bundle_a/`

Prompt folders were reorganized to mirror the actual Bundle A section headings:

- `01_overall_performance_snapshot/`
- `02_trends_over_time/`
- `03_audience_composition/`
- `04_content_strategy_deep_dive/`

Within those sections:

- each chart/table interpretation point has its own `prompt.md`,
- section-level synthesis prompts were added where helpful.
- later additions introduced:
  - report bookends for executive summary and conclusion
  - an editorial review / rewrite stage

### Artifact export

- `r_scripts/run/bundle_A/import_data.r`

This was rewired so it exports Bundle A artifacts to the talent's datalake report folder:

- `<datalake_root>/<talent>/reports/bundle_A/artifacts/figures/`
- `<datalake_root>/<talent>/reports/bundle_A/artifacts/tables/`

It also writes:

- `bundle_a_ai_inputs.json`
- `bundle_a_artifact_manifest.json`

Key implementation decisions:

- use the shared datalake helper for path consistency,
- align artifact filenames to prompt/report keys,
- overwrite previous PNG and CSV exports on rerun,
- keep artifact windows aligned to the same date window as the final report.

### Interpretation generation

Bundle A shell runners were grouped under:

- `bin/linux/render_reports/bundle_A/`

Current scripts:

- `run_bundle_A_artifacts.sh`
- `run_bundle_A_interpretations.sh`
- `run_bundle_A_render_only.sh`
- `run_bundle_A_full_pipeline.sh`

The main convenience entrypoint is:

- `bin/linux/render_reports/run_bundle_A_report.sh`

That top-level wrapper now runs the full Bundle A pipeline:

1. artifacts
2. interpretations
3. final render

The interpretation runner:

- reads the Bundle A artifacts from datalake,
- builds one `input.md` per prompt,
- calls `codex exec`,
- writes one `output.md` per prompt under:
  - `<datalake_root>/<talent>/reports/bundle_A/interpretations/<section>/<prompt>/output.md`

Later, a dedicated editorial rewrite runner was added:

- `bin/linux/render_reports/bundle_A/run_bundle_A_editorial_rewrite.sh`

That stage:

- gathers all client-facing paragraphs across the report,
- runs one full-report rewrite pass with `codex exec`,
- overwrites the section/bookend `output.md` files,
- leaves internal editorial assets under `interpretations/06_editorial_review/`

## Narrative style rules

All Bundle A prompts were normalized to use the same response pattern:

- one short paragraph only,
- interpretation first,
- then why it matters,
- then a natural recommended follow-up or bridge,
- no bullets,
- no labeled sections,
- no repetitive phrasing.

This was reinforced in two places:

- inside each `prompt.md`,
- inside the shell-side Codex input wrapper so the runner also insists on paragraph-only output.

Because paragraph-level generation still tended to sound prompt-by-prompt, a second-stage editorial rewrite pass was added later so the final rendered report uses revised, report-aware text rather than the first-draft prompt outputs.

## Report inclusion

- `templates/reports/Bundle_A/Bundle_A.Rmd`

The template was updated so it can:

- locate the current talent's `interpretations/` folder using `params$render_output_dir`,
- read each `output.md`,
- print the interpretation text below the appropriate chart or section intro,
- safely skip any missing interpretation file without breaking the render.

The template was also extended to render:

- an executive summary near the top of the report
- a conclusion at the end of the report

This keeps the final report flexible during testing:

- if text exists, it is included,
- if a prompt has not been run yet, the report still renders.

## Output routing

Bundle A outputs were standardized so all outputs for a talent live in one folder tree:

- `<datalake_root>/<talent>/reports/bundle_A/`

That folder now holds:

- `report/current/` for the most recent final HTML report,
- `report/archive/` for older final HTML reports,
- `artifacts/`,
- `interpretations/`

The render-only wrapper can skip existing generated interpretation markdown with `--no-interpretations`.

Final HTML filenames include the render date and analysis window, for example:

- `Bundle_A_2026-04-30_window_90d_leia_memoria_variance_project.html`

Within `interpretations/`, later additions include:

- `05_report_bookends/` for executive summary and conclusion
- `06_editorial_review/` for editorial QA and the rewrite stage work files

This routing is used consistently by:

- `import_data.r`,
- interpretation runners,
- `render_bundle_A.R`,
- the Bundle A shell wrappers.

## Testing completed

Bundle A was tested with one talent first:

- `Leia Memoria【Variance Project】`

Validation completed included:

1. artifact export to datalake,
2. single-prompt Codex test,
3. full prompt-set generation,
4. format check on generated markdown,
5. final HTML render with included interpretation text.

The Leia run confirmed:

- all 17 Bundle A prompts generated outputs,
- the outputs were one-paragraph narrative interpretations,
- the final HTML included the generated text.

## Commands used as reference

Full Bundle A pipeline:

```bash
bin/linux/render_reports/run_bundle_A_report.sh \
  --talent "Leia Memoria【Variance Project】" \
  --window-days 90 \
  --input-source datalake
```

Single prompt test:

```bash
bin/linux/render_reports/bundle_A/run_bundle_A_interpretations.sh \
  --talent "Leia Memoria【Variance Project】" \
  --prompt-filter "01_views_by_content_type" \
  --max-prompts 1
```

Render-only step:

```bash
bin/linux/render_reports/bundle_A/run_bundle_A_render_only.sh \
  --talent "Leia Memoria【Variance Project】" \
  --window-days 90 \
  --input-source datalake
```

Editorial rewrite only:

```bash
bin/linux/render_reports/bundle_A/run_bundle_A_editorial_rewrite.sh \
  --talent "Leia Memoria【Variance Project】"
```

## How to repeat this for another report bundle

To add customized text to another bundle, reuse the same sequence:

1. map the report heading structure,
2. create prompt folders aligned to those headings,
3. export the underlying tables and figures to the datalake report folder,
4. generate prompt-specific interpretation markdown from those artifacts,
5. read those markdown files back into the final Rmd,
6. keep the shell wrappers organized so one top-level wrapper can run the whole flow.

For Bundle B specifically, this note should be paired with:

- `prompts/reports/bundle_b/BUNDLE_B_CUSTOM_TEXT_IMPLEMENTATION_PROMPT.md`
