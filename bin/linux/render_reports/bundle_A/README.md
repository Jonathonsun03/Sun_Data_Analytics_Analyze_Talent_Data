# Bundle A Wrappers

This folder contains the Bundle A Linux wrapper scripts used to build artifacts, generate interpretations, apply the editorial rewrite, and render the final HTML report.

## Main entrypoints

Full report pipeline:

`bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh`

Top-level shortcut:

`bin/linux/render_reports/run_bundle_A_report.sh`

Artifacts only:

`bin/linux/render_reports/bundle_A/run_bundle_A_artifacts.sh`

Interpretations only:

`bin/linux/render_reports/bundle_A/run_bundle_A_interpretations.sh`

Editorial rewrite only:

`bin/linux/render_reports/bundle_A/run_bundle_A_editorial_rewrite.sh`

Render only:

`bin/linux/render_reports/bundle_A/run_bundle_A_render_only.sh`

## Recommended usage

Full Bundle A report for one talent:

```bash
bin/linux/render_reports/run_bundle_A_report.sh --talent "Rius Isonder Ch"
```

Equivalent direct call to the full Bundle A wrapper:

```bash
bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh --talent "Rius Isonder Ch"
```

Artifacts-only smoke test:

```bash
bin/linux/render_reports/bundle_A/run_bundle_A_artifacts.sh --talent "Rius Isonder Ch"
```

Dry run:

```bash
bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh --talent "Rius Isonder Ch" --dry-run
```

Skip AI interpretation and editorial rewrite:

```bash
bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh \
  --talent "Rius Isonder Ch" \
  --skip-interpretation \
  --skip-editorial-rewrite
```

## Talent naming

Bundle A wrappers resolve talents against datalake folder names.

Use the full folder name when possible:

- `Rius Isonder Ch`
- `Terberri Solaris Ch`
- `Avaritia Hawthorne 【Variance Project】`

Do not use the title-classification slug form here unless a wrapper explicitly says it accepts partial matching.

## Data source and outputs

Bundle A wrappers default to:

- input source: `datalake`
- output root: `<datalake_root>/<talent>/reports/bundle_A/`

Artifacts are written to:

- `<datalake_root>/<talent>/reports/bundle_A/artifacts/figures/`
- `<datalake_root>/<talent>/reports/bundle_A/artifacts/tables/`
- `<datalake_root>/<talent>/reports/bundle_A/artifacts/bundle_a_ai_inputs.json`
- `<datalake_root>/<talent>/reports/bundle_A/artifacts/bundle_a_artifact_manifest.json`

Interpretation markdown is written under:

- `<datalake_root>/<talent>/reports/bundle_A/interpretations/`

Final HTML report is written under:

- `<datalake_root>/<talent>/reports/bundle_A/report/current/`

Older current HTML reports are moved to:

- `<datalake_root>/<talent>/reports/bundle_A/report/archive/`

Use `--no-interpretations` on the render-only wrapper to render the report without reading existing generated interpretation markdown.

Bundle A report filenames include the render date and analysis window, for example:

- `Bundle_A_2026-04-30_window_90d_rius_isonder_ch.html`

## Title classification dependency

Bundle A joins title classifications from:

`classification/output/title_classifications/classification_export_gpt-5-mini_from_duckdb.csv`

If you refresh title classifications manually, make sure this export is current before running Bundle A.

## Missing audience demographics behavior

Some talents have analytics, monetary, and geography data, but their audience age/gender export is empty or only contains `(no data)` rows.

Bundle A now handles that case gracefully:

- artifacts still build
- the audience demographics artifact is omitted
- interpretation prompts for `03_audience_composition` are skipped
- the final report omits the Audience Composition section instead of failing

This means a full Bundle A report can still be generated even when age/gender demographics are unavailable.

## Useful flags

- `--talent NAME`
- `--talents "A,B,C"`
- `--talents-file PATH`
- `--all`
- `--window-days N`
- `--start-date YYYY-MM-DD`
- `--end-date YYYY-MM-DD`
- `--input-source staging|datalake`
- `--input-root PATH`
- `--datalake-root PATH`
- `--staging-root PATH`
- `--allow-partial-match`
- `--dry-run`

Use `--help` on any wrapper for full flag details.
