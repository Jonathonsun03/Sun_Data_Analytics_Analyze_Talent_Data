# Linux Report Render Wrappers

This folder contains Linux wrapper scripts that standardize how Bundle reports are rendered and where outputs are saved.

- `run_bundle_A_report.sh`
- `run_bundle_B_report.sh`
- `run_monthly_bundle_reports.sh`

These wrappers call the R render scripts, resolve roots, resolve talent folder names, and route outputs into the datalake structure.

Bundle A-specific scripts now live in:

- `bin/linux/render_reports/bundle_A/`
- `bin/linux/render_reports/bundle_A/README.md`

Bundle B-specific scripts now live in:

- `bin/linux/render_reports/bundle_B/`

The only top-level Bundle A wrapper intentionally kept is:

- `run_bundle_A_report.sh`

If you want the final client-facing Bundle A HTML with freshly generated interpretations and the editorial rewrite applied, use:

- `bin/linux/render_reports/run_bundle_A_report.sh`

Do not use `bin/linux/render_reports/bundle_A/run_bundle_A_artifacts.sh` for that purpose. The artifacts wrapper only exports figures and tables and does not render the final report.

Bundle A output layout is kept together per talent under one folder:

- `<datalake_root>/<talent>/reports/bundle_A/`
- final HTML report lives in that folder
- generated evidence files live in `artifacts/`
- generated interpretation text lives in `interpretations/`
- editorial rewrite work files also live under `interpretations/06_editorial_review/`

## What `run_bundle_A_report.sh` does

Path: `bin/linux/render_reports/run_bundle_A_report.sh`

It now serves as the main Bundle A entrypoint and wraps:

- `bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh`

Primary behavior:

1. Builds Bundle A artifacts.
2. Runs Bundle A interpretation generation.
3. Runs Bundle A editorial rewrite over the generated report text.
4. Renders the final Bundle A HTML report.

If you need the render-only step without artifacts or interpretations, use:

- `bin/linux/render_reports/bundle_A/run_bundle_A_render_only.sh`

## What `run_bundle_B_report.sh` does

Path: `bin/linux/render_reports/run_bundle_B_report.sh`

It now serves as the main Bundle B entrypoint and wraps:

- `bin/linux/render_reports/bundle_B/run_bundle_B_full_pipeline.sh`

Primary behavior:

1. Builds Bundle B artifacts.
2. Runs Bundle B interpretation generation.
3. Runs Bundle B editorial rewrite over the generated report text.
4. Renders the final Bundle B HTML report.

If you need the render-only step without artifacts or interpretations, use:

- `bin/linux/render_reports/bundle_B/run_bundle_B_render_only.sh`

## Defaults (Bundle A)

- Render script: `r_scripts/run/bundle_A/render_bundle_A.R`
- Bundle name: `bundle_A`
- Output prefix: `Bundle_A`
- Report subdir: `reports`
- Input source: `datalake`
- R binary: `Rscript`

For the full Bundle A pipeline, always pass an explicit talent selector such as `--talent`, `--talents`, `--talents-file`, or `--all`.

Note: talent folder resolution is exact by default and is checked against datalake folders.

## Common usage

Single talent:

```bash
bin/linux/render_reports/run_bundle_A_report.sh --talent "Leia Memoria【Variance Project】" --window-days 90 --input-source datalake
```

Full report with rewritten text:

```bash
bin/linux/render_reports/run_bundle_A_report.sh \
  --talent "Terberri Solaris Ch" \
  --window-days 90 \
  --input-source datalake
```

Batch from file:

```bash
bin/linux/render_reports/run_bundle_A_report.sh --talents-file notes/talent_list.txt --window-days 30 --input-source datalake
```

All talents from selected input source root:

```bash
bin/linux/render_reports/run_bundle_A_report.sh --all --window-days 60 --input-source datalake
```

Dry run (print commands only):

```bash
bin/linux/render_reports/run_bundle_A_report.sh --talent "Katya Sable 【Variance Project】" --window-days 90 --dry-run
```

Pass extra args through the wrapper stack:

```bash
bin/linux/render_reports/run_bundle_A_report.sh --talent "Avaritia Hawthorne 【Variance Project】" --window-days 90 -- --quiet
```

## Synthetic Demo Talent Workflow

When you want a client-safe sample report, generate a synthetic talent folder and keep its title classifications separate from the production export.

Generator entrypoint:

```bash
python3 py_scripts/run/demo_data/generate_demo_talent_dataset.py --talent-name "Northstar Story Lab Demo"
```

That generator creates:

- `<datalake_root>/<talent>/raw_data/video_analytics/video_analytics_<snapshot>.csv`
- `<datalake_root>/<talent>/raw_data/video_monetary/video_monetary_<snapshot>.csv`
- `<datalake_root>/<talent>/raw_data/video_demographics/video_demographics_<snapshot>.csv`
- `<datalake_root>/<talent>/raw_data/video_geography/video_geography_<snapshot>.csv`
- `<datalake_root>/<talent>/reports/demo_inputs/demo_title_classifications.csv`

For synthetic/demo talents, render Bundles A and B directly with the R render scripts and pass the isolated title CSV through `--titles-path`:

```bash
Rscript r_scripts/run/bundle_A/render_bundle_A.R \
  --talent "Northstar Story Lab Demo" \
  --data-source datalake \
  --titles-path "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Northstar Story Lab Demo/reports/demo_inputs/demo_title_classifications.csv"
```

```bash
Rscript r_scripts/run/bundle_B/render_bundle_B.R \
  --talent "Northstar Story Lab Demo" \
  --data-source datalake \
  --titles-path "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Northstar Story Lab Demo/reports/demo_inputs/demo_title_classifications.csv"
```

Notes:

- `--titles-path` keeps demo classifications out of the production `classification/output/title_classifications/` exports.
- The render scripts now accept `--titles-path` directly and forward that file into the Bundle A/B title join step.
- Generated demo HTML reports are written to the normal per-talent report folders under `<datalake_root>/<talent>/reports/bundle_A/` and `<datalake_root>/<talent>/reports/bundle_B/`.

## Talent resolution rules

By default, `--talent` must exactly match a folder name under datalake root.

If exact match fails, use:

- `--allow-partial-match`

This enables partial matching, but it can fail if multiple folders match.

## Window and naming behavior

Window segment is used in output prefix:

- `window_<N>d` when `--window-days N`
- `range_<start>_to_<end>` when both explicit dates are provided
- `from_<start>` / `until_<end>` for one-sided date ranges
- `all_data` when no window is passed
- overridden by `--window-label LABEL`

Final output file naming is controlled by the R script, but uses the wrapper-provided prefix, usually resulting in names like:

- `Bundle_A_window_90d_<talent_slug>.html`

Whether you use the wrapper or run `render_bundle_A.R` directly, the default Bundle A behavior is to keep each talent's Bundle A HTML, artifacts, and interpretations in that same `reports/bundle_A` folder tree unless you explicitly override `--output-dir`.

## Important options

- `--input-source staging|datalake`
- `--input-root PATH` (override talent data root used by render script)
- `--datalake-root PATH` (override output datalake root)
- `--staging-root PATH` (override staging root for discovery/render)
- `--report-subdir NAME` (default `reports`)
- `--bundle-name NAME` (default `bundle_A`)
- `--output-prefix NAME` (default `Bundle_A`)
- `--input PATH` (override input Rmd)
- `--rscript-bin PATH`
- `--quiet`
- `--dry-run`

## Troubleshooting

No exact talent match:

- Error example: `no exact folder match for talent query`
- Fix: use full folder name or add `--allow-partial-match`

Wrong root/source:

- Verify `--input-source`
- Verify `--input-root`, `--datalake-root`, and `--staging-root`
- For synthetic/demo talents, verify `--titles-path` points to the demo-only classification CSV

Render script not found:

- Verify path to `--render-script`
- Or run from repo root with defaults

## Defaults (Bundle B)

- Main wrapper: `bin/linux/render_reports/run_bundle_B_report.sh`
- Full pipeline wrapper: `bin/linux/render_reports/bundle_B/run_bundle_B_full_pipeline.sh`
- Render script: `r_scripts/run/bundle_B/render_bundle_B.R`
- Bundle name: `bundle_B`
- Output prefix: `Bundle_B`
- Report subdir: `reports`
- Input source: `datalake`

Bundle B output layout is kept together per talent under one folder:

- `<datalake_root>/<talent>/reports/bundle_B/`
- final HTML report lives in that folder
- generated evidence files live in `artifacts/`
- generated interpretation text lives in `interpretations/`

Use `--help` on either wrapper for full flag details.

## Bundle A Artifact + Pipeline Wrappers

Artifact builder:

- Path: `bin/linux/render_reports/bundle_A/run_bundle_A_artifacts.sh`
- Calls: `r_scripts/run/bundle_A/import_data.r`
- Writes:
  - `<datalake_root>/<talent>/reports/bundle_A/artifacts/figures/`
  - `<datalake_root>/<talent>/reports/bundle_A/artifacts/tables/`
  - `bundle_a_ai_inputs.json`
  - `bundle_a_artifact_manifest.json`

Interpretation generation:

- Path: `bin/linux/render_reports/bundle_A/run_bundle_A_interpretations.sh`
- Current role:
  - validates that Bundle A artifacts exist
  - builds one `input.md` per prompt under the datalake `interpretations/` tree
  - runs `codex exec` for each selected prompt
  - writes one `output.md` interpretation per prompt
  - includes section/chart prompts plus report bookends
- Test controls:
  - `--prompt-filter TEXT`
  - `--max-prompts N`

Editorial rewrite stage:

- Path: `bin/linux/render_reports/bundle_A/run_bundle_A_editorial_rewrite.sh`
- Role:
  - reads all client-facing Bundle A paragraphs
  - performs one full-report editorial rewrite pass with `codex exec`
  - overwrites the generated `output.md` files before final render
  - leaves internal QA outputs under `06_editorial_review/`

Full pipeline wrapper:

- Path: `bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh`
- Stage order:
  1. `run_bundle_A_artifacts.sh`
  2. `run_bundle_A_interpretations.sh`
  3. `run_bundle_A_editorial_rewrite.sh`
  4. `run_bundle_A_render_only.sh`

Example:

```bash
bin/linux/render_reports/run_bundle_A_report.sh \
  --talent "Leia Memoria【Variance Project】" \
  --window-days 90 \
  --input-source datalake
```

This command is the full Bundle A pipeline:

1. exports artifacts
2. generates interpretations
3. runs the editorial rewrite
4. renders the final HTML

Single-prompt Codex test:

```bash
bin/linux/render_reports/bundle_A/run_bundle_A_interpretations.sh \
  --talent "Leia Memoria【Variance Project】" \
  --prompt-filter "01_views_by_content_type" \
  --max-prompts 1
```

Skip the interpretation-stage scaffold:

```bash
bin/linux/render_reports/run_bundle_A_report.sh \
  --talent "Leia Memoria【Variance Project】" \
  --window-days 90 \
  --input-source datalake \
  --skip-interpretation
```

## Bundle B Artifact + Pipeline Wrappers

Artifact builder:

- Path: `bin/linux/render_reports/bundle_B/run_bundle_B_artifacts.sh`
- Calls: `r_scripts/run/bundle_B/import_data.r`
- Writes:
  - `<datalake_root>/<talent>/reports/bundle_B/artifacts/figures/`
  - `<datalake_root>/<talent>/reports/bundle_B/artifacts/tables/`
  - `bundle_b_ai_inputs.json`
  - `bundle_b_artifact_manifest.json`

Interpretation generation:

- Path: `bin/linux/render_reports/bundle_B/run_bundle_B_interpretations.sh`
- Current role:
  - validates that Bundle B artifacts exist
  - builds one `input.md` per prompt under the datalake `interpretations/` tree
  - runs `codex exec` for each selected prompt
  - writes one `output.md` interpretation per prompt
  - includes section/chart prompts plus report bookends
- Test controls:
  - `--prompt-filter TEXT`
  - `--max-prompts N`
  - `--dry-run`

Editorial rewrite:

- Path: `bin/linux/render_reports/bundle_B/run_bundle_B_editorial_rewrite.sh`
- Reads the generated client-facing Bundle B interpretation markdown
- Runs one full-report rewrite prompt with `codex exec`
- Overwrites the existing report-facing `output.md` files before render
- Supports:
  - `--prompt-filter TEXT`
  - `--dry-run`

Render-only step:

- Path: `bin/linux/render_reports/bundle_B/run_bundle_B_render_only.sh`
- Calls: `r_scripts/run/bundle_B/render_bundle_B.R`
- Writes final HTML into the same talent `reports/bundle_B` folder used by artifacts and interpretations

Full pipeline wrapper:

- Path: `bin/linux/render_reports/bundle_B/run_bundle_B_full_pipeline.sh`
- Stage order:
  1. `run_bundle_B_artifacts.sh`
  2. `run_bundle_B_interpretations.sh`
  3. `run_bundle_B_editorial_rewrite.sh`
  4. `run_bundle_B_render_only.sh`

Example:

```bash
bin/linux/render_reports/run_bundle_B_report.sh \
  --talent "Avaritia Hawthorne 【Variance Project】" \
  --window-days 90 \
  --input-source datalake
```

Single-prompt Codex test:

```bash
bin/linux/render_reports/bundle_B/run_bundle_B_interpretations.sh \
  --talent "Avaritia Hawthorne 【Variance Project】" \
  --prompt-filter "01_engagement_distribution_by_content_type" \
  --max-prompts 1
```

Skip interpretation and render from fresh artifacts:

```bash
bin/linux/render_reports/run_bundle_B_report.sh \
  --talent "Avaritia Hawthorne 【Variance Project】" \
  --window-days 90 \
  --input-source datalake \
  --skip-interpretation
```

## Monthly Run (Bundle A + Bundle B, 90 days, all talents)

Path: `bin/linux/render_reports/run_monthly_bundle_reports.sh`

This wrapper runs both bundle wrappers sequentially using:

- `--all`
- `--window-days 90` (default)

Example (analytics container paths):

```bash
bin/linux/render_reports/run_monthly_bundle_reports.sh \
  --input-source datalake \
  --input-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data \
  --datalake-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data
```

Dry run:

```bash
bin/linux/render_reports/run_monthly_bundle_reports.sh \
  --input-source datalake \
  --input-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data \
  --datalake-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data \
  --dry-run
```

Cron example (run at 03:00 on the 1st day of every month):

```cron
0 3 1 * * cd /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data && bin/linux/render_reports/run_monthly_bundle_reports.sh --input-source datalake --input-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data --datalake-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data >> /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/notes/logs/monthly_bundle_reports.log 2>&1
```

If using the log path above, create it once:

```bash
mkdir -p notes/logs
```
