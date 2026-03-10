# Linux Report Render Wrappers

This folder contains Linux wrapper scripts that standardize how Bundle reports are rendered and where outputs are saved.

- `run_bundle_A_report.sh`
- `run_bundle_B_report.sh`

These wrappers call the R render scripts, resolve roots, resolve talent folder names, and route outputs into the datalake structure.

## What `run_bundle_A_report.sh` does

Path: `bin/linux/render_reports/run_bundle_A_report.sh`

It wraps:

- `scripts/run/bundle_A/render_bundle_A.R`

Primary behavior:

1. Resolves repo root and datalake root.
2. Resolves talent(s) from `--talent`, `--talents`, `--talents-file`, or `--all`.
3. Resolves each talent to an output folder name under datalake.
4. Builds output path:
   - `<datalake_root>/<talent>/<report_subdir>/<bundle_name>/`
5. Builds output filename prefix:
   - `<output_prefix>_<window_segment>`
6. Calls the R script once per talent with the resolved args.

## Defaults (Bundle A)

- Render script: `scripts/run/bundle_A/render_bundle_A.R`
- Bundle name: `bundle_A`
- Output prefix: `Bundle_A`
- Report subdir: `reports`
- Input source: `datalake`
- R binary: `Rscript`

If no talent selector is provided, the wrapper defaults to:

- `Ava`

Note: talent folder resolution is exact by default and is checked against datalake folders.

## Common usage

Single talent:

```bash
bin/linux/render_reports/run_bundle_A_report.sh --talent "Leia Memoria【Variance Project】" --window-days 90 --input-source datalake
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

Pass extra args directly to `render_bundle_A.R`:

```bash
bin/linux/render_reports/run_bundle_A_report.sh --talent "Avaritia Hawthorne 【Variance Project】" --window-days 90 -- --quiet
```

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

Render script not found:

- Verify path to `--render-script`
- Or run from repo root with defaults

## Bundle B note

`run_bundle_B_report.sh` follows the same wrapper pattern, and defaults to:

- Render script: `scripts/run/bundle_B/render_bundle_B.R`
- Bundle name: `bundle_B`
- Output prefix: `Bundle_B`
- Input source: `datalake`

Use `--help` on either wrapper for full flag details.
