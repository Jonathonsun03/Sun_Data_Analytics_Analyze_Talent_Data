# Bundle F Wrappers

This folder contains the Bundle F Linux wrapper scripts used to render the day-of-week performance report.

## Main entrypoints

Full report pipeline:

`bin/linux/render_reports/bundle_F/run_bundle_F_full_pipeline.sh`

Top-level shortcut:

`bin/linux/render_reports/run_bundle_F_report.sh`

Render only:

`bin/linux/render_reports/bundle_F/run_bundle_F_render_only.sh`

Compatibility artifact stage:

`bin/linux/render_reports/bundle_F/run_bundle_F_artifacts.sh`

## Recommended usage

Full Bundle F report for one talent:

```bash
bin/linux/render_reports/run_bundle_F_report.sh --talent "Rius Isonder Ch"
```

Render one content type:

```bash
bin/linux/render_reports/run_bundle_F_report.sh \
  --talent "Rius Isonder Ch" \
  --content-type live
```

Dry run:

```bash
bin/linux/render_reports/run_bundle_F_report.sh --talent "Rius Isonder Ch" --dry-run
```

## Data source and outputs

Bundle F wrappers default to:

- input source: `datalake`
- output root: `<datalake_root>/<talent>/reports/bundle_f/`

Final HTML report is written under:

- `<datalake_root>/<talent>/reports/bundle_f/report/current/`

Older current HTML reports are moved to:

- `<datalake_root>/<talent>/reports/bundle_f/report/archive/`

Bundle F report filenames include the render date and analysis window, for example:

- `Bundle_F_2026-05-30_window_90d_rius_isonder_ch.html`

## Useful flags

- `--talent NAME`
- `--talents "A,B,C"`
- `--talents-file PATH`
- `--all`
- `--window-days N`
- `--window-days lifetime` for all available data
- `--start-date YYYY-MM-DD`
- `--end-date YYYY-MM-DD`
- `--content-type live|video|short|all`
- `--content-types "live,video,short"`
- `--reference-day Monday`
- `--input-source staging|datalake`
- `--input-root PATH`
- `--datalake-root PATH`
- `--staging-root PATH`
- `--allow-partial-match`
- `--dry-run`
