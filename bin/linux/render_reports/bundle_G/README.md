# Bundle G Wrappers

This folder contains the Bundle G Linux wrapper scripts used to render the topic-by-weekday recommendation report.

## Main entrypoints

Full report pipeline:

`bin/linux/render_reports/bundle_G/run_bundle_G_full_pipeline.sh`

Top-level shortcut:

`bin/linux/render_reports/run_bundle_G_report.sh`

Render only:

`bin/linux/render_reports/bundle_G/run_bundle_G_render_only.sh`

Compatibility artifact stage:

`bin/linux/render_reports/bundle_G/run_bundle_G_artifacts.sh`

## Recommended usage

Full Bundle G report for one talent:

```bash
bin/linux/render_reports/run_bundle_G_report.sh --talent "Rius Isonder Ch"
```

Render one content type:

```bash
bin/linux/render_reports/run_bundle_G_report.sh \
  --talent "Rius Isonder Ch" \
  --content-type live
```

Dry run:

```bash
bin/linux/render_reports/run_bundle_G_report.sh --talent "Rius Isonder Ch" --dry-run
```

## Data source and outputs

Bundle G wrappers default to:

- input source: `datalake`
- output root: `<datalake_root>/<talent>/reports/bundle_g/`

Final HTML report is written under:

- `<datalake_root>/<talent>/reports/bundle_g/report/current/`

Older current HTML reports are moved to:

- `<datalake_root>/<talent>/reports/bundle_g/report/archive/`

Bundle G report filenames include the render date and analysis window, for example:

- `Bundle_G_2026-05-30_window_90d_rius_isonder_ch.html`

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
