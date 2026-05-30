# Bundle E Wrappers

This folder contains Linux wrappers for Bundle E report artifacts and rendering.

## Main Entrypoints

Full Bundle E pipeline:

```bash
bin/linux/render_reports/run_bundle_E_report.sh --talent "Avaritia Hawthorne 【Variance Project】" --window-days 90
```

Equivalent direct call:

```bash
bin/linux/render_reports/bundle_E/run_bundle_E_full_pipeline.sh --talent "Avaritia Hawthorne 【Variance Project】" --window-days 90
```

Artifacts only:

```bash
bin/linux/render_reports/bundle_E/run_bundle_E_artifacts.sh --talent "Avaritia Hawthorne 【Variance Project】" --window-days 90
```

Render only:

```bash
bin/linux/render_reports/bundle_E/run_bundle_E_render_only.sh --talent "Avaritia Hawthorne 【Variance Project】" --window-days 90
```

## Scheduler Compatibility

Bundle E now accepts the same common report-runner inputs used by the scheduled
report dispatcher:

- `--talent NAME`
- `--talents "A,B,C"`
- `--talents-file PATH`
- `--all`
- `--window-days N`
- `--window-days lifetime` for all available data
- `--start-date YYYY-MM-DD`
- `--end-date YYYY-MM-DD`
- `--input-source staging|datalake`
- `--input-root PATH`
- `--datalake-root PATH`
- `--staging-root PATH`
- `--allow-partial-match`
- `--quiet`
- `--dry-run`

The wrappers resolve both display names and spreadsheet-style canonical names
against the selected data root. For example, this resolves to the Avaritia
DataLake folder:

```bash
bin/linux/render_reports/run_bundle_E_report.sh \
  --talent Avaritia_Hawthorne_Variance_Project \
  --window-days 90 \
  --input-source datalake \
  --datalake-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data \
  --dry-run
```

When `report_id` is `bundle_e`, `bin/linux/render_reports/run_scheduled_reports.sh`
dispatches to:

```bash
bin/linux/render_reports/bundle_E/run_bundle_E_full_pipeline.sh
```

## Current Pipeline

Bundle E currently runs:

1. artifact generation via `r_scripts/run/bundle_e/import_data.R`
2. report rendering via `r_scripts/run/bundle_e/render_bundle_e.R`

It does not yet have separate interpretation or editorial rewrite stages in the
Linux wrapper tree. The full-pipeline wrapper accepts no-interpretation and skip
flags for compatibility with the scheduler and ignores them.
