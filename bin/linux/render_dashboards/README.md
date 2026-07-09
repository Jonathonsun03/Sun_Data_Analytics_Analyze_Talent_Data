# Talent Dashboard Render Wrapper

This folder contains a Linux wrapper for rendering the Quarto creator analytics
dashboard for one talent at a time.

Dashboard template:

- `r_scripts/notebooks/dashboards/talent_dashboard/index.qmd`

## Basic Usage

Render the dashboard for one talent from the datalake:

```bash
bin/linux/render_dashboards/run_talent_dashboard.sh --talent "Avaritia Hawthorne"
```

Render a specific publish-date window:

```bash
bin/linux/render_dashboards/run_talent_dashboard.sh \
  --talent "Avaritia Hawthorne" \
  --start-date 2026-01-01 \
  --end-date 2026-06-30
```

Limit the dashboard to selected content types:

```bash
bin/linux/render_dashboards/run_talent_dashboard.sh \
  --talent "Avaritia Hawthorne" \
  --content-types live,video
```

Use staging data instead of the datalake:

```bash
bin/linux/render_dashboards/run_talent_dashboard.sh \
  --talent "Avaritia Hawthorne" \
  --data-source staging
```

## Options

- `--talent NAME`: talent folder/name/query passed to the dashboard params.
- `--data-source datalake|staging`: source root resolver used by the dashboard.
- `--data-root PATH`: explicit data root override.
- `--start-date YYYY-MM-DD`: optional publish-date start window.
- `--end-date YYYY-MM-DD`: optional publish-date end window.
- `--content-types LIST`: comma-separated list, usually `live,video,short`.
- `--reference-day DAY`: reference weekday for weekday comparison logic.
- `--output-dir PATH`: explicit directory override for rendered HTML.
- `--output FILE`: output HTML filename.
- `--dry-run`: print the command and temporary params file path without rendering.

## Output

By default, the wrapper resolves the selected talent folder and writes under
that talent's report directory:

```text
<talent_data_root>/<resolved_talent_folder>/reports/dashboard_overall/
```

For example, `--talent "Leia Memoria"` resolves to:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/reports/dashboard_overall/
```

The default filename is:

```text
creator_analytics_dashboard_<talent_slug>.html
```

Use `--output-dir` when you need to render to a local review directory instead
of the talent delivery folder.
