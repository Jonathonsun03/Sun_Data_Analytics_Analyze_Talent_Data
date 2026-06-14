# Talent Dashboard Render Wrapper

This folder contains a Linux wrapper for rendering the Quarto creator analytics
dashboard for one talent at a time.

Dashboard template:

- `templates/dashboards/talent_dashboard/index.qmd`

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
- `--output-dir PATH`: directory for rendered HTML.
- `--output FILE`: output HTML filename.
- `--dry-run`: print the command and temporary params file path without rendering.

## Output

By default, output is written under:

```text
outputs/dashboards/talent_dashboard/
```

The default filename is:

```text
creator_analytics_dashboard_<talent_slug>.html
```

For repeated client runs, use a talent-specific output filename or archive the
rendered HTML after review.
