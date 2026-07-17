# Talent Dashboard Render Wrapper

This folder contains a Linux wrapper for rendering the Quarto creator analytics
dashboard for one talent at a time.

Dashboard template:

- `r_scripts/notebooks/dashboards/talent_dashboard/index.qmd`

The wrapper produces a static HTML dashboard. The interactive Shiny dashboard
is a separate entrypoint:

- `r_scripts/notebooks/dashboards/talent_dashboard/dashboard.qmd`

## Container Environment Setup

In a restricted container, disable renv's optional system-library sandbox while
retaining its project-local package library, then restore the checked-in R
environment from the repository root:

```bash
export RENV_CONFIG_SANDBOX_ENABLED=FALSE
Rscript --vanilla setup_R_env.R
```

The dashboard runtime is R/Quarto-only and does not require Python. If the
repository's Python notebooks or pipeline tools are also needed, create their
separate environment with the tracked requirements file:

```bash
python3 -m venv .venv
.venv/bin/python -m pip install --upgrade pip
.venv/bin/python -m pip install -r py_scripts/requirements.txt
```

## Run the Interactive Dashboard Continuously

From the repository root, bind the Shiny dashboard to all container interfaces:

```bash
quarto serve r_scripts/notebooks/dashboards/talent_dashboard/dashboard.qmd \
  --host 0.0.0.0 \
  --port 3838
```

Use that command as the container's foreground command and publish container
port `3838`. Configure the container runtime's restart policy separately if the
process should restart after a failure or host reboot.

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
