# R Notebooks

This directory is the canonical home for interactive R analyses and rendered
notebook sources.

## Layout

- `dashboards/`
  - interactive and published dashboard notebooks
  - `dashboards/company_dashboard/dashboard.qmd` is the company-level Shiny
    dashboard for selecting a company, its talents, and an analytics date range
  - `dashboards/data_admin/classification_dashboard/dashboard.qmd` is the
    read-only Shiny operations view for title-classification runs, artifacts,
    tags, and coverage
  - `dashboards/data_admin/raw_data_dashboard/dashboard.qmd` is the read-only
    DuckDB explorer for talent analytics, subtitles, chat, ingestion history,
    and data-quality checks
- `models/`
  - model development, evaluation, diagnostics, and model explainers
  - group related notebooks by analytical family, such as `day_of_week/` or
    `ena/`
- `reports/`
  - descriptive and project-facing reports that primarily present results
- `tests/`
  - executable notebooks that validate repository or environment behavior
  - `title_classification_pipeline.qmd` documents the mixed R/Python title
    pipeline and runs read-only health checks against the unified DuckDB

## Boundaries

- Put reusable functions in `r_scripts/lib/`; notebooks should source those
  helpers instead of duplicating stable model logic.
- Put unattended or scheduled workflow entrypoints in `r_scripts/run/`.
- Do not create another `notebooks/` directory under `r_scripts/run/`.
- Resolve repository paths with `here::here()` and DataLake paths with the
  shared DataLake path helpers.
- Write non-code outputs and newly rendered artifacts to `/mnt/datalake/`
  unless an existing tracked artifact is explicitly being maintained.

## Model notebook families

- `models/day_of_week/`
  - weekday and topic-by-weekday modeling notebooks and their explainer
- `models/ena/variance/`
  - the Variance qualitative ENA and donation prediction notebook
- `models/ena/test/`
  - earlier ENA test analyses retained for reference
- `models/ena/money_chat/`
  - interactive monetary-chat ENA analysis
