# Data Administration Dashboards

This directory contains read-only internal dashboards for inspecting the unified
talent lakehouse and its supporting operational artifacts.

- `classification_dashboard/` monitors title-classification batches and data.
- `raw_data_dashboard/` explores talent catalog data, analytics snapshots,
  subtitles, chat logs, ingestion history, and data-quality signals.

Both dashboards resolve the lakehouse with `talent_lakehouse_db_path()` and must
remain read-only. They are administrative tools and should only be served behind
the authenticated internal reverse proxy.
