# Data Administration Dashboards

This directory contains read-only internal dashboards for inspecting the unified
talent lakehouse and its supporting operational artifacts.

- `raw_data_dashboard/` is the single administration dashboard. It explores
  talent catalog data, analytics snapshots, subtitles, chat logs, ingestion
  history, data-quality signals, and title-classification batches and data.

The dashboard resolves the lakehouse with `talent_lakehouse_db_path()` and must
remain read-only. It is an administrative tool and should only be served behind
the authenticated internal reverse proxy.
