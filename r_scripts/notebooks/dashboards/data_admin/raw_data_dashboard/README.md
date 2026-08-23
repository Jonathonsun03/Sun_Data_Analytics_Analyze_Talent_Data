# Raw Talent Data Administration Dashboard

`dashboard.qmd` is a read-only Quarto/Shiny explorer for the raw and canonical
talent data in the unified DuckDB lakehouse. Its scope includes:

- talent, channel, video, alias, and profile relations in `catalog`;
- raw-to-clean analytics snapshot relations and the latest-performance view;
- canonical subtitles and chat messages in `text`;
- relevant pipeline runs, ingestion events, source files, collection failures,
  and quality results in `ops`.

Classification, qualitative coding, normalization, compatibility, and legacy
relations are intentionally excluded.

The dashboard provides relation-level inventory and recency, safe row-limited
previews, talent and video filters, CSV downloads of the displayed preview,
column completeness and approximate cardinality, numeric summaries, talent
coverage, relationship and lineage checks, and recent ingestion activity.

## Run locally

From the repository root:

```bash
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  quarto serve \
  r_scripts/notebooks/dashboards/data_admin/raw_data_dashboard/dashboard.qmd \
  --host 127.0.0.1 \
  --port 3840
```

The host must provide the repository `.env` values used to resolve
`TALENT_DATALAKE_ROOT`. The dashboard always opens DuckDB in read-only mode.

This is an administrative view. Put it behind the authenticated internal
reverse proxy and do not expose port `3840` directly to the public internet.
