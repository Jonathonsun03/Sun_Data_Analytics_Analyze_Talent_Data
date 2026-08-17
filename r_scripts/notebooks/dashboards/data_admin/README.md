# Title Classification Operations Dashboard

`dashboard.qmd` is a live Quarto/Shiny operations dashboard for the title
classification pipeline. It reads the unified talent DuckDB and the retained
Batch API run manifests in read-only mode.

The dashboard provides:

- a compact latest-run status, title, pending, and last-checked overview;
- traceable recent run history;
- per-run candidate titles, applied results, talents, and tags;
- searchable and downloadable classification data across all title versions;
- complete searchable topic and tag inventories across all retained versions;
- current active-version coverage; and
- clear `legacy-untracked` labels when historical pipeline-run IDs were not
  recoverable during migration.

The layout uses four focused pages—Overview, Run Details, Browse Data, and
Topics & Tags—and stacks cards, plots, filters, and horizontally scrollable
tables on narrow screens.

## Run locally

From the repository root:

```bash
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  quarto serve r_scripts/notebooks/dashboards/data_admin/dashboard.qmd \
  --host 127.0.0.1 \
  --port 3839
```

## Host in Proxmox

Run the same command as the container or VM's foreground service and bind it to
the internal network interface:

```bash
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  quarto serve r_scripts/notebooks/dashboards/data_admin/dashboard.qmd \
  --host 0.0.0.0 \
  --port 3839
```

The host must mount the DataLake and provide the repository `.env` values used
to resolve `TALENT_DATALAKE_ROOT`. If the run artifacts are mounted somewhere
else, set `TITLE_CLASSIFICATION_BATCH_RUN_ROOT` explicitly.

This is an administrative view. Put it behind the same authenticated reverse
proxy used for other internal dashboards; do not expose port `3839` directly
to the public internet.

## Refresh behavior

The dashboard refreshes every five minutes by default and has a manual refresh
button. Refreshing never invokes the batch runner: it does not submit, check,
apply, retry, or modify classification data. Batch status therefore reflects
the scheduler's most recent hourly check rather than a separate OpenAI poll.
