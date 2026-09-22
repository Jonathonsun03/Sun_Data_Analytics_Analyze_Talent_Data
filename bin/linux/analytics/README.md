# Daily analytics

`run_daily_analytics.sh` is the single post-collection orchestrator for
durable derived analytics in this repository. The nightly Talent Repo ingestion
service invokes it only after the full collection and DataLake-to-lakehouse
reconciliation finish successfully.

The current pipeline refreshes viewer activity and engagement profiles from the
canonical unified DuckDB. Its output relations are:

- `analysis.viewer_video_activity`
- `analysis.viewer_profiles`
- `analysis.global_viewer_profiles`
- `analysis.company_viewer_profiles`

Run it from the repository root:

```bash
# Source/count validation; no database writes.
bin/linux/analytics/run_daily_analytics.sh --dry-run

# Transactionally publish all registered derived relations.
bin/linux/analytics/run_daily_analytics.sh --execute
```

The wrapper loads the repository `.env`, requires `TALENT_DATALAKE_ROOT` or
`TALENT_DATA_ROOT`, and writes logs plus a concurrency lock under
`<Talent DataLake root>/Logs/daily_analytics/`.

## Adding an analysis

Keep an analysis independently executable in `r_scripts/run/` or
`py_scripts/run/`. In `run_daily_analytics.sh`, add a small named
`run_<analysis>()` function, then register it with one `run_step` call in
`run_pipeline()` in dependency order. A failing step stops the pipeline and
causes the invoking nightly systemd service to fail visibly; previously
published relations remain protected by each publisher's own transaction.
