# Scheduled Reports Runner

`bin/linux/render_reports/run_scheduled_reports.sh` reads the client permissions
Google Sheet and runs report rows that are due.

The daily cron job calls this runner at 12:10 AM:

```bash
cd /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data && \
  bin/linux/render_reports/run_scheduled_reports.sh >> \
  /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/report_scheduler_logs/scheduled_reports.log 2>&1
```

## What It Reads

The runner uses the configured client permissions spreadsheet:

- `talent_report_schedule`
- `talents`

Schedule rows are selected when:

- `active` is `TRUE`
- `next_run` is today or earlier
- `report_id` is supported by the dispatcher

Supported report IDs:

- `bundle_a`
- `bundle_e`
- `bundle_f`

## What It Runs

The scheduler maps the sheet row to a bundle runner:

- `bundle_a` -> `bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh`
- `bundle_e` -> `bin/linux/render_reports/bundle_E/run_bundle_E_full_pipeline.sh`
- `bundle_f` -> `bin/linux/render_reports/bundle_F/run_bundle_F_full_pipeline.sh`

It passes:

- `canonical_talent_name` mapped through `talents.datalake_folder_name` as `--talent`
- numeric `window_days` as `--window-days`
- `window_days = lifetime` as an all-data run, with no `--window-days` passed to the bundle runner
- optional JSON in `report_params` for supported bundle-specific parameters
- configured source/root values as `--input-source` and `--datalake-root`

Bundle F currently supports these `report_params` keys:

- `content_type`, for example `{"content_type":"live"}`
- `content_types`, for example `{"content_types":["live","video"]}`
- `reference_day`, for example `{"content_type":"live","reference_day":"Monday"}`

After at least one successful real report run, it refreshes computed schedule
columns through the existing DuckDB-backed schedule sync:

- `last_run`
- `next_run`
- `schedule_status`
- `latest_report_path`
- `latest_report_file`
- `run_source`
- `updated_at`

## Common Commands

Dry-run all currently due rows:

```bash
bin/linux/render_reports/run_scheduled_reports.sh --dry-run
```

Dry-run one specific row:

```bash
bin/linux/render_reports/run_scheduled_reports.sh \
  --dry-run \
  --as-of-date 2026-08-25 \
  --report-id bundle_a \
  --talent Katya_Sable_Variance_Project \
  --window-days 90
```

Dry-run a lifetime/all-data row:

```bash
bin/linux/render_reports/run_scheduled_reports.sh \
  --dry-run \
  --as-of-date 2026-08-25 \
  --report-id bundle_e \
  --talent Katya_Sable_Variance_Project \
  --window-days lifetime
```

Run one specific row without generating or including interpretation text:

```bash
bin/linux/render_reports/run_scheduled_reports.sh \
  --as-of-date 2026-08-25 \
  --report-id bundle_a \
  --talent Katya_Sable_Variance_Project \
  --window-days 90 \
  --no-interpretation
```

Refresh computed schedule columns without running reports:

```bash
bin/linux/render_reports/run_scheduled_reports.sh --refresh-schedule-only
```

Run reports but skip the post-run schedule write:

```bash
bin/linux/render_reports/run_scheduled_reports.sh --skip-schedule-update
```

## Testing A New Schedule Row

1. Add or edit a row in `talent_report_schedule`.
2. Set `active` to `TRUE`.
3. Set `canonical_talent_name` to a value from the dropdown.
4. Set `report_id` to a supported report ID, such as `bundle_a`, `bundle_e`, or `bundle_f`.
5. Set `cadence_days` and `window_days`. Use a positive integer such as `90`,
   or use `lifetime` for all available data.
6. For Bundle F live-only, set `report_params` to `{"content_type":"live"}`.
7. Use `--as-of-date` to simulate the row being due.
8. Start with `--dry-run`.

Example:

```bash
bin/linux/render_reports/run_scheduled_reports.sh \
  --dry-run \
  --as-of-date 2026-12-31 \
  --report-id bundle_e \
  --talent Katya_Sable_Variance_Project \
  --window-days 90
```

If the dry run prints one due row and the expected bundle command, remove
`--dry-run` to execute it.

## Cron And Logs

Check the installed cron entry:

```bash
crontab -l
```

Read scheduler logs:

```bash
tail -n 200 /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/report_scheduler_logs/scheduled_reports.log
```

## Notes

- Running a bundle runner directly does not update the spreadsheet.
- Spreadsheet updates happen when `run_scheduled_reports.sh` completes at least
  one successful real report run, or when `--refresh-schedule-only` is used.
- Bundle A supports interpretation generation and editorial rewrite.
- Bundle E currently supports artifacts plus render. It accepts
  no-interpretation flags for scheduler compatibility.
- Bundle F currently supports a compatibility artifact stage plus render. It
  accepts no-interpretation flags for scheduler compatibility.
