# Google Drive Talent Data Push Helpers

This library treats the client permissions Google Sheet as the source of truth
for which talent data should be copied into a client-facing Google Drive folder.

Typical flow:

```r
source("r_scripts/lib/gdrive_push_talent_data/load_all.R")
gdrive_talent_load_all()

permissions <- gdrive_talent_read_permissions(
  sheet_id = "GOOGLE_SHEET_ID_OR_URL",
  active_only = FALSE
)

manifest <- gdrive_talent_manifest(data.frame(
  data_type = c("bundle_a", "chat_logs"),
  local_path_pattern = c(
    "/mnt/datalake/.../{talent_id}/reports/bundle_a/*.html",
    "/mnt/datalake/.../{talent_id}/chat_logs/*.csv"
  ),
  drive_subdir = c("bundle_a", "chat_logs"),
  stringsAsFactors = FALSE
))

plan <- gdrive_talent_build_upload_plan(
  permissions = permissions,
  manifest = manifest
)

gdrive_talent_execute_upload_plan(
  upload_plan = plan$upload_plan,
  root_folder_id = "GOOGLE_DRIVE_FOLDER_ID",
  dry_run = TRUE
)
```

Use `dry_run = FALSE` only after reviewing the generated upload plan.

## Talents sheet sync

The `talents` worksheet can be rebuilt from the DataLake talent folders,
DuckDB `talents` table, and title-classification profile config:

```r
source("r_scripts/lib/gdrive_push_talent_data/load_all.R")
gdrive_talent_load_all()

talents <- gdrive_talent_write_talents_sheet(
  sheet_id = "https://docs.google.com/spreadsheets/d/1fvsLKYB4xRIiEOn0CyjK63zuR1ttU4KRaofZbOniEFY/edit",
  worksheet = "talents",
  auth = TRUE
)
```

The sync refreshes canonical fields from code/data, but preserves existing
`active`, `aliases`, and `notes` values for matched talents before replacing
the worksheet contents. It also creates or refreshes a named range called
`Canonical_talent_name` over the `canonical_talent_name` data cells.

## Reports sheet sync

The `reports` worksheet can be rebuilt from bundle description files under
`r_scripts/run/`, report templates under `templates/reports/`, and rendered
report files in the DataLake:

```r
source("r_scripts/lib/gdrive_push_talent_data/load_all.R")
gdrive_talent_load_all()

reports <- gdrive_talent_write_reports_sheet(
  sheet_id = "https://docs.google.com/spreadsheets/d/1fvsLKYB4xRIiEOn0CyjK63zuR1ttU4KRaofZbOniEFY/edit",
  worksheet = "reports",
  auth = TRUE
)
```

Status is inferred as:

- `completed`: template Rmd, test HTML, and template artifacts are present.
- `in progress`: template Rmd exists but completion evidence is partial.
- `not started`: description exists but no report Rmd exists yet.

Existing `active` and `notes` values are preserved for matched reports.

## Talent report schedule sync

The `talent_report_schedule` worksheet is designed to stay human-editable:
you choose the talent, report, cadence, and window; the sync fills in the
latest detected run and next due date from rendered report files in the
DataLake. The writer only updates computed columns `G:N` when rows already
exist, so manual dropdown columns and formatting are preserved. It also
refreshes dropdown validation for `active`, `canonical_talent_name`, and
`report_id`.

```r
source("r_scripts/lib/gdrive_push_talent_data/load_all.R")
gdrive_talent_load_all()

schedule <- gdrive_talent_write_report_schedule_sheet(
  sheet_id = "https://docs.google.com/spreadsheets/d/1fvsLKYB4xRIiEOn0CyjK63zuR1ttU4KRaofZbOniEFY/edit",
  worksheet = "talent_report_schedule",
  auth = TRUE
)
```

Expected manual columns:

- `canonical_talent_name`
- `report_id`
- `cadence_days`
- `window_days`
- `active`
- `notes`

Computed columns:

- `last_run`
- `next_run`
- `schedule_status`
- `latest_report_path`
- `latest_report_file`
- `run_source`
- `updated_at`

Stale remote files can be identified with:

```r
stale <- gdrive_talent_build_stale_file_plan(
  upload_plan = plan$upload_plan,
  root_folder_id = "GOOGLE_DRIVE_FOLDER_ID"
)
```

Then archive them first:

```r
gdrive_talent_execute_stale_file_plan(
  stale_plan = stale,
  root_folder_id = "GOOGLE_DRIVE_FOLDER_ID",
  mode = "archive",
  dry_run = TRUE
)
```
