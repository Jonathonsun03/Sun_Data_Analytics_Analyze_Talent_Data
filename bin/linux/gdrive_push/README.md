# Google Drive Push

Shell wrappers for the Google Drive client-delivery workflow.

## Client Permissions Sheet Sync

Use this to update the spreadsheet control-plane tabs:

```bash
bin/linux/gdrive_push/run_client_permissions_sheet_sync.sh
```

This sync updates:

- `talents`
- `reports`
- `talent_report_schedule`

It is intentionally separate from the Drive folder/file push below.

## Drive Delivery Push

Primary entrypoint:

```bash
bin/linux/gdrive_push/run_gdrive_push.sh
```

Default behavior is dry-run. Use:

```bash
bin/linux/gdrive_push/run_gdrive_push.sh --execute
```

to create folders, and:

```bash
bin/linux/gdrive_push/run_gdrive_push.sh --execute --upload-files
```

to create folders and upload the selected delivery files.

To reorganize older root-level Bundle A/B report files already on Google Drive
into the new report folder layout:

```bash
bin/linux/gdrive_push/run_gdrive_push.sh --execute --migrate-report-files
```

To upload only Bundle A/B current report files into the report folders:

```bash
bin/linux/gdrive_push/run_gdrive_push.sh --execute --upload-files --report-files-only
```

To also share each delivery group folder with the `client_email` recipients
from the permissions sheet:

```bash
bin/linux/gdrive_push/run_gdrive_push.sh --execute --share-folders
```

To do the full workflow:

```bash
bin/linux/gdrive_push/run_gdrive_push.sh --execute --upload-files --migrate-report-files --share-folders
```

For Bundle A/B report permissions, the target structure is:

- `bundle_a/report/current`
- `bundle_a/report/archive`
- `bundle_b/report/current`
- `bundle_b/report/archive`
