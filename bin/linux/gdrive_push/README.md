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

To also share each delivery group folder with the `client_email` recipients
from the permissions sheet:

```bash
bin/linux/gdrive_push/run_gdrive_push.sh --execute --share-folders
```

To do the full workflow:

```bash
bin/linux/gdrive_push/run_gdrive_push.sh --execute --upload-files --share-folders
```
