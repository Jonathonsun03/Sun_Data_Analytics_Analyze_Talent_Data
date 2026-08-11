# py_scripts

This directory contains the maintained Python code for the repository.

## Environment setup

Create a project-local virtual environment and install the Python dependencies:

```bash
python3 -m venv .venv
.venv/bin/python -m pip install --upgrade pip
.venv/bin/python -m pip install -r py_scripts/requirements.txt
```

Select `.venv/bin/python` as the Python interpreter and notebook kernel in VS Code.
Most runnable Python scripts use only the standard library. The packages in
`requirements.txt` also support notebooks, DuckDB, and private Google Sheet reads.

## Structure

- `run/`
  - runnable Python entrypoints
  - these are task and pipeline scripts that can be executed directly
- `lib/`
  - reusable Python helpers shared by the runnable entrypoints

## Current stream-summary runners

- `run/stream_summaries/summary_classification/summary_classification_incremental.py`
- `run/stream_summaries/monetary_analysis/monetary_summary_classification_incremental.py`
- `run/stream_summaries/streamer_personality/personality_profile_v3_incremental_open_coding.py`
- `run/stream_summaries/streamer_personality/personality_profile_synthesis.py`
- `run/stream_summaries/streamer_personality/build_shared_behavior_baseline.py`
- `run/stream_summaries/streamer_personality/build_unique_personality_profiles.py`

## Other maintained entrypoints

- `run/demo_data/generate_demo_talent_dataset.py`
  - creates a synthetic talent folder in the datalake
  - writes realistic-looking analytics, monetary, demographic, geography, and isolated title-classification CSVs
  - intended for client-safe sample Bundle A/B reports without using real talent data
- `run/sync_cloudflare_permissions.py`
  - legacy importer for a private Google Sheet permission snapshot
  - not part of the current D1-first onboarding workflow
- `run/sync_d1_talent_catalog.py`
  - reads `catalog.talents` from DuckDB in read-only mode
  - maps exact talent codes and display metadata into Cloudflare D1
  - defaults to a production-safe preview and never changes client assignments

## D1 talent catalog synchronization

Cloudflare D1 is the single source of truth for client identities and
permissions. DuckDB remains the analytical source of truth for the talent
catalog because its exact `talent_code` values are the keys used by dashboard
queries. The synchronization copies only catalog metadata into D1; it never
grants a product or talent to a client.

Preview DuckDB-to-D1 changes:

```bash
.venv/bin/python py_scripts/run/sync_d1_talent_catalog.py
```

Apply pending D1 migrations and the idempotent upsert after reviewing the
preview:

```bash
.venv/bin/python py_scripts/run/sync_d1_talent_catalog.py --apply
```

The script reads the repository's configured DuckDB path, uses the sibling
applications repository's local Wrangler installation and Cloudflare token, and
takes an exclusive lock to prevent overlapping runs. Existing D1 talent IDs are
preserved when an exact display-name match is attached to a DuckDB code, so
existing client assignments are not broken.

An hourly user-systemd template is checked in under `config/systemd/`. Install
it with:

```bash
mkdir -p ~/.config/systemd/user
cp config/systemd/sun-data-talent-catalog-sync.* ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now sun-data-talent-catalog-sync.timer
systemctl --user list-timers sun-data-talent-catalog-sync.timer
```

The same apply command can instead be invoked as the final successful step of
the DuckDB catalog pipeline. The timer is a reconciliation safety net: repeated
runs are safe and do not modify `product_access`, `talent_access`, or
`permission_grants`.

For future payment onboarding, the payment service should create or activate
the D1 user and write source-owned rows to `permission_grants`. It should not
edit DuckDB and should not use this catalog-sync command to grant access.

## Legacy Google Sheet permission import

This importer is retained only for controlled one-off migrations from the private
workbook. It is not scheduled and the workbook is no longer authoritative.
Cloudflare D1 remains the identity and authorization source of truth.

For direct Google Sheet access, create a Google service account (or use Google
Application Default Credentials), enable read-only Sheets access, and share the
private workbook with that identity as a viewer. Keep credential JSON outside the
repository and set:

```bash
export GOOGLE_APPLICATION_CREDENTIALS=/secure/path/google-permissions-reader.json
export PERMISSIONS_SPREADSHEET_ID=your-spreadsheet-id
```

Preview the exact user and talent counts without changing D1:

```bash
.venv/bin/python py_scripts/run/sync_cloudflare_permissions.py
```

After reviewing the preview, apply pending schema migrations and synchronize the
remote D1 snapshot:

```bash
.venv/bin/python py_scripts/run/sync_cloudflare_permissions.py --apply
```

Wrangler uses the authenticated session in the sibling
`sun_Data_analytics_apps` project. Run `npx wrangler login` there once, or use a
scoped `CLOUDFLARE_API_TOKEN` environment variable. Generated SQL contains client
emails only in a mode-`0600` temporary file under `/tmp`; the command removes it
after Wrangler finishes.

CSV exports can be used when Google credentials are not configured:

```bash
.venv/bin/python py_scripts/run/sync_cloudflare_permissions.py \
  --permissions-csv /path/permissions.csv \
  --talents-csv /path/talents.csv
```

Sheet rules:

- Use one active row per client and talent assignment.
- `client_email` must match the email used by Cloudflare Access.
- `delivery_group_id` or `talent_id` must resolve to an active row on `talents`.
- `product_id` is optional and defaults to `youtube-analytics`.
- `access_start_date` and `access_end_date` are optional and are enforced by the Worker.
- Active incomplete rows always block synchronization; inactive onboarding rows are ignored.
- Add alternate names such as `Teri` to the `aliases` column on `talents` when possible.
- A sync replaces only Google Sheet grants; manual and future payment-source grants remain intact.

## Notes

- `tasks/` was a temporary holding area and is no longer the canonical location.
- New Python work should be placed in `run/` or `lib/` rather than `tasks/`.
