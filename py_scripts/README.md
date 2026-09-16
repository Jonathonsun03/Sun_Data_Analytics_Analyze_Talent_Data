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

The title-tag normalization review under
`py_scripts/notebooks/model_tests/tag_normalization/` uses deterministic rules
for transparent surface equivalences and RapidFuzz for conservative lexical
alias consolidation with numeric and compound-tag guardrails. Semantic
embeddings and topic clustering are deferred until tags have compatible
descriptor types.

Production uses the same functions through `lib/tag_normalization.py` and
`run/publish_tag_normalization.py`. The publisher previews by default and uses
`--execute` to activate a versioned mapping in the unified DuckDB.

## Structure

- `run/`
  - runnable Python entrypoints
  - these are task and pipeline scripts that can be executed directly
- `lib/`
  - reusable Python helpers shared by the runnable entrypoints
- `notebooks/`
  - inspectable Python analyses and model tests
  - the tag-normalization review lives under
    `notebooks/model_tests/tag_normalization/`

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

## Proxmox inference shutdown guard

`run/inference_shutdown_guard.py` is the host-side guard used by the R lifecycle
helper. It runs only as root on `pve-nlp`; its only power command is
`/usr/sbin/shutdown -h now`. The reusable implementation is in
`lib/inference_shutdown_guard.py`. It defaults to a dry run. Tests never issue
real power commands:

```bash
python3 -m unittest discover -s py_scripts/tests -p test_inference_shutdown_guard.py
```

The deployed CT 106 service installs `lib/inference_activity_coordinator.py`.
It atomically closes `/v1/*` admission and counts every admitted request through
response completion, including requests waiting for the model semaphore. The
Proxmox-side `run/inference_guard_adapter.py` reaches its loopback-only prepare
and release routes with `pct exec 106`. Health, process, and CPU checks are not
used as idle evidence. Each `with_inference_machine()` scope also holds a batch
reservation, so work between consecutive model requests remains visible.

### Service coordinator adapter contract

The root-owned config lives at `/etc/sun-data/inference-shutdown-guard.json` on
Proxmox. `prepare_command` and `release_command` are argument arrays with absolute
executables (no shell interpolation). They may use `pct exec 106 -- ...` to invoke
an installed coordinator inside the container. Both receive an additional unique
request ID as their last argument and must respond within five seconds.

Prepare must atomically stop all new inference admission and inspect running
requests, queued jobs, and batch reservations between requests from **all**
producers on this physical host. It must preserve the admission barrier until
explicit release or reboot, not expire it after a timeout. It returns exit zero
and JSON only when it has a complete observation, for example:

```json
{
  "protocol": 1,
  "request_id": "the supplied unique request ID",
  "ctid": 106,
  "coverage": "all_inference_work",
  "admission_closed": true,
  "drain_persistent": true,
  "active_requests": 0,
  "queued_jobs": 0,
  "active_batches": 0
}
```

All three counts must be integer zero. Missing fields, partial coverage,
stale request IDs, timeouts, malformed JSON, or nonzero counts block shutdown.
The coordinator protects other callers' reservations, including batches that
have not issued their next request yet. Callers using `with_inference_machine()`
register and release these reservations over container SSH. Other producers on
this physical host must use the same lifecycle scope to receive between-request
protection; direct HTTP requests remain protected while admitted.

Release receives the same request ID and must be idempotent, resume admission
only for that request's barrier, and prevent a delayed prepare with the same ID
from creating a new barrier after release. The guard attempts release after busy,
failed, and dry-run checks. If release fails, inspect the coordinator manually.
After any shutdown attempt, admission stays closed even if SSH or shutdown fails.

### Deployment after service integration is verified

Copy `py_scripts/lib/inference_shutdown_guard.py` and
`py_scripts/run/inference_shutdown_guard.py` to their corresponding paths beneath
`/opt/sun-data/py_scripts/` on Proxmox, preserving their relative layout. Install
the verified root-owned config with permissions `0600` in a root-owned directory.
Then check without powering off:

```bash
python3 /opt/sun-data/py_scripts/run/inference_shutdown_guard.py
```

Only after a successful integration test, configure the analytics caller:

```bash
export INFERENCE_MACHINE_SHUTDOWN_GUARD_COMMAND='python3 /opt/sun-data/py_scripts/run/inference_shutdown_guard.py'
```

The R helper passes the exact argument `shutdown -h now`, which enables execution.
The standalone CLI also supports `--execute`. Exit 0 means the dry run succeeded
or shutdown was accepted; 75 means busy, unverified, or inconclusive. A shutdown
attempt with uncertain/failing results returns 1 so R retains its reservation.
A host-wide nonblocking file lock prevents concurrent guards from racing. This
does not itself track service jobs; the coordinator provides that evidence.
