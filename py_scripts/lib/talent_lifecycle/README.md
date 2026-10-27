# Talent lifecycle controls

This subsystem owns processing policy, not talent data or client permissions.
The Cloudflare permission administration page is the operator interface. Its
existing administrator-only Worker routes call an authenticated origin bridge on
CT 104, which delegates to this package. The CLI remains a maintenance adapter.

| Module | Responsibility |
| --- | --- |
| `policy.py` | Validate and read control records; decide whether work is allowed |
| `store.py` | Serialize edits, reject stale revisions, atomically replace one record |
| `batches.py` | Check membership of already-built API batches |
| `../../run/talent_control.py` | Dashboard command adapter |
| `../../run/check_talent_processing.py` | Prompt-job admission check |

The R reader and dashboard adapter live in `r_scripts/lib/talent_lifecycle/`.
The ingestion repository loads this Python package through its thin
`py_scripts/lib/talent_data/lifecycle.py` adapter. Do not fork another copy.

## Behavior

One JSON file named `<talent_code>.json` is authoritative for both switches:

```json
{
  "version": 1,
  "talent_code": "TES1",
  "revision": 1,
  "retrieval_enabled": false,
  "processing_enabled": true,
  "channel_ids": ["UC-example"],
  "aliases": ["Example Talent"],
  "updated_at": "2026-10-27T00:00:00+00:00",
  "updated_by": "operator@example.com",
  "last_action": "archive"
}
```

- Archive/restore changes retrieval only. Pause/resume changes processing only.
- Existing data, credentials, permissions, and catalog visibility are preserved.
- Decisions are re-read at work boundaries. No restart is needed for new checks.
- A configured but missing directory, or malformed record, blocks new work.
- An unset `TALENT_CONTROL_ROOT` keeps existing behavior. An absent talent record
  in a configured directory defaults to both switches enabled for rollout.
- Channel IDs and exact aliases connect legacy folder workflows to talent codes.
  The dashboard records canonical names and filesystem-safe channel names.
- Existing ingestion exclusions remain an additional restriction. Restore does
  not override `config/excluded_talents.json`.

## Integration and deployment

See [ROLLOUT.md](ROLLOUT.md) for the actual CT 104/105 architecture, staged
configuration, ordered deployment commands and rollback. Nothing is configured
or deployed by checking in this source.

- The bridge lives in `../talent_control_bridge.py`; canonical identity lookup
  lives in `../talent_control_catalog.py`. Only that catalog adapter requires the
  already-installed DuckDB dependency. Policy/store/batch helpers remain stdlib.
- CT 104 retains the analysis and ingestion implementation from the verified
  handoff. Ingestion imports the sibling analysis library; do not fork it.
- CT 105 changes only its permission app source. Its client talent dashboard
  receives no lifecycle controls or writer access.
- The existing shared ext4 disk provides one `Controls/talents` directory.
  The dedicated bridge user owns it; existing matching GID 1000 (`jonathon`) in
  both containers reads it. New records use mode 0640, the lock 0600 and the
  directory 2750. Readers cannot replace, create or edit records. No membership
  or broad mount changes are necessary.
- Set `TALENT_CONTROL_ROOT` and `TALENT_CONTROL_MOUNT` in both backend repositories
  before admitting new worker processes. A configured missing directory/mount,
  unreadable directory or malformed record blocks new work. An unset root keeps
  compatibility before rollout; missing individual records retain enabled defaults.
- Only the existing atomic writer changes records, using flock, expected revisions
  and same-directory atomic replacement. The bridge returns HTTP 409 on stale saves.
- Status reads and changes require the separate `TALENT_BRIDGE_SECRET`. The Worker
  authenticates Access JWTs and the existing administrator identity first, then
  replaces the actor and Authorization headers. Neither browser identity headers
  nor browser-supplied aliases/channel IDs are accepted as authority.
- Catalog validation reads `catalog.talents`, `catalog.channels` and
  `catalog.talent_aliases` read-only on every request. Catalog active/visibility
  fields do not become lifecycle state. D1 continues to own entitlements only.
- `TALENT_LIFECYCLE_WRITES_ENABLED` is false unless explicitly set to `true` in
  the Worker. Enable it only after worker enforcement and origin authentication
  are verified. The pre-existing local raw-data Shiny controls stay disabled;
  do not set `TALENT_CONTROL_UI_ENABLED=true` as part of this deployment.

The origin is a small HTTP adapter, not a job scheduler or state service. It binds
only CT 104's tailnet address. CT 105's existing remotely managed Cloudflare Tunnel
forwards one new hostname to it over Tailscale. Public HTTPS plus the distinct
Worker-origin bearer secret protects the bridge. No real talent record is created
or changed as a deployment smoke test.

## Safe stopping and coverage

- Collection filters inventory, checks credentials before refresh, and guards
  prepared API clients before subsequent HTTP requests. Text collection rechecks
  between videos. An in-flight download/request can finish.
- Subtitle backfill checks between videos, punctuation blocks, speaker
  comparisons/retries, and before publication. Saved checkpoints survive a pause.
  A pause during a track drains the current backfill batch; the next run skips
  that talent and continues other work.
- Title batch selection/application and qualitative prep, batch submission,
  and application respect processing controls. Mixed prebuilt batches are held
  intact while a member is paused. Remote work already submitted is not cancelled.
- Common R/Python folder selectors honor processing controls, as do the maintained
  summary and monetary per-talent entrypoints.
- Prompt-driven wrappers reject new unscoped jobs while any talent is paused;
  use `--talent` for an enabled talent. A running Codex job is one admitted unit
  and may finish; it is not remotely interrupted by this switch.
- Viewer activity preserves paused per-talent rows and refreshes active rows.
  Company/global summaries are rebuilt from the stored per-talent results,
  including frozen results. Archive alone never freezes those results.

This is cooperative enforcement in maintained entrypoints, not filesystem access
revocation. Arbitrary notebooks, old processes, and custom direct API calls are
not a supported bypass or an instant cancellation mechanism. Add new workers
through the shared policy helpers and document their checkpoint boundary.

## Removal is separate

There is deliberately no delete action in these controls. A later removal
workflow must first archive retrieval and pause processing, establish that
workers and outstanding jobs cannot recreate data, then preview the exact raw
files, derived files, checkpoints, and database rows to remove. Credentials,
permissions, shared cross-talent aggregates, and retained provenance need an
explicit decision. Execute only against an approved scope with an explicit
confirmation. No talent data is deleted by archive, pause, or these changes.

## Focused checks

```bash
python3 -m unittest py_scripts.tests.test_talent_lifecycle py_scripts.tests.test_talent_control_bridge
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript --vanilla r_scripts/tests/test_viewer_activity_publish.R
```

The tests use temporary directories and disposable databases, never live data.
