# Creator Dashboard Library

The creator dashboard library is organized by responsibility. Dashboard
entrypoints should source only `load.R`, which loads shared dependencies and
dashboard modules in an explicit order.

## Layout

- `data/`: source access, filtering, and final dashboard-data assembly
- `auth/`: trusted Cloudflare Access headers and per-session talent authorization
- `metrics/`: domain-specific summary and metric preparation
- `recommendations/`: recommendation schema, domain rules, and story assembly
- `ui/`: reusable tables, cards, empty states, and other HTML components
- `adapters/`: dashboard-specific adapters around reusable report plots
- `load.R`: the composition root and public loading entrypoint

Reusable plot implementations live under `r_scripts/lib/plots/domains/`.
Adapters may select data, handle dashboard empty states, and apply
dashboard-specific Plotly or card formatting, but should not become a second
plot library.

## Naming

Filenames use their layer and domain rather than repeating `dashboard_`; the
parent directory already provides that context. Existing function names are
temporarily retained as the public API during the structural refactor.

New or renamed functions should put responsibility immediately after the
dashboard namespace, for example `dash_data_*`, `dash_metric_*`, `dash_rec_*`,
`dash_ui_*`, or `dash_plotly_*`. General plotting helpers should identify their
return type with suffixes such as `_prep`, `_ggplot`, `_plotly`, `_table`, or
`_card`.

## Dashboard authorization

The interactive dashboard defaults to fail-closed production authorization.
Cloudflare's permissions Worker must provide `X-SDA-Verified-Email` and
`X-SDA-Allowed-Talent-Codes` on every proxied dashboard request. The Shiny
session intersects those exact codes with `catalog.talents` before rendering the
selector and validates the selected code again before querying DuckDB.

The sidebar mounts the talent dropdown immediately, then the Shiny session
replaces its loading option with only the authorized talent choices after the
client connection is ready. The first authorized selection triggers the initial
dashboard load; later filter changes continue to use the Apply filters button.
Missing or invalid authorization leaves the control empty and does not fall
back to the full talent catalog.

`r_scripts/notebooks/dashboards/individual_videos/dashboard.qmd` uses the same
session access context. Its video selector lists every catalog video for the
authorized talent by title, and its analytics-history and full-transcript
queries are constrained by both `talent_code` and `video_id`. The transcript
view reads canonical streamer subtitles and chat messages without applying the
analytics snapshot date filter.

Each new Shiny session reloads the talent catalog from DuckDB so analytics
snapshot bounds do not remain pinned to the long-running Quarto process's
startup state. While a session remains open, its analytics freshness status refreshes
from DuckDB once per minute and reports the latest available daily snapshot and
the number of tracked videos collected in that snapshot. The date selector
filters the longitudinal `(video_id, snapshot_date)` panel. Cards and rankings
use the latest snapshot in the selected range, lifecycle panels use every
selected daily snapshot, and the overview performance chart retains its
original monthly aggregation. Video publication dates do not control the
dashboard date window.

## Company dashboards

Company-level dashboards reuse the creator dashboard's unified-DuckDB snapshot
semantics. `data/company.R` selects the latest available snapshot inside the
requested window separately for each selected talent, ensuring that cumulative
video metrics are never summed across daily snapshots. `metrics/company.R`
prepares company totals and the publication, topic, reference, and
collaboration summaries used by
`r_scripts/notebooks/dashboards/company_dashboard/dashboard.qmd`.

Company membership is explicit rather than inferred from talent names. Update
`config/dashboard/company_talents.csv` with one row per company/talent pair;
the loader validates every code against active `catalog.talents` rows.

For an explicit local-only preview, set both development mode and a narrow list
of test codes:

```bash
export DASHBOARD_AUTH_MODE=development
export DASHBOARD_DEV_ALLOWED_TALENT_CODES=LEI3,AVA1
export DASHBOARD_DEV_EMAIL=local@example.com
```

Production never falls back to all talents when the trusted headers are absent.
