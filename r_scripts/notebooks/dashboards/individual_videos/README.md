# Individual Video Dashboard

## Goal

This dashboard lets an authorized talent inspect one video's performance across
the available daily analytics snapshots. It is a focused complement to the
creator dashboard: the viewer selects a permitted talent, one of that talent's
videos, and one metric at a time.

Talent, video, and snapshot dates are global controls. The single-metric
selector appears on the Performance page, while the Plot Builder keeps its own
independent X and Y variable selectors.

All catalog videos for an authorized talent remain selectable by title. A video
that has not received an analytics snapshot yet returns a clear empty state
rather than being silently omitted.

The Plot Builder page places the X and Y selectors in a full-height control
column on the left and the visualization on the right. It retains a muted
full-history path and provides a Plotly snapshot slider that animates the
highlighted observation through the selected date window. The control column
also reports descriptive statistics for both variables and their paired
snapshot correlation.

Each selected variable displays its definition directly below the selector.
These definitions are centralized in the shared metric metadata and follow the
[YouTube Analytics metric definitions](https://developers.google.com/youtube/analytics/metrics),
with an additional note describing how the daily snapshot is interpreted in
this dashboard.

The Transcript page loads the full available text for the selected video from
the canonical `text.subtitle_units` and `text.chat_messages` relations. It
merges streamer subtitles and live chat chronologically and displays only the
speaker, video-relative timestamp, and dialogue. Transcript loading is scoped
to the authorized talent and video and is independent of the analytics
snapshot date range.

## Authorization

The dashboard uses the shared fail-closed authorization helpers in
`r_scripts/lib/dashboard/auth/access.R`. A session can only list talents in its
`X-SDA-Allowed-Talent-Codes` entitlement. Video catalog and history queries are
parameterized by both the authorized talent code and selected video ID, so a
browser-supplied video ID cannot cross talent boundaries.

## Metric semantics

- The selected date range filters analytics snapshots, not video publication
  dates.
- Views, watch time, revenue, and subscriber measures are displayed as the
  cumulative values recorded in each snapshot.
- Average view duration, average view percentage, and CPM are snapshot-level
  averages or rates.
- The interval chart divides change since the prior visible snapshot by the
  elapsed days. It does not imply that growth was uniform within the interval.
- The first available snapshot may occur after publication. The dashboard does
  not treat that first observed value as launch-day performance.

The video-age, cumulative-change, and elapsed-day approach follows the shared
Bundle E longitudinal report helpers while keeping dashboard-specific queries,
metric selection, and display adapters in the dashboard library.

## Local preview

Set development mode and a deliberately narrow set of talent codes:

```bash
export DASHBOARD_AUTH_MODE=development
export DASHBOARD_DEV_ALLOWED_TALENT_CODES=LEI3
export DASHBOARD_DEV_EMAIL=local@example.com

quarto serve dashboard.qmd --browser
```

Production must continue to use the trusted Cloudflare headers documented in
`r_scripts/lib/dashboard/README.md`.
