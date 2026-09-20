# Talent Data Administration Dashboard

`dashboard.qmd` is the single read-only Quarto/Shiny administration dashboard
for the unified talent lakehouse. Its raw-data scope includes:

- talent, channel, video, alias, and profile relations in `catalog`;
- raw-to-clean analytics snapshot relations and the latest-performance view;
- canonical subtitles and chat messages in `text`;
- relevant pipeline runs, ingestion events, source files, collection failures,
  and quality results in `ops`.

It also includes title-classification run, artifact, result, topic, tag, and
active-version coverage diagnostics. Qualitative coding, normalization,
compatibility, and legacy relations remain excluded.

The dashboard is one Quarto/Shiny application with three primary navigation
domains. Each domain has a horizontal secondary tab strip so only one focused
administrative view is shown at a time:

- **Data** contains Summary, Health Checks, Coverage, Explore Relations, and
  Descriptive Profiles.
- **Processing** contains Summary, Transcript Processes, Transcript Lookup, and
  Pipeline Activity.
- **Classification** contains Summary, Runs, Results, and Topics & Tags.

The Summary view in each domain is deliberately concise. Detailed tables,
filters, and downloads live in the corresponding secondary view rather than
being stacked into one long dashboard page. The Data summary avoids duplicating
the relation inventory: it shows active talents, catalog videos, checks needing
review, the completion time of the latest successful source-data extraction,
and one coverage visualization. Responsive CSS grids keep metric cards, controls,
and plots aligned on desktop and stack them on narrow screens. The
secondary tab strip scrolls horizontally on mobile rather than becoming a long
vertical list. The strip uses Shiny input selectors and mutually exclusive
conditional panels instead of nested Bootstrap tabs; this prevents inactive
tables and plots from occupying or clipping the active view. Visible Plotly and
DataTables widgets are resized after a secondary-view change.

The Processing summary reports only the latest recorded source-data pull: its
`ops.pipeline_runs.started_at` timestamp and whether its terminal status was
successful. Collection, ingestion, scraping, extraction, download, fetch, pull,
sync, and YouTube pipeline names qualify; downstream reconstruction, backfill,
cleaning, analysis, classification, and publication jobs do not. A running pull
is shown as not yet complete rather than successful.

Dashboard startup pins the R process locale to `C.UTF-8` when available, with
`C` as a guaranteed fallback. It also updates `LANG`, `LC_ALL`, and `LC_CTYPE`
inside the dashboard process so package calls that restore the environment
locale do not repeatedly emit `Sys.setlocale()` warnings.

All three domains share one process, one read-only lakehouse connection pattern,
and one refresh toolbar. Controls are located with the view they affect; the
classification run selector is not presented as a global data filter.

The Classification Runs view keeps its run selector and download control in a
full-width toolbar. Below it, a compact, vertically scrolling run-history table
occupies the narrower browser pane and the selected run's summary, plots, and
title table occupy the wider detail pane. Selecting a history row updates the
run selector, and changing the selector updates the highlighted row. The panes
stack on narrow screens.

Relation previews capture the relation, filters, and row limit at load time.
Their summary cards and CSV name remain tied to that loaded state, and export is
unavailable after controls change until the preview is loaded again. Talent and
video filters are disabled when the selected relation does not contain the
corresponding key. Explore Relations uses a compact master/detail workspace: a
selectable, vertically scrolling relation browser stays beside the filters and a
tall preview table on wider screens, then stacks above it on narrow screens.
Descriptive Profiles keeps its selector in a compact toolbar and presents its
two vertically scrolling result tables side by side when space permits. It does
not scan a relation until explicitly requested.

## Run locally

From the repository root:

```bash
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  quarto serve \
  r_scripts/notebooks/dashboards/data_admin/raw_data_dashboard/dashboard.qmd \
  --host 127.0.0.1 \
  --port 3840
```

The host must provide the repository `.env` values used to resolve
`TALENT_DATALAKE_ROOT`. If classification run artifacts are mounted elsewhere,
set `TITLE_CLASSIFICATION_BATCH_RUN_ROOT`. The dashboard always opens DuckDB in
read-only mode.

## Refresh behavior

The dashboard has one manual refresh control for raw-data and classification
snapshots. It does not refresh automatically and never submits, checks, applies,
retries, or modifies classification data.

This is an administrative view. Put it behind the authenticated internal
reverse proxy and do not expose port `3840` directly to the public internet.

## Daily transcript processing

The **Processing → Transcript Processes** view defaults to the newest recorded
`subtitle_sentence_backfill` batch and offers the 100 most recent batches.
It shows Eastern Time start/end times, elapsed processing duration, run status, readable
batch counts, and recorded issues from `ops.pipeline_runs`. Batch counts label
the backlog at the start separately from the per-run limit and the number of
videos actually attempted. Legacy run summaries are translated from their old
internal counter names when displayed.

Each new batch writes one row per attempted subtitle track to
`ops.subtitle_backfill_attempts`. The batch table therefore includes successful,
failed, skipped-current, and still-running attempts, with a page-local search
across video ID, title, talent, result, and error text. Relation preview filters
live on **Explore Relations** and do not appear as transcript controls. Batches
created before per-attempt logging can only show retained sentence/block records
and the limited failures preserved in the aggregate run summary.

The batch table resolves the per-video `subtitle_sentence_reconstruction` runs
recorded between the selected backfill batch's start and finish, then joins the
retained sentence/block records to catalog video titles and talent names. A
direct run-ID match remains supported for legacy records. Video ID is the
troubleshooting video code. Separate language, track type, source scope, and
pipeline version choices prevent mixing transcripts. The page uses a compact
master/detail layout: batch controls, summary, search, and a human-readable video
list occupy the narrower left column, while the selected transcript occupies the
wider right column.
Both panes are direct siblings inside `.transcript-processes-layout`, an explicit
CSS grid with a 2:3 column ratio. Only viewports at or below 900px stack the panes.
The video list and transcript content scroll within their respective panes.
Selecting one video row populates structured video and
processing metadata without changing the selected batch or search text. Reading
View emphasizes timestamps and sentence text; Audit / Table View retains block,
sentence, alignment, and approximate-timestamp fields. Failed attempts show their
recorded error instead of an empty transcript. The CSV download includes all
sentences plus video, title, talent, and run identifiers. Text is escaped when
displayed. Approximate alignment is reported as metadata, not automatically
classified as a processing failure.

Operational limits: processing duration excludes setup before the pipeline run
was opened. Unfinished runs have no recorded final duration. In legacy batches,
failed/in-progress videos may have only local RDS checkpoints and therefore be
absent from the batch table; the legacy aggregate error summary is also
truncated. Reprocessing can replace older sentence/block text, but new attempt
rows preserve batch membership and status. The dashboard never substitutes
newer text for a selected run. Missing processing tables and empty batches show
empty states; database query errors are surfaced within the transcript page.
All access remains read-only.

All dashboard timestamps are displayed in the `America/New_York` time zone and
include the applicable `EST` or `EDT` abbreviation. Database timestamps and
downloaded raw values remain unchanged in UTC for lineage and audit consistency.

Validate the query behavior with:

```bash
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  Rscript --vanilla r_scripts/tests/test_raw_data_admin_transcripts.R
```

## Transcript lookup

The **Processing → Transcript Lookup** view searches all retained cleaned
sentence transcripts independently of processing batches. An exact video ID or
case-insensitive title fragment returns every matching language, track type,
source scope, pipeline version, and publication run in
`text.subtitle_sentence_units`. Selecting one variant opens its complete clean
text in three synchronized views: grouped speaker turns, sentence reading, and
an audit table with the retained lineage fields. The selected variant can also
be exported to CSV.

Speaker-turn groupings prefer `inferred_speaker_turn_id` and fall back to the
source-caption `speaker_turn_id`. They identify stored conversational
boundaries, not verified speaker identities, so the interface labels them as
numbered turns rather than assigning people or names.
