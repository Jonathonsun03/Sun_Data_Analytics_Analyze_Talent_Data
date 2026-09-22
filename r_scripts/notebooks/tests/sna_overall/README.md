# Overall chatter profiles draft

This folder contains a read-only introduction to the maintained chatter
descriptive relations and the reusable user--video engagement network tools.

`chatter_network_explorer.qmd` is the client-facing, full-width D3 view. It
adds a scope summary and interaction guidance while using the same canonical
activity loader and shared JavaScript components as the audit notebook. Its
opening individual-chatter alluvial view connects chatter--video--stream-title
classification, with controls for attendance versus message weighting and
primary topics versus normalized title tags. Hovering a chatter emphasizes the
person's complete downstream path. View-contribution percentages use global
talent-level denominators computed before interactive filtering. It pairs that
view with a chatter--stream participation heatmap,
full-community layers, an engagement landscape, and a stream audience-overlap
matrix so clients can inspect community shape beyond the bounded node-link
view. The alluvial, heatmap, and overlap views include searchable multi-video
selectors; every available live stream in the selected talent catalog is
available. `sankey_max_videos`, `heatmap_max_videos`, and
`overlap_max_videos` control initial selections, while their corresponding
`*_max_selected` parameters protect readability. The shared selector can filter
by title, date preset or custom range, current primary topic, and normalized
keyword; filters narrow the list until the user explicitly selects or
deselects the matching streams. The alluvial and heatmap views also include a
search-first chatter selector that preserves the automatic top-chatter starting
point while allowing paginated browsing, name search, inclusive activity-count
filters, activity/name ordering, direct page jumps, page-level batch actions,
and a bounded manual selection from the full chatter catalog.

`overall_chatter_profiles.qmd` reads three primary analytical grains from the unified
lakehouse:

- one row per talent, chatter, and video;
- one row per talent and chatter; and
- one row per globally identified chatter across every talent.

The notebook documents the library interfaces for canonical activity loading,
general engagement summaries, and graph preparation/rendering. The primary
network is a three-layer D3 graph: users send messages in videos, and videos
belong to streamers. User--video edge width is the number of messages the user
sent in the video. The shared browser component lives under the repository's
top-level `js/` directory so R, Python, and direct HTML can reuse it. To remain
legible, the graph selects a bounded number of
high-message videos and active users; use `bipartite_max_videos` and
`bipartite_max_users` to adjust those bounds. Supplying `video_id` focuses the
graph on one video.

Refresh those relations with `r_scripts/run/publish_viewer_activity.R`. The
publisher defaults to a read-only dry run; set
`VIEWER_ACTIVITY_PUBLISH_DRY_RUN=false` to perform the transaction-safe full
refresh. It treats a YouTube channel ID (`user_id`) as the stable chatter
identifier and keeps public subscription dates and observed membership events
as separate, qualified attributes.

Render from the repository root:

```bash
quarto render \
  r_scripts/notebooks/tests/sna_overall/overall_chatter_profiles.qmd
```

Render the client explorer with:

```bash
quarto render \
  r_scripts/notebooks/tests/sna_overall/chatter_network_explorer.qmd
```

Limit the draft to one talent when iterating on the design:

```bash
quarto render \
  r_scripts/notebooks/tests/sna_overall/overall_chatter_profiles.qmd \
  -P talent_code:LEI3
```

Add `-P video_id:VIDEO_ID` to show the chatter-level counts for one video.

If Quarto stalls while activating the project environment, use the same
read-only fallback documented by the other repository audit notebooks:

```bash
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  quarto render \
  r_scripts/notebooks/tests/sna_overall/overall_chatter_profiles.qmd
```

Rendered reports are non-code artifacts and should be directed to the
configured DataLake when they need to be retained.
