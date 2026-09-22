# Overall chatter profiles draft

This folder contains a read-only introduction to the maintained chatter
descriptive relations and the reusable user--video engagement network tools.

`overall_chatter_profiles.qmd` reads three primary analytical grains from the unified
lakehouse:

- one row per talent, chatter, and video;
- one row per talent and chatter; and
- one row per globally identified chatter across every talent.

The notebook documents the library interfaces for canonical activity loading,
general engagement summaries, and graph preparation/rendering. The primary
network is a three-layer graph: users send messages in videos, and videos
belong to streamers. User--video edge width is the number of messages the user
sent in the video. To remain legible, the graph selects a bounded number of
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
