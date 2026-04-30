# Overall Themes Runners

This folder contains shell entry points for channel-level theme workflows.

Current runner:

- `overall_channel_summary.sh`
  - runs `prompts/overall_themes/overall_channel_summary.md` through `codex exec`
  - writes cumulative outputs under `<talent>/stream_summaries/overall_channel_summary/`

This category is separate from `summaries/` because `summaries/` is reserved for per-stream summary generation.
