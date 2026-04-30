# Overall Themes Prompts

This folder mirrors `bin/linux/codex_prompts/overall_themes/`.

Workflow roles:

- `overall_channel_summary.md` is the maintained spec for the overall channel summary workflow.
- The workflow consumes existing `stream_summaries/stream_summary_codex/*.md` files and builds cumulative channel-level theme outputs under `stream_summaries/overall_channel_summary/`.
- It is separate from `personality/` because channel-level recurring themes may include monetary behavior, audience pacing, care/boundaries, content habits, and other non-personality patterns.

Runner:

- `bin/linux/codex_prompts/overall_themes/overall_channel_summary.sh`
  - runs this prompt through `codex exec`

Archive rule:

- Use `archive/` for earlier drafts or transitional versions if this workflow changes substantially.
- Keep only the maintained default prompt at the top level.
