# Summary Prompts

This folder mirrors `bin/linux/codex_prompts/summaries/`.

Refactor logic:

- Keep maintained stream-summary prompt specs together under the same workflow category as their shell entry points.
- Use filenames that match the maintained shell scripts when possible:
  - `stream_summary_codex.sh` -> `stream_summary_codex.md`
- Older summary prompt variants belong in `archive/` so the top level stays reserved for the current maintained workflow.

Workflow roles:

- `stream_summary_codex.md` creates one per-stream markdown summary from each `text_playback/*.csv`.
- Whole-channel theme synthesis lives under `prompts/overall_themes/` because it is downstream of per-stream summaries and may include personality, monetary, pacing, and other recurring channel-level themes.

Scheduled stream-summary runs:

- `bin/linux/codex_prompts/summaries/stream_summary_codex_scheduled.sh` is the cron-friendly wrapper for this prompt.
- It scans talent folders for missing or empty stream summaries before launching Codex.
- It calls `stream_summary_codex.sh`; it does not define a separate summary prompt.
- Use `--talent "Nova Aokami Ch"` for one-talent catch-up runs.
- Use `--all-talents` or omit `--talent` for regular maintenance runs that pick up new text playback files across the corpus.
- Default run window is `00:04-05:05`, with a default cap of 20 missing or empty summaries per run.

Maintenance rule:

- If a new maintained summary workflow is added, place it here and keep the shell-script and prompt names aligned.
