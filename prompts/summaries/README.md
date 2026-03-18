# Summary Prompts

This folder mirrors `bin/linux/codex_prompts/summaries/`.

Refactor logic:

- Keep maintained stream-summary prompt specs together under the same workflow category as their shell entry points.
- Use filenames that match the maintained shell scripts when possible:
  - `summarizing_stream_v2.sh` -> `summarizing_stream_v2.md`
- Older summary prompt variants belong in `archive/` so the top level stays reserved for the current maintained workflow.

Maintenance rule:

- If a new maintained summary workflow is added, place it here and keep the shell-script and prompt names aligned.
