# Monetary Prompts

This folder mirrors `bin/linux/codex_prompts/monetary/`.

Refactor logic:

- Keep the maintained monetary prompt specs together in one folder because the shell entry points are grouped under the same workflow area.
- Use prompt filenames that match the shell script names so the relationship stays obvious:
  - `monetary_summary_classification.sh` -> `monetary_summary_classification.md`
  - `money_timestamps_incremental.sh` -> `money_timestamps_incremental.md`
- Treat files in this folder as the maintained prompt sources that the shell wrappers should target by default.

Maintenance rule:

- If a monetary prompt is superseded but worth keeping for reference, create `archive/` here and move the older spec there instead of mixing active and legacy prompt files together.
