# Prompts

This folder stores maintained prompt specifications for analysis and classification workflows.

The top-level categories intentionally mirror `bin/linux/codex_prompts/`:

- `monetary/`
  - Money-event timestamping and monetary relationship analysis prompts.
- `chat_personality/`
  - Chat-community personality prompts that write derived talent-local outputs under each talent's `qualitative coding/chat data` folder.
- `overall_themes/`
  - Whole-channel theme summaries derived from per-stream summaries.
- `streamer_personality/`
  - Personality open coding, uniqueness, profile synthesis, and related personality-code prompts.
- `shared_qualities/`
  - Cross-talent shared-baseline prompts.
- `summaries/`
  - Per-stream summary prompts.
- `title_classification/`
  - Maintained base prompt components used to publish a versioned title-classification record to DuckDB.

Organization rules:

- Keep maintained prompt specs in the category that matches the shell entry-point group.
- Use category-local `archive/` folders for superseded or reference-only prompt specs.
- Prefer prompt filenames that match the canonical shell script name when there is a one-to-one workflow.
- Keep a `README.md` in each top-level category when the folder has workflow-specific placement or archive rules.

What belongs here:

- Human-readable prompt specs that describe an analysis task or output format.
- Prompt documents used by Codex-driven qualitative analysis workflows.
- Prompt files that should be maintained independently from the title-classification system.

Rule of thumb:

- If a prompt is part of the title-classification compiler, keep the maintained source in `prompts/title_classification/` and publish a versioned snapshot to DuckDB.
- If a prompt is a standalone analysis spec for downstream qualitative work, keep it in `prompts/` under the matching workflow category.
