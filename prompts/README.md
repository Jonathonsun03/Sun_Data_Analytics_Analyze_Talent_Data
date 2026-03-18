# Prompts

This folder stores maintained prompt specifications for non-classification analysis workflows.

The top-level categories intentionally mirror `bin/linux/codex_prompts/`:

- `monetary/`
  - Money-event timestamping and monetary relationship analysis prompts.
- `personality/`
  - Personality open coding, uniqueness, profile synthesis, and related personality-code prompts.
- `shared_qualities/`
  - Cross-talent shared-baseline prompts.
- `summaries/`
  - Stream-summary classification prompts.

Organization rules:

- Keep maintained prompt specs in the category that matches the shell entry-point group.
- Use category-local `archive/` folders for superseded or reference-only prompt specs.
- Prefer prompt filenames that match the canonical shell script name when there is a one-to-one workflow.
- Keep a `README.md` in each top-level category when the folder has workflow-specific placement or archive rules.

What belongs here:

- Human-readable prompt specs that describe an analysis task or output format.
- Prompt documents used by Codex-driven qualitative analysis workflows.
- Prompt files that should be maintained independently from the title-classification system.

What does not belong here:

- Title-classification prompt assets under `classification/prompts/`.
  - That separate folder is active code/config input for the title-classification pipeline.
  - It contains the base system prompt, schema, definitions, talent overlays, and live-chat summarization prompts currently referenced by code.

Rule of thumb:

- If a prompt is part of the title-classification compiler or classification config, keep it in `classification/prompts/`.
- If a prompt is a standalone analysis spec for downstream qualitative work, keep it in `prompts/` under the matching workflow category.
