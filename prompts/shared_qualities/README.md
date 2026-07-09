# Shared Qualities Prompts

This folder mirrors `bin/linux/codex_prompts/shared_qualities/`.

Refactor logic:

- Keep cross-talent comparison prompts separate from talent-local personality prompts.
- The maintained shared-baseline prompt lives here because its shell entry point lives here:
  - `shared_behavior_baseline.sh` -> `shared_behavior_baseline.md`
- This separation makes it clear that the workflow is a cross-case synthesis step, not an individual talent-profile step.

Archive rule:

- Use `archive/` for earlier drafts or transitional versions of shared-baseline prompts.
- Keep only the maintained default prompt at the top level.
