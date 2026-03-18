# Personality Prompts

This folder mirrors `bin/linux/codex_prompts/personality/`.

Refactor logic:

- Keep maintained personality prompt specs beside each other because they are part of one staged pipeline.
- Match prompt filenames to the canonical shell entry points when there is a direct one-to-one relationship:
  - `personality_open_coding.sh` -> `personality_open_coding.md`
  - `personality_unique_features.sh` -> `personality_unique_features.md`
  - `personality_qualitative_codebook.sh` -> `personality_qualitative_codebook.md`
  - `personality_profile.sh` -> `personality_profile.md`
- Keep the cross-talent shared-baseline prompt out of this folder. That workflow lives under `prompts/shared_qualities/` because its shell entry point lives under `bin/linux/codex_prompts/shared_qualities/`.

Pipeline note:

- The maintained flow is:
  1. `personality_open_coding.md`
  2. `../shared_qualities/shared_behavior_baseline.md`
  3. `personality_unique_features.md`
  4. `personality_qualitative_codebook.md`
  5. `personality_profile.md`

Archive rule:

- Use `archive/` for superseded prompt specs that are still useful for historical reference or runner compatibility.
- Do not point the maintained shell wrappers at `archive/` unless you are intentionally reviving a legacy workflow.
