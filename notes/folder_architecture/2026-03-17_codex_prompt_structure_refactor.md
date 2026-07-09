# Codex Prompt Structure Refactor

Date:

- 2026-03-17

Scope:

- reorganized `prompts/` so its maintained top-level folders mirror `bin/linux/codex_prompts/`
- updated maintained shell wrappers to point at the new prompt paths
- added local `README.md` files in the prompt folders to explain the refactor logic and archive rules

Why this was done:

- the prompt layout had drifted away from the shell-entry-point layout
- related prompts were spread across mixed naming styles and inconsistent folders
- wrapper defaults and prompt locations were harder to trace than they needed to be
- a mirrored structure makes it easier to answer:
  - which shell script owns this prompt
  - where a new prompt should live
  - where superseded prompts should be archived

What changed:

1. Maintained prompt folders were aligned to the shell categories:
   - `prompts/monetary/`
   - `prompts/personality/`
   - `prompts/shared_qualities/`
   - `prompts/summaries/`

2. Maintained prompt filenames were normalized to match the shell entry points where there is a one-to-one workflow:
   - `monetary_summary_classification.sh` -> `monetary_summary_classification.md`
   - `money_timestamps_incremental.sh` -> `money_timestamps_incremental.md`
   - `personality_open_coding.sh` -> `personality_open_coding.md`
   - `personality_unique_features.sh` -> `personality_unique_features.md`
   - `personality_qualitative_codebook.sh` -> `personality_qualitative_codebook.md`
   - `personality_profile.sh` -> `personality_profile.md`
   - `shared_behavior_baseline.sh` -> `shared_behavior_baseline.md`
   - `summarizing_stream_v2.sh` -> `summarizing_stream_v2.md`

3. Older or reference-only prompt specs were moved into category-local archive folders instead of leaving them mixed with maintained defaults.

4. Supporting references were updated so the new layout is documented in-place:
   - `prompts/README.md`
   - prompt category `README.md` files
   - selected runner/doc references that previously mentioned old prompt paths

Resulting logic:

- `bin/linux/codex_prompts/` defines the maintained shell entry points
- `prompts/` mirrors that structure for maintained prompt specs
- top-level prompt files are the current maintained defaults
- `archive/` folders hold superseded or reference-only variants

Expected maintenance benefit:

- easier onboarding for future prompt edits
- fewer broken path references when prompt files move
- clearer distinction between active prompts and historical prompt versions
- more predictable placement for new workflows and prompt revisions
