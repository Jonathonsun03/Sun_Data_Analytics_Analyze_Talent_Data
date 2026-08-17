# Title Classification Prompts

This folder contains the maintained source for title-classification prompt versions.

This is active code input, not archival notes.

## What Uses This Folder

Primary code paths:

- `r_scripts/lib/title_classification/prompt_builder.R`
  - Compiles the prompt components and code definitions.
- `r_scripts/run/title_classification/publish_title_version.R`
  - Publishes a complete, queryable version snapshot to `classification.title_versions`.
- `r_scripts/run/title_classification/03_self_test_classification.R`
  - Validates that the prompt bundle, schema, and DuckDB columns stay aligned.
- `r_scripts/run/title_classification/talent_profile/build_talent_profile.R`
  - Uses the `discovery/` prompts and publishes reusable profiles to `catalog.talent_profiles`.

## Folder Layout

- `base/`
  - Core title-classification prompt parts.
  - Includes the system prompt, instructions, content-type rules, and base output schema.
- `definitions/`
  - Definition files that expand the classification object with boolean fields such as `collaborative_energy` and `personality_conversation`.
  - These are compiled into both the user prompt and the schema.
- `discovery/`
  - Discovery prompts used when building reusable talent profiles.
- `archive/`
  - Older prompt files from an earlier title-classification setup.

## Important Notes

- Runtime prompt snapshots are database rows, not files in this directory.
- A title version stores its prompt, definitions, and output schema together in `classification.title_versions`.
- Reusable, versioned talent profiles live in `catalog.talent_profiles`; title guidance is one context within a profile rather than the profile's sole purpose.
