# Classification Prompts

This folder contains the prompt assets and prompt configuration inputs used by the title-classification pipeline, plus the live-chat summarization prompts currently reused by the stream-summary workflow.

This is active code input, not archival notes.

## What Uses This Folder

Primary code paths:

- `scripts/lib/stream_classification/prompt_builder.R`
  - Loads the title-classification prompt bundle from the files in this folder.
- `scripts/run/Title_classification/title_classification/02_classify_pending_titles.R`
  - Uses the compiled prompt bundle for LLM title classification.
- `scripts/run/Title_classification/title_classification/03_self_test_classification.R`
  - Validates that the prompt bundle, schema, and DuckDB columns stay aligned.
- `scripts/run/Title_classification/talent_profile/build_talent_profile.R`
  - Uses the `discovery/` prompts and writes talent overlays under `talents/`.
- `scripts/run/Title_classification/talent_profile/sync_talent_profiles.R`
  - Syncs `classification/config/talent_profiles.json` from the folders in `talents/`.
- `scripts/lib/stream_summaries/args.R`
  - Uses `live_chat_logs/chat_log_summarization_openai.md` as the default summarization prompt.

## Folder Layout

- `base/`
  - Core title-classification prompt parts.
  - Includes the system prompt, instructions, content-type rules, and base output schema.
- `definitions/`
  - Definition files that expand the classification object with boolean fields such as `collaborative_energy` and `personality_conversation`.
  - These are compiled into both the user prompt and the schema.
- `talents/`
  - Talent-specific overlays and optional overrides.
  - Also stores `full_prompt/` dumps written at runtime so each classification run has a prompt artifact on disk.
- `discovery/`
  - GPT discovery prompts used when building talent prompt profiles from title histories.
- `live_chat_logs/`
  - Prompt files for stream-summary style chat-log summarization.
  - The OpenAI version is still wired into the maintained summarization runner.
- `Title_classification/`
  - Older prompt files from an earlier title-classification setup.
  - Keep these only if they are still useful for reference or migration.

## Important Notes

- Do not move this folder casually. Paths are referenced by `classification/config/talent_profiles.json` and by multiple scripts under `scripts/lib/stream_classification/` and `scripts/run/Title_classification/`.
- If prompt assets are reorganized, update the config and all prompt-loading code together.
- The maintained stream-summary runner currently depends on `live_chat_logs/chat_log_summarization_openai.md`.
- The old Ollama replay-analysis runner has been removed; if `chat_log_summarization_ollama.md` is no longer needed, it can be reviewed separately.
