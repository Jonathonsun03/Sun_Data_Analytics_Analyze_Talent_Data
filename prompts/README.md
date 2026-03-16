# Prompts

This folder stores maintained prompt specifications for analysis workflows that are not part of the title-classification prompt compiler.

Current use:

- `Monetary_analysis/`
  - Prompt specs for summary-level monetary analysis and theme extraction.
- `stream_summaries/`
  - Prompt specs for stream-summary level qualitative analysis built on top of summary corpora and replay data.
- `personality/`
  - Prompt specs for streamer personality profiling and open-coding analysis built from stream-summary assets.

What belongs here:

- Human-readable prompt specs that describe an analysis task or output format.
- Prompt documents used by stream-summary or qualitative analysis workflows.
- Prompt files that should be maintained independently from the title-classification system.

What does not belong here:

- Title-classification prompt assets under `classification/prompts/`.
  - That separate folder is active code/config input for the title-classification pipeline.
  - It contains the base system prompt, schema, definitions, talent overlays, and live-chat summarization prompts currently referenced by code.

Rule of thumb:

- If a prompt is part of the title-classification compiler or classification config, keep it in `classification/prompts/`.
- If a prompt is a standalone analysis spec for downstream qualitative work, keep it in `prompts/`.
