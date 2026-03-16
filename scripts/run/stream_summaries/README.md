`scripts/run/stream_summaries/` holds runnable entrypoints for stream-summary analysis workflows.

Use this split for future work:
- Put runnable scripts here.
- Put reusable helpers in `scripts/lib/stream_summaries/`.
- Put prompt specs in `prompts/stream_summaries/`.
- Keep `notes/` for draft ideas, not maintained runners.

For streamer personality work specifically:
- Treat it as a `stream_summaries` workflow, not a standalone `streamer_personality` area.
- Name runners after the output they generate, for example `run_personality_profile.py`.
- If the shared parsing or evidence-building logic starts diverging, move that logic into `scripts/lib/stream_summaries/` and keep the runners thin.
- Keep only the current personality runner in `scripts/run/stream_summaries/personality/`.
- Move superseded personality runners into `scripts/run/stream_summaries/personality/archive/` instead of leaving multiple inactive versions beside the active one.

For summary classification work:
- Keep runnable entrypoints in `scripts/run/stream_summaries/summary_classification/`.
- Keep reusable parsing, verification, or codebook helpers in `scripts/lib/stream_summaries/`.
