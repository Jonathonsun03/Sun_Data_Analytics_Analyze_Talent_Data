`py_scripts/run/stream_summaries/` holds the maintained Python entrypoints for stream-summary analysis workflows.

Use this split for future work:
- Keep runnable Python scripts here under `py_scripts/run/stream_summaries/`.
- Move reusable Python helpers into `py_scripts/lib/`.
- Keep shared R summarization helpers in `r_scripts/lib/stream_summaries/`.
- Keep prompt specs in `prompts/`, organized to mirror `bin/linux/codex_prompts/` (`summaries/`, `monetary/`, `personality/`, and `shared_qualities/`).

Current subareas:
- `summary_classification/` for summary-driven qualitative coding.
- `monetary_analysis/` for raw-text money-event analysis.
- `personality/` for personality profiling and synthesis workflows.
- `personality/archive/` for superseded runners kept only for historical reference.
