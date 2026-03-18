# py_scripts

This directory contains the maintained Python code for the repository.

## Structure

- `run/`
  - runnable Python entrypoints
  - these are task and pipeline scripts that can be executed directly
- `lib/`
  - reusable Python helpers shared by the runnable entrypoints

## Current stream-summary runners

- `run/stream_summaries/summary_classification/summary_classification_incremental.py`
- `run/stream_summaries/monetary_analysis/monetary_summary_classification_incremental.py`
- `run/stream_summaries/personality/personality_profile_v3_incremental_open_coding.py`
- `run/stream_summaries/personality/personality_profile_synthesis.py`
- `run/stream_summaries/personality/build_shared_behavior_baseline.py`
- `run/stream_summaries/personality/build_unique_personality_profiles.py`

## Notes

- `tasks/` was a temporary holding area and is no longer the canonical location.
- New Python work should be placed in `run/` or `lib/` rather than `tasks/`.
