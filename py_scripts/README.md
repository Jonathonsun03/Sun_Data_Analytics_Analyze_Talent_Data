# py_scripts

This directory contains the maintained Python code for the repository.

## Environment setup

Create a project-local virtual environment and install the Python dependencies:

```bash
python3 -m venv .venv
.venv/bin/python -m pip install --upgrade pip
.venv/bin/python -m pip install -r py_scripts/requirements.txt
```

Select `.venv/bin/python` as the Python interpreter and notebook kernel in VS Code.
The runnable Python scripts currently use only the standard library; the packages
in `requirements.txt` support VS Code/Quarto notebooks and DuckDB dataframe output.

## Structure

- `run/`
  - runnable Python entrypoints
  - these are task and pipeline scripts that can be executed directly
- `lib/`
  - reusable Python helpers shared by the runnable entrypoints

## Current stream-summary runners

- `run/stream_summaries/summary_classification/summary_classification_incremental.py`
- `run/stream_summaries/monetary_analysis/monetary_summary_classification_incremental.py`
- `run/stream_summaries/streamer_personality/personality_profile_v3_incremental_open_coding.py`
- `run/stream_summaries/streamer_personality/personality_profile_synthesis.py`
- `run/stream_summaries/streamer_personality/build_shared_behavior_baseline.py`
- `run/stream_summaries/streamer_personality/build_unique_personality_profiles.py`

## Other maintained entrypoints

- `run/demo_data/generate_demo_talent_dataset.py`
  - creates a synthetic talent folder in the datalake
  - writes realistic-looking analytics, monetary, demographic, geography, and isolated title-classification CSVs
  - intended for client-safe sample Bundle A/B reports without using real talent data

## Notes

- `tasks/` was a temporary holding area and is no longer the canonical location.
- New Python work should be placed in `run/` or `lib/` rather than `tasks/`.
