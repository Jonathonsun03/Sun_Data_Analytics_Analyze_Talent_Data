# Personality Pipeline and Qualitative Code Log

Last updated: 2026-03-17

## What Was Built

Today we built and documented a multi-step personality-analysis pipeline for the talent dataset.

The core idea is:

1. Build talent-local open coding from text-derived evidence.
2. Build a cross-talent shared baseline so generic streamer traits are not mistaken for uniqueness.
3. Build talent-specific unique-feature outputs against that shared baseline.
4. Build a cumulative qualitative code log that can grow and be recoded over time.
5. Keep the broader personality profile as a readable synthesis artifact rather than the primary coding layer.

This work was designed to reduce a recurring problem in the earlier personality outputs:
- different talents were getting overly similar personality summaries
- generic streamer traits were being overinterpreted as if they were unique

## Canonical Prompt Files

Current maintained personality prompts:

- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/prompts/personality/personality_open_coding.md`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/prompts/personality/personality_profile.md`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/prompts/personality/personality_unique_features.md`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/prompts/personality/personality_qualitative_codebook.md`

Shared baseline prompt:

- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/prompts/shared_behavior_baseline.md`

## Canonical Shell Runners

Current maintained Codex prompt wrappers:

- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/personality/personality_open_coding.sh`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/shared_qualities/shared_behavior_baseline.sh`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/personality/personality_unique_features.sh`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/personality/personality_qualitative_codebook.sh`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/personality/personality_profile.sh`

These are the canonical entry points. Earlier top-level forwarding shims were removed so the categorized scripts are now the maintained paths.

## Pipeline Order

### 1. Personality Open Coding

Purpose:
- generate talent-local recurring codes from stream-derived text
- retain evidence rows and emergent code families
- create the local qualitative base before any cross-talent comparison

Key design choice:
- uniqueness pressure was added at the open-coding stage so generic codes are demoted earlier instead of surviving into later summaries

Main runner:
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/personality/personality_open_coding.sh`

Primary outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<talent>/stream_summaries/overall_themes/personality_open_coding/...`

### 2. Shared Behavior Baseline

Purpose:
- identify which interaction patterns are common across talents
- separate shared baseline behaviors from shared-but-different patterns
- create a stricter comparison frame for later uniqueness work

Why this step matters:
- without it, downstream profiles tend to call generic streamer traits unique

Main runner:
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/shared_qualities/shared_behavior_baseline.sh`

Primary outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_baseline_codex.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_codebook.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/talent_shared_behavior_matrix.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_evidence.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_state.json`

### 3. Unique Personality Features

Purpose:
- identify what makes each talent non-interchangeable relative to the shared baseline
- separate distinctive versions of shared behaviors from truly non-baseline traits

Main runner:
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/personality/personality_unique_features.sh`

Primary outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<talent>/stream_summaries/overall_themes/personality_unique_features/current/unique_personality_profile_codex.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<talent>/stream_summaries/overall_themes/personality_unique_features/current/unique_personality_evidence_log_codex.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<talent>/stream_summaries/overall_themes/personality_unique_features/current/unique_personality_state.json`

Important guardrails added today:
- comparisons should be anonymous by default
- other streamers are named only when a clear same-company or same-roster signal exists
- narrow preferences or rules, such as spoiler policies, should not become major personality dimensions unless they clearly reveal a broader recurring relational style

### 4. Qualitative Code Log

Purpose:
- maintain a cumulative cross-talent qualitative coding registry
- preserve a durable coding vocabulary while still allowing recoding over time

This is intentionally a log, not a frozen one-shot codebook.

Main runner:
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/personality/personality_qualitative_codebook.sh`

Primary outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log_codex.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log_state.json`

Snapshot outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/snapshots/personality_qualitative_code_log_YYYY-MM-DD_HH-MM-SS_±HHMM.md`

Code-log structure:
- `Primary Code ID`
- `Primary Code`
- `Secondary Code ID`
- `Secondary Code`
- `Definition`
- `Date added`
- `Examples from text`

Primary/secondary rule:
- a primary code is a mid-level analytic family
- a primary code may stand on its own
- a secondary code is only added when a recurring subtype or variant creates meaningful analytic clarity
- a single example belongs in `Examples from text`, not as a new secondary code

### 5. Broader Personality Profile

Purpose:
- produce the broader readable synthesis artifact for each talent
- support monthly and overall personality views
- describe stability and shifts over time

Main runner:
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/bin/linux/codex_prompts/personality/personality_profile.sh`

Primary outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<talent>/stream_summaries/overall_themes/personality_profile/...`

Conceptual role:
- this is the readable synthesis layer
- it is downstream of coding and comparison, not a replacement for them

## Why the Pipeline Works This Way

The design is intentionally layered:

- open coding builds local evidence
- shared baseline builds comparison discipline
- unique features build contrast
- qualitative code log builds a reusable analytic vocabulary
- broader profile builds the readable synthesis

This reduces two major failure modes:

- generic streamer behaviors being mistaken for uniqueness
- all talents converging into interchangeable personality summaries

## Python Script Reorganization

The Python work that emerged during the personality-pipeline build was reorganized into `py_scripts/` for consistency.

Current maintained Python locations:

- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/py_scripts/lib/repo_paths.py`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/py_scripts/run/stream_summaries/personality/build_shared_behavior_baseline.py`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/py_scripts/run/stream_summaries/personality/build_unique_personality_profiles.py`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/py_scripts/run/stream_summaries/personality/build_personality_qualitative_code_log.py`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/py_scripts/run/stream_summaries/personality/personality_profile_v3_incremental_open_coding.py`
- `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/py_scripts/run/stream_summaries/personality/personality_profile_synthesis.py`

Organizational decision:
- `py_scripts/lib/` is for reusable helpers
- `py_scripts/run/` is for runnable Python entrypoints
- temporary `tasks/` placement was cleaned up

## Logging Convention

The Codex prompt wrappers now write logs to processed-log space rather than repo-local logs:

- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/personality/...`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/shared_qualities/...`

This keeps prompt-run logs with processed outputs instead of mixing them into the repository.

## Practical Run Order

For a fresh personality pass, the recommended order is:

1. `personality_open_coding.sh`
2. `shared_behavior_baseline.sh`
3. `personality_unique_features.sh`
4. `personality_qualitative_codebook.sh`
5. `personality_profile.sh`

## Open Maintenance Notes

- The qualitative code log is intended to evolve over time rather than freeze immediately.
- Shared baseline and unique-feature prompts should continue to be treated as the main comparative layers.
- If future profile outputs start sounding interchangeable again, review:
  - the shared baseline
  - uniqueness-dimension filters
  - whether overly generic primary codes are dominating the qualitative log
