`bin/linux/codex_prompts/` contains the maintained shell entry points for the Codex prompt workflows in this repository.

The maintained prompt specs intentionally mirror this folder tree under `prompts/`.
Each prompt category should document its local naming and archive rules with a nearby `README.md`.

## Canonical entry points

Use the scripts inside the category folders as the real run targets:

- `personality/personality_open_coding.sh`
- `shared_qualities/shared_behavior_baseline.sh`
- `personality/personality_qualitative_codebook.sh`
- `personality/personality_unique_features.sh`
- `personality/personality_profile.sh`
- `summaries/summarizing_stream_v2.sh`
- `monetary/monetary_summary_classification.sh`
- `monetary/money_timestamps_incremental.sh`

## Personality pipeline

The personality workflow is designed as a staged qualitative pipeline rather than a single summary pass.

### 1. Talent-local open coding

Run:
- `personality/personality_open_coding.sh`

What it does:
- builds each talent's retained open codes
- collects evidence rows from stream-derived text
- creates the talent-local qualitative base

Why it exists:
- uniqueness should not be inferred directly from vague impressions
- this step identifies recurring, text-supported behavior patterns for each talent first

Typical outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<talent>/stream_summaries/overall_themes/personality_open_coding/...`

### 2. Cross-talent shared baseline

Run:
- `shared_qualities/shared_behavior_baseline.sh`

What it does:
- compares the talent-local open-coding outputs across the set
- identifies shared interaction patterns
- builds the cross-talent baseline used to separate common behavior from distinctive behavior

Why it exists:
- many streamer traits are common across the dataset
- without a shared baseline, downstream profiles tend to overstate generic traits as unique

Typical outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/...`

### 3. Talent-unique features

Run:
- `personality/personality_unique_features.sh`

What it does:
- compares each talent's open-coded patterns against the shared baseline
- identifies what falls outside the baseline
- identifies distinctive versions of shared behaviors

Why it exists:
- this is the step that answers what makes a streamer non-interchangeable
- it is downstream of both open coding and shared-baseline modeling on purpose

Typical outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<talent>/stream_summaries/overall_themes/personality_unique_features/...`

### 4. Cross-talent qualitative code log

Run:
- `personality/personality_qualitative_codebook.sh`

What it does:
- maintains a cumulative cross-talent qualitative code log
- updates primary and secondary qualitative codes over time
- supports adding, revising, merging, and splitting codes as the corpus grows

Why it exists:
- this is the reusable coding registry that sits above individual talent outputs
- it preserves continuity while still allowing recoding when better structure emerges

Typical outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/...`

### 5. Broader personality profile

Run:
- `personality/personality_profile.sh`

What it does:
- produces the broader monthly and overall personality synthesis
- can describe stable traits and shifts over time

Why it exists:
- this is the readable synthesis artifact, not the raw coding layer
- it complements the uniqueness report rather than replacing it

Typical outputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<talent>/stream_summaries/overall_themes/personality_profile/...`

## Why this design works

This pipeline is intentionally split into layers:

- `personality_open_coding` finds talent-local recurring patterns from text-derived evidence
- `shared_behavior_baseline` defines what is common across the dataset
- `personality_unique_features` identifies what is distinctive only after that baseline exists
- `personality_profile` turns the analysis into a broader human-readable synthesis

That separation matters because it reduces two common failure modes:

- generic streamer traits being mislabeled as unique
- downstream profiles converging into interchangeable summaries

In short:
- local coding builds evidence
- shared baseline builds comparison
- unique features build contrast
- profile builds readable synthesis

## Other workflow groups

### Summaries

- `summaries/summarizing_stream_v2.sh`
- used for stream-summary classification and supporting summary artifacts

### Monetary

- `monetary/monetary_summary_classification.sh`
- `monetary/money_timestamps_incremental.sh`
- used for money-related classification and timestamp extraction

## Logging

All wrappers write logs to the datalake under:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/`

The log directory mirrors this `bin/linux/codex_prompts/` tree, with one folder per shell entry point:
- `personality/personality_open_coding/`
- `personality/personality_profile/`
- `personality/personality_unique_features/`
- `shared_qualities/shared_behavior_baseline/`
- `summaries/summarizing_stream_v2/`
- `monetary/monetary_summary_classification/`
- `monetary/money_timestamps_incremental/`

Most Codex-driven wrappers also write a final-message markdown file alongside the `.log` file in that same per-script folder.

## Maintenance rule

Keep only the categorized scripts in this directory tree as the maintained entry points.
Do not add new top-level forwarding shims unless there is a strong compatibility reason.
