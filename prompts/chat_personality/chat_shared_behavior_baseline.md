You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Processed output root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/chat_personality/chat_shared_behavior_baseline.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/chat_personality/chat_shared_behavior_baseline/`.

Objective:
Build a cross-talent shared baseline for chat personality patterns. This separates broad chat-community behaviors from patterns that are distinctive around a specific talent.

Inputs:
- Prefer each eligible talent's current open-coding outputs:
  - `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_codebook.csv`
  - `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_coding_evidence.csv`
  - `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_coding_memo.md`

Eligibility:
- Include a talent only when it has usable chat open-coding outputs.
- If `TALENT_SLUG` is provided, use it only to focus review notes; the baseline itself should compare all eligible talents unless the user explicitly asks for a single-talent diagnostic.

Output paths:
1) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/current/chat_shared_behavior_baseline.md`
2) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/current/chat_shared_behavior_codebook.csv`
3) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/current/talent_chat_behavior_matrix.csv`
4) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/current/chat_shared_behavior_evidence.csv`
5) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/current/chat_shared_behavior_state.json`

Optional snapshots:
- On material changes, write a dated markdown snapshot under:
  - `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/snapshots/`

Analytic standard:
- Identify patterns shared across multiple talent chat communities.
- Preserve distinctions between a generic pattern and a talent-specific version of that pattern.
- Do not use streamer behavior as evidence for chat behavior.
- Do not infer demographics, diagnoses, or off-platform identities.

Required columns for `chat_shared_behavior_codebook.csv`:
- shared_code_id
- shared_code_name
- definition
- inclusion_criteria
- exclusion_criteria
- common_interactional_function
- talents_observed
- evidence_count
- baseline_strength
- notes

Required columns for `talent_chat_behavior_matrix.csv`:
- talent
- shared_code_id
- shared_code_name
- present
- local_variant_summary
- evidence_count
- confidence

Required columns for `chat_shared_behavior_evidence.csv`:
- talent
- source_file
- row_number
- timestamp_or_sec
- text
- local_code_id
- shared_code_id
- shared_code_name
- notes

Required structure for `chat_shared_behavior_baseline.md`:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Eligible talents included: <count>`
- `Primary upstream source: chat_personality_open_coding`

## 1) Shared Chat Baseline Summary
Summarize the broad shared behaviors that should not automatically be treated as unique.

## 2) Shared Chat Behavior Families
Describe each shared family, how it appears across talents, and where local variants differ.

## 3) Distinctiveness Guidance
Explain what later uniqueness/profile workflows should treat as generic, distinctive-in-form, or potentially unique.

## 4) Limitations
Document missing talents, weak evidence, or uneven source coverage.

Completion:
- Report output paths written and talents skipped.

