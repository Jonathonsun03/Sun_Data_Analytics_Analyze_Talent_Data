You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Processed output root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/concept_areas/definitions/chat_personality

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/chat_personality/chat_personality_qualitative_codebook.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/chat_personality/chat_personality_qualitative_codebook/`.

Objective:
Maintain a cumulative cross-talent qualitative code log for chat personality and community interaction style.

Inputs:
- Talent-local chat open coding:
  - `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_codebook.csv`
  - `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_coding_evidence.csv`
- Talent-local unique chat features:
  - `<talent>/qualitative coding/chat data/chat_personality_unique_features/current/unique_chat_personality_profile.md`
  - `<talent>/qualitative coding/chat data/chat_personality_unique_features/current/unique_chat_personality_evidence_log.csv`
- Shared chat baseline:
  - `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/concept_areas/interaction_views/chat_shared/current/...`

Output paths:
1) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/concept_areas/definitions/chat_personality/current/chat_personality_qualitative_code_log.csv`
2) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/concept_areas/definitions/chat_personality/current/chat_personality_qualitative_code_log.md`
3) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/concept_areas/definitions/chat_personality/current/chat_personality_qualitative_code_log_state.json`

Optional snapshots:
- On material changes, write a dated markdown snapshot under:
  - `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/concept_areas/definitions/chat_personality/snapshots/`

Codebook standard:
- This is a chat-side codebook only.
- Do not merge streamer-personality codes into this codebook.
- Codes should describe observable chat/community behavior and interactional function.
- Retain continuity with prior current codebook outputs when they exist.
- Add, merge, split, or retire codes only when evidence supports it.

Required columns for `chat_personality_qualitative_code_log.csv`:
- primary_code_id
- primary_code
- secondary_code_id
- secondary_code
- definition
- inclusion_criteria
- exclusion_criteria
- interactional_function
- examples_from_text
- talents_observed
- date_added
- date_updated
- status
- notes

Required structure for `chat_personality_qualitative_code_log.md`:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Primary source layers: chat_personality_open_coding + chat_shared_interactions + chat_personality_unique_features`

## 1) Codebook Summary
Summarize additions, merges, splits, retirements, and major structural changes.

## 2) Current Chat Personality Codes
Describe the active code families.

## 3) Evidence And Revision Notes
Explain why codes were retained or changed.

## 4) Limitations
Document missing talents, sparse evidence, or ambiguity.

Required keys for `chat_personality_qualitative_code_log_state.json`:
- `analysis_conducted`
- `source_layers`
- `active_code_count`
- `retired_code_count`
- `talents_included`
- `changes_made`
- `limitations`

Completion:
- Report output paths written.

