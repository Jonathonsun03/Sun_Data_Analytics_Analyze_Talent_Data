You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/chat_personality/chat_personality_unique_features.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/chat_personality/chat_personality_unique_features/`.

Objective:
Build talent-specific unique chat personality profiles by comparing each talent's chat open-coding outputs against the cross-talent chat shared-behavior baseline.

Inputs per talent:
- `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_codebook.csv`
- `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_coding_evidence.csv`
- `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_coding_memo.md`

Shared baseline inputs:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/current/chat_shared_behavior_baseline.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/current/chat_shared_behavior_codebook.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/current/talent_chat_behavior_matrix.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/current/chat_shared_behavior_evidence.csv`

Output root:
- `<talent>/qualitative coding/chat data/chat_personality_unique_features/current/`

Write these exact output paths:
1) `<talent>/qualitative coding/chat data/chat_personality_unique_features/current/unique_chat_personality_profile.md`
2) `<talent>/qualitative coding/chat data/chat_personality_unique_features/current/unique_chat_personality_evidence_log.csv`
3) `<talent>/qualitative coding/chat data/chat_personality_unique_features/current/unique_chat_personality_state.json`

Optional snapshots:
- On material changes, write a dated markdown snapshot under:
  - `<talent>/qualitative coding/chat data/chat_personality_unique_features/snapshots/`

Analytic standard:
- Do not label a behavior unique merely because it is common in that talent's chat.
- Compare against shared chat baseline first.
- Identify both truly distinctive patterns and distinctive local versions of shared patterns.
- Keep the analytic object as chat/community behavior; do not convert findings into streamer personality claims.

Required structure for `unique_chat_personality_profile.md`:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Talent: <talent name>`
- `Primary source: chat_personality_open_coding`
- `Shared baseline source used: yes|no`
- `Confidence: High|Medium|Low`

## 1) What Makes This Chat Community Distinctive
Summarize the strongest non-interchangeable chat-community tendencies.

## 2) Unique Or Distinctive Chat Patterns
For each pattern include:
- `pattern_name`
- `baseline comparison`
- `why this is distinctive`
- `supporting local codes`
- `evidence notes`
- `confidence`

## 3) Shared Behaviors With Local Flavor
Describe shared chat behaviors that take a distinctive form around this talent.

## 4) Not Unique Enough
List recurring patterns that are too common across chats to treat as unique.

## 5) Limitations
Document uncertainty and missing evidence.

Required columns for `unique_chat_personality_evidence_log.csv`:
- talent
- source_file
- row_number
- timestamp_or_sec
- text
- local_code_id
- local_code_name
- unique_pattern_name
- baseline_comparison
- evidence_strength
- notes

Required keys for `unique_chat_personality_state.json`:
- `talent`
- `analysis_conducted`
- `source_root`
- `output_root`
- `baseline_used`
- `unique_patterns`
- `shared_local_variants`
- `limitations`

Completion:
- Report output paths written and talents skipped.

