You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Qualitative coding output root:

<talent>/qualitative coding/chat data/

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/chat_personality/chat_personality_profile.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/chat_personality/chat_personality_profile/`.

Execution guard:
- If you are reading this prompt inside a `codex exec` session that was launched by `bin/linux/codex_prompts/chat_personality/chat_personality_profile.sh`, do not invoke `bin/linux/codex_prompts/chat_personality/chat_personality_profile.sh` again.
- Do not run `codex exec` from inside this workflow.
- Perform file inspection and output writing directly in the current Codex run.
- If scripting is useful for CSV inspection or output writing, use `python3`, not `python`.

Optional talent scope:
- The shell runner accepts `--talent "<exact talent folder name>"`.
- When `TALENT_SLUG` is provided by the runner, process only that exact talent folder and replace any talent placeholder with that folder name.
- When no `TALENT_SLUG` is provided, process every eligible talent.

Objective:
Build text-grounded chat personality outputs for every eligible talent. The analytic object is the chat community's visible interaction style around that talent, not the streamer's personality.

Core distinction:
- `streamer_personality` describes the streamer/talent's text-visible behavior.
- `chat_personality` describes the audience/chat community's text-visible behavior.
- Do not infer streamer personality from chat behavior alone.
- Do not treat streamer speech as chat evidence.
- Use streamer lines only as local context for what chat appears to be responding to.

Important limitations:
- You do NOT have visual cues.
- You do NOT have full vocal prosody from audio.
- You do NOT know individual chatters' off-platform identities or stable traits.
- Interpret only recurring community-level patterns visible in text.
- If evidence is weak, say `insufficient evidence`.

Talent discovery rules:
- Discover all direct child directories inside `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`.
- Exclude non-talent aggregate directories such as `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/VarianceProject`.
- Treat a directory as eligible only if it contains one or more text replay CSV files under `<talent>/text_playback/`.
- Process every eligible talent found at runtime unless a talent scope is provided.

Primary inputs for each talent:
- `<talent>/text_playback/*.csv`

Expected input handling:
- Prefer rows whose metadata identifies them as chat, audience, viewer, comment, or message rows.
- If the CSV has a `source`, `speaker`, `message_type`, `author`, `name`, or similar column, use it to separate chat rows from streamer/subtitle rows.
- If source columns are ambiguous, inspect a small sample and document the rule used.
- Never modify source CSVs.

Output organization:
- All talent-local derived chat personality outputs must be written under:
  - `<talent>/qualitative coding/chat data/`
- The folder name must be exactly `chat data`.
- This folder sits inside each talent's `qualitative coding` directory.
- Do not write chat personality outputs loose under `<talent>/qualitative coding/`.
- Do not write chat personality outputs loose under `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/`.
- Create talent-specific output folders as needed.

Write these exact output paths for each talent:
1) `<talent>/qualitative coding/chat data/chat_personality_profile.md`
2) `<talent>/qualitative coding/chat data/chat_personality_evidence_log.csv`
3) `<talent>/qualitative coding/chat data/chat_personality_state.json`

Optional snapshots:
- If a prior current markdown exists and this run materially changes it, also write:
  - `<talent>/qualitative coding/chat data/snapshots/chat_personality_profile_YYYY-MM-DD_HH-MM-SS_offset.md`

Analytic standard:
- Identify recurring chat-community interaction patterns, not one-off jokes.
- Ground every major claim in quoted chat evidence from multiple streams when available.
- Separate what chat does from what the streamer does in response.
- Describe interactional function: support, teasing, coordination, in-joke maintenance, correction, hype, concern, boundary-testing, moderation alignment, lore-building, or other emergent patterns.
- Avoid broad labels like `nice`, `chaotic`, `supportive`, or `parasocial` unless the evidence explains the concrete behavior behind the label.
- Do not diagnose chat, infer demographics, or make claims about individual users beyond text-visible behavior.

Required structure for `chat_personality_profile.md`:

Start the document with:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Talent: <talent name>`
- `Source root: <talent>/text_playback/`
- `Output root: <talent>/qualitative coding/chat data/`
- `Replay files inspected: <count>`
- `Chat rows considered: <count>`
- `Streamer/context rows used only as context: <count>`
- `Confidence: High|Medium|Low`

## 1) Chat Community Personality Summary
- 2 to 4 paragraphs describing the chat community's recurring visible style.
- Name the strongest 3 to 6 evidence-backed tendencies.
- Explain what makes this chat community distinctive around this talent.

## 2) Major Chat Interaction Patterns
- Present 3 to 8 patterns.
- For each pattern include:
  - `pattern_name`
  - `what chat does`
  - `interactional function`
  - `how common it appears`
  - `evidence notes`

## 3) Relationship To Streamer Context
- Explain how chat appears to respond to the streamer's prompts, jokes, boundaries, questions, or emotional tone.
- Keep this as context. Do not convert chat behavior into streamer personality claims.

## 4) Recurring Language, Rituals, And In-Jokes
- List recurring phrases, rituals, memes, or coordination behaviors only when they show a pattern across multiple moments.
- Explain what each marker appears to accomplish socially.

## 5) Boundaries And Risk Notes
- Note where chat appears to respect, test, negotiate, or ignore boundaries.
- Include uncertainty notes for ambiguous cases.
- Include at least one warning about the risk of over-reading isolated chat messages.

## 6) Evidence Highlights
- Include 8 to 20 short quoted chat examples.
- Each quote must include enough metadata to find it again: source file, row number if available, video_id if available, timestamp/seconds if available.
- Quotes should be short and should not include private personal data beyond what already appears in the source text.

Required columns for `chat_personality_evidence_log.csv`:
- talent
- source_file
- video_id
- row_number
- timestamp_or_sec
- chat_identifier
- text
- pattern_name
- interactional_function
- streamer_context_summary
- evidence_strength
- notes

Required keys for `chat_personality_state.json`:
- `talent`
- `analysis_conducted`
- `source_root`
- `output_root`
- `files_inspected`
- `chat_rows_considered`
- `context_rows_used`
- `patterns`
- `limitations`

Completion:
- Report output paths written.
- Report any talents skipped and why.
- Keep the final response concise.
