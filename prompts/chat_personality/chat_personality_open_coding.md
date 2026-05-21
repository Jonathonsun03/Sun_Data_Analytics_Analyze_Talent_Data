You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/chat_personality/chat_personality_open_coding.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/chat_personality/chat_personality_open_coding/`.

Execution guard:
- If this prompt was launched by `bin/linux/codex_prompts/chat_personality/chat_personality_open_coding.sh`, do not invoke that shell script again.
- Do not run `codex exec` from inside this workflow.
- Perform file inspection and output writing directly in the current Codex run.
- If scripting is useful for CSV inspection or output writing, use `python3`, not `python`.

Objective:
Build talent-local open coding for chat personality. The analytic object is chat-visible audience/community behavior around a talent, not the streamer's personality.

Talent discovery:
- Discover direct child directories of `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`.
- Exclude non-talent aggregate directories such as `VarianceProject`.
- A talent is eligible when `<talent>/text_playback/` contains one or more CSV files.
- If `TALENT_SLUG` is provided by the runner, process only that exact talent folder.

Source handling:
- Read `<talent>/text_playback/*.csv`.
- Prefer rows whose metadata identifies them as chat, audience, viewer, comment, or message rows.
- Use streamer/subtitle rows only as local context for what chat appears to be responding to.
- Never modify source CSVs.

Date-window scope:
- If the runner provides `WINDOW_START_DATE` or `WINDOW_END_DATE`, focus new coding on rows, files, stream dates, upload dates, or source metadata inside that window.
- If a source item has no resolvable date during a window-limited run, skip it for new coding and mention the skip count if available.

Output root:
- `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/`

Write these exact output paths:
1) `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_codebook.csv`
2) `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_coding_evidence.csv`
3) `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_coding_memo.md`
4) `<talent>/qualitative coding/chat data/chat_personality_open_coding/current/chat_open_coding_state.json`

Optional snapshots:
- On material changes, write a dated markdown snapshot under:
  - `<talent>/qualitative coding/chat data/chat_personality_open_coding/snapshots/`

Coding standard:
- Generate emergent codes from repeated chat behavior; do not impose the streamer-personality codebook.
- Separate chat behavior from streamer behavior.
- A code should describe observable interactional behavior and its apparent function.
- Do not diagnose chatters or infer off-platform identity traits.
- Do not promote isolated jokes, one-off spam, or repeated words into a code unless their social function recurs.

Required columns for `chat_open_codebook.csv`:
- code_id
- code_name
- definition
- inclusion_criteria
- exclusion_criteria
- interactional_function
- example_quote
- source_count
- row_count
- confidence
- notes

Required columns for `chat_open_coding_evidence.csv`:
- talent
- source_file
- video_id
- row_number
- timestamp_or_sec
- chat_identifier
- text
- streamer_context_summary
- code_id
- code_name
- evidence_strength
- notes

Required structure for `chat_open_coding_memo.md`:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Talent: <talent name>`
- `Source files inspected: <count>`
- `Chat rows considered: <count>`
- `Context rows used: <count>`
- `Date window: <unbounded|start to end>`

## 1) Open-Coding Summary
Summarize the strongest recurring chat-community patterns.

## 2) Retained Chat Codes
Describe each retained code and why it meets the recurrence threshold.

## 3) Rejected Or Merged Candidate Codes
Briefly list candidates that were too rare, too generic, or merged into broader codes.

## 4) Limitations
State ambiguity, date-window limits, missing source limits, and over-reading risks.

Required keys for `chat_open_coding_state.json`:
- `talent`
- `analysis_conducted`
- `source_root`
- `output_root`
- `date_window`
- `files_inspected`
- `chat_rows_considered`
- `context_rows_used`
- `codes_retained`
- `limitations`

Completion:
- Report output paths written and talents skipped.

