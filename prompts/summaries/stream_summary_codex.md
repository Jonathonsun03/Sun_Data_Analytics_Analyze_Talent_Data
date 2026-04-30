You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/summaries/stream_summary_codex.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/summaries/stream_summary_codex/`.

Execution guard:
- If you are reading this prompt inside a `codex exec` session that was launched by `bin/linux/codex_prompts/summaries/stream_summary_codex.sh`, do not invoke `bin/linux/codex_prompts/summaries/stream_summary_codex.sh` again.
- Do not run `codex exec` from inside this workflow.
- Perform the file inspection and summary-writing work directly in the current Codex run.
- The shell entry point is only for the outer human/operator command.
- If scripting is useful for CSV inspection or output writing, use `python3`, not `python`.

Repository workflow rules:
- This is the maintained Codex prompt for creating per-stream summary markdown files from text replay CSVs.
- Keep this prompt under `prompts/summaries/`.
- Do not create or modify raw input files.
- Do not create new scripts unless explicitly asked. This workflow can be executed directly by Codex from this prompt.

Objective:
Create one text-grounded stream summary markdown file for each eligible text replay CSV that does not already have a matching summary.

Scope:
- Process every direct talent folder under `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data` that contains `text_playback/*.csv`.
- Skip aggregate or non-talent folders such as `VarianceProject`.
- Do not process only one talent unless the user explicitly narrows the scope.
- Treat completion as file-based: if a matching output summary already exists, skip it unless the user explicitly asks to rerun or refresh summaries.

Primary input:
- `<talent>/text_playback/*.csv`

Optional supporting inputs:
- `<talent>/Chat/Original/*_chat.csv`, only when needed to verify a quote or resolve a visible ambiguity.
- `<talent>/Subtitles/Processed/*_subtitles.csv`, only when needed to verify subtitle context.

Output folder:
- `<talent>/stream_summaries/stream_summary_codex/`

Output filename:
- For each input `<talent>/text_playback/<stream_stem>.csv`, write:
  - `<talent>/stream_summaries/stream_summary_codex/<stream_stem>_summary.md`

Skip rule:
- Before processing an input CSV, check whether the exact output path already exists and is non-empty.
- If it exists and is non-empty, skip that stream and include it in the completion summary as skipped.
- If it is missing or empty, create or repair it.

Input expectations:
- Text replay CSVs normally include rows from chat and subtitles.
- Use available columns such as `timecode`, `time_in_seconds`, `source`, `speaker`, `text`, `message_type`, `paid_amount_text`, `paid_amount_value`, and `paid_currency` when present.
- If column names differ, inspect the header and adapt conservatively.
- If a CSV cannot be read or lacks usable text, write no summary for that stream and report the problem in the final completion summary.

Evidence rules:
- Use only text visible in the input CSV or verified supporting raw CSVs.
- Do not invent events, quotes, timestamps, paid messages, content context, motivations, or off-platform history.
- Do not infer vocal tone, facial expression, camera behavior, or body language.
- If evidence is thin, repetitive, or ambiguous, say so directly.
- Quotes should be short and traceable to a raw row.
- Prefer paraphrase for broad patterns; reserve direct quotes for compact examples that matter.

Summary metadata:
Start every output markdown with:

```
Source summary prompt version: stream_summary_codex_v1
Source summary prompt path/label: prompts/summaries/stream_summary_codex.md
Source text replay file: <absolute input CSV path>
Analysis conducted: YYYY-MM-DD HH:MM TZ
```

Required output structure:

```
# Stream Summary: <readable stream title or filename stem>

Source summary prompt version: stream_summary_codex_v1
Source summary prompt path/label: prompts/summaries/stream_summary_codex.md
Source text replay file: <absolute input CSV path>
Analysis conducted: YYYY-MM-DD HH:MM TZ

## 1) Interaction Overview
1-2 concise paragraphs describing the stream's visible interaction pattern: who is speaking, how chat and streamer relate, and whether the exchange appears chat-led, streamer-led, collaborative, quiet, chaotic, ritualized, or otherwise distinctive.

## 2) Paid Message Inventory
- State whether paid messages are visible.
- If paid messages are visible, summarize count, currency/amount visibility, and the main kinds of paid-message content.
- If none are visible, say that no paid messages are visible in this transcript.

## 3) Conditions Surrounding Payment
If paid messages are visible, describe the surrounding stream conditions using raw evidence:
- What was happening just before or during the paid message.
- Whether the streamer acknowledged it.
- Whether chat momentum or streamer behavior visibly shifted.

If paid messages are not visible, state that payment-linked conditions cannot be assessed for this stream.

## 4) Functional Roles of Money
Use inductive categories only when supported by evidence, such as:
- appreciation/support
- request or prompt
- joke/escalation
- milestone/celebration
- comfort/reassurance
- community ritual
- unclear

If there is no paid-message evidence, state that no money role is inferable from this transcript.

## 5) Paid vs Non-Paid Interaction Differences
When paid messages exist, compare paid and non-paid interactions in a short table with these dimensions:
- content
- tone
- length/detail
- streamer response
- chat response

When paid messages do not exist, use the same table and mark paid interaction as not observable.

## 6) Emerging Relationship Patterns
Summarize the most important recurring relationship pattern visible in the transcript, such as hosting style, reciprocity, audience steering, care/reassurance, teasing, roleplay, conflict management, or routine community maintenance.

## 7) Evidence Limits and Ambiguities
List concrete limitations, missing context, or ambiguous points. Include missing raw classes, sparse chat, subtitle-only stretches, absent paid messages, or unreadable rows when relevant.
```

Quality bar:
- Keep each summary useful for later cross-stream synthesis.
- Be specific enough that a later workflow can compare streams, but do not overfit to one-off details.
- Make outputs readable, stable, and consistently structured.
- Do not use generated prior summaries as evidence for the new summary.

Final response requirements:
- Print a concise per-talent completion summary.
- Include counts for created, skipped, failed, and total eligible text replay CSVs.
- List output folders touched.
- If any failures occurred, include the exact input path and reason.
