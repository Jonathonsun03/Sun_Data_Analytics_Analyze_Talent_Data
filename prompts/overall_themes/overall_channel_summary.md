You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/overall_themes/overall_channel_summary.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/overall_themes/overall_channel_summary/`.

Optional talent scope:
- The shell runner accepts `--talent "<exact talent folder name>"`.
- When `TALENT_SLUG` is provided by the runner, process only that exact talent folder and replace any talent placeholder with that folder name.
- When no `TALENT_SLUG` is provided, process every eligible talent.

Objective:
Build an incremental per-talent overall channel summary from stream-summary markdown files. This workflow should only incorporate newly available stream summaries that have not yet been rolled into the current cumulative overall-channel output.

Scope rules:
- Process every talent folder directly under the data root that contains `stream_summaries/`.
- Skip the aggregate folder `VarianceProject`.
- Do not process only one talent unless explicitly instructed.

Input paths per talent:
1) `<talent>/stream_summaries/stream_summary_codex/*.md` (primary summary inputs)
2) `<talent>/text_playback/*.csv` (raw evidence verification)
3) `<talent>/Chat/Original/*_chat.csv` (raw evidence verification)
4) `<talent>/stream_summaries/overall_channel_summary/current/overall_channel_summary_state.json` if present

Input usage rules:
- Use `stream_summary_codex/*.md` as the primary analytic input for this workflow.
- Use raw logs only to verify quoted examples and prevent unsupported claims.
- Do not quote generated markdown outputs as evidence unless the quote is independently verified in a raw CSV.
- Do not hallucinate quotes, timestamps, video IDs, or summary provenance.
- If a talent is missing one raw input class, continue with available inputs and note the limitation.
- Do not modify raw inputs.

Write outputs to these exact paths:
1) `<talent>/stream_summaries/overall_channel_summary/current/overall_channel_summary.md`
2) `<talent>/stream_summaries/overall_channel_summary/current/overall_channel_summary_state.json`
3) `<talent>/stream_summaries/overall_channel_summary/snapshots/overall_channel_summary_YYYY-MM-DD_HH-MM-SS_±HHMM.md`

Folder organization rules:
- Keep overall channel summary outputs under `stream_summaries/overall_channel_summary/`.
- Keep the live cumulative markdown and state file only in `overall_channel_summary/current/`.
- Keep dated historical markdown snapshots only in `overall_channel_summary/snapshots/`.
- Do not drop new overall-channel markdown files loose in `overall_themes/`.
- Do not create or update `money_timestamps.csv` in this workflow. That is a separate task.

Incremental eligibility rules:
- Before processing a talent, check whether `overall_channel_summary_state.json` exists.
- If the state file exists, use it to determine which summary `video_id`s have already been incorporated.
- Eligible summaries for this run are only those whose `video_id`s are not yet recorded as processed in the state file.
- If no state file exists, treat the talent as an initial bootstrap run and build the cumulative output from all available summary markdown files.
- If the state file exists but the current markdown output is missing or empty, treat the talent as incomplete and rebuild using already-processed summary `video_id`s plus the newly eligible ones.
- Never delete prior snapshots just to rerun the workflow.

Summary provenance rules:
- Each run must record what source summary version was used.
- Determine this from explicit metadata if present in the source summary markdowns or from workflow-provided run context if available.
- If all newly incorporated summaries share the same explicit version label, record that label.
- If newly incorporated summaries have conflicting explicit version labels, record `mixed`.
- If the source summaries do not expose reliable version metadata, record `unknown`.
- Do not guess a summary version from prose style alone.

Required contents for `overall_channel_summary.md`:

Start the document with:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Update scope: <initial bootstrap|incremental update>`
- `New summary video_ids incorporated this run: <count>`
- `Cumulative summary video_ids incorporated: <count>`
- `Source summary prompt version: <value|mixed|unknown>`
- `Source summary prompt path/label: <value|mixed|unknown>`

## 1) Streamer and Chat Relationship Overview
- 1-3 paragraphs summarizing the streamer’s general text-visible style, audience relationship, and recurring interaction stance as reflected in the cumulative incorporated summaries.
- Make this descriptive, not diagnostic.
- If evidence is mixed or weak, say so directly.
- Include one short note on whether the newest summaries materially changed the cumulative picture or mostly reinforced prior themes.

## 2) Incremental Summary Classification Notes
- Briefly document:
  - new summaries scanned this run
  - cumulative summaries scanned
  - recurring themes retained
  - newly added or newly strengthened themes this run
  - any missing or pending raw-verification limitations

## 3) Qualitative Codebook
- Include a codebook of recurring interaction patterns observed across the cumulative incorporated summaries.
- For each retained code include:
  - code_name
  - definition
  - inclusion_criteria
  - exclusion_criteria
  - observed_summary_count
  - 2-3 verified examples with `[timecode] speaker (video_id): "quote"` where available
- If a code was newly retained in the current run, label it as `new_this_run: yes`.

## 4) Recurring Interaction Patterns
- Summarize the most stable patterns in:
  - humor/play style
  - response to chat
  - pacing/control
  - care, reassurance, or boundary behavior
- Use evidence-backed prose, not just bullet fragments.

## 5) Coverage and Gaps
- Note any content areas, stream types, or evidence situations that appear underrepresented in the incorporated summary set.
- Distinguish between:
  - no summary yet available
  - summary available but raw verification weak
  - summary available and incorporated

## 6) Evidence Limits and Uncertainty
- Include at least 3 limitations or uncertainty notes.
- Explicitly note that this is a summary-driven workflow with raw-log verification, not a raw-log-first full recoding workflow.
- Explicitly note that this is text-only evidence and does not include visual cues or full vocal prosody.

Requirements for `overall_channel_summary_state.json`:
- `talent`
- `analysis_conducted_at`
- `update_scope`
- `processed_summary_video_ids`
- `new_summary_video_ids_this_run`
- `cumulative_summaries_scanned`
- `source_summary_prompt_version`
- `source_summary_prompt_path_or_label`
- `latest_snapshot_path`
- `notes`

Snapshot rules:
- On each run that writes or refreshes the current markdown, also write a dated snapshot markdown in `overall_channel_summary/snapshots/`.
- The snapshot markdown must include:
  - the snapshot date/time
  - the current cumulative `overall_channel_summary.md` content for that run
  - the full text of this overall-channel-summary prompt
  - the recorded source summary version/path information for the incorporated summaries

Evidence constraints:
- Every quoted example in `overall_channel_summary.md` must be traceable to a raw CSV row when a quote is presented as direct evidence.
- If a raw quote cannot be verified, do not include it as a direct quote.
- Summaries may drive interpretation, but direct examples must not be fabricated.

Validation and reporting:
- After writing files, print a per-talent completion summary including:
  - talent
  - new summary count
  - cumulative summary count
  - source summary prompt version
  - current output paths
  - snapshot path
- Spot-check at least one quoted raw example per talent by searching the source CSVs and reporting the matching file path and line.

Execution rule:
- Execute end-to-end without asking for confirmation unless blocked by permissions.
