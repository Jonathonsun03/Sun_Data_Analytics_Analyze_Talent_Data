You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/monetary/money_timestamps_incremental.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/monetary/money_timestamps_incremental/`.

Optional talent scope:
- The shell runner accepts `--talent "<exact talent folder name>"`.
- When `TALENT_SLUG` is provided by the runner, process only that exact talent folder and replace any talent placeholder with that folder name.
- When no `TALENT_SLUG` is provided, process every eligible talent.

Objective:
Maintain an incremental per-talent paid-event log from raw text logs. This workflow is responsible only for detecting, validating, and updating money-event timestamps.

Scope rules:
- Process every talent folder directly under the data root that contains `stream_summaries/`.
- Skip the aggregate folder `VarianceProject`.
- Do not process only one talent unless explicitly instructed.

Input paths per talent:
1) `<talent>/text_playback/*.csv` (primary input)
2) `<talent>/Chat/Original/*_chat.csv` (primary input)
3) `<talent>/stream_summaries/overall_themes/money_timestamps/current/money_timestamps_state.json` if present

Write outputs to these exact paths:
1) `<talent>/stream_summaries/overall_themes/money_timestamps.csv`
2) `<talent>/stream_summaries/overall_themes/money_timestamps/current/money_timestamps_state.json`
3) `<talent>/stream_summaries/overall_themes/money_timestamps/snapshots/money_timestamps_YYYY-MM-DD_HH-MM-SS_±HHMM.csv`

Folder organization rules:
- Keep the shared downstream CSV at the root path `overall_themes/money_timestamps.csv`.
- Keep money-timestamp workflow state under `overall_themes/money_timestamps/current/`.
- Keep dated CSV snapshots under `overall_themes/money_timestamps/snapshots/`.
- Do not drop extra money-timestamp artifacts loose in `overall_themes/`.

Incremental eligibility rules:
- Before processing a talent, check whether `money_timestamps_state.json` exists.
- If the state file exists, use it to determine which `video_id`s have already been scanned for paid events.
- Eligible files for this run are only those whose `video_id`s are not yet recorded as processed in the money state file.
- If no state file exists, treat the talent as an initial bootstrap run and scan all eligible raw files.
- If the state file exists but `money_timestamps.csv` is missing or empty, rebuild it using already-processed `video_id`s plus the newly eligible ones.

Money-event detection rules:
- Detect paid events from raw logs only.
- Prefer explicit paid fields when available.
- If `message_type` signals a paid event, include it even when amount fields are partially missing.
- Do not invent amounts or currencies.
- If `video_title` is not directly available in the raw row, derive it from the best available file-level metadata and be consistent within the run.
- If no money events exist for a talent, still write the CSV with header only.

Required columns for `money_timestamps.csv`:
- `video_id`
- `video_title`
- `time_in_seconds`
- `timecode`
- `username`
- `message_type`
- `paid_amount`
- `paid_currency`
- `message`

Requirements for `money_timestamps_state.json`:
- `talent`
- `analysis_conducted_at`
- `update_scope`
- `processed_video_ids`
- `new_video_ids_this_run`
- `money_events_detected_cumulative`
- `money_events_detected_this_run`
- `latest_snapshot_path`
- `notes`

Snapshot rules:
- On each run that writes or refreshes `money_timestamps.csv`, also write a dated CSV snapshot in `money_timestamps/snapshots/`.
- Never delete prior snapshots unless explicitly instructed.

Validation and reporting:
- After writing files, print a per-talent completion summary including:
  - talent
  - new videos scanned
  - cumulative videos scanned
  - money events detected this run
  - money events detected cumulative
  - current CSV path
  - snapshot path
- Spot-check at least one paid event row per talent when money events exist and report the matching source CSV file path and line.

Execution rule:
- Execute end-to-end without asking for confirmation unless blocked by permissions.
