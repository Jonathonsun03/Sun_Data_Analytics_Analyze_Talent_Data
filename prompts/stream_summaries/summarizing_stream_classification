You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Task:
For every talent folder under this root that contains a `stream_summaries` directory, generate qualitative analysis outputs in:

<talent>/stream_summaries/overall_themes

Do this for all talents, not just one.

Requirements per talent:
1) Read all files in:
   - `<talent>/stream_summaries/stream_summary_codex` (for corpus context)
   - `<talent>/text_playback/*.csv` and `<talent>/Chat/Original/*.csv` (raw evidence source)

2) Create:
   - `overall_themes_codex.md`
   - `money_timestamps.csv`

3) `overall_themes_codex.md` must include:
   - Overall description of the streamer and their relationship to chat.
   - A qualitative codebook with:
     - code name
     - definition
     - inclusion criteria
     - exclusion criteria
     - observed count
     - 2–3 raw-text examples with timestamps and video_id
   - A section listing key money-giving timestamps (with total count), and explicitly point to the full CSV.

4) `money_timestamps.csv` must include one row per paid event with columns:
   - video_id
   - video_title
   - time_in_seconds
   - timecode
   - username
   - message_type
   - paid_amount
   - paid_currency
   - message

5) Evidence constraints (strict):
   - Code examples and money timestamps must come from RAW logs (`text_playback` / `Chat/Original`), not summaries.
   - Do not hallucinate examples.
   - If no money events exist for a talent, write that explicitly and still create CSV with header only.

6) Validation:
   - After writing files, print a per-talent completion summary including output paths and money-event counts.
   - Spot-check at least one quoted raw example per talent by searching the source CSVs and report the match line.

Execute end-to-end without asking for confirmation unless blocked by permissions.
