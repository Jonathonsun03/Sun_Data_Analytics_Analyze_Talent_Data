You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/personality/personality_open_coding.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/personality/personality_open_coding/`.

Optional talent scope:
- The shell runner accepts `--talent "<exact talent folder name>"`.
- When `TALENT_SLUG` is provided by the runner, process only that exact talent folder and replace any talent placeholder with that folder name.
- When no `TALENT_SLUG` is provided, process every eligible talent.

Talent discovery rules:
- Process every direct talent folder under `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data` that has usable stream-summary inputs.
- Skip aggregate or demo folders such as `VarianceProject` and `Northstar Story Lab Demo`.
- Do not use a hardcoded talent list.
- Do not process only one talent unless explicitly instructed.

Objective:
Build an incremental text-grounded personality analysis using OPEN CODING (emergent codes), not a predefined code list. This is a weekly update workflow: analyze only newly eligible videos not yet incorporated into the v3 personality profile, then merge those findings into a living cumulative v3 output set.

Core analytic standard:
- The open-coding workflow must foreground what makes each streamer non-interchangeable, not just what makes them readable as a streamer.
- If a code, theme family, or personality summary could be copied into another talent's profile with only the name changed, it is too generic and must be rewritten, merged upward, or demoted.
- The codebook should not be a bag of broad streamer-common traits; it should capture this streamer's specific interaction patterns.

Important limitations:
- You do NOT have visual cues (facial expression, camera behavior, body language).
- You do NOT have full vocal prosody from audio.
- Infer only from text evidence (chat logs, streamer chat posts, subtitles, summaries).
- Do not over-claim. If evidence is weak, say "insufficient evidence".
- Treat this as a cumulative profile with incremental updates, not a full historical rerun each week.
- Do not let generic traits such as `playful`, `chatty`, `audience-aware`, `appreciative`, or `energetic` dominate the retained code structure unless they are rewritten into this streamer's distinctive version of those behaviors.

Scope:
Process all eligible talent folders discovered under the talent data root.

For each eligible talent path, use these exact input paths:
1) `<talent>/stream_summaries/stream_summary_codex/*.md` (context only)
2) `<talent>/text_playback/*.csv` (primary evidence)
3) `<talent>/Chat/Original/*_chat.csv` (primary evidence)
4) `<talent>/stream_summaries/overall_themes/money_timestamps.csv` if present
5) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/personality_profile_v3_state.json` if present (state/eligibility only, never evidence)

Input filtering rules:
- Use only these files as analytic inputs:
  - `<talent>/stream_summaries/stream_summary_codex/*.md`
  - `<talent>/text_playback/*.csv`
  - `<talent>/Chat/Original/*_chat.csv`
  - `<talent>/stream_summaries/overall_themes/money_timestamps.csv` if present
- Do not use previously generated personality profile files, prior codebooks, prior evidence CSVs, or any existing v1/v2/v3 outputs as evidence inputs.
- Treat existing generated outputs only as completion markers, prior-summary context, or merge targets, not as source material for analytic claims.

Write outputs to these exact output paths:
1) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/personality_profile_v3_open_coding.md`
2) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_codebook_v3.csv`
3) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_coding_evidence_v3.csv`
4) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/personality_profile_v3_state.json`
5) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/snapshots/personality_profile_v3_open_coding_YYYY-MM-DD_HH-MM-SS_±HHMM.md`

Folder organization rules:
- Keep baseline personality outputs under `<talent>/stream_summaries/overall_themes/personality/`.
- Keep open-coding outputs under `<talent>/stream_summaries/overall_themes/personality_open_coding/`.
- Keep the active v3 files only in `personality_open_coding/v3/current/`.
- Keep dated markdown history files only in `personality_open_coding/v3/snapshots/`.
- Do not drop new personality or open-coding outputs loose in `overall_themes/`.

Incremental eligibility rules:
- Before processing a talent, check whether `personality_profile_v3_state.json` exists.
- If the state file exists, use it to determine which `video_id`s have already been incorporated into v3.
- Eligible files for this run are only those whose `video_id`s are not yet recorded as processed in the v3 state file.
- If no state file exists, treat the talent as an initial v3 bootstrap run and build v3 from all eligible source files.
- If the state file exists but one or more v3 output files are missing or empty, treat the talent as incomplete and regenerate the missing v3 outputs using the already-processed `video_id`s plus the newly eligible ones.
- Never delete prior outputs just to rerun the workflow.
- Do not modify any raw input files.
- On each run that writes v3 outputs, also write a dated snapshot markdown in `personality_open_coding/v3/snapshots/` that includes:
  - the run date/time
  - the current cumulative v3 profile content for that run
  - the full prompt text used for that run

Newest-video window rules:
- Prefer a file/video-level incremental workflow, not a full archive rescan for interpretation.
- Determine recency using available metadata in the source files or stable file discovery order.
- If there is ambiguity about which files are new, rely on `video_id` membership in the v3 state file, not file modification time alone.
- If a new stream has a summary file but is missing one of the primary evidence files (`text_playback` or chat), include it only if the available evidence is sufficient for quoted claims; otherwise mark it as pending/incomplete in the state file notes and skip analytic claims for that video.

Open-coding method (required):

Phase 1: Incremental stream-level open coding
- Read each newly eligible stream as an independent interaction case.
- Generate in-vivo or descriptive codes directly from text (short labels, 1-4 words).
- Do not force data into preset categories.
- Code interactional units (single line or short exchange windows) with timestamped evidence.
- Prefer codes that preserve specific interaction form, social stance, or recurring ritual over flat sentiment labels.
- Avoid retaining generic lexical buckets as final codes unless they are clearly reframed into a more interpretable interaction pattern.

Phase 2: Constant comparison against the cumulative v3 profile
- Compare new codes within the weekly batch and against the previously retained v3 code structure.
- Merge semantically equivalent codes.
- Split overly broad codes into specific ones when needed.
- Keep memos on why merges/splits were made.
- Distinguish between:
  - genuinely new emergent codes
  - stronger evidence for already known codes
  - weakening or absent support for previously prominent themes
- During merge/split decisions, prefer talent-specific interpretive labels over generic shared-trait labels.
- If multiple surface codes mostly point to the same deeper behavior, collapse them into a richer behavior-level code instead of keeping near-duplicate lexical buckets.
- If a retained code is common across peers, annotate in memo logic what is distinctive about this streamer's version or demote it from headline importance.

Phase 3: Higher-order clustering
- Group emergent codes into higher-order themes (families) after coding is complete.
- Theme names must come from observed patterns, not imported frameworks.
- Update theme-family descriptions when the new weekly evidence materially changes the cumulative interpretation.
- Theme families should describe distinctive interactional patterns, not generic creator traits.
- At least 2 of the major cumulative theme families should be distinctiveness-forward families that would not fit peer talents without major rewriting.

Phase 4: Cross-talent contrast
- Compare emergent theme families across talents.
- Identify what is distinctive vs shared.
- When possible, note whether the newest batch reinforced an existing contrast or introduced a new distinction.

Phase 4.5: Uniqueness filter before final retention
- Before finalizing codes and theme families, separate:
  - shared baseline streamer traits
  - this streamer's distinctive version of a shared trait
  - genuinely unique or unusually emphasized patterns
- If a candidate code or family is mostly shared across peers, either:
  - rewrite it into a more specific talent-shaped code or family, or
  - demote it from headline prominence
- Ask of every major retained code/family: `would this still feel specifically true if I swapped in another talent's name?`
- If yes, refine it until the answer is no or reclassify it as background context.

Evidence and reliability rules:
- Every analytic claim must map to raw evidence rows in CSV logs.
- Summaries can guide sampling but cannot be the sole evidence for claims.
- No hallucinated quotes, timestamps, donors, or interpretations.
- For each retained code, include:
  - clear operational definition
  - inclusion criteria
  - exclusion criteria
  - cumulative frequency count
  - cumulative stream coverage count
  - at least 3 examples where possible (if fewer exist, state exact count)
- Keep low-frequency but conceptually important codes if justified in memo.
- Include at least 2 negative cases (counterexamples) per major theme family where cumulative evidence allows.
- Clearly distinguish weekly-batch evidence from cumulative totals in process notes or state metadata.
- Do not retain a code as major only because it is frequent. Frequency without distinctiveness is not enough.
- Prefer behavior-level codes over filler-response codes unless the filler itself serves a distinctive recurring social function.

Required structure for `personality_profile_v3_open_coding.md`:

Start the document with:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Incremental update scope: <initial bootstrap|weekly update>`
- `New video_ids incorporated this run: <count>`

## 1) Personality Signature (Emergent, Cumulative)
- 1-2 paragraphs describing the streamer’s personality style as it currently emerges from the cumulative v3 open coding.
- Confidence level: High / Medium / Low.
- Explicit limitations from text-only modality.
- Include 1 short note on whether this run materially changed the personality signature or mainly reinforced prior themes.
- The opening must answer what makes this streamer unique compared with the peers in this dataset.
- Do not open with broad shared traits unless they are immediately specified in this streamer's distinctive form.

## 2) Incremental Open Coding Process Notes
- Briefly document how codes were generated, merged, split, and elevated into themes for the new batch.
- Report:
  - new streams scanned this run
  - cumulative streams scanned
  - raw codes generated this run
  - final retained cumulative codes
  - newly added codes this run
  - merged/retired codes this run if any
- Include a short note on which generic candidate codes were rewritten, merged, or demoted for lack of distinctiveness if applicable.

## 3) Emergent Codebook (Talent-Specific, Cumulative)
- Present the final retained open codes for this talent.
- For each code include:
  - code_label
  - operational_definition
  - inclusion_criteria
  - exclusion_criteria
  - cumulative frequency_count
  - cumulative stream_coverage_count
  - 3 evidence quotes with `[timecode] speaker (video_id): "quote"`
- If a code was introduced in the current run, label it as `new_this_run: yes`.
- The retained codebook should emphasize behaviorally meaningful, talent-shaped codes over generic streamer-common labels.
- If a code represents this streamer's distinctive version of a shared trait, make that specificity visible in the code label or operational definition.

## 4) Emergent Theme Families
- Group codes into higher-order theme families.
- For each family:
  - family summary
  - member codes
  - strongest supporting evidence
  - 2 counterexamples/limits
  - note whether this family was reinforced, expanded, narrowed, or unchanged by the current run
- At least 2 major theme families should be clearly distinctiveness-forward rather than merely broad shared categories.

## 5) Idiosyncratic Charm Markers
- Identify recurring, non-generic markers unique to this streamer’s text presence.
- Include catchphrases, interaction rituals, humor signatures, response stance, and social positioning.
- Provide evidence from multiple streams when possible.
- Note whether any marker is newly emergent in the current weekly batch.

## 6) Money-Linked Personality Patterns (Supplement, not primary driver)
- Use money events as contextual probes, not as the coding backbone.
- For this run, sample at least 20 paid events from newly eligible videos where available (or all events if <20).
- Report how monetary moments amplify, dampen, or reveal emergent personality themes.
- Distinguish between:
  - current-run money observations
  - cumulative money-linked patterns

## 7) Cross-Talent Distinctiveness
- Contrast this talent against at least 2 other talents using emergent themes.
- Answer: what makes this streamer non-interchangeable?
- Do not merely list similarities and differences. Explain what combination of codes and theme families makes this streamer specifically this streamer.
- Include at least 2 contrasts about how the streamer performs a shared behavior differently from peers.
- Include one "can resemble X, but differs by Y" statement.
- Note whether the newest batch sharpened or softened any prior contrast.

## 8) Validity and Uncertainty
- 3 strongest evidence-backed conclusions.
- 3 uncertainty points.
- 1 section on possible coder bias and how evidence reduced it.
- Include 1 brief note on incremental-risk bias:
  - risk of over-weighting the newest batch
  - how cumulative evidence was used to prevent overreaction
- Include 1 brief note about the risk of over-reading generic streamer traits as distinctive personality.

Required structure for `open_codebook_v3.csv`:
Columns:
- talent
- code_label
- theme_family
- operational_definition
- inclusion_criteria
- exclusion_criteria
- frequency_count
- stream_coverage_count
- new_this_run
- memo_notes

Required structure for `open_coding_evidence_v3.csv`:
Columns:
- talent
- code_label
- theme_family
- video_id
- time_in_seconds
- timecode
- source (chat|subtitle|summary)
- speaker
- quote
- evidence_role (primary|supporting|counterexample)
- monetary_context (none|pre_event|event|post_event)
- incorporated_in_run

Required structure for `personality_profile_v3_state.json`:
- `talent`
- `analysis_conducted_at`
- `update_scope`
- `processed_video_ids`
- `new_video_ids_this_run`
- `pending_video_ids`
- `cumulative_streams_scanned`
- `raw_codes_generated_this_run`
- `final_codes_retained_cumulative`
- `theme_families_cumulative`
- `evidence_rows_cumulative`
- `money_events_sampled_this_run`
- `latest_snapshot_path`
- `notes`

Execution rules:
- Evaluate all eligible discovered talents for v3 eligibility.
- Identify newly eligible videos using `video_id` tracking from the v3 state file.
- Process only talents with at least one new eligible `video_id`, unless a talent has missing/empty v3 outputs that require repair.
- Create missing output folders if needed.
- When updating v3, overwrite only the v3 output files for the target talent.
- Preserve cumulative continuity by merging newly coded evidence into the existing v3 outputs.

Final report to user:
- Before processing, report which of the 4 talents are:
  - v3 not started
  - v3 up to date
  - v3 has new eligible videos
  - v3 incomplete/repair needed
- Per talent:
  - new_streams_scanned
  - cumulative_streams_scanned
  - raw_codes_generated_this_run
  - final_codes_retained_cumulative
  - new_codes_added_this_run
  - theme_families_cumulative
  - evidence_rows_logged_cumulative
  - money_events_sampled_this_run
  - output file paths
  - state file path
- Spot-check at least 2 quoted lines per talent updated in this run by showing exact source-file matches.
