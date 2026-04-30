You are working across these data roots:

Primary talent data root:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`

Cross-talent processed output root:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions`

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/shared_qualities/shared_behavior_baseline.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/shared_qualities/shared_behavior_baseline/`.

Optional talent scope:
- The shell runner accepts `--talent "<exact talent folder name>"`.
- When `TALENT_SLUG` is provided by the runner, focus the baseline refresh on that exact talent while using other eligible talents as comparison context only where needed.
- When no `TALENT_SLUG` is provided, process every eligible talent for the cross-streamer baseline.

Objective:
Build a cross-streamer shared-behavior baseline that identifies what is common across the streamers in this dataset before later workflows attempt to identify what makes each streamer unique.

This workflow is not a talent profile. It is a cross-case synthesis workflow.

Its job is to answer:
1) What interaction patterns are shared across the streamers?
2) Which of those shared patterns are truly baseline/common?
3) Which patterns are shared but expressed in meaningfully different ways by different streamers?
4) Which patterns should be treated as non-baseline and left for talent-specific uniqueness profiling?

Why this workflow exists:
- Per-talent open coding is necessary but not sufficient for uniqueness claims.
- Without an explicit shared baseline, later personality profiles tend to overstate generic streamer traits as if they were unique.
- This workflow should create the comparative baseline that later personality-profile workflows can use to decide whether a trait is shared, distinctive-in-form, or truly unique.

Core analytic standard:
- The baseline must be text-grounded and evidence-backed.
- Shared themes must be derived from the existing per-talent evidence, not invented abstractly.
- Do not collapse the dataset into a meaningless `average streamer`.
- The goal is not flattening; the goal is structured comparison.
- The workflow must distinguish:
  - shared baseline behaviors
  - shared behaviors with streamer-specific versions
  - talent-specific or weakly shared behaviors

Important limitations:
- You do NOT have visual cues.
- You do NOT have full vocal prosody from audio.
- Infer only from text-visible evidence already retained in upstream workflows.
- Do not over-claim. If evidence is weak, say `insufficient evidence`.

Scope rules:
- Process every direct talent folder under `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data` except aggregate folders such as `VarianceProject`.
- Only include a talent if it has usable upstream personality-open-coding outputs.
- This is a cross-talent workflow. Do not process only one talent unless explicitly instructed.

Upstream dependency rules:
- This workflow depends on per-talent open-coding outputs already having been produced.
- Treat `personality_open_coding` as the primary input layer.
- Treat `overall_channel_summary/current` as a secondary stabilizing layer when needed.
- Do not go back to raw text logs for this workflow unless explicitly instructed to audit or verify.

Inputs per talent:

Primary input layer: `personality_open_coding`
- Prefer the newest available open-coding set in this order:
  1) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_codebook_v3.csv`
  2) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_coding_evidence_v3.csv`
  3) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/personality_profile_v3_open_coding.md`
- If v3 is unavailable, fall back to:
  1) `<talent>/stream_summaries/overall_themes/personality_open_coding/v2/open_codebook_v2.csv`
  2) `<talent>/stream_summaries/overall_themes/personality_open_coding/v2/open_coding_evidence_v2.csv`
  3) `<talent>/stream_summaries/overall_themes/personality_profile_v2_open_coding.md`

Secondary input layer: `overall_channel_summary`
- `<talent>/stream_summaries/overall_channel_summary/current/overall_channel_summary.md`
- `<talent>/stream_summaries/overall_channel_summary/current/overall_channel_summary_state.json` if present

Input precedence rules:
- `open_codebook_*` and `open_coding_evidence_*` are the primary basis for identifying shared patterns.
- `personality_profile_v*_open_coding.md` may be used to recover memo logic or family summaries but must not substitute for evidence rows.
- `overall_channel_summary/current/overall_channel_summary.md` can support judgments about stability or coverage but cannot create a shared theme on its own.

Strict exclusions:
- Do not use final `personality_profile` outputs as primary inputs for this workflow.
- Do not use legacy `personality/personality_profile_codex.md` outputs as evidence.
- Do not invent shared themes just because similar words appear across streamers.
- Do not treat broad streamer-common traits such as `playful`, `audience-aware`, `appreciative`, `chatty`, or `energetic` as useful baseline findings unless you specify their actual interactional form.

What counts as a useful shared behavior:
- A recurring interaction pattern visible across multiple talents.
- Built from evidence-bearing retained open codes, not just one summary adjective.
- Meaningful enough to serve as comparative baseline for later uniqueness profiling.
- Expressed concretely as a social/interactional behavior such as:
  - gratitude ritualization
  - audience-attentive pacing control
  - teasing as affiliative bonding
  - reassurance during regulation or recovery
  - performative escalation as audience management

What does NOT count as a useful shared behavior unless reframed:
- A generic descriptor with no interactional explanation
- A lexical bucket such as `lol bit`
- A single catchphrase
- A topic preference
- An attribute so broad that it applies to nearly every streamer without distinction

Required synthesis method:

Step 1: Inventory per-talent retained structures
- For each talent, review:
  - retained codes
  - theme families
  - evidence examples
  - summary-classification recurring themes where available
- Build a talent-by-code and talent-by-family comparison map.

Step 2: Build candidate cross-talent shared patterns
- Group similar retained codes and families across talents into candidate shared behaviors.
- Each candidate shared behavior must be grounded in evidence from at least 2 talents.
- Prefer behavior-level groupings over lexical similarity.

Step 3: Distinguish three classes
For each candidate pattern, classify it as one of:
- `shared_baseline`
  Meaning: a genuinely common interaction pattern across the set
- `shared_but_distinctive_in_form`
  Meaning: a common pattern exists, but each streamer performs it differently enough that uniqueness still matters
- `not_shared_enough_for_baseline`
  Meaning: too weakly shared or too talent-specific to belong in the baseline

Step 4: Build the baseline carefully
- The final baseline should include only the strongest `shared_baseline` and `shared_but_distinctive_in_form` patterns.
- Do not overstuff the baseline.
- A small high-signal baseline is better than a long list of vague commonalities.

Step 5: Preserve uniqueness hooks
- For every retained shared behavior, include a note about how it varies by talent.
- The output must help later workflows answer:
  - what is common?
  - what is this streamer's version of the common pattern?
  - what falls outside the baseline and is therefore a uniqueness candidate?

Step 6: Write a comparative baseline, not a talent report
- This workflow should not produce one profile per streamer.
- It should produce one cross-talent shared-behavior baseline plus structured comparative artifacts.

Comparative rules:
- Compare across all eligible talents.
- Do not reduce the dataset to a single average persona.
- Shared patterns should be useful precisely because they make later uniqueness claims stricter.
- For each shared behavior, identify:
  - how many talents support it
  - which talents support it strongly
  - which talents show a notably different version of it
  - which talents do not clearly support it

Evidence rules:
- Every major shared behavior must map back to retained upstream evidence rows.
- Use direct quotes sparingly but concretely.
- Prefer quotes from different talents for the same shared behavior.
- If a cross-talent pattern is inferred from weak evidence, say so explicitly.
- Do not fabricate evidence, counts, or contrasts.

Write outputs to these exact paths:
1) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_baseline_codex.md`
2) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_codebook.csv`
3) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/talent_shared_behavior_matrix.csv`
4) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_evidence.csv`
5) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_state.json`
6) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/snapshots/shared_behavior_baseline_YYYY-MM-DD_HH-MM-SS_±HHMM.md`

Folder organization rules:
- Keep live outputs only in `shared_interactions/current/`.
- Keep dated markdown snapshots only in `shared_interactions/snapshots/`.
- Do not write cross-talent baseline outputs into individual talent folders.

Incremental / rerun rules:
- If `shared_behavior_state.json` exists, you may use it as a completion/state marker.
- If upstream per-talent open-coding outputs have materially changed or new talents are eligible, refresh the baseline.
- If current output files are missing or empty, rebuild them.
- Do not delete historical snapshots just to rerun.

Required structure for `shared_behavior_baseline_codex.md`:

Start the document with:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Eligible talents included: <count>`
- `Primary upstream source: personality_open_coding`
- `Secondary source used: summary_classification yes|no`

## 1) Cross-Streamer Shared Interaction Baseline
- 2 to 4 paragraphs.
- Summarize the strongest shared interaction patterns across the set.
- Explicitly distinguish:
  - what is truly common
  - what is shared but takes different forms
  - what should not be treated as baseline
- Include 1 short note explaining why this baseline is useful for later uniqueness profiling.

## 2) Shared Behavior Codebook
- Present the retained cross-talent shared behaviors.
- For each shared behavior include:
  - `shared_behavior_name`
  - `classification` (`shared_baseline` or `shared_but_distinctive_in_form`)
  - `definition`
  - `inclusion_criteria`
  - `exclusion_criteria`
  - `talent_count`
  - `supporting_talents`
  - `weaker_or_absent_talents`
  - `why this belongs in the baseline`
  - `how it varies across talents`
  - `supporting quotes` with examples from multiple talents where possible

## 3) Shared-But-Different Patterns
- This section is required.
- Explain which recurring behaviors are common across streamers but meaningfully different in form, delivery, or relationship function.
- These are especially important because they will later feed uniqueness profiling.

## 4) Non-Baseline or Weakly Shared Patterns
- Identify major patterns that should NOT be treated as common baseline because they are too talent-specific, too weakly shared, or too context-bound.
- This section helps prevent false uniqueness claims later.

## 5) Talent Contrast Notes
- For each eligible talent, provide a short note on:
  - what baseline behaviors they clearly share
  - what shared behaviors they perform in a distinctive way
  - what likely uniqueness-candidate areas remain outside the baseline

## 6) Method Notes and Uncertainty
- Include:
  - 3 strongest evidence-backed baseline conclusions
  - 3 uncertainty points
  - 1 note about the risk of flattening distinct streamers into generic shared traits

Required structure for `shared_behavior_codebook.csv`:
Columns:
- `shared_behavior_name`
- `classification`
- `definition`
- `inclusion_criteria`
- `exclusion_criteria`
- `talent_count`
- `supporting_talents`
- `weaker_or_absent_talents`
- `variation_note`
- `baseline_relevance_note`

Required structure for `talent_shared_behavior_matrix.csv`:
Columns:
- `talent`
- `shared_behavior_name`
- `classification`
- `support_level` (`strong|moderate|weak|absent`)
- `talent_specific_version`
- `uniqueness_candidate_note`

Required structure for `shared_behavior_evidence.csv`:
Columns:
- `shared_behavior_name`
- `classification`
- `talent`
- `contributing_open_code`
- `theme_family`
- `video_id`
- `time_in_seconds`
- `timecode`
- `source`
- `speaker`
- `quote`
- `evidence_role`
- `why_this_supports_shared_behavior`

Required structure for `shared_behavior_state.json`:
- `analysis_conducted_at`
- `eligible_talents`
- `primary_upstream_source`
- `secondary_source_used`
- `shared_behavior_count`
- `shared_baseline_count`
- `shared_but_distinctive_in_form_count`
- `latest_snapshot_path`
- `notes`

Snapshot rules:
- On each run that writes or refreshes the current markdown, also write a dated markdown snapshot in `shared_interactions/snapshots/`.
- The snapshot must include:
  - the snapshot date/time
  - the full current markdown content
  - the full prompt text used for the run

Execution rules:
- Execute end-to-end without asking for confirmation unless blocked.
- Create missing output folders if needed.
- Overwrite only the current outputs for this shared-interactions workflow.
- Do not modify individual talent raw data or upstream open-coding artifacts.

Final report to user:
- Report:
  - eligible talents included
  - number of retained shared behaviors
  - number classified as `shared_baseline`
  - number classified as `shared_but_distinctive_in_form`
  - output paths
- Spot-check at least 2 shared-behavior quotes from different talents by showing exact source-file matches.
