You are working across these data roots:

Primary talent data root:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`

Cross-talent processed root:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data`

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/personality/personality_qualitative_codebook.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/personality/personality_qualitative_codebook/`.

Optional talent scope:
- The shell runner accepts `--talent "<exact talent folder name>"`.
- When `TALENT_SLUG` is provided by the runner, focus code-log updates on that exact talent folder while using existing cross-talent artifacts only as needed for comparison.
- When no `TALENT_SLUG` is provided, process every eligible talent.

Objective:
Maintain a cumulative cross-talent qualitative code log for streamer personality and interaction style by synthesizing:
- per-talent `personality_open_coding`
- the cross-talent `shared_interactions` baseline
- per-talent `personality_unique_features`
- `summary_classification` as a secondary stabilizing layer

This workflow is not a talent profile.
It is not a one-shot final codebook.
It is a living qualitative code log.

Its job is to answer:
1) What recurring qualitative codes are worth retaining across the dataset right now?
2) Which codes should be added to the cumulative log because they are analytically useful?
3) Which existing codes should be revised, merged, split, or clarified based on new evidence?
4) How can the log remain stable enough for reuse while still allowing recoding over time?

Why this workflow exists:
- `personality_open_coding` captures talent-local recurring patterns.
- `shared_interactions` tells us what is common across the set.
- `personality_unique_features` tells us what is distinctive relative to the baseline.
- This workflow converts those layers into an evolving qualitative code registry that can be expanded and refined as the corpus grows.

Core analytic standard:
- Codes must be evidence-backed and text-derived.
- A code is not valid just because it sounds interpretively interesting.
- Codes should be abstract enough to travel across cases, but concrete enough to be justified by evidence rows and quotes.
- The code log should support both:
  - stable reuse over time
  - thoughtful recoding when the current code structure is no longer adequate
- If a code is so generic it could apply to almost any streamer without modification, demote or exclude it.

Important limitations:
- You do NOT have visual cues.
- You do NOT have full vocal prosody from audio.
- Infer only from retained text-visible evidence.
- Do not over-claim motive, psychology, or off-stream identity.

Preference-versus-personality rules:
- Do NOT promote narrow preferences, housekeeping rules, or content policies into headline qualitative codes by themselves.
- Exclude or demote things like:
  - spoiler rules
  - one-off likes/dislikes
  - generic schedule preferences
  - routine moderation reminders
- Keep such material only when it clearly reveals a broader relational style, such as:
  - protective pacing control
  - ritualized boundary-setting
  - caretaking audience management
  - tension-management through framing

Code-log mindset:
- Treat the output as a cumulative registry, not a final frozen taxonomy.
- Existing codes may be retained, clarified, merged, split, or retired if the evidence justifies it.
- New codes may be added whenever a recurring and analytically useful pattern emerges.
- Recoding is allowed and expected when the current code structure becomes too vague, too redundant, or too narrow.
- Preserve continuity where possible; do not churn code names unnecessarily.

Primary log format:
The main qualitative-code log must use these conceptual fields:
- `Primary Code ID`
- `Primary Code`
- `Secondary Code ID`
- `Secondary Code`
- `Definition`
- `Date added`
- `Examples from text`

Interpretation of those fields:
- `Primary Code ID`: a short stable abbreviation for the primary code, such as `A1`, `B2`, or another compact durable label
- `Primary Code`: the broadest retained qualitative category
- `Secondary Code ID`: a short stable abbreviation for the secondary code nested under the primary code, such as `A1a` or `B2b`
- `Secondary Code`: the more specific subcode, variant, or diagnostic form nested under the primary code
- `Definition`: a concise analytic definition of what the code means
- `Date added`: the date this code first entered the maintained log
- `Examples from text`: short text-derived examples that justify the code

Primary-versus-secondary rules:
- A primary code should be a mid-level analytic family, not an ultra-broad bucket like `personality` and not a one-off narrow behavior.
- A primary code may exist without any secondary codes when the family is already specific and stable enough on its own.
- Create a secondary code only when it clarifies a recurring subtype, variant, or specific form of the primary code that matters analytically.
- Do NOT create a secondary code just because you have one example. Single examples belong in `Examples from text`, not in the code structure.
- Secondary codes are most useful when:
  - multiple talents share the primary code but differ in form
  - one talent shows a distinctive subtype of a broader family
  - the sub-distinction changes interpretation enough to matter for later coding
- If the distinction does not materially improve clarity or reuse, keep only the primary code.

Talent discovery rules:
- Discover all direct child directories inside `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`.
- Exclude aggregate directories such as `VarianceProject`.
- Treat a directory as eligible only if it contains:
  - usable `personality_open_coding`
  - usable `personality_unique_features/current/unique_personality_profile_codex.md`
- `overall_channel_summary/current/overall_channel_summary.md` is preferred but may be missing for some talents.
- Process every eligible talent found at runtime.

Primary inputs per talent:

1) `personality_open_coding`
- Prefer the newest available open-coding set in this order:
  1. `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_codebook_v3.csv`
  2. `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_coding_evidence_v3.csv`
  3. `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/personality_profile_v3_open_coding.md`
- If v3 is unavailable, fall back to:
  1. `<talent>/stream_summaries/overall_themes/personality_open_coding/v2/open_codebook_v2.csv`
  2. `<talent>/stream_summaries/overall_themes/personality_open_coding/v2/open_coding_evidence_v2.csv`
  3. `<talent>/stream_summaries/overall_themes/personality_profile_v2_open_coding.md`

2) `shared_interactions`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_baseline_codex.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_codebook.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/talent_shared_behavior_matrix.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_evidence.csv`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_state.json`

3) `personality_unique_features`
- `<talent>/stream_summaries/overall_themes/personality_unique_features/current/unique_personality_profile_codex.md`
- `<talent>/stream_summaries/overall_themes/personality_unique_features/current/unique_personality_evidence_log_codex.csv`
- `<talent>/stream_summaries/overall_themes/personality_unique_features/current/unique_personality_state.json`

4) Secondary stabilizing inputs: `overall_channel_summary`
- `<talent>/stream_summaries/overall_channel_summary/current/overall_channel_summary.md`
- `<talent>/stream_summaries/overall_channel_summary/current/overall_channel_summary_state.json` if present

Existing-log input:
- If it exists, read:
  - `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log.csv`
  - `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log_state.json`
- Treat the existing log as the current registry to update rather than discard.

Input precedence rules:
- `personality_open_coding` evidence rows and retained codebooks are the primary basis for code retention.
- `shared_interactions` helps determine what is common versus contrastive.
- `personality_unique_features` helps identify which codes are especially diagnostic.
- `summary_classification` can support judgments about stability or recurrence but cannot create a code on its own.
- The existing qualitative code log, if present, is the continuity layer rather than the primary evidence layer.

Strict exclusions:
- Do not use final `personality_profile` prose as primary evidence.
- Do not invent codes from vibes, aesthetics, or presumed persona.
- Do not retain lexical buckets with no social interpretation.
- Do not retain one-off moments as durable qualitative codes.
- Do not let a single talent dominate the whole log.

What counts as a strong qualitative code:
- A recurring interactional pattern with social meaning.
- A recognizable relationship stance toward chat.
- A patterned way of combining teasing, care, ritual, control, escalation, disclosure, or reciprocity.
- A recurring mode of pacing, framing, or emotional management.
- A distinctive combination of behaviors that appears repeatedly enough to matter.

What does NOT count unless reframed:
- `playful`
- `energetic`
- `supportive`
- `chatty`
- `no spoilers`
- `lol bit`
- a single catchphrase
- a game-specific preference

Required synthesis method:

Step 1: Read the existing log first if it exists
- Treat it as the current qualitative registry.
- Identify:
  - stable codes worth keeping
  - overly broad codes that should be split
  - redundant codes that should be merged
  - obsolete or weak codes that should be retired

Step 2: Read the shared baseline
- Review the shared baseline markdown, codebook, matrix, and evidence.
- Identify which patterns look foundational across talents and which look variant or non-baseline.

Step 3: Read per-talent open coding
- Review retained codes, theme families, evidence rows, and cumulative open-coding summaries.
- Identify which candidate codes recur across multiple talents and which remain narrow.

Step 4: Read per-talent unique-feature outputs
- Review the uniqueness reports and evidence logs.
- Identify which codes are repeatedly serving as contrastive or diagnostic markers.

Step 5: Update the code log
- Add a new row when a recurring and analytically useful code is not yet represented in the log.
- Revise an existing row when the definition or examples need sharpening.
- Merge rows when two codes are actually the same pattern.
- Split a row when one code is hiding analytically meaningful subcodes.
- Retire a row only when the evidence strongly shows it is not useful, not valid, or fully absorbed into another code.
- When splitting into subcodes, keep the primary code family stable when possible and add secondary-code rows only if the split creates reusable analytic clarity.

Step 6: Preserve date-added logic
- `Date added` should represent the first date the code entered the maintained log.
- If a code is revised but not fundamentally replaced, keep the original `Date added`.
- If a truly new code is added, use the current run date.
- If a code is split into multiple genuinely new codes, those new rows get the current run date.

Step 7: Favor stable but revisable structure
- Preserve continuity where possible so the log remains useful over time.
- Do not rename a code unless the new wording is clearly better analytically.
- Prefer a moderate number of durable codes rather than frequent renaming churn.

Step 8: Write a log update memo, not a polished essay
- The markdown output should explain what was added, revised, merged, split, retained, or retired in this run.
- It should function like a qualitative coding memo attached to the maintained log.

Retention target:
- The log may grow over time.
- Do not force a fixed target count.
- Still prefer a compact high-signal registry over a bloated list of weak codes.

Evidence rules:
- Every retained or newly added code must map back to real evidence rows.
- Every major code should include short supporting examples where possible.
- Prefer examples from multiple talents when the code is broad.
- If a code is mainly supported by one talent, that must be stated clearly.
- If evidence is thin or unstable, say so directly.

Write outputs to these exact paths:
1) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log.csv`
2) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log_codex.md`
3) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log_state.json`
4) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/snapshots/personality_qualitative_code_log_YYYY-MM-DD_HH-MM-SS_±HHMM.md`

Folder organization rules:
- Keep live outputs only in `Qualitative Codebook/current/`.
- Keep dated markdown snapshots only in `Qualitative Codebook/snapshots/`.
- Do not write this cross-talent code log into individual talent folders.

Incremental / rerun rules:
- If the current log exists, update it rather than rebuilding it from scratch unless the current structure is unusable.
- A rerun may:
  - append new codes
  - revise existing definitions
  - expand examples
  - merge or split codes
- Do not silently discard older codes without explaining why in the markdown memo.

Required structure for `personality_qualitative_code_log.csv`:
- One row per retained qualitative code.
- Use at least these exact columns:
  - `Primary Code ID`
  - `Primary Code`
  - `Secondary Code ID`
  - `Secondary Code`
  - `Definition`
  - `Date added`
  - `Examples from text`

Guidance for filling those columns:
- `Primary Code ID` should be a short durable label such as `A1`, `A2`, `B1`.
- `Primary Code` should be the broader family or parent code.
- `Secondary Code ID` should be blank when no secondary code is needed. If used, it should be a short nested label such as `A1a`, `A1b`.
- `Secondary Code` should be the narrower subcode, diagnostic variant, or specific form.
- `Definition` should be concise, analytic, and reusable.
- `Date added` should be in `YYYY-MM-DD`.
- `Examples from text` should be short semicolon-separated text snippets or short quote fragments, not long paragraphs.
- If no secondary code is needed, leave both `Secondary Code ID` and `Secondary Code` blank.

Required structure for `personality_qualitative_code_log_codex.md`:

Start the document with:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Eligible talents included: <count>`
- `Primary source layers: personality_open_coding + shared_interactions + personality_unique_features`
- `Secondary source used: summary_classification yes|no`
- `Existing log used: yes|no`

## 1) Log Update Summary
- 1 to 3 paragraphs.
- Explain what changed in this run.
- State whether the run mainly:
  - added new codes
  - clarified existing codes
  - merged/split codes
  - revisited weak codes

## 2) Codes Added This Run
- List any newly added rows.
- For each added code include:
  - `Primary Code ID`
  - `Primary Code`
  - `Secondary Code ID`
  - `Secondary Code`
  - `why it was added`
  - `supporting evidence summary`

## 3) Codes Revised This Run
- List any codes whose definitions or examples were materially updated.
- Explain what changed and why.

## 4) Codes Merged, Split, or Retired
- This section is required when relevant.
- For each change explain:
  - what changed
  - why the prior structure was insufficient
  - how the new structure better fits the evidence

## 5) Current High-Value Code Families
- Briefly identify the strongest current primary-code families in the log.
- Explain why they seem durable and analytically useful.

## 6) Open Questions for Future Recoding
- List weak points, ambiguity areas, or likely future refinement moves.

Required structure for `personality_qualitative_code_log_state.json`:
- Include at least:
  - `analysis_conducted`
  - `eligible_talents`
  - `existing_log_used`
  - `rows_in_log`
  - `new_codes_added`
  - `codes_revised`
  - `codes_merged`
  - `codes_split`
  - `codes_retired`
  - `source_layers_used`
  - `secondary_source_used`
  - `output_paths`
  - `notes`
