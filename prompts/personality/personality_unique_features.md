You are working across these data roots:

Primary talent data root:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`

Cross-talent baseline root:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions`

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/personality/personality_unique_features.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/personality/personality_unique_features/`.

Objective:
Build a talent-specific unique personality profile for each eligible streamer by using the cross-streamer shared-behavior baseline plus each talent's own open-coded evidence.

This workflow is a downstream uniqueness synthesis workflow.

It should answer:
1) What makes this streamer unique relative to the shared baseline?
2) Which shared behaviors does this streamer express in a distinctive way?
3) Which behaviors fall outside the shared baseline and therefore function as stronger uniqueness markers?
4) What combination of interaction tendencies makes this streamer non-interchangeable?

Best-fit workflow assumption:
- For Codex, the best approach is NOT to re-read all raw stream data from scratch here.
- Instead, use:
  - the shared baseline artifacts as the comparative frame
  - `personality_open_coding` as the primary talent-specific evidence layer
  - `summary_classification` as a secondary stabilizing layer
- Re-reading raw text should be a fallback only if auditing or resolving a contradiction.

Why this workflow exists:
- `personality_open_coding` identifies talent-local recurring patterns.
- `shared_interactions` identifies what is common across the set.
- This workflow converts that comparison into a disciplined statement of uniqueness.

Core analytic standard:
- Uniqueness must be evidence-backed, not inferred from vibes.
- A trait is not unique just because it appears often in one talent.
- A trait becomes uniqueness-relevant when one of these is true:
  - it falls outside the shared baseline
  - it is much more emphasized than in peers
  - it is a distinctive version of a shared behavior
  - it forms a distinctive combination with other behaviors
- If a claim could be copied into another talent's report with only the name changed, it is too generic.

Preference-versus-personality rules:
- Do NOT treat isolated preferences, rules, or content-management policies as personality dimensions by themselves.
- Examples that should usually be excluded as standalone uniqueness dimensions:
  - spoiler preferences or spoiler avoidance
  - stated likes/dislikes about games or media
  - one-off schedule preferences
  - format housekeeping rules
  - generic chat-management reminders
- A preference or policy may be included only if the evidence shows it is a repeated expression of a broader relational or performative style, such as:
  - control of audience pacing
  - protective/caretaking mediation
  - ritualized boundary-setting
  - tension-management through rules or framing
- In that case, the dimension must be named for the broader style, not for the literal preference itself.
- Bad dimension labels:
  - `no spoilers`
  - `dislikes spoilers`
  - `asks chat not to spoil`
- Better dimension labels:
  - `protective pacing control around unfolding experiences`
  - `ritualized audience-boundary management during live discovery`
  - `caretaking control of shared suspense`
- If you cannot reframe a repeated preference into a broader interpersonal style with strong evidence, demote it to minor supporting evidence or exclude it entirely.

Important limitations:
- You do NOT have visual cues.
- You do NOT have full vocal prosody from audio.
- Infer only from retained text-visible evidence.
- Do not over-claim. If evidence is weak, say `insufficient evidence`.

Anonymity and comparison-display rules:
- By default, do NOT name other streamers in the written uniqueness report.
- Default comparison language should be anonymous, such as:
  - `other talents in the dataset`
  - `peer streamers in this corpus`
  - `the shared baseline group`
- Only name peer talents when there is a clear same-company or same-roster signal, such as a shared tag in the talent name, folder name, or other explicit upstream metadata.
- If no clear same-company grouping exists, keep all comparisons anonymous.
- Even when same-company naming is allowed, only name peers when that improves interpretability; do not overuse names.

Talent discovery rules:
- Discover all direct child directories inside `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`.
- Exclude aggregate directories such as `VarianceProject`.
- Treat a directory as eligible only if it contains:
  - usable `personality_open_coding` outputs
  - usable `summary_classification/current/overall_themes_codex.md`
- Process every eligible talent found at runtime.

Primary cross-talent baseline inputs:
1) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_baseline_codex.md`
2) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_codebook.csv`
3) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/talent_shared_behavior_matrix.csv`
4) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_evidence.csv`
5) `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current/shared_behavior_state.json`

Primary talent-specific inputs per talent: `personality_open_coding`
- Prefer the newest available open-coding set in this order:
  1) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_codebook_v3.csv`
  2) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_coding_evidence_v3.csv`
  3) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/personality_profile_v3_open_coding.md`
- If v3 is unavailable, fall back to:
  1) `<talent>/stream_summaries/overall_themes/personality_open_coding/v2/open_codebook_v2.csv`
  2) `<talent>/stream_summaries/overall_themes/personality_open_coding/v2/open_coding_evidence_v2.csv`
  3) `<talent>/stream_summaries/overall_themes/personality_profile_v2_open_coding.md`

Secondary talent-specific inputs per talent: `summary_classification`
- `<talent>/stream_summaries/overall_themes/summary_classification/current/overall_themes_codex.md`
- `<talent>/stream_summaries/overall_themes/summary_classification/current/summary_classification_state.json` if present

Input precedence rules:
- The shared-interactions baseline defines what counts as common versus non-baseline.
- `open_coding_evidence_*` is the main source for talent-specific claims and quotes.
- `open_codebook_*` defines what the retained talent-local codes mean.
- `summary_classification` may support claims about stability or recurring visibility but cannot create a uniqueness claim by itself.

Strict exclusions:
- Do not use old legacy personality reports as evidence.
- Do not use broad generic streamer traits as uniqueness claims unless you specify the streamer's distinctive version.
- Do not invent archetypes unless clearly warranted by evidence.
- Do not claim motives, diagnosis, trauma history, or off-stream identity traits.

What counts as a uniqueness feature:
- A talent-specific interaction tendency that is weakly shared, non-baseline, or expressed in a notably different way.
- A distinctive version of a shared behavior, such as:
  - a specific form of gratitude ritual
  - a specific style of teasing
  - a specific way of blending care and control
  - a specific pattern of theatricality
- A combination of traits that together make the streamer non-interchangeable.

What does NOT count as a uniqueness feature unless reframed:
- A broad trait such as `playful`, `supportive`, `energetic`, or `chatty`
- A lexical bucket such as `lol bit`
- A single catchphrase without social interpretation
- A one-off moment
- A trait already established as shared baseline unless you explain the talent-specific version

Required synthesis method:

Step 1: Read the shared baseline first
- Review the shared behavior codebook, baseline markdown, talent matrix, and shared evidence.
- Identify:
  - which behaviors are shared baseline
  - which are shared but distinctive in form
  - which are non-baseline

Step 2: Read the talent-specific open-coding structure
- Review retained codes, theme families, evidence rows, and cumulative summaries for the target talent.
- Identify which retained talent-local behaviors:
  - align with the shared baseline
  - represent the talent's specific version of a shared behavior
  - sit outside the baseline

Step 3: Build uniqueness dimensions
- Build 3 to 6 major uniqueness dimensions for the talent.
- These should be a mix of:
  - distinctive versions of shared behaviors
  - non-baseline behaviors
  - unusually emphasized combinations of behaviors
- At least 2 dimensions should come from non-baseline or weakly shared patterns when evidence allows.
- Dimension names should be descriptive and person-shaped, not literal policy labels or surface topic labels.

Step 4: Run a baseline-vs-unique check
For every candidate uniqueness dimension, state which of these it is:
- `distinctive_version_of_shared_behavior`
- `outside_shared_baseline`
- `distinctive_combination_of_traits`
- `insufficiently_unique`

If a candidate is only `insufficiently_unique`, demote or remove it.
Also demote or remove a candidate if it is better described as a mere preference, housekeeping rule, or content policy rather than a recurring interpersonal style.

Step 5: Write a uniqueness profile, not a generic personality profile
- The opening and major sections must answer:
  - what is uniquely characteristic here?
  - how does that differ from the shared baseline?
  - why does that make this streamer non-interchangeable?

Interchangeability test:
- Before finalizing the report, ask:
  - if I replaced this talent's name with another talent's name, would most of this still sound plausible?
- If yes, the report is too generic and must be revised.

Evidence rules:
- Every major uniqueness dimension must be backed by exact retained evidence rows.
- Each uniqueness dimension should include at least 3 short quotes where possible.
- Prefer quotes from different streams.
- If evidence is thin, say so directly.
- Do not use summary prose as the only support for a uniqueness claim.
- Do not use repeated mentions of a narrow preference alone as sufficient support for a major personality dimension.

Cross-talent comparison rules:
- Compare each talent against the shared baseline and against at least 2 peers conceptually.
- Prefer comparison statements about:
  - interaction form
  - relationship stance
  - pacing/control style
  - humor/performance structure
  - care, boundaries, reciprocity, and ritual form
- Avoid vague `more chaotic`, `more sweet`, or `more fun` language without specifying how.
- When no clear same-company grouping exists, phrase these contrasts anonymously.
- When a clear same-company grouping exists, you may include named peer comparisons within that company group.

Money-linkage rules:
- Use money-context evidence only as a supplement.
- Include it only if it helps reveal the talent's distinctive relationship style.

Write outputs to these exact paths per talent:
1) `<talent>/stream_summaries/overall_themes/personality_unique_features/current/unique_personality_profile_codex.md`
2) `<talent>/stream_summaries/overall_themes/personality_unique_features/current/unique_personality_evidence_log_codex.csv`
3) `<talent>/stream_summaries/overall_themes/personality_unique_features/current/unique_personality_state.json`
4) `<talent>/stream_summaries/overall_themes/personality_unique_features/snapshots/unique_personality_profile_YYYY-MM-DD_HH-MM-SS_±HHMM.md`

Folder organization rules:
- Keep live outputs only in `personality_unique_features/current/`.
- Keep dated markdown snapshots only in `personality_unique_features/snapshots/`.
- Do not overwrite the general `personality_profile` outputs with this uniqueness-specific workflow.

Required structure for `unique_personality_profile_codex.md`:

Start the document with:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Talent: <talent name>`
- `Primary talent source: <v3 current|v2 fallback>`
- `Shared baseline source used: yes`
- `Summary-classification source used: yes|no`

## 1) Unique Personality Signature
- 1 to 2 paragraphs.
- Answer directly: what makes this streamer unique in this dataset?
- Name the 2 to 4 strongest uniqueness dimensions.
- State confidence: `High`, `Medium`, or `Low`.
- State what cannot be known from text-only evidence.

## 2) Baseline Comparison Notes
- Briefly explain:
  - which shared baseline behaviors this talent clearly shares
  - which shared behaviors they perform in a distinctive way
  - which important traits appear to sit outside the shared baseline
- Use anonymous wording unless same-company naming is explicitly justified by a shared company/roster tag.

## 3) Major Uniqueness Dimensions
- Present 3 to 6 major uniqueness dimensions.

For each dimension include:
- `dimension_name`
- `uniqueness_type` (`distinctive_version_of_shared_behavior` | `outside_shared_baseline` | `distinctive_combination_of_traits`)
- `why this is unique in the dataset`
- `shared baseline comparison`
- `contributing open codes`
- `evidence pattern`
- `supporting quotes`
- `limits / counterevidence`

## 4) Distinctive Versions of Shared Behaviors
- This section is required.
- Explain how this streamer performs shared baseline behaviors in a different way from peers.
- Focus on the streamer's specific version of common patterns.

## 5) Non-Baseline Markers and Rituals
- Identify recurring markers, routines, or social patterns that do not fit comfortably into the shared baseline.
- Explain why they matter for uniqueness.

## 6) Relational Uniqueness With Chat
- Explain what is distinctive about how this streamer positions themself with chat.
- Address closeness, authority, reciprocity, teasing, reassurance, boundaries, and audience management.

## 7) Cross-Talent Contrast
- Compare this talent against at least 2 peers or the relevant peer group.
- Include at least 2 contrasts about how the talent performs a shared behavior differently from peers.
- Default wording should be anonymous.
- If a clear same-company tag exists, you may include named peer contrasts within that company group.
- Use one of these sentence forms:
  - anonymous default: `Could be confused with other talents in the dataset on Y, but differs in Z.`
  - same-company allowed: `Could be confused with X on Y, but differs in Z.`

## 8) Optional Money-Linked Distinctiveness
- Use only if enough evidence exists.
- Explain whether money-linked moments reveal a distinctive relationship style for this talent.
- If evidence is sparse, say `insufficient evidence`.

## 9) Validity, Limits, and Uncertainty
- List 3 strongest evidence-backed uniqueness conclusions.
- List 3 uncertainty points.
- Include 1 note about the risk of mistaking shared streamer traits for uniqueness.
- Include 1 note about text-only modality limits.

Required structure for `unique_personality_evidence_log_codex.csv`:
Columns:
- `talent`
- `dimension_name`
- `uniqueness_type`
- `shared_behavior_name`
- `contributing_open_code`
- `theme_family`
- `video_id`
- `time_in_seconds`
- `timecode`
- `source`
- `speaker`
- `quote`
- `evidence_role`
- `why_this_supports_uniqueness`

Required structure for `unique_personality_state.json`:
- `talent`
- `analysis_conducted_at`
- `primary_talent_source`
- `shared_baseline_source_used`
- `summary_classification_source_used`
- `uniqueness_dimension_count`
- `latest_snapshot_path`
- `notes`

Snapshot rules:
- On each run that writes or refreshes the current markdown, also write a dated markdown snapshot in `personality_unique_features/snapshots/`.
- The snapshot must include:
  - the snapshot date/time
  - the full current markdown content
  - the full prompt text used for the run

Execution rules:
- Process all eligible talents discovered at runtime.
- Create missing output folders if needed.
- Overwrite only the current outputs for this uniqueness workflow.
- Do not modify raw data or upstream open-coding/shared-baseline artifacts.

Final report to user:
- For each talent, report:
  - primary talent source used
  - whether shared baseline was used
  - uniqueness dimensions retained
  - evidence rows used
  - output paths
- Spot-check at least 2 quoted lines per talent against the source evidence file.
