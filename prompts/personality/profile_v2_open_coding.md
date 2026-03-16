You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Hardcoded talent directories to process:
- /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Avaritia Hawthorne 【Variance Project】
- /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Katya Sable 【Variance Project】
- /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】
- /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Terberri Solaris Ch

Objective:
Build a text-grounded personality analysis using OPEN CODING (emergent codes), not a predefined code list. Let relational style, affective patterns, and idiosyncrasies arise from the data itself.

Important limitations:
- You do NOT have visual cues (facial expression, camera behavior, body language).
- You do NOT have full vocal prosody from audio.
- Infer only from text evidence (chat logs, streamer chat posts, subtitles, summaries).
- Do not over-claim. If evidence is weak, say "insufficient evidence".

Scope:
Process all 4 hardcoded talents above.

For each hardcoded talent path, use these exact input paths:
1) `<talent>/stream_summaries/stream_summary_codex/*.md` (context only)
2) `<talent>/text_playback/*.csv` (primary evidence)
3) `<talent>/Chat/Original/*_chat.csv` (primary evidence)
4) `<talent>/stream_summaries/overall_themes/money_timestamps.csv` if present

Input filtering rules:
- Use only these files as analytic inputs:
  - `<talent>/stream_summaries/stream_summary_codex/*.md`
  - `<talent>/text_playback/*.csv`
  - `<talent>/Chat/Original/*_chat.csv`
  - `<talent>/stream_summaries/overall_themes/money_timestamps.csv` if present
- Do not use previously generated personality profile files, prior codebooks, prior evidence CSVs, or any existing v1/v2 outputs as evidence inputs.
- Treat existing generated outputs only as completion markers, not as source material.

Write outputs to these exact output paths:
1) `<talent>/stream_summaries/overall_themes/personality_profile_v2_open_coding.md`
2) `<talent>/stream_summaries/overall_themes/open_codebook_v2.csv`
3) `<talent>/stream_summaries/overall_themes/open_coding_evidence_v2.csv`

Resume / skip rules:
- Before processing a talent, check whether all 3 required v2 output files already exist and are non-empty:
  1) `<talent>/stream_summaries/overall_themes/personality_profile_v2_open_coding.md`
  2) `<talent>/stream_summaries/overall_themes/open_codebook_v2.csv`
  3) `<talent>/stream_summaries/overall_themes/open_coding_evidence_v2.csv`
- If all 3 files exist and are non-empty, treat that talent as already completed and skip it.
- Do not recompute completed talents unless the user explicitly requests a rerun.
- If only some output files exist, treat the talent as incomplete and regenerate only the missing or empty v2 outputs for that talent.
- Never delete prior outputs just to rerun the workflow.
- Do not modify any raw input files.

Open-coding method (required):

Phase 1: Stream-level open coding
- Read each stream as an independent interaction case.
- Generate in-vivo or descriptive codes directly from text (short labels, 1-4 words).
- Do not force data into preset categories.
- Code interactional units (single line or short exchange windows) with timestamped evidence.

Phase 2: Constant comparison
- Compare codes within and across streams for the same talent.
- Merge semantically equivalent codes.
- Split overly broad codes into specific ones when needed.
- Keep memos on why merges/splits were made.

Phase 3: Higher-order clustering
- Group emergent codes into higher-order themes (families) after coding is complete.
- Theme names must come from observed patterns, not imported frameworks.

Phase 4: Cross-talent contrast
- Compare emergent theme families across talents.
- Identify what is distinctive vs shared.

Evidence and reliability rules:
- Every analytic claim must map to raw evidence rows in CSV logs.
- Summaries can guide sampling but cannot be the sole evidence for claims.
- No hallucinated quotes, timestamps, donors, or interpretations.
- For each retained code, include:
  - clear operational definition
  - inclusion criteria
  - exclusion criteria
  - frequency count
  - stream coverage count
  - at least 3 examples where possible (if fewer exist, state exact count)
- Keep low-frequency but conceptually important codes if justified in memo.
- Include at least 2 negative cases (counterexamples) per major theme family.

Required structure for `personality_profile_v2_open_coding.md`:

## 1) Personality Signature (Emergent)
- 1-2 paragraphs describing the streamer’s personality style as it emerged from open coding.
- Confidence level: High / Medium / Low.
- Explicit limitations from text-only modality.

## 2) Open Coding Process Notes
- Briefly document how codes were generated, merged, split, and elevated into themes.
- Report coding totals: raw codes, merged codes, final retained codes.

## 3) Emergent Codebook (Talent-Specific)
- Present the final retained open codes for this talent.
- For each code include:
  - code_label
  - operational_definition
  - inclusion_criteria
  - exclusion_criteria
  - frequency_count
  - stream_coverage_count
  - 3 evidence quotes with `[timecode] speaker (video_id): "quote"`

## 4) Emergent Theme Families
- Group codes into higher-order theme families.
- For each family:
  - family summary
  - member codes
  - strongest supporting evidence
  - 2 counterexamples/limits

## 5) Idiosyncratic Charm Markers
- Identify recurring, non-generic markers unique to this streamer’s text presence.
- Include catchphrases, interaction rituals, humor signatures, response stance, and social positioning.
- Provide evidence from multiple streams when possible.

## 6) Money-Linked Personality Patterns (Supplement, not primary driver)
- Use money events as contextual probes, not as the coding backbone.
- For at least 20 sampled paid events (or all events if <20), examine:
  - pre-event context window (60-120 sec before)
  - event text
  - response window (60-120 sec after)
- Report how monetary moments amplify, dampen, or reveal emergent personality themes.

## 7) Cross-Talent Distinctiveness
- Contrast this talent against at least 2 other talents using emergent themes.
- Answer: what makes this streamer non-interchangeable?
- Include one "can resemble X, but differs by Y" statement.

## 8) Validity and Uncertainty
- 3 strongest evidence-backed conclusions.
- 3 uncertainty points.
- 1 section on possible coder bias and how evidence reduced it.

Required structure for `open_codebook_v2.csv`:
Columns:
- talent
- code_label
- theme_family
- operational_definition
- inclusion_criteria
- exclusion_criteria
- frequency_count
- stream_coverage_count
- memo_notes

Required structure for `open_coding_evidence_v2.csv`:
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

Execution rules:
- Evaluate all 4 hardcoded talents for eligibility.
- Skip any talent whose 3 required v2 output files already exist and are non-empty.
- Process only talents that are missing one or more required v2 outputs, unless the user explicitly requests a full rerun.
- Create missing output folders if needed.
- When rerunning an incomplete talent, overwrite only the missing or target v2 files for that talent.

Final report to user:
- Before processing, report which of the 4 talents are complete vs incomplete based on the required v2 output files.
- Per talent:
  - streams_scanned
  - raw_codes_generated
  - final_codes_retained
  - theme_families
  - evidence_rows_logged
  - money_events_sampled
  - output file paths
- Spot-check at least 2 quoted lines per talent by showing exact source-file matches.
