You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Run logging rules:
- The canonical shell entry point for this workflow is `bin/linux/codex_prompts/personality/personality_profile.sh`.
- Save Codex run logs and final-message markdown files to `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/personality/personality_profile/`.

Objective:
Build text-grounded personality profile outputs for every eligible talent. Each talent should receive two reporting windows saved under that talent's `personality_profile/` directory:

1) Monthly personality highlights
2) Overall personality highlights

This is a downstream synthesis workflow, not a fresh open-coding pass. Use existing `personality_open_coding` outputs as the primary evidence base and `summary_classification` outputs as a secondary context layer.

Core analytic standard:
- The report must describe personality as an emergent pattern from the text.
- Themes must NOT be pre-determined.
- Themes must be justified by quoted evidence.
- Repeated words or catchphrases alone do NOT automatically count as personality themes.
- The synthesis must explain what the open-coded patterns mean about the streamer’s relational style, affective style, and interaction habits.
- The synthesis must foreground what makes this streamer non-interchangeable, not just what makes them legible as a streamer.
- If a claim could be copied into another talent's report with only the name changed, it is too generic and must be rewritten or demoted.

Important limitations:
- You do NOT have visual cues.
- You do NOT have full vocal prosody from audio.
- Infer only from text-visible evidence.
- Do not over-claim. If evidence is weak, say `insufficient evidence`.

Talent discovery rules:
- Discover all direct child directories inside `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`.
- Exclude non-talent aggregate directories such as `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/VarianceProject`.
- Treat a directory as eligible only if it contains:
  - `stream_summaries/overall_themes/personality_open_coding/`
  - `stream_summaries/overall_themes/summary_classification/current/overall_themes_codex.md`
- Process every eligible talent found at runtime. Do not hardcode a fixed talent list.

Use these inputs for each talent:

Primary evidence layer: `personality_open_coding`
- Prefer the newest available open-coding set in this order:
  1) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_codebook_v3.csv`
  2) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/open_coding_evidence_v3.csv`
  3) `<talent>/stream_summaries/overall_themes/personality_open_coding/v3/current/personality_profile_v3_open_coding.md`
- If v3 is unavailable, fall back to:
  1) `<talent>/stream_summaries/overall_themes/personality_open_coding/v2/open_codebook_v2.csv`
  2) `<talent>/stream_summaries/overall_themes/personality_open_coding/v2/open_coding_evidence_v2.csv`
  3) `<talent>/stream_summaries/overall_themes/personality_open_coding/v2/personality_profile_v2_open_coding.md`

Secondary context layer: `summary_classification`
- `<talent>/stream_summaries/overall_themes/summary_classification/current/overall_themes_codex.md`
- `<talent>/stream_summaries/overall_themes/summary_classification/current/summary_classification_state.json` if present

Optional month-resolution inputs:
- Any reliable per-video date metadata found inside the talent directory may be used only to assign a `video_id` to a calendar month.
- Accept reliable month evidence such as:
  - explicit stream/upload dates in metadata files
  - explicit calendar dates in source filenames or titles
  - explicit dates inside summary markdown
- Never infer month from seasonal keywords alone such as `Christmas`, `Valentine`, `summer`, or `anniversary`.

Input precedence rules:
- `open_coding_evidence_*.csv` is the main evidence source for claims and quotes.
- `open_codebook_*.csv` defines what retained emergent codes mean and how often they occur overall.
- `personality_profile_v*_open_coding.md` can help interpret code families and memo logic, but do not merely restate it.
- `summary_classification/current/overall_themes_codex.md` is a secondary source used to check stability, prevalence, and channel-level context.
- `summary_classification` may support a claim, sharpen a contrast, or flag a limit, but it cannot create a major personality theme by itself if the open-coding evidence does not support that theme.

Strict exclusions:
- Do not use the old baseline personality report structure as evidence.
- Do not impose a predefined affective codebook.
- Do not invent archetypes unless the evidence clearly warrants that language.
- Do not treat a frequent lexical marker such as `lol`, `lmao`, or `oh my god` as a personality conclusion without interpreting its interactional function.
- Do not claim motives, diagnosis, trauma history, or stable off-stream identity traits from text snippets alone.
- Do not let generic traits such as `playful`, `chatty`, `audience-aware`, `appreciative`, or `energetic` dominate the profile unless you specify the distinctive way this streamer enacts them.

Two-window reporting model:

Window 1: Monthly personality highlights
- Build a month-sliced personality report for every resolved `YYYY-MM` with usable evidence.
- These reports should answer: what did this streamer feel like this month?
- Monthly reports should emphasize what was especially visible, intense, or characteristic in that month.
- Monthly reports may describe temporary emphasis or month-bound shifts, not just stable traits.

Window 2: Overall personality highlights
- Build one cumulative overall report per talent across all available evidence.
- The overall report should not flatten the timeline into a generic average.
- It must distinguish:
  - stable core personality tendencies
  - recently strengthened tendencies
  - weakened or less-visible tendencies
  - episodic or month-bound tendencies
- If the evidence does not support a real shift, say the personality picture appears mostly stable.

Month assignment rules:
- Create a `video_id -> year_month` mapping using only reliable date evidence.
- Use the earliest reliable rule in this order:
  1) explicit metadata date
  2) explicit calendar date in filename or title
  3) explicit date in summary text
- If a `video_id` cannot be assigned to a month confidently, do not force it into a monthly report.
- Unresolved-month videos may still be used in the overall report.
- In monthly reports, state how many videos were excluded due to unresolved month metadata.

What counts as a valid personality dimension:
- A higher-order pattern that says something meaningful about how the streamer interacts with chat.
- Built from multiple related open codes, not from one phrase alone.
- Supported by quotes from more than one stream where possible.
- Interpretable as a trait-like interaction tendency such as:
  - how the streamer handles closeness or distance
  - how they manage attention, pace, or control
  - how they use humor or performance
  - how they reassure, redirect, tease, include, or distance
  - how they turn donations, rituals, or callbacks into relationship work
- Specific enough that it helps distinguish this streamer from peers, not merely place them inside a broad shared bucket.

What does NOT count as a valid major dimension unless carefully reframed:
- A raw lexical bucket such as `lol bit` or `lmao bit`
- A single catchphrase
- A topic preference
- A one-off emotional moment
- A generic adjective with no quote support
- A common streamer trait with no explanation of this streamer's specific version of it

Required synthesis method:

Step 1: Read the open-coding structure first
- Review retained code labels, definitions, counts, and stream coverage.
- Review the evidence CSV to understand what those codes actually look like in raw text.
- Separate lexical surface forms from deeper interactional meaning.

Step 2: Build higher-order personality dimensions
- Merge related open codes into 3 to 6 personality dimensions for each report window.
- Dimension names must be text-derived and interpretive, not imported from a preset schema.
- Good dimension names describe an observed tendency, such as:
  - ritualized appreciation as community glue
  - theatrical escalation as social play
  - protective reassurance under fast pacing
- At least 2 of the major dimensions in the overall report should be distinctiveness-forward dimensions that would not fit the peer talents without major rewriting.

Step 2.5: Run a uniqueness filter before drafting
- Compare the candidate dimensions against the other eligible talents before finalizing them.
- Separate:
  - shared baseline streamer traits
  - this streamer's distinctive version of a shared trait
  - genuinely unique or unusually emphasized traits
- If a candidate dimension is mostly shared across peers, either:
  - rewrite it into a more specific dimension, or
  - demote it to background context instead of a headline trait.
- Ask of every major dimension: `what is this streamer's version of this behavior, and how is it different from the others?`

Step 3: Build monthly windows
- Filter evidence by month-assigned `video_id`.
- For each month, identify:
  - strongest visible dimensions that month
  - any newly intensified or especially salient behaviors
  - which overall traits were present but muted
- Do not pretend every month has a radically new personality. Many months will mainly reinforce the existing pattern.

Step 4: Build the overall window
- Use all available evidence, including month-resolved and unresolved-month evidence.
- Identify stable core dimensions across the corpus.
- Compare monthly windows to the overall baseline to describe shifts over time.
- Treat a shift as valid only if it is supported by enough evidence to suggest a real change in visible interaction style, not random variation.

Step 5: Use summary classification as a check, not a substitute
- Confirm whether proposed dimensions are also visible in cumulative summary-classification output.
- Use summary-classification findings to discuss stability, prevalence, and channel-level context.
- If summary classification conflicts with open-coding interpretation, say so explicitly and privilege the rawer evidence layer.

Step 6: Write personality reports, not code dumps
- The final outputs should explain what the streamer is like.
- Translate open-coded findings into interpretable personality portraits.
- Keep all major claims traceable to quotes.

Quote and evidence rules:
- Every major dimension must include at least 3 short quotes where possible.
- Quotes must be exact text from evidence-bearing files.
- Format quotes as: `[timecode] speaker (video_id): "quote"`
- Prefer quotes from different streams.
- If a dimension has thin evidence, say so directly.
- Do not use summary prose as the only support for a theme.

Counterevidence rules:
- For each major dimension, include at least 1 limit, counterexample, or uncertainty note.
- If a pattern is frequent but narrow in context, say that.
- If a pattern may partly reflect transcript artifacts or standardized summary phrasing, say that.

Distinctiveness rules:
- Answer the question: what makes this streamer non-interchangeable?
- Compare against all other eligible talents when feasible; at minimum compare against 2 other eligible talents from the same dataset.
- Use contrasts grounded in available open-coding and summary-classification outputs.
- Distinctiveness must appear throughout the report, not only in the final comparison section.
- If a trait is shared by several talents, specify the streamer's distinctive version of that trait.
- Prefer contrasts about interaction form, relationship stance, and recurring social pattern over vague tone words.
- Do not rely on vague `more chaotic`, `more sweet`, or `more fun` language without evidence-backed specifics.

Interchangeability test:
- Before finalizing any monthly or overall report, perform this check:
  - if you could replace the streamer's name with another eligible talent and most of the report would still feel accurate, the report is not specific enough
- Revise until the headline dimensions, opening summary, and distinctiveness section clearly identify what is uniquely characteristic about this streamer.

Money-linkage rules:
- If the evidence CSV includes `monetary_context`, use it to comment on how personality appears around money moments.
- This is a supplement, not the backbone of the report.
- Focus on what donation moments reveal about relationship style.

Write outputs under these exact directories:
- Monthly root:
  - `<talent>/stream_summaries/overall_themes/personality_profile/monthly/`
- Overall root:
  - `<talent>/stream_summaries/overall_themes/personality_profile/overall/`

Write these exact output paths for each resolved month `YYYY-MM`:
1) `<talent>/stream_summaries/overall_themes/personality_profile/monthly/YYYY-MM/personality_monthly_highlights_codex.md`
2) `<talent>/stream_summaries/overall_themes/personality_profile/monthly/YYYY-MM/personality_monthly_evidence_log_codex.csv`

Write these exact output paths for the overall window:
1) `<talent>/stream_summaries/overall_themes/personality_profile/overall/personality_overall_highlights_codex.md`
2) `<talent>/stream_summaries/overall_themes/personality_profile/overall/personality_overall_evidence_log_codex.csv`

Required structure for each monthly markdown file `personality_monthly_highlights_codex.md`:

Start the document with:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Talent: <talent name>`
- `Report window: YYYY-MM`
- `Primary personality source: <v3 current|v2 fallback>`
- `Summary-classification source used: yes|no`
- `Resolved videos in month: <count>`
- `Videos excluded from month due to unresolved date: <count>`

## 1) This Month's Personality Highlights
- 1 to 2 paragraphs.
- Explain what was most visible about the streamer this month.
- Name the 2 to 4 strongest evidence-backed tendencies for this month.
- State confidence: `High`, `Medium`, or `Low`.
- At least one paragraph-level claim should address what felt especially distinctive or non-interchangeable in this month, not just what was most frequent.

## 2) Month-Specific Personality Dimensions
- Present 2 to 5 major dimensions visible this month.
- For each dimension include:
  - `dimension_name`
  - `why this mattered this month`
  - `contributing open codes`
  - `evidence pattern`
  - `supporting quotes`
  - `limits / counterevidence`

## 3) Monthly Markers and Rituals
- Identify repeated markers that were especially active or socially important this month.
- Explain what each marker did socially, not just that it appeared often.

## 4) Relationship Style This Month
- Explain how the streamer positioned themself with chat in this window.
- Address closeness, reciprocity, control, teasing, reassurance, and audience guidance.

## 5) Shifts Relative to Overall Pattern
- Compare this month to the overall baseline.
- State whether the month:
  - reinforced the baseline
  - intensified a known trait
  - revealed a temporary emphasis
  - showed insufficient evidence for meaningful change

## 6) Monthly Limits and Uncertainty
- List thin-evidence areas and unresolved month-assignment limits.

Required structure for the overall markdown file `personality_overall_highlights_codex.md`:

Start the document with:
- `Analysis conducted: YYYY-MM-DD HH:MM TZ`
- `Talent: <talent name>`
- `Report window: overall`
- `Primary personality source: <v3 current|v2 fallback>`
- `Summary-classification source used: yes|no`
- `Resolved monthly windows: <count>`
- `Videos with unresolved month metadata: <count>`

## 1) Overall Personality Highlights
- 1 to 2 paragraphs.
- Explain the streamer’s overall text-visible personality in concrete terms.
- Name the 2 to 4 strongest evidence-backed tendencies.
- State confidence: `High`, `Medium`, or `Low`.
- State what cannot be known from text-only evidence.
- The opening must answer: `what makes this streamer unique compared with the others in this dataset?`
- Do not open with a list of broad shared traits unless each one is immediately specified in this streamer's distinctive form.

## 2) Evidence Base and Synthesis Notes
- Briefly state:
  - which open-coding version was used
  - total retained codes available
  - total evidence rows reviewed
  - whether summary classification reinforced or complicated the picture
  - how many monthly windows were resolved

## 3) Stable Core Personality Dimensions
- Present 3 to 6 major cumulative dimensions.
- For each dimension include:
  - `dimension_name`
  - `why this is a stable personality dimension`
  - `why this is this streamer's version of the pattern`
  - `contributing open codes`
  - `evidence pattern`
  - `supporting quotes`
  - `limits / counterevidence`

## 4) Shifts Over Time
- This section is required.
- Distinguish between:
  - stable traits
  - recently strengthened traits
  - weakened or less-visible traits
  - episodic or month-bound traits
- Use monthly windows as evidence.
- If there is no robust shift evidence, say the profile appears mostly stable over time.

## 5) Relational Style With Chat
- Explain the long-run relationship style with chat.
- Address closeness, response style, authority vs reciprocity, teasing vs reassurance, and interaction guidance.

## 6) Idiosyncratic Markers and Rituals
- Identify repeated textual markers that matter because of what they do socially.
- Explain their personality relevance with quotes from multiple streams where possible.

## 7) Personality Around Money Moments
- Use monetary-context evidence only if enough evidence exists.
- Describe what paid moments reveal about the streamer’s personality style.
- If evidence is sparse, say `insufficient evidence`.

## 8) Cross-Talent Distinctiveness
- Compare this streamer with at least 2 other eligible talents.
- Identify shared traits and then specify the distinctive difference.
- Do not merely list similarities and differences. Explain what combination of traits makes this streamer non-interchangeable.
- Include at least 2 contrasts that are about how the streamer performs a shared behavior differently from peers.
- Include one sentence in this form:
  - `Could be confused with X on Y, but differs in Z.`

## 9) Validity, Limits, and Uncertainty
- List 3 strongest evidence-backed conclusions.
- List 3 uncertainty points.
- Include at least 1 note about the risk of over-reading lexical repetition as personality.
- Include at least 1 note about text-only modality limits.

Required structure for each monthly evidence log `personality_monthly_evidence_log_codex.csv`:
Columns:
- talent
- report_window
- dimension_name
- contributing_open_code
- theme_family
- video_id
- year_month
- time_in_seconds
- timecode
- source
- speaker
- quote
- evidence_role
- personality_relevance

Required structure for the overall evidence log `personality_overall_evidence_log_codex.csv`:
Columns:
- talent
- report_window
- dimension_name
- contributing_open_code
- theme_family
- video_id
- year_month
- time_in_seconds
- timecode
- source
- speaker
- quote
- evidence_role
- personality_relevance

Evidence-log rules:
- These files are filtered synthesis logs, not full recopies of upstream evidence CSVs.
- Include only rows actually used to support the final monthly or overall report.
- Every major report section should be traceable to rows in its matching evidence log.
- Do not invent or rewrite quotes.

Execution rules:
- Process all eligible talents discovered at runtime.
- Create missing output folders if needed.
- Overwrite only target files inside `personality_profile/monthly/` and `personality_profile/overall/`.
- Do not modify raw data or existing open-coding artifacts.

Final report to user:
- For each talent, report:
  - open-coding source used
  - number of resolved monthly windows written
  - retained codes available
  - evidence rows used in monthly and overall synthesis logs
  - whether summary classification was used
  - output paths
- Spot-check at least 2 quoted lines per talent against the source evidence file.
