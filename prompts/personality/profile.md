You are working in this data root:

/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data

Hardcoded talent directories to process:
- /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Avaritia Hawthorne 【Variance Project】
- /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Katya Sable 【Variance Project】
- /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】
- /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Terberri Solaris Ch

Objective:
Preserve the existing monetary analysis, then add a personality-layer analysis that captures each streamer’s unique textual charm, affective style, and relational dynamics with chat.

Important limitations:
- You do NOT have visual cues (facial expression, motion, camera framing).
- You do NOT have full vocal prosody from audio.
- You MUST infer personality only from text evidence (chat logs, streamer chat posts, subtitles, stream summaries).
- Do not over-claim. If evidence is weak, say "insufficient evidence".

Scope:
Process only the 4 hardcoded talent directories listed above.

For each hardcoded talent path, use these exact input paths:
1) `<talent>/stream_summaries/stream_summary_codex/*.md` (context only)
2) `<talent>/text_playback/*.csv` (primary evidence)
3) `<talent>/Chat/Original/*_chat.csv` (primary evidence)
4) `<talent>/stream_summaries/overall_themes/money_timestamps.csv` if present (monetary linkage)

Write outputs to these exact output paths:
1) `<talent>/stream_summaries/overall_themes/personality_profile_codex.md`
2) `<talent>/stream_summaries/overall_themes/personality_evidence_log.csv`

Required structure for `personality_profile_codex.md`:

## 1) Streamer Personality Signature (text-derived)
- 1 paragraph, concrete and non-generic.
- Explicitly state confidence level: High / Medium / Low.
- Include what cannot be known due to modality limits.

## 2) Affective-Relational Codebook (Predefined)
Use these exact baseline codes and meanings. Do not invent new meanings for these labels. You may add extra codes only if clearly justified by repeated evidence.

For each code in the output report, provide:
- Baseline definition (copy from below, then optionally add a brief talent-specific note)
- Inclusion criteria
- Exclusion criteria
- Observed count
- 3 raw-text examples with: `[timecode] speaker (video_id): "quote"`

### PLAYFUL_CHAOS
- Definition: A shared comedic mode where streamer/chat intentionally produce non-literal mismatch text that disrupts literal conversation for playful bonding.
- Inclusion criteria:
  - Require at least 1 non-literal mismatch marker: category collision, impossible/fantastical claim used jokingly, deliberate contradiction, or mock-serious language for trivial events.
  - Plus at least 1 uptake marker: callback/one-up by another speaker, laughter marker (`lol`, `lmao`, `haha`), emote-like playful response, or explicit "chaos/what is happening" framing.
- Exclusion criteria:
  - Simple excitement only (e.g., `LET'S GO!!!`) without non-literal mismatch markers.
  - Routine off-topic chat with no joke continuation.
  - Hostile conflict without affiliative play cues.

### WARM_NURTURANCE
- Definition: Caretaking, reassurance, tenderness, or protective framing toward chat/community.
- Inclusion criteria:
  - At least 1 care marker (`it's okay`, `take your time`, `proud of you`, `you're fine`, `be safe`) directed to a person/group.
  - Or concern-check + comfort sequence (`are you okay?` followed by reassurance).
- Exclusion criteria:
  - Generic politeness (`thanks`, `hi`) without explicit care content.
  - Pure logistics/mod instructions with no emotional support intent.

### INTIMATE_SELF_DISCLOSURE
- Definition: Personal revelation of feelings, vulnerabilities, private experiences, or identity-significant reflections.
- Inclusion criteria:
  - First-person statement + internal-state marker (`I feel`, `I'm scared`, `I've been struggling`, `I was stressed`, `I cried`, `I needed`) with personal stakes.
  - Life-context details that make the statement personally consequential (health, finances, burnout, family, identity, grief, insecurity).
- Exclusion criteria:
  - Surface status updates (`starting soon`, `be right back`, `mic issue`).
  - Lore/performance lines that do not disclose real felt experience.

### PERFORMATIVE_THEATRICALITY
- Definition: Deliberate roleplay/performance voice, dramatic framing, or stage-like presentation style.
- Inclusion criteria:
  - At least 1 stylization marker: mock proclamation, character voice, ceremonial phrasing, scripted bit framing, or dramatic register shift.
  - Often includes performative audience address (`ladies and gentlemen`, `welcome to`, `behold`, etc.) or text stage cues.
- Exclusion criteria:
  - Plain commentary/instruction without roleplay or dramatic register.
  - Excitement alone without theatrical framing.

### COMMANDING_HOST_ENERGY
- Definition: Strong host control over pace, norms, agenda, and audience direction.
- Inclusion criteria:
  - Explicit directive or norm statement (`no spoilers`, `listen up`, `we're moving on`, `do X now`, `stop Y`).
  - Agenda-control markers: segment transitions, priority setting, pacing calls, rule reinforcement.
- Exclusion criteria:
  - Suggestions phrased as low-commitment options.
  - Chat-led drift where streamer does not assert direction.

### CO_REGULATION_WITH_CHAT
- Definition: Streamer and chat mutually modulate emotional intensity (calming, hyping, grounding, recovering).
- Inclusion criteria:
  - Minimum 2-turn reciprocal pattern: one side signals distress/hype/confusion, other side responds with calming/grounding/amplifying language.
  - Evidence of adjustment in subsequent turns (tone softens, focus returns, hype synchronizes, conflict de-escalates).
- Exclusion criteria:
  - One-sided venting/hype with no visible response shift.
  - Single comfort/hype line not embedded in an interaction loop.

### BOUNDARY_SETTING
- Definition: Explicit limit-setting around behavior, topics, expectations, or parasocial conduct.
- Inclusion criteria:
  - Clear limit language (`do not`, `don't`, `not okay`, `I won't`, `we don't do that here`).
  - Norm enforcement or consent boundary with target behavior/topic specified.
- Exclusion criteria:
  - Soft preferences (`I'd rather not`) without enforceable limit.
  - General policy text not tied to interaction context.

### TEASING_BANTER
- Definition: Playful mockery, friendly roasting, or ironic challenge exchanges that signal relational familiarity.
- Inclusion criteria:
  - Playful jab/roast + affiliative cue (laughter marker, emotes, known in-joke, affectionate handle).
  - Prefer reciprocal evidence: target responds in-kind or positively.
- Exclusion criteria:
  - Insults/slurs/derogation without affiliative cues.
  - Mockery followed by defensiveness, silence, or conflict escalation.

### APPRECIATION_RITUAL
- Definition: Repeated gratitude scripts and recognition routines that reinforce community belonging.
- Inclusion criteria:
  - Recurrent gratitude template across multiple streams (same or similar phrasing patterns).
  - Recognition routines: donor/member/name callouts, milestone thanks, closing thank-you sequences.
- Exclusion criteria:
  - Single isolated `thank you` with no repeated structure.
  - Pure acknowledgement that lacks gratitude language.

### MONETARY_MEANING_FRAME
- Definition: The social meaning assigned to money moments (care, joke, obligation, request leverage, celebration, tax, etc.).
- Inclusion criteria:
  - Monetary cue (`paid_message`, amount mention, membership gift) plus meaning cue (e.g., support, joke-tax, apology, celebration, request, duty).
  - Streamer or chat explicitly interprets why money is being sent or how it should be understood.
- Exclusion criteria:
  - Amount-only mention with no interpretive framing.
  - Purely technical payment processing comments.

## 3) Idiosyncrasies and Unique Charm Markers
Identify recurring, distinctive textual markers such as:
- repeated phrases / catchphrases
- characteristic humor pattern
- typical response rhythm to chat
- signature ways of thanking/responding to donations
- recurring relational stance (peer, boss, caretaker, gremlin, etc.)

For each marker:
- why it is distinctive
- 3 supporting raw examples from different streams where possible

## 4) Personality-Monetary Interaction Link
Using monetary events + local context windows, analyze how personality appears around money moments.
- For at least 20 sampled paid events (or all events if <20), compare:
  - pre-donation context (about 60–120 sec before)
  - donation message moment
  - immediate response window (about 60–120 sec after)
- Describe recurring pattern(s): e.g., gratitude ritual, joke framing, emotional softening, escalation, boundary reinforcement.
- Distinguish support-as-care vs support-as-performance vs support-as-request when evidence exists.

## 5) Distinctiveness Test (cross-streamer)
This section must answer: "What makes this streamer non-interchangeable?"
- Compare this streamer against at least 2 other talents in the same dataset.
- Use evidence-backed contrasts (not vibes).
- Include one short "could be confused with X, but differs by Y" note.

## 6) Validity and Uncertainty Notes
- List 3 strongest evidence-backed conclusions.
- List 3 uncertainty points where text-only data may mislead interpretation.

Required structure for `personality_evidence_log.csv`:
Columns:
- talent
- code
- marker
- video_id
- time_in_seconds
- timecode
- source (chat|subtitle|summary)
- speaker
- quote
- evidence_role (primary|supporting|counterexample)

Evidence rules (strict):
- Every analytic claim must map to raw evidence rows in CSV logs.
- Summaries can guide sampling but cannot be the sole evidence for claims.
- Do not invent quotes, timestamps, or donors.
- If a code is absent, report count = 0 and write "no qualifying evidence".

Execution rules:
- Run end-to-end for all 4 hardcoded talents.
- Create missing output folders if needed.
- Overwrite `personality_profile_codex.md` and `personality_evidence_log.csv` only for this personality pass.

Final report to user:
- Per talent: total streams scanned, total evidence rows logged, total monetary events used in section 4, output paths.
- Spot-check at least 2 quoted lines per talent by showing exact source-file matches.
