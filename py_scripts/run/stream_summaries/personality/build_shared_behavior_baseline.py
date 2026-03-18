#!/usr/bin/env python3

import csv
import json
import os
import sys
import textwrap
from datetime import datetime
from zoneinfo import ZoneInfo


TALENT_ROOT = "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data"
OUTPUT_ROOT = (
    "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions"
)
CURRENT_DIR = os.path.join(OUTPUT_ROOT, "current")
SNAPSHOT_DIR = os.path.join(OUTPUT_ROOT, "snapshots")
TIMEZONE = ZoneInfo("America/New_York")

PROMPT_TEXT = """You are working across these data roots:

Primary talent data root:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data`

Cross-talent processed output root:
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions`

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
- Treat `summary_classification/current` as a secondary stabilizing layer when needed.
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

Secondary input layer: `summary_classification`
- `<talent>/stream_summaries/overall_themes/summary_classification/current/overall_themes_codex.md`
- `<talent>/stream_summaries/overall_themes/summary_classification/current/summary_classification_state.json` if present

Input precedence rules:
- `open_codebook_*` and `open_coding_evidence_*` are the primary basis for identifying shared patterns.
- `personality_profile_v*_open_coding.md` may be used to recover memo logic or family summaries but must not substitute for evidence rows.
- `summary_classification/current/overall_themes_codex.md` can support judgments about stability or coverage but cannot create a shared theme on its own.

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
- Spot-check at least 2 shared-behavior quotes from different talents by showing exact source-file matches."""

BEHAVIORS = [
    {
        "shared_behavior_name": "gratitude ritualization",
        "classification": "shared_baseline",
        "definition": "Recurring supporter recognition turns in which the streamer explicitly thanks, names, or ceremonially acknowledges audience support.",
        "inclusion_criteria": "Include repeated thank-you loops, named supporter recognition, and appreciation turns that visibly organize the social exchange around support events.",
        "exclusion_criteria": "Exclude one-off courtesy thanks with no repeated ritual structure and exclude hype language that does not actually acknowledge supporters.",
        "baseline_relevance_note": "All four talents repeatedly turn support into an interactional ritual, so later uniqueness profiling should not treat thanking or supporter acknowledgment as unique on its own.",
        "why_this_belongs_in_baseline": "This is the clearest cross-talent common pattern: support is rarely treated as background noise, and each streamer repeatedly converts it into a visible social loop.",
        "code_labels": [
            "thank ritual",
            "thanks ritual",
            "appreciate ritual",
            "grateful ritual",
            "ty ritual",
        ],
        "talent_versions": {
            "Avaritia Hawthorne 【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Pairs thanks with dramatic membership ceremony and fast roll-call style recognition.",
                "uniqueness_candidate_note": "The unique signal is the mock-grandiose welcome framing attached to the thanks, not the presence of thanking itself.",
            },
            "Katya Sable 【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Acknowledgment is brisk and sardonic, often sounding like a dry salute rather than a soft gratitude moment.",
                "uniqueness_candidate_note": "What may later profile as distinctive is the clipped, deadpan delivery layered onto the thanks.",
            },
            "Leia Memoria【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Thanks are warm and ceremonial, often framed as welcome, blessing, or appreciative companionship.",
                "uniqueness_candidate_note": "Leia's time-zone blessing and hospitable welcome language are the distinctive hook, not gratitude itself.",
            },
            "Terberri Solaris Ch": {
                "support_level": "strong",
                "talent_specific_version": "Recognition leans communal and reciprocal, with repeated 'appreciate y'all' language and check-in gratitude.",
                "uniqueness_candidate_note": "The distinctiveness lies in the communal reciprocity tone and self-care/check-in framing around the thanks.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Avaritia Hawthorne 【Variance Project】",
                "code_label": "thank ritual",
                "video_id": "-6_UhwQ_mgQ",
                "timecode": "00:10:36",
                "why": "Shows explicit, repeated gratitude as a foregrounded ritual rather than a passing courtesy.",
            },
            {
                "talent": "Katya Sable 【Variance Project】",
                "code_label": "thank ritual",
                "video_id": "-E2Uhivz2EA",
                "timecode": "00:11:14",
                "why": "Shows direct supporter acknowledgment in Katya's own clipped register.",
            },
            {
                "talent": "Leia Memoria【Variance Project】",
                "code_label": "thank ritual",
                "video_id": "-a2fAj67p10",
                "timecode": "00:04:30",
                "why": "Shows gratitude as a warm welcome ritual rather than generic politeness.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "appreciate ritual",
                "video_id": "-SyviYaElhM",
                "timecode": "00:30:25",
                "why": "Shows explicit appreciation framed as a communal waiting/participation acknowledgment.",
            },
        ],
    },
    {
        "shared_behavior_name": "audience-attentive pacing control",
        "classification": "shared_baseline",
        "definition": "Visible steering of chat attention, segment timing, or interaction flow so the streamer can redirect, delay, or refocus the audience in real time.",
        "inclusion_criteria": "Include explicit redirection, 'hold on' management, focus commands, timing resets, and pace-setting turns that structure what the audience should attend to next.",
        "exclusion_criteria": "Exclude purely game-mechanical time references or private task narration with no visible audience-management function.",
        "baseline_relevance_note": "Flow control is routine across the set, so later profiles should not treat ordinary pace-steering or redirective audience management as distinctive by default.",
        "why_this_belongs_in_baseline": "Each talent repeatedly manages interaction tempo rather than letting chat or gameplay drift without commentary. The shared function is strong even though the tone differs.",
        "code_labels": [
            "request pace",
            "time pace",
            "focus pace",
            "mods pace",
            "listen up pace",
            "starting now pace",
            "next up pace",
        ],
        "talent_versions": {
            "Avaritia Hawthorne 【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Often interrupts herself to reset sequence, check counters, or re-stage the next interaction beat with chat.",
                "uniqueness_candidate_note": "What may later matter is the theatrical, stop-start re-staging style layered onto otherwise common flow control.",
            },
            "Katya Sable 【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Uses more command-style pacing and sharper redirects, including timeout talk and blunt transition control.",
                "uniqueness_candidate_note": "Katya's distinctiveness is the abrasive or disciplinary edge of the pacing, not pace control itself.",
            },
            "Leia Memoria【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Pacing often sounds like coaching or rallying language: 'focus,' 'lock in,' and collective refocusing.",
                "uniqueness_candidate_note": "Leia's trainer-like coaching cadence is the likely uniqueness hook, not the existence of focus management.",
            },
            "Terberri Solaris Ch": {
                "support_level": "moderate",
                "talent_specific_version": "Flow control is gentler and more self-disclosed, often explaining the need to refocus rather than issuing hard commands.",
                "uniqueness_candidate_note": "Terberri's likely differentiator is the softer, self-explanatory style of redirection.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Avaritia Hawthorne 【Variance Project】",
                "code_label": "request pace",
                "video_id": "6UuPyE0eBV8",
                "timecode": "01:19:13",
                "why": "Shows active sequencing and audience reorientation around the next visible interaction beat.",
            },
            {
                "talent": "Katya Sable 【Variance Project】",
                "code_label": "request pace",
                "video_id": "2_5PUIzN4tM",
                "timecode": "00:03:00",
                "why": "Shows explicit control over when the interaction should move forward.",
            },
            {
                "talent": "Leia Memoria【Variance Project】",
                "code_label": "focus pace",
                "video_id": "-a2fAj67p10",
                "timecode": "00:14:54",
                "why": "Shows direct audience-facing refocusing language that structures the collective pace.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "focus pace",
                "video_id": "-SyviYaElhM",
                "timecode": "00:24:26",
                "why": "Shows flow control through explicit self-explanation and temporary refocus rather than pure command.",
            },
        ],
    },
    {
        "shared_behavior_name": "reassurance during strain or recovery",
        "classification": "shared_baseline",
        "definition": "Repeated turns that normalize setbacks, soothe distress, or reassure the audience, the streamer, or an on-stream target during frustration, embarrassment, or fatigue.",
        "inclusion_criteria": "Include 'it's okay,' 'don't worry,' care-taking reassurances, worth/comfort statements, and repeated calming turns that clearly regulate tension.",
        "exclusion_criteria": "Exclude neutral status updates or generic positivity with no visible soothing or normalizing function.",
        "baseline_relevance_note": "Reassurance is common enough across the set that later workflows should only treat it as unique when the form or relationship function is unusually distinctive.",
        "why_this_belongs_in_baseline": "The exact tone varies, but all four streamers visibly spend interactional effort lowering tension after mistakes, awkwardness, or strain.",
        "code_labels": [
            "it's okay care",
            "don't worry care",
            "take care care",
            "got care",
            "okay care",
            "proud care",
            "no worries care",
            "safe care",
            "take time care",
            "drink water care",
        ],
        "talent_versions": {
            "Avaritia Hawthorne 【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Reassurance often arrives in bantering or dramatic language that still works to defuse tension.",
                "uniqueness_candidate_note": "Avaritia's distinctiveness is the humorous or mock-menacing wrapper around the soothing move.",
            },
            "Katya Sable 【Variance Project】": {
                "support_level": "moderate",
                "talent_specific_version": "Care exists, but it is thinner and more deflective, often sounding like brusque damage control rather than overt nurturance.",
                "uniqueness_candidate_note": "Katya may later stand out for how reassurance is folded into blunt dismissal rather than sustained care-taking.",
            },
            "Leia Memoria【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Reassurance is sustained and repetitive, often sounding openly nurturing toward chat or on-stream characters.",
                "uniqueness_candidate_note": "Leia's likely unique hook is the repeated maternal or caretaking cadence layered onto otherwise shared reassurance.",
            },
            "Terberri Solaris Ch": {
                "support_level": "strong",
                "talent_specific_version": "Care language is affirming and worth-focused, often sounding like gentle check-in support.",
                "uniqueness_candidate_note": "Terberri's differentiator is the explicit worth/comfort language and softer communal reassurance style.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Avaritia Hawthorne 【Variance Project】",
                "code_label": "it's okay care",
                "video_id": "-6_UhwQ_mgQ",
                "timecode": "01:33:57",
                "why": "Shows explicit normalization and soothing, even while keeping Avaritia's joking tone.",
            },
            {
                "talent": "Katya Sable 【Variance Project】",
                "code_label": "don't worry care",
                "video_id": "-E2Uhivz2EA",
                "timecode": "00:53:27",
                "why": "Shows direct audience reassurance in Katya's shorter, more clipped form.",
            },
            {
                "talent": "Leia Memoria【Variance Project】",
                "code_label": "it's okay care",
                "video_id": "-a2fAj67p10",
                "timecode": "03:24:40",
                "why": "Shows extended repeated soothing as a clear tension-regulation move.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "it's okay care",
                "video_id": "17ElKDZTHAM",
                "timecode": "00:49:35",
                "why": "Shows reassurance framed as direct affirmation of another's worth.",
            },
        ],
    },
    {
        "shared_behavior_name": "audience-facing self-disclosure windows",
        "classification": "shared_but_distinctive_in_form",
        "definition": "Recurring first-person disclosure of needs, feelings, anxiety, overwhelm, or recovery states that is made visible to the audience rather than kept off-stage.",
        "inclusion_criteria": "Include first-person disclosures of anxiety, strain, tiredness, overwhelm, or personal need when they function as audience-facing transparency or relational context.",
        "exclusion_criteria": "Exclude purely mechanical task narration unless the line clearly frames the streamer's state, vulnerability, or felt difficulty.",
        "baseline_relevance_note": "The dataset should not treat basic audience-facing need/feeling disclosure as unique by default, but the meaning of those disclosures differs enough by talent that later uniqueness work still matters.",
        "why_this_belongs_in_baseline": "All four talents recurrently expose some part of their inner state, but the open-coding layer shows that those disclosures range from overt vulnerability to more instrumental self-narration.",
        "code_labels": [
            "need disclosure",
            "feel disclosure",
            "needed disclosure",
            "anxiety disclosure",
            "i'm scared disclosure",
            "i'm tired disclosure",
            "i'm worried disclosure",
            "i'm sad disclosure",
            "cried disclosure",
            "depressed disclosure",
            "depressing disclosure",
            "mental health disclosure",
            "i'm anxious disclosure",
            "i'm stressed disclosure",
            "stressed disclosure",
            "burnout disclosure",
            "tired disclosure",
        ],
        "talent_versions": {
            "Avaritia Hawthorne 【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Disclosures often foreground anxiety, frustration, or discomfort while staying performative and chat-aware.",
                "uniqueness_candidate_note": "The likely unique signal is how vulnerability is mixed with dramatic bite rather than the existence of disclosure itself.",
            },
            "Katya Sable 【Variance Project】": {
                "support_level": "moderate",
                "talent_specific_version": "Self-disclosure is present but more sardonic and irritation-forward, often embedded inside complaint or rant language.",
                "uniqueness_candidate_note": "Katya may later differentiate through the acerbic, grievance-shaped form of disclosure.",
            },
            "Leia Memoria【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Disclosures frequently read as transparent updates about anxiety, pressure, or how a moment is landing emotionally.",
                "uniqueness_candidate_note": "Leia's distinctiveness is the openness and gentle relational framing of those disclosures.",
            },
            "Terberri Solaris Ch": {
                "support_level": "strong",
                "talent_specific_version": "Self-disclosure often sounds like overwhelm, self-check-in, or soft explanation for pacing or focus changes.",
                "uniqueness_candidate_note": "Terberri's likely unique hook is the check-in tone that turns disclosure into communal regulation.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Avaritia Hawthorne 【Variance Project】",
                "code_label": "anxiety disclosure",
                "video_id": "-DKfOVi3wyY",
                "timecode": "00:16:42",
                "why": "Shows direct audience-facing naming of anxiety rather than leaving internal state implicit.",
            },
            {
                "talent": "Katya Sable 【Variance Project】",
                "code_label": "feel disclosure",
                "video_id": "-E2Uhivz2EA",
                "timecode": "01:18:28",
                "why": "Shows self-state disclosure embedded in Katya's more irritated, pressure-oriented delivery.",
            },
            {
                "talent": "Leia Memoria【Variance Project】",
                "code_label": "anxiety disclosure",
                "video_id": "L3x-_p2UE3c",
                "timecode": "00:11:00",
                "why": "Shows clear naming of social anxiety as audience-facing context.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "feel disclosure",
                "video_id": "-SyviYaElhM",
                "timecode": "00:37:06",
                "why": "Shows open acknowledgment of overwhelm as a relational explanation for the moment.",
            },
        ],
    },
    {
        "shared_behavior_name": "performative teasing and welcome escalation",
        "classification": "shared_but_distinctive_in_form",
        "definition": "Recurring use of joking address terms, welcome lines, or exaggerated bits that turn chat contact into a stylized performance move.",
        "inclusion_criteria": "Include repeated welcome slogans, teasing pet names, exaggerated mock-threats, and performative escalation used to manage rapport or hype up interaction.",
        "exclusion_criteria": "Exclude isolated slang, catchphrases with no interactional function, or generic excitement that does not shape the social relationship with chat.",
        "baseline_relevance_note": "Playful escalation is shared, but the form is highly talent-shaped. Later profiles should treat the shared behavior as common while preserving each talent's specific style of play.",
        "why_this_belongs_in_baseline": "All four talents repeatedly stylize audience contact through bits and teasing, but the relationship function differs enough that this behavior is common without being interchangeable.",
        "code_labels": [
            "baby bit",
            "welcome bit",
            "blood bit",
            "kill bit",
            "darling bit",
            "queen bit",
            "bite bit",
            "wild bit",
            "dear bit",
            "bozo bit",
            "skill issue bit",
            "nerd bit",
            "chaos bit",
            "goblin bit",
            "gremlin bit",
            "cursed bit",
            "behold bit",
            "stinky bit",
            "lol bit",
            "ceremony bit",
            "minions bit",
            "what happening bit",
            "wtf bit",
            "loser bit",
            "clown bit",
            "coward bit",
            "eat bit",
            "haha bit",
            "lmao bit",
            "unhinged bit",
        ],
        "talent_versions": {
            "Avaritia Hawthorne 【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Escalation is mock-ominous and theatrical, with repeated ceremonial welcome slogans and darker joke color.",
                "uniqueness_candidate_note": "The likely unique hook is the grandiose menace and ritual welcome framing, not teasing itself.",
            },
            "Katya Sable 【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Bits skew abrasive, deadpan, and insult-flavored, turning chat contact into sardonic banter.",
                "uniqueness_candidate_note": "Katya's uniqueness lies in the sharper insult register and sardonic flirt/taunt pattern.",
            },
            "Leia Memoria【Variance Project】": {
                "support_level": "strong",
                "talent_specific_version": "Escalation often sounds affectionate or caretaking, especially when 'baby' language becomes protective or nurturing.",
                "uniqueness_candidate_note": "Leia's unique signal is the affectionate, protective version of the common teasing/welcome move.",
            },
            "Terberri Solaris Ch": {
                "support_level": "moderate",
                "talent_specific_version": "Bits are present but more situational, often tied to welcome frames, randomizer/twist language, or brief playful jabs.",
                "uniqueness_candidate_note": "Terberri's differentiator is the event-frame or challenge-frame version of welcome escalation.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Avaritia Hawthorne 【Variance Project】",
                "code_label": "welcome bit",
                "video_id": "-6_UhwQ_mgQ",
                "timecode": "00:01:24",
                "why": "Shows a repeated ceremonial welcome line used to stylize audience entry.",
            },
            {
                "talent": "Katya Sable 【Variance Project】",
                "code_label": "baby bit",
                "video_id": "3YIh6ASfaLo",
                "timecode": "01:51:27",
                "why": "Shows teasing address as an affiliative-but-sharp performance move.",
            },
            {
                "talent": "Leia Memoria【Variance Project】",
                "code_label": "baby bit",
                "video_id": "-a2fAj67p10",
                "timecode": "02:56:52",
                "why": "Shows the same escalation family functioning as affectionate protectiveness rather than mock-threat.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "welcome bit",
                "video_id": "5yq9VTcBm3o",
                "timecode": "01:19:51",
                "why": "Shows stylized welcome language used as playful scene-setting for the interaction.",
            },
        ],
    },
]

NON_BASELINE_PATTERNS = [
    {
        "name": "guardrail enforcement",
        "judgment": "not_shared_enough_for_baseline",
        "reason": "All four talents show some boundary-setting, but the retained guardrail families are much smaller than the main baseline patterns and are often tied to narrow moderation or spoiler contexts.",
        "follow_up": "Leave this for talent-specific uniqueness work or local context analysis rather than treating it as a dataset-wide baseline.",
        "selectors": [
            {
                "talent": "Avaritia Hawthorne 【Variance Project】",
                "code_label": "don't ask guardrail",
                "video_id": "3rudZU5OxdI",
                "timecode": "00:17:08",
            },
            {
                "talent": "Katya Sable 【Variance Project】",
                "code_label": "stop asking guardrail",
                "video_id": "-E2Uhivz2EA",
                "timecode": "02:07:32",
            },
            {
                "talent": "Leia Memoria【Variance Project】",
                "code_label": "no spoilers guardrail",
                "video_id": "BZ_0Pqccang",
                "timecode": "01:14:28",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "don't spam guardrail",
                "video_id": "1auJ3fgkihs",
                "timecode": "03:23:09",
            },
        ],
    },
    {
        "name": "explicit co-regulation loops",
        "judgment": "not_shared_enough_for_baseline",
        "reason": "Co-regulation loop families exist, but they are sparse and uneven across the set, with especially thin evidence in Katya and Terberri compared with the main retained families.",
        "follow_up": "Treat these as context-specific texture rather than a core baseline until more upstream evidence accumulates.",
        "selectors": [],
    },
    {
        "name": "money-linked framing",
        "judgment": "not_shared_enough_for_baseline",
        "reason": "Money framing appears in only three talents and at very low retained counts, so it is too narrow and event-bound to stabilize a cross-streamer baseline.",
        "follow_up": "Keep it outside the baseline unless a later rerun finds materially broader support.",
        "selectors": [],
    },
]

SUMMARY_STABILIZER_NOTE = (
    "Current summary-classification files broadly stabilize the same cross-talent picture: "
    "Avaritia, Katya, and Leia all retain rapid flow steering, performative hype/bits, "
    "active chat acknowledgment, warm reassurance, and support-driven momentum, while "
    "Terberri retains performative hype/bits, gentle pace steering, and active chat "
    "acknowledgment. That secondary layer supports the main baseline judgments without "
    "creating new ones."
)

TALENT_CONTRAST_NOTES = {
    "Avaritia Hawthorne 【Variance Project】": (
        "Avaritia clearly shares the baseline through overt thanking, active flow resets, "
        "and repeated reassurance. The more distinctive shared forms are the mock-ominous "
        "welcome ceremony and the way anxiety or discomfort is aired through dramatic, biting language. "
        "Likely uniqueness candidates remain in the darker theatrical framing, sharper guardrails, "
        "and bone-zone or menace-coded persona language that sit outside the shared baseline itself."
    ),
    "Katya Sable 【Variance Project】": (
        "Katya shares the baseline through supporter acknowledgment, pacing control, and recurring "
        "though thinner reassurance. Shared behaviors become distinctive when they turn abrasive: "
        "pacing sounds more disciplinary, teasing sounds more sardonic, and self-disclosure is often "
        "complaint- or irritation-shaped. Likely uniqueness candidates remain in the harsher ejective "
        "moderation style, insult-flavored banter, and rant-driven political or grievance cadence."
    ),
    "Leia Memoria【Variance Project】": (
        "Leia strongly shares all retained baseline behaviors, especially gratitude, focus control, "
        "reassurance, and audience-facing disclosure. Her distinctive shared forms are the coach-like "
        "'focus/lock in' pacing, the openly nurturing reassurance, and the affectionate 'my baby' style "
        "of escalation. Likely uniqueness candidates remain in the maternal-protective framing, blessing/welcome "
        "language, and trainer/idol presentation that exceeds the common baseline."
    ),
    "Terberri Solaris Ch": (
        "Terberri shares the baseline most clearly through communal gratitude, gentle pace steering, "
        "open self-check-in disclosure, and affirming reassurance. The more distinctive shared forms are "
        "the communal 'appreciate y'all' reciprocity and welcome framing tied to twists, randomizers, or check-ins. "
        "Likely uniqueness candidates remain in the softer self-care rhetoric, communal affirmation style, "
        "and anti-spam/moderation language that is present but not baseline."
    ),
}


def talent_sort_key(name):
    return name.lower()


def read_csv_rows(path):
    with open(path, newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def write_csv(path, fieldnames, rows):
    with open(path, "w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def read_text(path):
    with open(path, encoding="utf-8") as handle:
        return handle.read()


def get_talent_dirs():
    talents = []
    for entry in os.listdir(TALENT_ROOT):
        full_path = os.path.join(TALENT_ROOT, entry)
        if not os.path.isdir(full_path):
            continue
        if entry == "VarianceProject":
            continue
        talents.append(entry)
    return sorted(talents, key=talent_sort_key)


def pick_open_coding_paths(talent):
    base = os.path.join(TALENT_ROOT, talent, "stream_summaries", "overall_themes")
    candidates = [
        {
            "version": "v3",
            "codebook": os.path.join(
                base,
                "personality_open_coding",
                "v3",
                "current",
                "open_codebook_v3.csv",
            ),
            "evidence": os.path.join(
                base,
                "personality_open_coding",
                "v3",
                "current",
                "open_coding_evidence_v3.csv",
            ),
            "markdown": os.path.join(
                base,
                "personality_open_coding",
                "v3",
                "current",
                "personality_profile_v3_open_coding.md",
            ),
        },
        {
            "version": "v2",
            "codebook": os.path.join(
                base, "personality_open_coding", "v2", "open_codebook_v2.csv"
            ),
            "evidence": os.path.join(
                base, "personality_open_coding", "v2", "open_coding_evidence_v2.csv"
            ),
            "markdown": os.path.join(
                base, "personality_open_coding", "v2", "personality_profile_v2_open_coding.md"
            ),
        },
    ]
    for candidate in candidates:
        if all(os.path.exists(candidate[key]) for key in ("codebook", "evidence", "markdown")):
            return candidate
    return None


def parse_summary_themes(summary_md_path):
    if not summary_md_path or not os.path.exists(summary_md_path):
        return []
    for line in read_text(summary_md_path).splitlines():
        if line.startswith("- recurring themes retained:"):
            theme_blob = line.split(":", 1)[1].strip()
            return [part.strip() for part in theme_blob.split(",") if part.strip()]
    return []


def load_talent_context(talent):
    chosen = pick_open_coding_paths(talent)
    if not chosen:
        return None

    base = os.path.join(TALENT_ROOT, talent, "stream_summaries", "overall_themes")
    summary_md = os.path.join(
        base, "summary_classification", "current", "overall_themes_codex.md"
    )
    summary_state = os.path.join(
        base, "summary_classification", "current", "summary_classification_state.json"
    )

    codebook_rows = read_csv_rows(chosen["codebook"])
    evidence_rows = read_csv_rows(chosen["evidence"])

    for row in codebook_rows:
        row["frequency_count_num"] = int(float(row.get("frequency_count") or 0))
        row["stream_coverage_count_num"] = int(float(row.get("stream_coverage_count") or 0))

    return {
        "talent": talent,
        "open_coding_version": chosen["version"],
        "codebook_path": chosen["codebook"],
        "evidence_path": chosen["evidence"],
        "markdown_path": chosen["markdown"],
        "summary_md_path": summary_md if os.path.exists(summary_md) else None,
        "summary_state_path": summary_state if os.path.exists(summary_state) else None,
        "summary_themes": parse_summary_themes(summary_md),
        "codebook_rows": codebook_rows,
        "evidence_rows": evidence_rows,
    }


def find_evidence_row(contexts, selector):
    rows = contexts[selector["talent"]]["evidence_rows"]
    matches = []
    for row in rows:
        if row.get("code_label") != selector["code_label"]:
            continue
        if row.get("video_id") != selector["video_id"]:
            continue
        if row.get("timecode") != selector["timecode"]:
            continue
        matches.append(row)
    if len(matches) != 1:
        raise RuntimeError(
            "Expected exactly one evidence row for "
            f"{selector['talent']} {selector['code_label']} {selector['video_id']} {selector['timecode']}, "
            f"found {len(matches)}"
        )
    return matches[0]


def matching_codebook_rows(context, code_labels):
    code_set = set(code_labels)
    return [row for row in context["codebook_rows"] if row["code_label"] in code_set]


def count_supporting_talents(behavior, eligible_talents):
    return sum(
        1
        for talent in eligible_talents
        if behavior["talent_versions"].get(talent, {}).get("support_level") != "absent"
    )


def supporting_talents_string(behavior, eligible_talents):
    strong = [
        talent
        for talent in eligible_talents
        if behavior["talent_versions"].get(talent, {}).get("support_level") == "strong"
    ]
    return ", ".join(strong) if strong else "none"


def weaker_or_absent_string(behavior, eligible_talents):
    weaker = []
    for talent in eligible_talents:
        support_level = behavior["talent_versions"].get(talent, {}).get("support_level", "absent")
        if support_level != "strong":
            weaker.append(f"{talent} ({support_level})")
    return ", ".join(weaker) if weaker else "none"


def variation_note(behavior, eligible_talents):
    parts = []
    for talent in eligible_talents:
        version = behavior["talent_versions"].get(talent, {}).get("talent_specific_version")
        if version:
            parts.append(f"{talent}: {version}")
    return " | ".join(parts)


def summarize_behavior_support(contexts, behavior, eligible_talents):
    lines = []
    for talent in eligible_talents:
        rows = matching_codebook_rows(contexts[talent], behavior["code_labels"])
        total_frequency = sum(row["frequency_count_num"] for row in rows)
        max_coverage = max((row["stream_coverage_count_num"] for row in rows), default=0)
        support_level = behavior["talent_versions"].get(talent, {}).get("support_level", "absent")
        lines.append(
            f"{talent}: {support_level}; retained frequency {total_frequency}; max stream coverage {max_coverage}"
        )
    return " | ".join(lines)


def markdown_quote_line(talent, row):
    return f'- {talent} [{row["video_id"]} {row["timecode"]}]: "{row["quote"]}"'


def generate_markdown(analysis_dt, eligible_talents, contexts, evidence_rows_by_behavior):
    secondary_used = "yes" if any(contexts[t]["summary_md_path"] for t in eligible_talents) else "no"
    shared_baseline_count = sum(
        1 for behavior in BEHAVIORS if behavior["classification"] == "shared_baseline"
    )
    distinctive_count = sum(
        1
        for behavior in BEHAVIORS
        if behavior["classification"] == "shared_but_distinctive_in_form"
    )

    lines = [
        f"Analysis conducted: {analysis_dt.strftime('%Y-%m-%d %H:%M %Z')}",
        f"Eligible talents included: {len(eligible_talents)}",
        "Primary upstream source: personality_open_coding",
        f"Secondary source used: summary_classification {secondary_used}",
        "",
        "## 1) Cross-Streamer Shared Interaction Baseline",
        (
            "Across the eligible talents, the clearest shared baseline is not a generic "
            "'streamer personality' but a set of repeated interactional routines. All four "
            "talents ritualize audience support, repeatedly steer pacing in public, and use "
            "reassurance to regulate awkwardness, mistakes, or strain. Those three behaviors "
            "are strong enough to treat as baseline/common rather than as evidence of uniqueness."
        ),
        "",
        (
            "Two other patterns are shared but should be carried forward as variation-sensitive. "
            "All four talents open audience-facing self-disclosure windows, but the form ranges "
            "from explicit anxiety or overwhelm to more sardonic, task-embedded self-narration. "
            "Likewise, all four use performative teasing or welcome escalation, but the social function "
            "changes by talent: ominous ceremony, abrasive banter, affectionate caretaking, or situational scene-setting."
        ),
        "",
        (
            "Several visible patterns should not be treated as baseline. Guardrail enforcement is "
            "present across the set but too sparse and context-bound to stabilize a cross-streamer norm. "
            "Explicit co-regulation loops are even thinner, and money-linked framing is too weakly shared. "
            "This baseline is useful for later uniqueness profiling because it raises the threshold: a later workflow "
            "must show not just that a talent thanks, redirects, reassures, discloses, or teases, but how their version differs from the common set."
        ),
        "",
        f"Secondary stabilizer note: {SUMMARY_STABILIZER_NOTE}",
        "",
        "## 2) Shared Behavior Codebook",
    ]

    for behavior in BEHAVIORS:
        evidence_rows = evidence_rows_by_behavior[behavior["shared_behavior_name"]]
        lines.extend(
            [
                f"### {behavior['shared_behavior_name']}",
                f"- shared_behavior_name: {behavior['shared_behavior_name']}",
                f"- classification: {behavior['classification']}",
                f"- definition: {behavior['definition']}",
                f"- inclusion_criteria: {behavior['inclusion_criteria']}",
                f"- exclusion_criteria: {behavior['exclusion_criteria']}",
                f"- talent_count: {count_supporting_talents(behavior, eligible_talents)}",
                f"- supporting_talents: {supporting_talents_string(behavior, eligible_talents)}",
                f"- weaker_or_absent_talents: {weaker_or_absent_string(behavior, eligible_talents)}",
                f"- why this belongs in the baseline: {behavior['why_this_belongs_in_baseline']}",
                f"- how it varies across talents: {variation_note(behavior, eligible_talents)}",
                "- supporting quotes:",
            ]
        )
        for talent in eligible_talents:
            row = evidence_rows[talent]
            lines.append(markdown_quote_line(talent, row))
        lines.append(
            f"- comparison support summary: {summarize_behavior_support(contexts, behavior, eligible_talents)}"
        )
        lines.append("")

    lines.extend(
        [
            "## 3) Shared-But-Different Patterns",
            (
                "The strongest variation-sensitive pattern is audience-facing self-disclosure. "
                "Avaritia and Terberri often expose anxiety, overwhelm, or discomfort quite directly, "
                "Leia tends to make those states legible in a gentler, more relational way, and Katya's "
                "version more often arrives as pressure, irritation, or complaint. The shared behavior is real, "
                "but later uniqueness profiling should focus on how disclosure is framed and what relationship work it does."
            ),
            "",
            (
                "Performative teasing and welcome escalation is also clearly shared without being interchangeable. "
                "Avaritia's escalation is grandiose and mock-ominous, Katya's is sharper and more insult-flavored, "
                "Leia's is affectionate and protective, and Terberri's is more situational and frame-setting. "
                "These are common enough to block false uniqueness claims, but different enough that delivery and relationship function still matter."
            ),
            "",
            (
                "Even some baseline behaviors vary in useful ways. Gratitude is common, yet it can sound like ceremony, "
                "dry salute, blessing, or communal appreciation; pacing control can sound disciplinary, theatrical, coach-like, "
                "or gently self-explanatory. Those differences should feed later talent-specific profiling only after the shared baseline is acknowledged."
            ),
            "",
            "## 4) Non-Baseline or Weakly Shared Patterns",
        ]
    )

    for pattern in NON_BASELINE_PATTERNS:
        lines.extend(
            [
                f"### {pattern['name']}",
                f"- classification: {pattern['judgment']}",
                f"- why not baseline: {pattern['reason']}",
                f"- downstream handling: {pattern['follow_up']}",
            ]
        )
        if pattern["selectors"]:
            lines.append("- example evidence:")
            for selector in pattern["selectors"]:
                row = find_evidence_row(contexts, selector)
                lines.append(markdown_quote_line(selector["talent"], row))
        else:
            lines.append("- example evidence: insufficient evidence for a stronger retained cross-talent quote set.")
        lines.append("")

    lines.extend(["## 5) Talent Contrast Notes"])
    for talent in eligible_talents:
        lines.append(f"- {talent}: {TALENT_CONTRAST_NOTES[talent]}")

    lines.extend(
        [
            "",
            "## 6) Method Notes and Uncertainty",
            "- 3 strongest evidence-backed baseline conclusions:",
            "- Ritualized supporter acknowledgment is the strongest common interaction routine across all four talents.",
            "- Public pacing control is a true shared norm even though its tone ranges from blunt command to gentle self-explanation.",
            "- Reassurance during strain or recovery is broadly shared enough to count as baseline, especially in Avaritia, Leia, and Terberri.",
            "- 3 uncertainty points:",
            "- The self-disclosure families are analytically useful but some upstream rows mix emotional disclosure with task narration, so those judgments remain cautious.",
            "- This workflow is text-only, so delivery differences that depend on facial expression, timing, or vocal warmth cannot be claimed here.",
            "- Guardrails appear in all four talents, but the evidence is too sparse and context-specific to claim a stable dataset-wide boundary style.",
            "- 1 note about the risk of flattening distinct streamers into generic shared traits:",
            "- The main risk is treating common streamer behaviors such as thanking, redirecting, or joking with chat as if they erase talent-specific style; this baseline is intentionally narrow so later profiles can compare form, function, and exceptions instead of collapsing everyone into an average persona.",
            "",
            f"Retained shared behaviors: {len(BEHAVIORS)} total; {shared_baseline_count} shared_baseline; {distinctive_count} shared_but_distinctive_in_form.",
        ]
    )

    return "\n".join(lines).rstrip() + "\n"


def build_codebook_csv_rows(eligible_talents):
    rows = []
    for behavior in BEHAVIORS:
        rows.append(
            {
                "shared_behavior_name": behavior["shared_behavior_name"],
                "classification": behavior["classification"],
                "definition": behavior["definition"],
                "inclusion_criteria": behavior["inclusion_criteria"],
                "exclusion_criteria": behavior["exclusion_criteria"],
                "talent_count": count_supporting_talents(behavior, eligible_talents),
                "supporting_talents": supporting_talents_string(behavior, eligible_talents),
                "weaker_or_absent_talents": weaker_or_absent_string(behavior, eligible_talents),
                "variation_note": variation_note(behavior, eligible_talents),
                "baseline_relevance_note": behavior["baseline_relevance_note"],
            }
        )
    return rows


def build_matrix_csv_rows(eligible_talents):
    rows = []
    for talent in eligible_talents:
        for behavior in BEHAVIORS:
            version = behavior["talent_versions"][talent]
            rows.append(
                {
                    "talent": talent,
                    "shared_behavior_name": behavior["shared_behavior_name"],
                    "classification": behavior["classification"],
                    "support_level": version["support_level"],
                    "talent_specific_version": version["talent_specific_version"],
                    "uniqueness_candidate_note": version["uniqueness_candidate_note"],
                }
            )
    return rows


def build_evidence_rows(contexts):
    output_rows = []
    behavior_evidence_lookup = {}
    for behavior in BEHAVIORS:
        behavior_rows = {}
        for selector in behavior["evidence_selectors"]:
            row = find_evidence_row(contexts, selector)
            behavior_rows[selector["talent"]] = row
            output_rows.append(
                {
                    "shared_behavior_name": behavior["shared_behavior_name"],
                    "classification": behavior["classification"],
                    "talent": selector["talent"],
                    "contributing_open_code": row["code_label"],
                    "theme_family": row["theme_family"],
                    "video_id": row["video_id"],
                    "time_in_seconds": row["time_in_seconds"],
                    "timecode": row["timecode"],
                    "source": row["source"],
                    "speaker": row["speaker"],
                    "quote": row["quote"],
                    "evidence_role": row["evidence_role"],
                    "why_this_supports_shared_behavior": selector["why"],
                }
            )
        behavior_evidence_lookup[behavior["shared_behavior_name"]] = behavior_rows
    return output_rows, behavior_evidence_lookup


def ensure_output_dirs():
    os.makedirs(CURRENT_DIR, exist_ok=True)
    os.makedirs(SNAPSHOT_DIR, exist_ok=True)


def snapshot_path_for(dt):
    timestamp = dt.strftime("%Y-%m-%d_%H-%M-%S_%z")
    return os.path.join(SNAPSHOT_DIR, f"shared_behavior_baseline_{timestamp}.md")


def write_snapshot(path, analysis_dt, markdown):
    content = "\n".join(
        [
            f"Snapshot taken: {analysis_dt.strftime('%Y-%m-%d %H:%M:%S %Z (%z)')}",
            "",
            "## Full current markdown content",
            "",
            markdown.rstrip(),
            "",
            "## Full prompt text used for the run",
            "",
            "```text",
            PROMPT_TEXT.rstrip(),
            "```",
            "",
        ]
    )
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(content)


def main():
    ensure_output_dirs()

    analysis_dt = datetime.now(TIMEZONE)
    talent_contexts = {}
    for talent in get_talent_dirs():
        context = load_talent_context(talent)
        if context:
            talent_contexts[talent] = context

    eligible_talents = sorted(talent_contexts.keys(), key=talent_sort_key)
    if not eligible_talents:
        raise RuntimeError("No eligible talents with usable personality_open_coding inputs were found.")

    missing_profiles = [talent for talent in eligible_talents if talent not in TALENT_CONTRAST_NOTES]
    if missing_profiles:
        raise RuntimeError(
            "Missing talent contrast notes for eligible talents: " + ", ".join(missing_profiles)
        )

    evidence_rows, behavior_evidence_lookup = build_evidence_rows(talent_contexts)
    markdown = generate_markdown(
        analysis_dt,
        eligible_talents,
        talent_contexts,
        behavior_evidence_lookup,
    )

    markdown_path = os.path.join(CURRENT_DIR, "shared_behavior_baseline_codex.md")
    codebook_csv_path = os.path.join(CURRENT_DIR, "shared_behavior_codebook.csv")
    matrix_csv_path = os.path.join(CURRENT_DIR, "talent_shared_behavior_matrix.csv")
    evidence_csv_path = os.path.join(CURRENT_DIR, "shared_behavior_evidence.csv")
    state_json_path = os.path.join(CURRENT_DIR, "shared_behavior_state.json")
    snapshot_path = snapshot_path_for(analysis_dt)

    with open(markdown_path, "w", encoding="utf-8") as handle:
        handle.write(markdown)

    write_csv(
        codebook_csv_path,
        [
            "shared_behavior_name",
            "classification",
            "definition",
            "inclusion_criteria",
            "exclusion_criteria",
            "talent_count",
            "supporting_talents",
            "weaker_or_absent_talents",
            "variation_note",
            "baseline_relevance_note",
        ],
        build_codebook_csv_rows(eligible_talents),
    )

    write_csv(
        matrix_csv_path,
        [
            "talent",
            "shared_behavior_name",
            "classification",
            "support_level",
            "talent_specific_version",
            "uniqueness_candidate_note",
        ],
        build_matrix_csv_rows(eligible_talents),
    )

    write_csv(
        evidence_csv_path,
        [
            "shared_behavior_name",
            "classification",
            "talent",
            "contributing_open_code",
            "theme_family",
            "video_id",
            "time_in_seconds",
            "timecode",
            "source",
            "speaker",
            "quote",
            "evidence_role",
            "why_this_supports_shared_behavior",
        ],
        evidence_rows,
    )

    write_snapshot(snapshot_path, analysis_dt, markdown)

    state = {
        "analysis_conducted_at": analysis_dt.isoformat(),
        "eligible_talents": eligible_talents,
        "primary_upstream_source": "personality_open_coding",
        "secondary_source_used": any(
            talent_contexts[talent]["summary_md_path"] for talent in eligible_talents
        ),
        "shared_behavior_count": len(BEHAVIORS),
        "shared_baseline_count": sum(
            1 for behavior in BEHAVIORS if behavior["classification"] == "shared_baseline"
        ),
        "shared_but_distinctive_in_form_count": sum(
            1
            for behavior in BEHAVIORS
            if behavior["classification"] == "shared_but_distinctive_in_form"
        ),
        "latest_snapshot_path": snapshot_path,
        "notes": [
            "Primary judgments are grounded in personality_open_coding codebooks and evidence rows.",
            "summary_classification was used only as a stabilizing secondary layer.",
            "Guardrails, co-regulation loops, and money-linked framing were reviewed but not retained in the baseline.",
        ],
    }
    with open(state_json_path, "w", encoding="utf-8") as handle:
        json.dump(state, handle, indent=2, ensure_ascii=False)
        handle.write("\n")

    summary = {
        "eligible_talents": len(eligible_talents),
        "shared_behavior_count": len(BEHAVIORS),
        "shared_baseline_count": state["shared_baseline_count"],
        "shared_but_distinctive_in_form_count": state[
            "shared_but_distinctive_in_form_count"
        ],
        "markdown_path": markdown_path,
        "snapshot_path": snapshot_path,
    }
    print(json.dumps(summary, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        raise
