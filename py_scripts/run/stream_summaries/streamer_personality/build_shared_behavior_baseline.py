#!/usr/bin/env python3

import argparse
import csv
import json
import os
import sys
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo


REPO_ROOT = Path(__file__).resolve().parents[4]
LIB_ROOT = REPO_ROOT / "py_scripts" / "lib"
if str(LIB_ROOT) not in sys.path:
    sys.path.insert(0, str(LIB_ROOT))

from shared_interactions_eligibility import discover_shared_interaction_eligibility

PROMPT_PATH = REPO_ROOT / "prompts" / "shared_qualities" / "shared_behavior_baseline.md"
TALENT_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
OUTPUT_ROOT = Path(
    "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/concept_areas/interaction_views/monetary_personality_shared"
)
CURRENT_DIR = OUTPUT_ROOT / "current"
SNAPSHOT_DIR = OUTPUT_ROOT / "snapshots"
TIMEZONE = ZoneInfo("America/New_York")


BEHAVIORS = [
    {
        "shared_behavior_name": "gratitude ritualization",
        "classification": "shared_baseline",
        "definition": "Recurring supporter-recognition turns in which audience presence, raids, memberships, waiting, or check-ins are explicitly converted into visible gratitude rituals.",
        "inclusion_criteria": "Include repeated thank-you loops, appreciation turns, welcome-back thanks, and supporter acknowledgment that visibly organizes the social exchange around audience support.",
        "exclusion_criteria": "Exclude one-off courtesy thanks with no repeated ritual structure and exclude generic positive reactions that do not actually acknowledge audience support.",
        "baseline_relevance_note": "Both eligible talents repeatedly foreground audience support instead of treating it as background noise, so later uniqueness profiling should not treat thanking or audience acknowledgment as unique on its own.",
        "why_this_belongs_in_baseline": "This is the clearest common routine across Nova and Terberri: both repeatedly stop the flow to acknowledge support, raids, waiting, or check-ins as a social ritual.",
        "code_labels": [
            "thank ritual",
            "thanks ritual",
            "appreciate ritual",
            "grateful ritual",
            "ty ritual",
        ],
        "talent_versions": {
            "Nova Aokami Ch": {
                "support_level": "strong",
                "talent_specific_version": "Gratitude is often fused with raid handoffs, membership welcomes, and 'bestie' framing, so the thank-you move doubles as hype and audience intimacy.",
                "uniqueness_candidate_note": "Nova's distinctiveness lies in the fast-switching raid and membership theatrics layered onto the gratitude loop.",
            },
            "Terberri Solaris Ch": {
                "support_level": "strong",
                "talent_specific_version": "Gratitude leans communal and relational, often thanking viewers for waiting, checking in, or simply being present through the stream's rhythm shifts.",
                "uniqueness_candidate_note": "Terberri's differentiator is the softer communal reciprocity and check-in framing around the same shared gratitude routine.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Nova Aokami Ch",
                "code_label": "thank ritual",
                "video_id": "-LQp6UP-qH8",
                "timecode": "01:03:55",
                "why": "Shows Nova turning a raid into an explicit supporter acknowledgment sequence rather than a passing thank-you.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "appreciate ritual",
                "video_id": "-SyviYaElhM",
                "timecode": "00:30:25",
                "why": "Shows Terberri explicitly appreciating viewers for waiting, making audience patience part of the interaction ritual.",
            },
        ],
    },
    {
        "shared_behavior_name": "audience-attentive pacing control",
        "classification": "shared_baseline",
        "definition": "Visible steering of chat attention, transitions, or interaction timing so the streamer can redirect, pin, explain, or refocus the audience in real time.",
        "inclusion_criteria": "Include direct flow control, mod-routing, link fixes, focus resets, and explicit pace-setting turns that structure what the audience should attend to next.",
        "exclusion_criteria": "Exclude purely game-mechanical time references or isolated filler words with no visible audience-management function.",
        "baseline_relevance_note": "Multiple eligible talents actively manage pace and audience orientation, so later profiles should not treat ordinary flow steering as distinctive without showing how the steering is performed.",
        "why_this_belongs_in_baseline": "Nova and Terberri both repeatedly manage interaction tempo rather than letting chat or the stream drift unmanaged, even though one sounds sharper and the other sounds gentler.",
        "code_labels": [
            "request pace",
            "time pace",
            "mods pace",
            "focus pace",
        ],
        "talent_versions": {
            "Nova Aokami Ch": {
                "support_level": "strong",
                "talent_specific_version": "Pacing control is brisk and directive, often using mods, pinning, and quick audience routing to keep the interaction moving.",
                "uniqueness_candidate_note": "Nova's likely differentiator is the sharper command-and-routing cadence layered onto common pace control.",
            },
            "Terberri Solaris Ch": {
                "support_level": "strong",
                "talent_specific_version": "Flow control is more self-explanatory and soft, often pausing to explain the need to focus or reset before continuing.",
                "uniqueness_candidate_note": "Terberri stands out for using focus and self-explanation rather than pure command as the main pacing tool.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Nova Aokami Ch",
                "code_label": "mods pace",
                "video_id": "QY3j-9u7Vnw",
                "timecode": "00:09:32",
                "why": "Shows Nova using mods and pinning language to direct audience attention and tighten the stream's immediate flow.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "focus pace",
                "video_id": "-SyviYaElhM",
                "timecode": "00:24:26",
                "why": "Shows Terberri explicitly asking for a focus reset, making the pace-management move visible and audience-facing.",
            },
        ],
    },
    {
        "shared_behavior_name": "reassurance during strain or recovery",
        "classification": "shared_baseline",
        "definition": "Repeated turns that normalize setbacks, soothe tension, or reassure chat, the streamer, or an on-stream target during mistakes, embarrassment, frustration, or fatigue.",
        "inclusion_criteria": "Include 'it's okay,' 'don't worry,' validating language, worth-focused reassurance, and repeated soothing turns that clearly regulate tension.",
        "exclusion_criteria": "Exclude neutral logistics or generic positivity with no visible calming, validating, or soothing function.",
        "baseline_relevance_note": "Care and reassurance are common enough across the two-talents set that later workflows should only treat them as unique when the form or relationship function is clearly distinctive.",
        "why_this_belongs_in_baseline": "Multiple eligible talents repeatedly lower tension after awkward, frustrating, or fragile moments, though the form ranges from faster validation to more openly nurturing reassurance.",
        "code_labels": [
            "it's okay care",
            "don't worry care",
            "take care care",
            "no worries care",
            "got care",
            "safe care",
            "proud care",
            "okay care",
            "take time care",
        ],
        "talent_versions": {
            "Nova Aokami Ch": {
                "support_level": "strong",
                "talent_specific_version": "Reassurance is quick and validating, often folded into banter or fast reassurance loops such as 'it's okay' and 'you're so valid.'",
                "uniqueness_candidate_note": "Nova may later differentiate through the speed and slang-heavy validation wrapped around the same common soothing move.",
            },
            "Terberri Solaris Ch": {
                "support_level": "strong",
                "talent_specific_version": "Reassurance is gentler and more care-forward, often sounding like an extended calming or worth-affirming check-in.",
                "uniqueness_candidate_note": "Terberri's likely uniqueness hook is the sustained, openly caretaking tone of reassurance rather than reassurance itself.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Nova Aokami Ch",
                "code_label": "don't worry care",
                "video_id": "0Gc2s_Mv38Q",
                "timecode": "00:39:41",
                "why": "Shows Nova using direct validation and reassurance to ease strain rather than only reacting to gameplay.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "it's okay care",
                "video_id": "17ElKDZTHAM",
                "timecode": "00:49:35",
                "why": "Shows Terberri pairing reassurance with explicit worth-affirmation, making the calming function unmistakable.",
            },
        ],
    },
    {
        "shared_behavior_name": "audience-facing self-disclosure windows",
        "classification": "shared_but_distinctive_in_form",
        "definition": "Recurring first-person disclosure of anxiety, stress, overwhelm, fatigue, or personal need that is made visible to the audience rather than kept off-stage.",
        "inclusion_criteria": "Include first-person disclosures of anxiety, stress, overwhelm, fatigue, or personal need when they function as audience-facing transparency or relational context.",
        "exclusion_criteria": "Exclude purely mechanical task narration unless the line clearly frames the streamer's felt state, vulnerability, or personal difficulty.",
        "baseline_relevance_note": "Multiple eligible talents openly surface internal state often enough that later profiles should not mark basic vulnerability disclosure as unique without showing how the disclosure works differently.",
        "why_this_belongs_in_baseline": "Internal-state disclosure is common across the eligible set, but the form ranges from sharper stress reporting to softer overwhelm and need narration.",
        "code_labels": [
            "need disclosure",
            "feel disclosure",
            "needed disclosure",
            "anxiety disclosure",
            "i'm scared disclosure",
            "i'm tired disclosure",
            "i'm worried disclosure",
            "mental health disclosure",
            "i'm stressed disclosure",
            "stressed disclosure",
            "i'm overwhelmed disclosure",
            "depressed disclosure",
            "depressing disclosure",
        ],
        "talent_versions": {
            "Nova Aokami Ch": {
                "support_level": "strong",
                "talent_specific_version": "Self-disclosure often arrives as sharp stress, anxiety, or dramatic self-reporting that stays highly chat-aware and performative.",
                "uniqueness_candidate_note": "Nova's distinctive signal is the way stress and anxiety disclosures are intensified and folded into a more volatile comedic register.",
            },
            "Terberri Solaris Ch": {
                "support_level": "strong",
                "talent_specific_version": "Self-disclosure is softer and more regulation-oriented, often explaining overwhelm, fatigue, or needs as part of keeping the audience with the moment.",
                "uniqueness_candidate_note": "Terberri may later stand out for turning disclosure into communal self-check-ins rather than dramatic escalation.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Nova Aokami Ch",
                "code_label": "anxiety disclosure",
                "video_id": "65U_EYxYfs4",
                "timecode": "01:29:13",
                "why": "Shows Nova naming a strong anxiety state directly rather than leaving internal strain implicit.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "feel disclosure",
                "video_id": "-SyviYaElhM",
                "timecode": "00:37:06",
                "why": "Shows Terberri making overwhelm visible as audience-facing context for the moment.",
            },
        ],
    },
    {
        "shared_behavior_name": "performative teasing and welcome escalation",
        "classification": "shared_but_distinctive_in_form",
        "definition": "Recurring use of stylized welcome lines, pet-name address, or theatrical escalation that turns audience contact into a performance move rather than plain acknowledgment.",
        "inclusion_criteria": "Include repeated membership or welcome slogans, teasing address terms, performative escalation, or scene-setting bits used to shape rapport with chat.",
        "exclusion_criteria": "Exclude isolated slang, one-off catchphrases, or generic excitement with no visible interactional function.",
        "baseline_relevance_note": "Playful escalation is shared, but the form is not interchangeable. Later profiles should treat the broad behavior as common while preserving each talent's specific social function and tone.",
        "why_this_belongs_in_baseline": "Stylized audience contact recurs across the eligible set, but some talents lean louder and more taunt-coded while others keep the move gentler and more scene-setting.",
        "code_labels": [
            "baby bit",
            "welcome bit",
            "kill bit",
            "skill issue bit",
            "queen bit",
            "blood bit",
            "chaos bit",
            "wild bit",
            "bite bit",
            "ceremony bit",
            "behold bit",
        ],
        "talent_versions": {
            "Nova Aokami Ch": {
                "support_level": "strong",
                "talent_specific_version": "Escalation skews louder and more membership-, bestie-, and baby-coded, often mixing welcome rituals with playful taunt energy.",
                "uniqueness_candidate_note": "Nova's likely unique hook is the denser mix of membership ceremony, bestie language, and sharper insult-coded escalation.",
            },
            "Terberri Solaris Ch": {
                "support_level": "moderate",
                "talent_specific_version": "Escalation is more situational and frame-setting, often using welcome or twist language to turn the current segment into a playful social scene.",
                "uniqueness_candidate_note": "Terberri's differentiator is the lighter event-frame and scene-setting version of the same broad playful move.",
            },
        },
        "evidence_selectors": [
            {
                "talent": "Nova Aokami Ch",
                "code_label": "welcome bit",
                "video_id": "17LEB_ERvbU",
                "timecode": "01:51:49",
                "why": "Shows Nova turning membership acknowledgment into a stylized welcome performance rather than a plain greeting.",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "welcome bit",
                "video_id": "5yq9VTcBm3o",
                "timecode": "01:19:51",
                "why": "Shows Terberri using a repeated welcome frame as playful scene-setting rather than simple greeting.",
            },
        ],
    },
]


NON_BASELINE_PATTERNS = [
    {
        "name": "guardrail enforcement",
        "judgment": "not_shared_enough_for_baseline",
        "reason": "Boundary-setting appears in the eligible set, but the retained guardrail families are much smaller than the main baseline behaviors and usually appear in narrow moderation or etiquette moments.",
        "follow_up": "Treat boundary enforcement as context-specific comparison texture rather than a stable cross-streamer baseline for the current eligible set.",
        "selectors": [
            {
                "talent": "Nova Aokami Ch",
                "code_label": "don't weird guardrail",
                "video_id": "WekPsciUVTo",
                "timecode": "00:32:06",
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
        "name": "acute distress framing",
        "judgment": "not_shared_enough_for_baseline",
        "reason": "Some talents occasionally name anxiety, stress, or depressive states explicitly, but the broader retained disclosure families are much stronger than these sharper distress labels, so the acute form is too uneven to treat as baseline.",
        "follow_up": "Keep high-intensity distress framing available for uniqueness or risk review, but do not collapse it into the shared baseline without broader evidence.",
        "selectors": [
            {
                "talent": "Nova Aokami Ch",
                "code_label": "anxiety disclosure",
                "video_id": "8xhK2NF8FDo",
                "timecode": "02:01:02",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "i'm stressed disclosure",
                "video_id": "F2cHJc7qios",
                "timecode": "04:58:16",
            },
        ],
    },
    {
        "name": "hard-edged insult or menace bits",
        "judgment": "not_shared_enough_for_baseline",
        "reason": "Nova has a much denser insult-coded escalation cluster, while Terberri's playful escalation leans more toward welcome and situational framing. That makes the sharper aggressive register too talent-specific to count as baseline.",
        "follow_up": "Leave insult-heavy or menace-coded bit language for later uniqueness profiling instead of treating it as a shared channel norm.",
        "selectors": [
            {
                "talent": "Nova Aokami Ch",
                "code_label": "skill issue bit",
                "video_id": "22RBB4jklyI",
                "timecode": "00:10:12",
            },
            {
                "talent": "Terberri Solaris Ch",
                "code_label": "welcome bit",
                "video_id": "5yq9VTcBm3o",
                "timecode": "01:19:51",
            },
        ],
    },
]


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--talent",
        dest="focused_talent",
        help="Optional focus talent. Keeps the workflow cross-talent but orders output and notes around the named talent.",
    )
    parser.add_argument(
        "--profile-days",
        type=int,
        default=31,
        help="Require a non-empty overall personality profile modified within this many days. Default: 31.",
    )
    parser.add_argument(
        "--include-stale-profiles",
        action="store_true",
        help="Include talents with older personality profile outputs if their open-coding outputs are usable.",
    )
    return parser.parse_args()


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
    return Path(path).read_text(encoding="utf-8")


def build_prompt_text(focused_talent):
    prompt_text = read_text(PROMPT_PATH).rstrip()
    if focused_talent:
        prompt_text += (
            "\n\nAdditional talent scope requested by the user:\n"
            f"- TALENT_SLUG: {focused_talent}\n"
            f"- Focus this run on the talent folder exactly named: {focused_talent}\n"
            f"- Replace any talent placeholder or slug in this prompt with the exact talent folder name above.\n"
            "- If the workflow is cross-talent, use other eligible talents only as comparison context needed for the focused output.\n"
            "- Keep the final response concise and include output path(s) touched.\n"
        )
    return prompt_text


def get_talent_dirs(profile_days, require_recent_profile):
    eligibility = discover_shared_interaction_eligibility(
        TALENT_ROOT,
        profile_days=profile_days,
        require_recent_profile=require_recent_profile,
    )
    return sorted(
        [item.name for item in eligibility if item.eligible],
        key=talent_sort_key,
    )


def pick_open_coding_paths(talent):
    base = TALENT_ROOT / talent / "stream_summaries" / "overall_themes"
    candidates = [
        {
            "version": "v3",
            "codebook": base / "personality_open_coding" / "v3" / "current" / "open_codebook_v3.csv",
            "evidence": base / "personality_open_coding" / "v3" / "current" / "open_coding_evidence_v3.csv",
            "markdown": base / "personality_open_coding" / "v3" / "current" / "personality_profile_v3_open_coding.md",
        },
        {
            "version": "v2",
            "codebook": base / "personality_open_coding" / "v2" / "open_codebook_v2.csv",
            "evidence": base / "personality_open_coding" / "v2" / "open_coding_evidence_v2.csv",
            "markdown": base / "personality_open_coding" / "v2" / "personality_profile_v2_open_coding.md",
        },
    ]
    for candidate in candidates:
        if candidate["codebook"].exists() and candidate["evidence"].exists():
            return candidate
    return None


def pick_overall_channel_summary_paths(talent):
    root = TALENT_ROOT / talent / "stream_summaries"
    candidates = [
        (
            root / "overall_channel_summary" / "current" / "overall_channel_summary.md",
            root / "overall_channel_summary" / "current" / "overall_channel_summary_state.json",
        ),
        (
            root / "overall_themes" / "summary_classification" / "current" / "overall_themes_codex.md",
            root / "overall_themes" / "summary_classification" / "current" / "summary_classification_state.json",
        ),
    ]
    for summary_md, summary_state in candidates:
        if summary_md.exists():
            return summary_md, summary_state
    return candidates[0]


def parse_summary_themes(summary_md_path):
    if not summary_md_path or not Path(summary_md_path).exists():
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

    summary_md, summary_state = pick_overall_channel_summary_paths(talent)
    codebook_rows = read_csv_rows(chosen["codebook"])
    evidence_rows = read_csv_rows(chosen["evidence"])

    for row in codebook_rows:
        row["frequency_count_num"] = int(float(row.get("frequency_count") or 0))
        row["stream_coverage_count_num"] = int(float(row.get("stream_coverage_count") or 0))

    return {
        "talent": talent,
        "open_coding_version": chosen["version"],
        "codebook_path": str(chosen["codebook"]),
        "evidence_path": str(chosen["evidence"]),
        "markdown_path": str(chosen["markdown"]) if chosen["markdown"].exists() else None,
        "summary_md_path": str(summary_md) if summary_md.exists() else None,
        "summary_state_path": str(summary_state) if summary_state.exists() else None,
        "summary_themes": parse_summary_themes(summary_md),
        "codebook_rows": codebook_rows,
        "evidence_rows": evidence_rows,
    }


def order_talents(eligible_talents, focused_talent):
    ordered = sorted(eligible_talents, key=talent_sort_key)
    if focused_talent and focused_talent in ordered:
        return [focused_talent] + [talent for talent in ordered if talent != focused_talent]
    return ordered


def find_evidence_row(contexts, selector):
    if selector["talent"] not in contexts:
        return None
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
        return None
    return matches[0]


def matching_codebook_rows(context, code_labels):
    code_set = set(code_labels)
    return [row for row in context["codebook_rows"] if row["code_label"] in code_set]


def infer_talent_version(contexts, behavior, talent):
    explicit = behavior["talent_versions"].get(talent)
    rows = matching_codebook_rows(contexts[talent], behavior["code_labels"])
    total_frequency = sum(row["frequency_count_num"] for row in rows)
    max_coverage = max((row["stream_coverage_count_num"] for row in rows), default=0)
    if explicit:
        return explicit
    if total_frequency <= 0 or max_coverage <= 0:
        support_level = "absent"
    elif max_coverage >= 3 or total_frequency >= 10:
        support_level = "strong"
    else:
        support_level = "moderate"
    if rows:
        top = sorted(
            rows,
            key=lambda row: (row["stream_coverage_count_num"], row["frequency_count_num"]),
            reverse=True,
        )[0]
        version = (
            f"Supported through retained open-coding rows such as `{top['code_label']}` "
            f"in `{top['theme_family']}`."
        )
    else:
        version = "No retained open-coding rows matched this behavior family in the current inputs."
    return {
        "support_level": support_level,
        "talent_specific_version": version,
        "uniqueness_candidate_note": (
            f"the talent-specific version of `{top['code_label']}` within `{top['theme_family']}`"
            if rows
            else "the talent-specific form of the retained shared pattern"
        ),
    }


def count_supporting_talents(contexts, behavior, eligible_talents):
    return sum(
        1
        for talent in eligible_talents
        if infer_talent_version(contexts, behavior, talent)["support_level"] != "absent"
    )


def supporting_talents_string(contexts, behavior, eligible_talents):
    supporting = [
        talent
        for talent in eligible_talents
        if infer_talent_version(contexts, behavior, talent)["support_level"] in {"strong", "moderate"}
    ]
    return ", ".join(supporting) if supporting else "none"


def weaker_or_absent_string(contexts, behavior, eligible_talents):
    weaker = []
    for talent in eligible_talents:
        support_level = infer_talent_version(contexts, behavior, talent)["support_level"]
        if support_level != "strong":
            weaker.append(f"{talent} ({support_level})")
    return ", ".join(weaker) if weaker else "none"


def variation_note(contexts, behavior, eligible_talents):
    parts = []
    for talent in eligible_talents:
        version = infer_talent_version(contexts, behavior, talent)["talent_specific_version"]
        if version:
            parts.append(f"{talent}: {version}")
    return " | ".join(parts)


def summarize_behavior_support(contexts, behavior, eligible_talents):
    lines = []
    for talent in eligible_talents:
        rows = matching_codebook_rows(contexts[talent], behavior["code_labels"])
        total_frequency = sum(row["frequency_count_num"] for row in rows)
        max_coverage = max((row["stream_coverage_count_num"] for row in rows), default=0)
        support_level = infer_talent_version(contexts, behavior, talent)["support_level"]
        lines.append(
            f"{talent}: {support_level}; retained frequency {total_frequency}; max stream coverage {max_coverage}"
        )
    return " | ".join(lines)


def supported_talents(contexts, behavior, ordered_talents, levels=None):
    levels = levels or {"strong", "moderate"}
    return [
        talent
        for talent in ordered_talents
        if infer_talent_version(contexts, behavior, talent)["support_level"] in levels
    ]


def short_behavior_label(name):
    labels = {
        "gratitude ritualization": "gratitude rituals",
        "audience-attentive pacing control": "audience-attentive pacing control",
        "reassurance during strain or recovery": "reassurance during strain or recovery",
        "audience-facing self-disclosure windows": "audience-facing self-disclosure",
        "performative teasing and welcome escalation": "performative teasing and welcome escalation",
    }
    return labels.get(name, name)


def join_names(names):
    if not names:
        return "none"
    if len(names) == 1:
        return names[0]
    if len(names) == 2:
        return f"{names[0]} and {names[1]}"
    return ", ".join(names[:-1]) + f", and {names[-1]}"


def dynamic_baseline_reason(contexts, behavior, ordered_talents):
    supporting = supported_talents(contexts, behavior, ordered_talents)
    strong = supported_talents(contexts, behavior, ordered_talents, {"strong"})
    weaker = [
        talent
        for talent in ordered_talents
        if infer_talent_version(contexts, behavior, talent)["support_level"] != "strong"
    ]
    sentence = (
        f"Supported in {len(supporting)}/{len(ordered_talents)} eligible talents; "
        f"strong in {join_names(strong) if strong else 'none'}."
    )
    if weaker:
        sentence += f" Weaker or more limited support appears in {join_names(weaker)}."
    return sentence


def build_talent_contrast_note(contexts, talent):
    baseline_behaviors = []
    distinctive_behaviors = []
    uniqueness_candidates = []

    for behavior in BEHAVIORS:
        version = infer_talent_version(contexts, behavior, talent)
        if version["support_level"] not in {"strong", "moderate"}:
            continue
        label = short_behavior_label(behavior["shared_behavior_name"])
        if behavior["classification"] == "shared_baseline":
            baseline_behaviors.append(label)
        else:
            distinctive_behaviors.append(label)
            uniqueness_candidates.append(version["uniqueness_candidate_note"].rstrip("."))

    non_baseline_hits = []
    for pattern in NON_BASELINE_PATTERNS:
        if any(selector["talent"] == talent for selector in pattern["selectors"]):
            non_baseline_hits.append(pattern["name"])

    baseline_text = join_names(baseline_behaviors[:4]) if baseline_behaviors else "no clear retained baseline behaviors"
    distinctive_text = (
        join_names(distinctive_behaviors[:3])
        if distinctive_behaviors
        else "no strongly retained shared-but-different patterns"
    )

    uniqueness_parts = []
    if non_baseline_hits:
        uniqueness_parts.append(f"non-baseline areas such as {join_names(non_baseline_hits[:2])}")
    if uniqueness_candidates:
        uniqueness_parts.append(uniqueness_candidates[0])
    uniqueness_text = (
        "; ".join(uniqueness_parts)
        if uniqueness_parts
        else "no clear uniqueness-candidate note could be inferred from the retained comparison rows alone"
    )

    return (
        f"Clearly shares the baseline through {baseline_text}. "
        f"Distinctive shared forms are most visible in {distinctive_text}. "
        f"Likely uniqueness-candidate areas remain in {uniqueness_text}."
    )


def build_baseline_summary_paragraphs(contexts, ordered_talents):
    baseline_behaviors = [
        behavior for behavior in BEHAVIORS if behavior["classification"] == "shared_baseline"
    ]
    distinctive_behaviors = [
        behavior
        for behavior in BEHAVIORS
        if behavior["classification"] == "shared_but_distinctive_in_form"
    ]
    baseline_parts = [
        f"{short_behavior_label(behavior['shared_behavior_name'])} ({len(supported_talents(contexts, behavior, ordered_talents))}/{len(ordered_talents)})"
        for behavior in baseline_behaviors
    ]
    distinctive_parts = [
        f"{short_behavior_label(behavior['shared_behavior_name'])} ({len(supported_talents(contexts, behavior, ordered_talents))}/{len(ordered_talents)})"
        for behavior in distinctive_behaviors
    ]
    paragraph_1 = (
        f"Across {len(ordered_talents)} eligible talents with usable upstream open-coding outputs "
        f"({', '.join(ordered_talents)}), the strongest shared baseline centers on "
        f"{'; '.join(baseline_parts)}. These routines recur widely enough to function as comparative baseline rather than talent-specific claims."
    )
    paragraph_2 = (
        "Shared but variation-sensitive patterns remain important. "
        f"{'; '.join(distinctive_parts)} recur across the set, but their delivery and relationship function differ enough by talent that later uniqueness profiling still needs to track form rather than just presence."
    )
    paragraph_3 = (
        "Patterns should stay out of baseline when the support is sparse, overly context-bound, or concentrated in sharper sub-forms that do not recur broadly enough across the set. "
        "This baseline is useful for later uniqueness profiling because it raises the threshold for uniqueness claims and prevents generic streamer routines from being overstated as distinctive."
    )
    return paragraph_1, paragraph_2, paragraph_3


def build_shared_but_different_section(contexts, ordered_talents):
    paragraphs = []
    for behavior in BEHAVIORS:
        if behavior["classification"] != "shared_but_distinctive_in_form":
            continue
        supporting = supported_talents(contexts, behavior, ordered_talents)
        variation_parts = []
        for talent in supporting:
            version = infer_talent_version(contexts, behavior, talent)["talent_specific_version"]
            variation_parts.append(f"{talent}: {version}")
        paragraphs.append(
            f"{behavior['shared_behavior_name'].capitalize()} is retained across {len(supporting)}/{len(ordered_talents)} eligible talents. "
            + " | ".join(variation_parts)
        )
    return paragraphs


def markdown_quote_line(talent, row):
    return f'- {talent} [{row["video_id"]} {row["timecode"]}]: "{row["quote"]}"'


def representative_evidence_row(context, code_labels):
    code_set = set(code_labels)
    rows = [row for row in context["evidence_rows"] if row.get("code_label") in code_set]
    if not rows:
        return None
    primary = [row for row in rows if row.get("evidence_role") == "primary"] or rows
    return sorted(
        primary,
        key=lambda row: (
            row.get("code_label", ""),
            row.get("video_id", ""),
            float(row.get("time_in_seconds") or 0),
        ),
    )[0]


def build_secondary_stabilizer_note(contexts, eligible_talents):
    notes = []
    for talent in eligible_talents:
        themes = contexts[talent]["summary_themes"]
        if themes:
            notes.append(f"{talent}: {', '.join(themes)}")
    if not notes:
        return "No current summary_classification or overall_channel_summary layer was available for the eligible talents."
    if len(notes) == 1:
        return (
            "Secondary stabilizer note: only one eligible talent currently has a usable summary-classification layer. "
            + notes[0]
            + ". This layer supports stability checking but does not create new shared themes."
        )
    return (
        "Secondary stabilizer note: usable summary-classification themes were available for multiple eligible talents. "
        + " | ".join(notes)
        + ". These layers support the same broad interaction picture without replacing the open-coding evidence rows."
    )


def generate_markdown(
    analysis_dt,
    eligible_talents,
    ordered_talents,
    contexts,
    evidence_rows_by_behavior,
    focused_talent,
):
    secondary_used = "yes" if any(contexts[t]["summary_md_path"] for t in eligible_talents) else "no"
    shared_baseline_count = sum(
        1 for behavior in BEHAVIORS if behavior["classification"] == "shared_baseline"
    )
    distinctive_count = sum(
        1
        for behavior in BEHAVIORS
        if behavior["classification"] == "shared_but_distinctive_in_form"
    )

    intro, variation, non_baseline = build_baseline_summary_paragraphs(
        contexts, ordered_talents
    )

    if focused_talent:
        non_baseline += (
            f" In this run, {focused_talent} is the focal talent, so the comparison is phrased to separate shared routines from that talent's distinctive form."
        )

    lines = [
        f"Analysis conducted: {analysis_dt.strftime('%Y-%m-%d %H:%M %Z')}",
        f"Eligible talents included: {len(eligible_talents)}",
        "Primary upstream source: personality_open_coding",
        f"Secondary source used: summary_classification {secondary_used}",
        "",
        "## 1) Cross-Streamer Shared Interaction Baseline",
        intro,
        "",
        variation,
        "",
        non_baseline,
        "",
        build_secondary_stabilizer_note(contexts, eligible_talents),
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
                f"- talent_count: {count_supporting_talents(contexts, behavior, eligible_talents)}",
                f"- supporting_talents: {supporting_talents_string(contexts, behavior, eligible_talents)}",
                f"- weaker_or_absent_talents: {weaker_or_absent_string(contexts, behavior, eligible_talents)}",
                f"- why this belongs in the baseline: {dynamic_baseline_reason(contexts, behavior, ordered_talents)}",
                f"- how it varies across talents: {variation_note(contexts, behavior, ordered_talents)}",
                "- supporting quotes:",
            ]
        )
        for talent in ordered_talents:
            row = evidence_rows.get(talent)
            if row:
                lines.append(markdown_quote_line(talent, row))
            else:
                lines.append(f"- {talent}: no matching evidence row found in retained open-coding evidence.")
        lines.append(
            f"- comparison support summary: {summarize_behavior_support(contexts, behavior, ordered_talents)}"
        )
        lines.append("")

    lines.extend(["## 3) Shared-But-Different Patterns"])
    for paragraph in build_shared_but_different_section(contexts, ordered_talents):
        lines.extend([paragraph, ""])
    lines.append("## 4) Non-Baseline or Weakly Shared Patterns")

    for pattern in NON_BASELINE_PATTERNS:
        lines.extend(
            [
                f"### {pattern['name']}",
                f"- classification: {pattern['judgment']}",
                f"- why not baseline: {pattern['reason']}",
                f"- downstream handling: {pattern['follow_up']}",
                "- example evidence:",
            ]
        )
        for selector in pattern["selectors"]:
            row = find_evidence_row(contexts, selector)
            if row:
                lines.append(markdown_quote_line(selector["talent"], row))
        lines.append("")

    lines.append("## 5) Talent Contrast Notes")
    for talent in ordered_talents:
        note = build_talent_contrast_note(contexts, talent)
        lines.append(f"- {talent}: {note}")

    lines.extend(
        [
            "",
            "## 6) Method Notes and Uncertainty",
            "- 3 strongest evidence-backed baseline conclusions:",
            "- Ritualized supporter acknowledgment is a recurring common interaction routine when supported across the eligible set.",
            "- Public pacing control is a shared norm when retained pace-control codes recur across talents.",
            "- Reassurance during strain or recovery should count as baseline only when supported by multiple talents' retained care/reassurance evidence.",
            "- 3 uncertainty points:",
            "- Secondary stabilization may be uneven when not every eligible talent has a current overall_channel_summary layer.",
            "- This workflow is text-only, so differences that depend on facial expression, timing, or vocal warmth cannot be claimed here.",
            "- Some disclosure rows mix broad need statements with sharper vulnerability, so the boundary between routine self-report and higher-stakes disclosure remains cautious.",
            "- 1 note about the risk of flattening distinct streamers into generic shared traits:",
            "- The main risk is treating gratitude, pace control, reassurance, and playful audience contact as if they erase style differences; this baseline stays narrow so later profiles can compare form, function, and exceptions instead of collapsing streamers into one average persona.",
            "",
            f"Retained shared behaviors: {len(BEHAVIORS)} total; {shared_baseline_count} shared_baseline; {distinctive_count} shared_but_distinctive_in_form.",
        ]
    )

    return "\n".join(lines).rstrip() + "\n"


def build_codebook_csv_rows(contexts, eligible_talents):
    rows = []
    for behavior in BEHAVIORS:
        rows.append(
            {
                "shared_behavior_name": behavior["shared_behavior_name"],
                "classification": behavior["classification"],
                "definition": behavior["definition"],
                "inclusion_criteria": behavior["inclusion_criteria"],
                "exclusion_criteria": behavior["exclusion_criteria"],
                "talent_count": count_supporting_talents(contexts, behavior, eligible_talents),
                "supporting_talents": supporting_talents_string(contexts, behavior, eligible_talents),
                "weaker_or_absent_talents": weaker_or_absent_string(contexts, behavior, eligible_talents),
                "variation_note": variation_note(contexts, behavior, eligible_talents),
                "baseline_relevance_note": behavior["baseline_relevance_note"],
            }
        )
    return rows


def build_matrix_csv_rows(contexts, ordered_talents):
    rows = []
    for talent in ordered_talents:
        for behavior in BEHAVIORS:
            version = infer_talent_version(contexts, behavior, talent)
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
        for talent, context in contexts.items():
            row = representative_evidence_row(context, behavior["code_labels"])
            if not row:
                continue
            behavior_rows[talent] = row
            output_rows.append(
                {
                    "shared_behavior_name": behavior["shared_behavior_name"],
                    "classification": behavior["classification"],
                    "talent": talent,
                    "contributing_open_code": row["code_label"],
                    "theme_family": row["theme_family"],
                    "video_id": row["video_id"],
                    "time_in_seconds": row["time_in_seconds"],
                    "timecode": row["timecode"],
                    "source": row["source"],
                    "speaker": row["speaker"],
                    "quote": row["quote"],
                    "evidence_role": row["evidence_role"],
                    "why_this_supports_shared_behavior": "Representative retained open-coding evidence for this shared behavior family.",
                }
            )
        behavior_evidence_lookup[behavior["shared_behavior_name"]] = behavior_rows
    return output_rows, behavior_evidence_lookup


def ensure_output_dirs():
    CURRENT_DIR.mkdir(parents=True, exist_ok=True)
    SNAPSHOT_DIR.mkdir(parents=True, exist_ok=True)


def snapshot_path_for(dt):
    timestamp = dt.strftime("%Y-%m-%d_%H-%M-%S_%z")
    return SNAPSHOT_DIR / f"shared_behavior_baseline_{timestamp}.md"


def write_snapshot(path, analysis_dt, markdown, prompt_text):
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
            prompt_text.rstrip(),
            "```",
            "",
        ]
    )
    path.write_text(content, encoding="utf-8")


def main():
    args = parse_args()
    ensure_output_dirs()

    analysis_dt = datetime.now(TIMEZONE)
    talent_contexts = {}
    for talent in get_talent_dirs(
        profile_days=args.profile_days,
        require_recent_profile=not args.include_stale_profiles,
    ):
        context = load_talent_context(talent)
        if context:
            talent_contexts[talent] = context

    eligible_talents = sorted(talent_contexts.keys(), key=talent_sort_key)
    if not eligible_talents:
        raise RuntimeError("No eligible talents with usable personality_open_coding inputs were found.")

    if args.focused_talent and args.focused_talent not in eligible_talents:
        raise RuntimeError(
            f"Focused talent '{args.focused_talent}' is not eligible for this workflow."
        )

    ordered_talents = order_talents(eligible_talents, args.focused_talent)
    prompt_text = build_prompt_text(args.focused_talent)
    evidence_rows, behavior_evidence_lookup = build_evidence_rows(talent_contexts)
    markdown = generate_markdown(
        analysis_dt,
        eligible_talents,
        ordered_talents,
        talent_contexts,
        behavior_evidence_lookup,
        args.focused_talent,
    )

    markdown_path = CURRENT_DIR / "shared_behavior_baseline_codex.md"
    codebook_csv_path = CURRENT_DIR / "shared_behavior_codebook.csv"
    matrix_csv_path = CURRENT_DIR / "talent_shared_behavior_matrix.csv"
    evidence_csv_path = CURRENT_DIR / "shared_behavior_evidence.csv"
    state_json_path = CURRENT_DIR / "shared_behavior_state.json"
    snapshot_path = snapshot_path_for(analysis_dt)

    markdown_path.write_text(markdown, encoding="utf-8")

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
        build_codebook_csv_rows(talent_contexts, ordered_talents),
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
        build_matrix_csv_rows(talent_contexts, ordered_talents),
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

    write_snapshot(snapshot_path, analysis_dt, markdown, prompt_text)

    notes = [
        "Primary judgments are grounded in personality_open_coding codebooks and evidence rows.",
        "summary_classification was used only as a stabilizing secondary layer where available.",
        "Guardrails, acute distress framing, and harder-edged insult/menace bits were reviewed but not retained in the baseline.",
    ]
    if args.focused_talent:
        notes.append(
            f"Focused talent for this run: {args.focused_talent}; other eligible talents were kept only as comparison context."
        )

    state = {
        "analysis_conducted_at": analysis_dt.isoformat(),
        "eligible_talents": ordered_talents,
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
        "latest_snapshot_path": str(snapshot_path),
        "notes": notes,
        "profile_days": args.profile_days,
        "require_recent_profile": not args.include_stale_profiles,
    }
    state_json_path.write_text(
        json.dumps(state, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )

    summary = {
        "eligible_talents": len(eligible_talents),
        "shared_behavior_count": len(BEHAVIORS),
        "shared_baseline_count": state["shared_baseline_count"],
        "shared_but_distinctive_in_form_count": state[
            "shared_but_distinctive_in_form_count"
        ],
        "markdown_path": str(markdown_path),
        "snapshot_path": str(snapshot_path),
        "focused_talent": args.focused_talent,
    }
    print(json.dumps(summary, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
