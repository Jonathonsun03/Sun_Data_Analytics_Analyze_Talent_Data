#!/usr/bin/env python3
"""Build talent-specific unique personality profiles from shared baseline artifacts."""

from __future__ import annotations

import csv
import json
import math
import re
import sys
import argparse
from collections import Counter, defaultdict
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Tuple
from zoneinfo import ZoneInfo


THIS_FILE = Path(__file__).resolve()
PY_SCRIPTS_ROOT = next(
    parent
    for parent in [*THIS_FILE.parents]
    if (parent / "lib" / "repo_paths.py").exists()
)
LIB_ROOT = PY_SCRIPTS_ROOT / "lib"
if str(LIB_ROOT) not in sys.path:
    sys.path.insert(0, str(LIB_ROOT))

from repo_paths import find_repo_root


REPO_ROOT = find_repo_root(Path(__file__))
DATA_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
BASELINE_ROOT = Path(
    "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/shared_interactions/current"
)
PROMPT_PATH = (
    REPO_ROOT
    / "prompts"
    / "personality"
    / "personality_unique_features.md"
)
TIMEZONE = ZoneInfo("America/New_York")
ANALYSIS_NOW = datetime.now(TIMEZONE)
ANALYSIS_STAMP = ANALYSIS_NOW.strftime("%Y-%m-%d %H:%M %Z")
SNAPSHOT_STAMP = ANALYSIS_NOW.strftime("%Y-%m-%d_%H-%M-%S_%z")

BEHAVIOR_ORDER = [
    "performative teasing and welcome escalation",
    "audience-facing self-disclosure windows",
    "audience-attentive pacing control",
    "gratitude ritualization",
    "reassurance during strain or recovery",
]
BASELINE_LABELS = {
    "performative teasing and welcome escalation": "performative teasing / welcome escalation",
    "audience-facing self-disclosure windows": "audience-facing self-disclosure",
    "audience-attentive pacing control": "audience-attentive pacing control",
    "gratitude ritualization": "gratitude ritualization",
    "reassurance during strain or recovery": "reassurance during strain or recovery",
}
OUTSIDE_FAMILY_ORDER = [
    "guardrails",
    "co_regulation",
    "money",
]
TARGET_DIMENSION_COUNT = 4
STREAMER_SPEAKERS = {"stream", "streamer", "host"}
SPECIAL_DIMENSION_SPECS = {
    "Avaritia Hawthorne 【Variance Project】": [
        {
            "dimension_name": "Ceremonial ominous supporter induction",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "gratitude ritualization",
            "theme_families": ["Thank recognition loops", "Welcome bit escalation"],
            "code_labels": ["thank ritual", "thanks ritual", "appreciate ritual", "welcome bit"],
            "keywords": ["right side of history", "bone zone", "welcome", "member"],
            "why_unique": "Avaritia's gratitude ritual is not generic appreciation; supporter recognition repeatedly becomes an initiation ceremony with ominous or mock-grandiose welcome language.",
            "shared_baseline_comparison": "Gratitude ritualization is baseline across the set, but Avaritia's version fuses thanks with initiation language, staged welcome slogans, and faux membership ceremony rather than plain appreciation.",
            "limits": "The ritual style is clear in text, but text-only evidence cannot capture how much the effect depends on vocal delivery or timing.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "shared_baseline_variant",
        },
        {
            "dimension_name": "Menace-coded body/flirt register",
            "uniqueness_type": "outside_shared_baseline",
            "shared_behavior_name": "",
            "theme_families": ["Welcome bit escalation"],
            "code_labels": ["blood bit", "kill bit", "bite bit", "darling bit", "welcome bit"],
            "keywords": ["blood", "bite", "kill", "darling", "bone"],
            "why_unique": "The shared baseline includes teasing and escalation, but not this dense body-horror flirt register. In Avaritia's evidence, menace-coded attraction language recurs often enough to function as a sharper uniqueness marker.",
            "shared_baseline_comparison": "This sits beyond the retained shared baseline because the dataset-wide baseline does not normalize blood / bite / kill / bone imagery as a common rapport style.",
            "limits": "This claim is strong on lexical and interactional patterning, but it still describes text-visible style rather than motive or persona outside stream.",
            "source_kind": "outside_family",
            "weakness_bucket": "outside_shared_baseline",
        },
        {
            "dimension_name": "Vulnerability delivered as a bit",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "audience-facing self-disclosure windows",
            "theme_families": ["I'M self-disclosure windows"],
            "code_labels": [
                "need disclosure",
                "feel disclosure",
                "anxiety disclosure",
                "i'm anxious disclosure",
                "mental health disclosure",
                "autistic disclosure",
            ],
            "keywords": ["anxiety", "mental health", "autistic", "boobs", "bury my anxiety"],
            "why_unique": "Avaritia does disclose stress or vulnerability, but the disclosure often stays punchlined, stylized, or staged for chat rather than arriving as a soft aside.",
            "shared_baseline_comparison": "Audience-facing self-disclosure is shared across the dataset, but Avaritia's version remains overtly performative and joke-aware instead of reading as gentle transparency or complaint-driven venting.",
            "limits": "The disclosure pattern is robust, but some examples mix real strain with streamer bit work, so the emphasis should stay on delivery style rather than on severity of feeling.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "weakly_shared_pattern",
        },
        {
            "dimension_name": "Public restaging of the room",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "audience-attentive pacing control",
            "theme_families": ["Time pacing control"],
            "code_labels": ["request pace", "time pace", "focus pace", "mods pace"],
            "keywords": ["hold on", "member counter", "doing a funny", "wait one second", "counter"],
            "why_unique": "When Avaritia manages flow, the control move often feels like stage direction. She resets counters, pauses the room, and publicly re-stages the next beat rather than simply redirecting chat.",
            "shared_baseline_comparison": "Pacing control is baseline across the set, but Avaritia's version is especially stop-start and theatrical, with visible public resets instead of coach-style or gently explanatory pacing.",
            "limits": "This is strong in retained evidence, though still text-only; visual staging or comedic timing could intensify the effect beyond what appears here.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "shared_baseline_variant",
        },
        {
            "dimension_name": "Confrontational audience-boundary correction",
            "uniqueness_type": "outside_shared_baseline",
            "shared_behavior_name": "",
            "theme_families": ["Not guardrails"],
            "code_labels": ["don't ask guardrail", "please stop guardrail", "stop asking guardrail"],
            "keywords": ["don't ask", "stop asking", "please stop", "none of your business", "cult"],
            "why_unique": "Avaritia's repeated anti-question lines matter less as standalone rules than as a recurring way of publicly snapping the audience back into place. The correction style is direct, performative, and distinct enough to function as a local interaction marker.",
            "shared_baseline_comparison": "The shared baseline explicitly leaves guardrail enforcement outside the baseline. Avaritia's repeated anti-question corrections therefore matter here as a sharper audience-boundary style rather than as generic housekeeping.",
            "limits": "Coverage is lower than the top families, so this should be read as a secondary uniqueness signal rather than the center of the profile.",
            "source_kind": "outside_family",
            "weakness_bucket": "outside_shared_baseline",
        },
    ],
    "Katya Sable 【Variance Project】": [
        {
            "dimension_name": "Disciplinary stage-management",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "audience-attentive pacing control",
            "theme_families": ["Request pacing control"],
            "code_labels": ["request pace", "time pace", "focus pace"],
            "keywords": ["timed him out", "door hit your ass", "talking stream", "wait five more minutes"],
            "why_unique": "Katya's pacing is not just fast or attentive; it is openly managerial, with hard waits, timeout talk, and blunt exits that make control visible as discipline.",
            "shared_baseline_comparison": "Pacing control is baseline across the set, but Katya's version is noticeably sharper and more ejective than the coaching, self-explanatory, or theatrical variants visible elsewhere in the dataset.",
            "limits": "This dimension is well-supported in text, though the exact level of harshness still depends partly on tone not available here.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "shared_baseline_variant",
        },
        {
            "dimension_name": "Complaint-shaped transparency",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "audience-facing self-disclosure windows",
            "theme_families": ["I'M self-disclosure windows"],
            "code_labels": ["feel disclosure", "need disclosure", "anxiety disclosure"],
            "keywords": ["ulcer", "nervous", "rant reserve", "exhausted", "complain"],
            "why_unique": "Katya does disclose nerves or strain, but the clearest version comes through irritation, complaint, or rant cadence rather than soft vulnerability.",
            "shared_baseline_comparison": "Self-disclosure is shared, but Katya's version is less gently transparent and less stylized than other recurrent forms in the dataset. The distinctiveness comes from the complaint-forward register.",
            "limits": "The examples are thinner than the pacing or banter families, so this claim should stay tied to the exact retained rows rather than broaden into a total personality claim.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "weakly_shared_pattern",
        },
        {
            "dimension_name": "Insult-flirt escalation",
            "uniqueness_type": "distinctive_combination_of_traits",
            "shared_behavior_name": "performative teasing and welcome escalation",
            "theme_families": ["Baby bit escalation"],
            "code_labels": ["baby bit", "kill bit", "bozo bit", "skill issue bit"],
            "keywords": ["baby doll", "baby girl", "baby karen", "kill you", "bozo", "skill issue"],
            "why_unique": "Katya's teasing is more specific than generic banter: pet-name flirtation, insult slang, and mock violence recur in the same register, making the relationship style less interchangeable than 'playful teasing' would suggest.",
            "shared_baseline_comparison": "The shared baseline retains teasing as common but variation-sensitive. Katya's version stands out because the register stays abrasive and flirt-adjacent rather than ceremonial, protective, or situational.",
            "limits": "This is still an interactional-style claim, not a motive claim. The same words can land differently with unseen tone or facial expression.",
            "source_kind": "combination",
            "weakness_bucket": "weakly_shared_pattern",
        },
        {
            "dimension_name": "Abrupt anti-pestering boundary control",
            "uniqueness_type": "outside_shared_baseline",
            "shared_behavior_name": "",
            "theme_families": ["Don'T guardrails"],
            "code_labels": ["please stop guardrail", "don't ask guardrail", "stop asking guardrail"],
            "keywords": ["stop asking", "can you stop asking", "don't ask", "dumb questions", "shut up"],
            "why_unique": "Katya's boundary-setting recurs as abrupt anti-pestering control rather than low-drama cleanup. The pattern matters because the room is managed through visible shut-downs, not just through neutral moderation reminders.",
            "shared_baseline_comparison": "The shared baseline keeps guardrails outside the baseline. Katya's correction style therefore functions as a stronger audience-control marker instead of a generic moderation trait.",
            "limits": "This family is much smaller than her main pacing and banter families, so it should be treated as a sharp secondary differentiator.",
            "source_kind": "outside_family",
            "weakness_bucket": "outside_shared_baseline",
        },
        {
            "dimension_name": "Boss/casino/employment world-building",
            "uniqueness_type": "outside_shared_baseline",
            "shared_behavior_name": "",
            "theme_families": ["Baby bit escalation", "Thank recognition loops", "Request pacing control"],
            "code_labels": ["welcome bit", "thank ritual", "baby bit", "request pace"],
            "keywords": ["casino", "employment", "boss", "supporting the casino", "staff"],
            "why_unique": "Katya repeatedly frames audience relations through boss/casino/employment fiction. That running social world is not part of the shared baseline and makes her audience contact less interchangeable.",
            "shared_baseline_comparison": "The shared baseline accounts for thanking, pacing, and teasing separately, but not for recurring casino / boss / employment world-building as the relationship frame itself.",
            "limits": "This pattern is conceptually strong but depends on the repeated fiction staying visible across multiple rows; it should not be stretched into a total off-stream persona claim.",
            "source_kind": "outside_family",
            "weakness_bucket": "outside_shared_baseline",
        },
    ],
    "Leia Memoria【Variance Project】": [
        {
            "dimension_name": "Ceremonial hostess framing",
            "uniqueness_type": "distinctive_combination_of_traits",
            "shared_behavior_name": "gratitude ritualization; performative teasing and welcome escalation",
            "theme_families": ["Thank recognition loops", "Baby bit escalation"],
            "code_labels": ["thank ritual", "thanks ritual", "appreciate ritual", "welcome bit"],
            "keywords": ["bless", "goddess", "tavern", "academy", "traveler"],
            "why_unique": "Leia does not simply thank or welcome people. She repeatedly frames acknowledgment as blessing, hospitality, or in-world hosting, which makes gratitude feel like ritual reception rather than generic support recognition.",
            "shared_baseline_comparison": "Gratitude and welcome escalation are both shared, but Leia's version is more hostess-like and ceremonial than the initiation, dry-salute, or communal-check-in variants visible elsewhere in the dataset.",
            "limits": "The ritual tone is clear in text, though its full warmth may depend on tone and delivery that text cannot capture.",
            "source_kind": "combination",
            "weakness_bucket": "weakly_shared_pattern",
        },
        {
            "dimension_name": "Coach / trainer lock-in rhetoric",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "audience-attentive pacing control",
            "theme_families": ["Focus pacing control", "I'M self-disclosure windows"],
            "code_labels": ["request pace", "focus pace", "time pace", "listen up pace", "need disclosure"],
            "keywords": ["focus", "lock in", "need to focus", "everyone let's focus"],
            "why_unique": "Leia's pacing control repeatedly sounds like coaching or training. The room is rallied, refocused, and pushed to lock in rather than simply redirected.",
            "shared_baseline_comparison": "Pacing control is baseline, but Leia's version is more coach-like than the disciplinary, self-explanatory, or theatrical re-staging variants visible elsewhere in the dataset.",
            "limits": "This section tracks text-visible rhetoric and pacing control, not whether viewers actually experience the channel as coaching in a broader sense.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "shared_baseline_variant",
        },
        {
            "dimension_name": "Maternal-protective care/play blend",
            "uniqueness_type": "distinctive_combination_of_traits",
            "shared_behavior_name": "performative teasing and welcome escalation; reassurance during strain or recovery",
            "theme_families": ["Baby bit escalation", "Take reassurance stance"],
            "code_labels": ["baby bit", "it's okay care", "don't worry care", "got care", "proud care", "take care care"],
            "keywords": ["my baby", "save my baby", "little baby", "superstar", "don't worry"],
            "why_unique": "Leia's teasing and reassurance repeatedly fuse into a caretaker-to-dependent frame. 'Baby' language functions less like a cute lexical habit and more like a protective social role.",
            "shared_baseline_comparison": "The dataset baseline retains reassurance and playful escalation, but Leia's version repeatedly merges them into protective rescue or caretaker language rather than insult, menace, or low-key permission.",
            "limits": "Because this dimension synthesizes play and care families together, it should be read as a repeated interactional blend rather than a claim about fixed identity.",
            "source_kind": "combination",
            "weakness_bucket": "weakly_shared_pattern",
        },
        {
            "dimension_name": "Open anxiety / pressure disclosure without losing warmth",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "audience-facing self-disclosure windows",
            "theme_families": ["I'M self-disclosure windows"],
            "code_labels": ["need disclosure", "feel disclosure", "i'm scared disclosure", "anxiety disclosure", "burnout disclosure", "mental health disclosure"],
            "keywords": ["social anxiety", "i'm so scared", "overwhelmed", "anxiety attack", "pressure"],
            "why_unique": "Leia is unusually explicit about anxiety, fear, or pressure, but the disclosures still stay warm and relational rather than bitter, sardonic, or purely utilitarian.",
            "shared_baseline_comparison": "Self-disclosure is shared, but Leia's version keeps emotional legibility and warmth together more consistently than the peer set.",
            "limits": "This dimension is strong in retained evidence, but it still describes text-visible openness rather than inner state outside stream.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "weakly_shared_pattern",
        },
        {
            "dimension_name": "Protective suspense-boundary enforcement",
            "uniqueness_type": "outside_shared_baseline",
            "shared_behavior_name": "",
            "theme_families": ["Not guardrails"],
            "code_labels": ["no spoilers guardrail", "please stop guardrail", "do not guardrail", "stop asking guardrail"],
            "keywords": ["no spoilers", "no backsit", "no backseat", "open fire", "please no spoilers"],
            "why_unique": "Leia's spoiler and backseat rows matter because they repeatedly frame rules as protection of shared suspense and audience pacing, not just as literal policy reminders. The boundary style is explicit, protective, and sometimes theatrically sharpened.",
            "shared_baseline_comparison": "The shared baseline keeps guardrails outside common baseline status. Leia's suspense-protective corrections therefore function as a stronger local uniqueness marker than generic housekeeping would.",
            "limits": "This family is smaller than Leia's core gratitude, pacing, care, and disclosure families, so it should stay secondary in the profile.",
            "source_kind": "outside_family",
            "weakness_bucket": "outside_shared_baseline",
        },
    ],
    "Terberri Solaris Ch": [
        {
            "dimension_name": "Reciprocal gratitude/check-in loop",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "gratitude ritualization",
            "theme_families": ["Thank recognition loops"],
            "code_labels": ["thank ritual", "appreciate ritual", "thanks ritual"],
            "keywords": ["appreciate y'all", "check in", "thank you everyone", "being here"],
            "why_unique": "Terberri's gratitude is unusually reciprocal and check-in heavy. People are thanked not only for support events but for waiting, checking in, or simply being there.",
            "shared_baseline_comparison": "Gratitude ritualization is baseline across the set, but Terberri's version leans communal and reciprocal rather than ceremonial, sardonic, or blessing-oriented.",
            "limits": "The effect is very clear in text, though text-only evidence still cannot fully capture vocal softness.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "shared_baseline_variant",
        },
        {
            "dimension_name": "Need-led flow control",
            "uniqueness_type": "distinctive_combination_of_traits",
            "shared_behavior_name": "audience-facing self-disclosure windows; audience-attentive pacing control",
            "theme_families": ["I'M self-disclosure windows", "Focus pacing control"],
            "code_labels": ["need disclosure", "feel disclosure", "focus pace", "time pace"],
            "keywords": ["need to focus", "need a rest", "need to end stream", "sorry y'all"],
            "why_unique": "What stands out is not just that Terberri steers pacing, but that pacing control is repeatedly justified through her own current capacity, fatigue, or need to refocus.",
            "shared_baseline_comparison": "Pacing and self-disclosure both appear in the baseline, but Terberri's version repeatedly knots them together into self-explanatory, need-led control rather than command, coaching, or theatrical restaging.",
            "limits": "This is a combination claim, so it depends on repeated co-occurrence across families rather than on one exclusive code.",
            "source_kind": "combination",
            "weakness_bucket": "weakly_shared_pattern",
        },
        {
            "dimension_name": "Worth/self-care reassurance",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "reassurance during strain or recovery",
            "theme_families": ["Take reassurance stance"],
            "code_labels": ["it's okay care", "take care care", "no worries care", "don't worry care", "proud care"],
            "keywords": ["worth it", "don't have to apologize", "take care", "no worries", "enjoy"],
            "why_unique": "Terberri's reassurance repeatedly leans toward permission, worth affirmation, and practical self-care rather than toward sharper deflection or overt roleplayed caretaking.",
            "shared_baseline_comparison": "Reassurance is baseline, but Terberri's version is more worth-affirming and permission-giving than the sharper damage-control, banter-wrapped, or overtly protective variants visible elsewhere in the dataset.",
            "limits": "The pattern is strong in retained rows, though text-only evidence cannot fully recover vocal warmth or pacing.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "shared_baseline_variant",
        },
        {
            "dimension_name": "Situational chaos/twist welcomes",
            "uniqueness_type": "distinctive_version_of_shared_behavior",
            "shared_behavior_name": "performative teasing and welcome escalation",
            "theme_families": ["Welcome bit escalation"],
            "code_labels": ["welcome bit", "chaos bit", "queen bit", "ceremony bit"],
            "keywords": ["welcome to the twist", "welcome to the chaos", "randomizer"],
            "why_unique": "Terberri's bit style is less ceremonial or abrasive than peers. Playful welcomes usually work as scene-setting for the current twist, randomizer, or gimmick rather than as a stable persona script.",
            "shared_baseline_comparison": "Teasing and escalation are shared, but Terberri's version is more situational and game-frame oriented than the rest of the set.",
            "limits": "This family is smaller than gratitude, disclosure, and reassurance, so it should be treated as a meaningful but not dominant uniqueness signal.",
            "source_kind": "shared_behavior",
            "weakness_bucket": "weakly_shared_pattern",
        },
        {
            "dimension_name": "Light social-hygiene boundaries",
            "uniqueness_type": "outside_shared_baseline",
            "shared_behavior_name": "",
            "theme_families": ["Don'T guardrails"],
            "code_labels": ["don't weird guardrail", "don't spam guardrail", "do not guardrail"],
            "keywords": ["don't be weird", "don't spam", "follow back", "please do not ask"],
            "why_unique": "When Terberri sets boundaries, the dominant pattern is low-drama social hygiene rather than a harsh moderation persona. That is still outside the shared baseline and therefore uniqueness-relevant.",
            "shared_baseline_comparison": "The shared baseline leaves guardrails outside common baseline status. Terberri's guardrails matter because they recur, but they read as cleanup and social hygiene rather than as a major authority performance.",
            "limits": "This pattern is weaker than the channel's gratitude, disclosure, and reassurance families, so it should stay secondary.",
            "source_kind": "outside_family",
            "weakness_bucket": "outside_shared_baseline",
        },
    ],
}


def read_csv(path: Path) -> List[dict]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, fieldnames: Sequence[str], rows: Sequence[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def strip_variant_suffix(talent: str) -> str:
    base = re.sub(r"[【\[].*?[】\]]", "", talent)
    return re.sub(r"\s+", " ", base).strip()


def short_talent_name(talent: str) -> str:
    base = strip_variant_suffix(talent)
    parts = base.split()
    if not parts:
        return talent
    if len(parts) >= 2 and parts[-1].lower() == "ch":
        return parts[0]
    return parts[0]


def shared_roster_tag(talent: str) -> str:
    match = re.search(r"[【\[]([^】\]]+)[】\]]", talent)
    return match.group(1).strip() if match else ""


def can_name_peer(target_talent: str, peer_talent: str) -> bool:
    target_tag = shared_roster_tag(target_talent)
    peer_tag = shared_roster_tag(peer_talent)
    return bool(target_tag and peer_tag and target_tag == peer_tag)


def prioritized_peers(target_talent: str, peer_order: Sequence[str]) -> List[str]:
    same_roster = [peer for peer in peer_order if can_name_peer(target_talent, peer)]
    others = [peer for peer in peer_order if peer not in same_roster]
    return same_roster + others


def normalize_token(text: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", (text or "").lower())


def sec_to_timecode(seconds: float) -> str:
    if math.isnan(seconds) or seconds < 0:
        return "00:00:00"
    total = int(round(seconds))
    hours = total // 3600
    minutes = (total % 3600) // 60
    secs = total % 60
    return f"{hours:02d}:{minutes:02d}:{secs:02d}"


def parse_int(value: object) -> int:
    try:
        return int(float(str(value).strip()))
    except Exception:
        return 0


def parse_float(value: object) -> float:
    try:
        return float(str(value).strip())
    except Exception:
        return math.nan


def shorten_quote(text: str, limit: int = 140) -> str:
    cleaned = re.sub(r"\s+", " ", (text or "").strip())
    if len(cleaned) <= limit:
        return cleaned
    return cleaned[: limit - 3].rstrip() + "..."


def family_bucket(theme_family: str) -> str:
    lowered = (theme_family or "").lower()
    if "guardrail" in lowered:
        return "guardrails"
    if "co-regulation" in lowered:
        return "co_regulation"
    if "money framing" in lowered:
        return "money"
    return "shared_or_other"


def parse_summary_context(md_path: Optional[Path], state_path: Optional[Path]) -> dict:
    info = {
        "used": False,
        "retained_codes": [],
        "relationship_descriptors": [],
        "reciprocity": {},
        "pacing": {},
    }
    if not md_path or not md_path.exists():
        return info

    text = md_path.read_text(encoding="utf-8")
    info["used"] = True
    if state_path and state_path.exists():
        try:
            state = json.loads(state_path.read_text(encoding="utf-8"))
            info["retained_codes"] = list(state.get("retained_code_names") or [])
        except Exception:
            pass

    match = re.search(
        r"text-visible relationship with chat is most often described as ([^.]+)\.",
        text,
        re.I,
    )
    if match:
        descriptors = [part.strip() for part in match.group(1).split(",") if part.strip()]
        info["relationship_descriptors"] = descriptors

    reciprocity_match = re.search(
        r"reciprocity is described as ([^.]+?) summaries\.",
        text,
        re.I,
    )
    if reciprocity_match:
        counts = {}
        for label, value in re.findall(r"(high|moderate|low) in (\d+)", reciprocity_match.group(1), re.I):
            counts[label.lower()] = int(value)
        info["reciprocity"] = counts

    pacing_match = re.search(
        r"pacing is most often ([^.]+?) summaries\.",
        text,
        re.I,
    )
    if pacing_match:
        counts = {}
        for label, value in re.findall(r"(rapid|moderate|slow) in (\d+)", pacing_match.group(1), re.I):
            counts[label.lower()] = int(value)
        info["pacing"] = counts
    return info


@dataclass
class TalentSource:
    talent: str
    root: Path
    source_label: str
    codebook_path: Path
    evidence_path: Path
    profile_path: Path
    summary_md_path: Optional[Path]
    summary_state_path: Optional[Path]


@dataclass
class EvidenceRow:
    talent: str
    code_label: str
    theme_family: str
    video_id: str
    time_in_seconds: float
    timecode: str
    source: str
    speaker: str
    quote: str
    evidence_role: str
    monetary_context: str
    incorporated_in_run: str


@dataclass
class FamilyStat:
    theme_family: str
    total_frequency: int
    max_stream_coverage: int
    codes: List[Tuple[str, int, int]]


@dataclass
class Dimension:
    dimension_name: str
    uniqueness_type: str
    shared_behavior_name: str
    why_unique: str
    shared_baseline_comparison: str
    contributing_open_codes: List[str]
    theme_families: List[str]
    evidence_pattern: str
    supporting_rows: List[EvidenceRow]
    limits: str
    source_kind: str
    weakness_bucket: str


@dataclass
class TalentBundle:
    source: TalentSource
    codebook_rows: List[dict]
    evidence_rows: List[EvidenceRow]
    summary_context: dict
    family_stats: Dict[str, FamilyStat]
    code_stats: Dict[str, dict]


def discover_talents(talent_scope: Optional[str] = None) -> List[TalentSource]:
    sources: List[TalentSource] = []
    for talent_dir in sorted(path for path in DATA_ROOT.iterdir() if path.is_dir()):
        if talent_dir.name == "VarianceProject":
            continue
        if talent_scope and talent_dir.name != talent_scope:
            continue
        summary_md = talent_dir / "stream_summaries/overall_channel_summary/current/overall_channel_summary.md"
        if not summary_md.exists():
            summary_md = (
                talent_dir
                / "stream_summaries"
                / "overall_themes"
                / "summary_classification"
                / "current"
                / "overall_themes_codex.md"
            )
        if not summary_md.exists():
            if talent_scope:
                summary_md = None
            else:
                continue
        if summary_md is None and not talent_scope:
            continue

        v3_root = (
            talent_dir
            / "stream_summaries"
            / "overall_themes"
            / "personality_open_coding"
            / "v3"
            / "current"
        )
        v2_root = (
            talent_dir
            / "stream_summaries"
            / "overall_themes"
            / "personality_open_coding"
            / "v2"
        )
        v3_paths = {
            "codebook": v3_root / "open_codebook_v3.csv",
            "evidence": v3_root / "open_coding_evidence_v3.csv",
            "profile": v3_root / "personality_profile_v3_open_coding.md",
        }
        v2_paths = {
            "codebook": v2_root / "open_codebook_v2.csv",
            "evidence": v2_root / "open_coding_evidence_v2.csv",
            "profile": talent_dir
            / "stream_summaries"
            / "overall_themes"
            / "personality_open_coding"
            / "v2"
            / "personality_profile_v2_open_coding.md",
        }

        if all(path.exists() for path in v3_paths.values()):
            source_label = "v3 current"
            selected = v3_paths
        elif all(path.exists() for path in v2_paths.values()):
            source_label = "v2 fallback"
            selected = v2_paths
        else:
            continue

        summary_state = None
        if summary_md is not None:
            summary_state = summary_md.parent / "overall_channel_summary_state.json"
            if not summary_state.exists():
                summary_state = summary_md.parent / "summary_classification_state.json"
        sources.append(
            TalentSource(
                talent=talent_dir.name,
                root=talent_dir,
                source_label=source_label,
                codebook_path=selected["codebook"],
                evidence_path=selected["evidence"],
                profile_path=selected["profile"],
                summary_md_path=summary_md,
                summary_state_path=summary_state if summary_state and summary_state.exists() else None,
            )
        )
    if talent_scope and not sources:
        raise SystemExit(f"Talent not eligible or not found for exact scope: {talent_scope}")
    return sources


def load_talent_bundle(source: TalentSource) -> TalentBundle:
    codebook_rows = read_csv(source.codebook_path)
    evidence_rows = [
        EvidenceRow(
            talent=row.get("talent", source.talent),
            code_label=row.get("code_label", ""),
            theme_family=row.get("theme_family", ""),
            video_id=row.get("video_id", ""),
            time_in_seconds=parse_float(row.get("time_in_seconds", "")),
            timecode=row.get("timecode", "") or sec_to_timecode(parse_float(row.get("time_in_seconds", ""))),
            source=row.get("source", ""),
            speaker=row.get("speaker", ""),
            quote=row.get("quote", "").strip(),
            evidence_role=row.get("evidence_role", ""),
            monetary_context=row.get("monetary_context", "none"),
            incorporated_in_run=row.get("incorporated_in_run", ""),
        )
        for row in read_csv(source.evidence_path)
    ]
    summary_context = parse_summary_context(source.summary_md_path, source.summary_state_path)

    family_totals: Dict[str, dict] = defaultdict(lambda: {"frequency": 0, "coverage": 0, "codes": []})
    code_stats: Dict[str, dict] = {}
    for row in codebook_rows:
        freq = parse_int(row.get("frequency_count"))
        coverage = parse_int(row.get("stream_coverage_count"))
        code_label = row.get("code_label", "")
        theme_family = row.get("theme_family", "")
        family_totals[theme_family]["frequency"] += freq
        family_totals[theme_family]["coverage"] = max(family_totals[theme_family]["coverage"], coverage)
        family_totals[theme_family]["codes"].append((code_label, freq, coverage))
        code_stats[code_label] = {
            "theme_family": theme_family,
            "frequency": freq,
            "coverage": coverage,
            "memo_notes": row.get("memo_notes", ""),
        }

    family_stats = {
        family: FamilyStat(
            theme_family=family,
            total_frequency=info["frequency"],
            max_stream_coverage=info["coverage"],
            codes=sorted(info["codes"], key=lambda item: (item[1], item[2], item[0]), reverse=True),
        )
        for family, info in family_totals.items()
    }

    return TalentBundle(
        source=source,
        codebook_rows=codebook_rows,
        evidence_rows=evidence_rows,
        summary_context=summary_context,
        family_stats=family_stats,
        code_stats=code_stats,
    )


def load_baseline() -> dict:
    codebook_rows = read_csv(BASELINE_ROOT / "shared_behavior_codebook.csv")
    matrix_rows = read_csv(BASELINE_ROOT / "talent_shared_behavior_matrix.csv")
    shared_evidence_rows = read_csv(BASELINE_ROOT / "shared_behavior_evidence.csv")
    baseline_markdown = (BASELINE_ROOT / "shared_behavior_baseline_codex.md").read_text(encoding="utf-8")

    behavior_codebook = {row["shared_behavior_name"]: row for row in codebook_rows}
    matrix_by_talent: Dict[str, Dict[str, dict]] = defaultdict(dict)
    for row in matrix_rows:
        matrix_by_talent[row["talent"]][row["shared_behavior_name"]] = row

    shared_by_talent_behavior: Dict[Tuple[str, str], List[dict]] = defaultdict(list)
    family_behavior_map: Dict[Tuple[str, str], set] = defaultdict(set)
    code_behavior_map: Dict[Tuple[str, str], set] = defaultdict(set)
    for row in shared_evidence_rows:
        key = (row["talent"], row["shared_behavior_name"])
        shared_by_talent_behavior[key].append(row)
        family_behavior_map[(row["talent"], row["theme_family"])].add(row["shared_behavior_name"])
        code_behavior_map[(row["talent"], row["contributing_open_code"])].add(row["shared_behavior_name"])

    non_baseline_markers = set()
    if "### guardrail enforcement" in baseline_markdown:
        non_baseline_markers.add("guardrails")
    if "### explicit co-regulation loops" in baseline_markdown:
        non_baseline_markers.add("co_regulation")
    if "### money-linked framing" in baseline_markdown:
        non_baseline_markers.add("money")

    return {
        "behavior_codebook": behavior_codebook,
        "matrix_by_talent": matrix_by_talent,
        "shared_by_talent_behavior": shared_by_talent_behavior,
        "family_behavior_map": family_behavior_map,
        "code_behavior_map": code_behavior_map,
        "non_baseline_markers": non_baseline_markers,
        "baseline_markdown": baseline_markdown,
    }


def is_streamer_row(talent: str, speaker: str) -> bool:
    normalized = normalize_token((speaker or "").lstrip("@"))
    if normalized in STREAMER_SPEAKERS:
        return True
    tokens = [normalize_token(piece) for piece in strip_variant_suffix(talent).split()]
    return any(token and token in normalized for token in tokens)


def family_rows(bundle: TalentBundle, theme_families: Iterable[str], code_labels: Iterable[str] = ()) -> List[EvidenceRow]:
    theme_families = set(theme_families)
    code_labels = set(code_labels)
    rows = []
    for row in bundle.evidence_rows:
        if not is_streamer_row(bundle.source.talent, row.speaker):
            continue
        if row.theme_family in theme_families or row.code_label in code_labels:
            rows.append(row)
    return rows


def distinctive_keywords(version_text: str, dimension_name: str, extra_codes: Iterable[str]) -> List[str]:
    text = " ".join([version_text or "", dimension_name or "", " ".join(extra_codes)]).lower()
    keyword_groups = [
        "ceremony",
        "ceremonial",
        "roll-call",
        "dramatic",
        "member",
        "welcome",
        "right side of history",
        "mock",
        "ominous",
        "dark",
        "sardonic",
        "dry",
        "salute",
        "brisk",
        "command",
        "redirect",
        "blunt",
        "focus",
        "lock in",
        "coach",
        "gentle",
        "self-disclosed",
        "anxiety",
        "frustration",
        "discomfort",
        "irritation",
        "complaint",
        "overwhelm",
        "worth",
        "baby",
        "twist",
        "appreciate y'all",
        "bless",
        "protective",
        "nurturing",
        "reassurance",
        "spam",
        "weird",
        "ask",
        "not okay",
    ]
    return [keyword for keyword in keyword_groups if keyword in text]


def score_row_for_keywords(row: EvidenceRow, keywords: Sequence[str]) -> int:
    text = f"{row.code_label} {row.theme_family} {row.quote}".lower()
    score = 0
    for keyword in keywords:
        if keyword and keyword in text:
            score += 3 if " " in keyword else 2
    if row.evidence_role == "primary":
        score += 2
    if row.source == "subtitle":
        score += 1
    return score


def pick_rows(
    rows: Sequence[EvidenceRow],
    *,
    keywords: Sequence[str],
    desired: int = 3,
) -> List[EvidenceRow]:
    seen_quotes = set()
    deduped: List[EvidenceRow] = []
    for row in rows:
        key = (row.video_id, row.timecode, row.quote)
        if key in seen_quotes:
            continue
        seen_quotes.add(key)
        deduped.append(row)

    ranked = sorted(
        deduped,
        key=lambda row: (
            score_row_for_keywords(row, keywords),
            1 if row.evidence_role == "primary" else 0,
            1 if row.source == "subtitle" else 0,
            len(row.quote),
        ),
        reverse=True,
    )

    selected: List[EvidenceRow] = []
    seen_video_ids = set()
    positive_ranked = [row for row in ranked if score_row_for_keywords(row, keywords) > 0]
    first_pass = positive_ranked if positive_ranked else ranked
    for row in first_pass:
        if row.video_id not in seen_video_ids:
            selected.append(row)
            seen_video_ids.add(row.video_id)
        if len(selected) >= desired:
            return selected
    for row in ranked:
        if row not in selected:
            selected.append(row)
        if len(selected) >= desired:
            break
    return selected


def infer_shared_dimension_name(shared_behavior_name: str, version_text: str) -> str:
    lowered = (version_text or "").lower()
    if shared_behavior_name == "gratitude ritualization":
        if "ceremony" in lowered or "ceremonial" in lowered or "roll-call" in lowered:
            return "Ceremonial gratitude roll-calls"
        if "sardonic" in lowered or "dry salute" in lowered or "brisk" in lowered:
            return "Dry-salute gratitude"
        if "blessing" in lowered or "warm" in lowered or "welcome" in lowered:
            return "Blessing-style welcome gratitude"
        if "communal" in lowered or "reciprocal" in lowered or "appreciate y'all" in lowered:
            return "Communal check-in gratitude"
        return "Distinctive gratitude ritualization"

    if shared_behavior_name == "audience-attentive pacing control":
        if "re-stage" in lowered or "counter" in lowered or "reset sequence" in lowered:
            return "Theatrical flow resets"
        if "command-style" in lowered or "sharper redirects" in lowered or "blunt" in lowered:
            return "Command-style redirect control"
        if "coaching" in lowered or "lock in" in lowered or "rallying" in lowered:
            return "Coach-like lock-in pacing"
        if "gentler" in lowered or "self-disclosed" in lowered or "explaining the need" in lowered:
            return "Gentle self-explained focus resets"
        return "Distinctive pacing control"

    if shared_behavior_name == "reassurance during strain or recovery":
        if "bantering" in lowered or "dramatic" in lowered:
            return "Banter-wrapped reassurance"
        if "deflective" in lowered or "damage control" in lowered or "brusque" in lowered:
            return "Brusque damage-control reassurance"
        if "nurturing" in lowered or "sustained" in lowered:
            return "Sustained nurturing reassurance"
        if "worth-focused" in lowered or "check-in" in lowered or "affirming" in lowered:
            return "Worth-affirming reassurance"
        return "Distinctive reassurance"

    if shared_behavior_name == "audience-facing self-disclosure windows":
        if "anxiety" in lowered or "frustration" in lowered or "discomfort" in lowered:
            return "Performative anxiety transparency"
        if "sardonic" in lowered or "irritation" in lowered or "complaint" in lowered or "rant" in lowered:
            return "Complaint-shaped self-disclosure"
        if "pressure" in lowered or "emotionally" in lowered or "transparent" in lowered:
            return "Emotionally transparent pressure updates"
        if "overwhelm" in lowered or "self-check-in" in lowered:
            return "Soft overwhelm check-ins"
        return "Distinctive self-disclosure"

    if shared_behavior_name == "performative teasing and welcome escalation":
        if "mock-ominous" in lowered or "theatrical" in lowered or "darker" in lowered:
            return "Mock-ominous welcome ceremony"
        if "abrasive" in lowered or "insult-flavored" in lowered or "deadpan" in lowered:
            return "Insult-flavored deadpan banter"
        if "affectionate" in lowered or "caretaking" in lowered or "protective" in lowered:
            return "Protective baby-talk escalation"
        if "twist" in lowered or "situational" in lowered or "randomizer" in lowered:
            return "Twist-framed playful welcomes"
        return "Distinctive teasing and welcome escalation"

    return shared_behavior_name.title()


def infer_outside_dimension_name(bundle: TalentBundle, family_stat: FamilyStat) -> str:
    bucket = family_bucket(family_stat.theme_family)
    code_labels = [code for code, _, _ in family_stat.codes]
    joined = " ".join(code_labels).lower()
    if bucket == "guardrails":
        if "spam" in joined or "weird" in joined:
            return "Explicit anti-spam / anti-weird guardrails"
        if "please stop" in joined or "stop asking" in joined or "don't ask" in joined:
            return "Direct audience-correction guardrails"
        return "Recurring guardrail enforcement"
    if bucket == "co_regulation":
        if "let's go" in joined or "woo" in joined or "hype" in joined:
            return "Crowd-rally co-regulation loops"
        return "Call-and-response regulation loops"
    if bucket == "money":
        return "Money-linked relationship framing"
    return family_stat.theme_family


def dominant_label(counts: Dict[str, int], fallback: str) -> str:
    if not counts:
        return fallback
    return max(counts.items(), key=lambda item: item[1])[0]


def split_behavior_names(value: str) -> List[str]:
    return [part.strip() for part in (value or "").split(";") if part.strip()]


def similarity_score(left: TalentBundle, right: TalentBundle) -> float:
    families = sorted(set(left.family_stats) | set(right.family_stats))
    if not families:
        return 0.0
    left_vector = []
    right_vector = []
    for family in families:
        l = left.family_stats.get(family)
        r = right.family_stats.get(family)
        left_vector.append((l.total_frequency if l else 0) / max((l.max_stream_coverage if l else 1), 1))
        right_vector.append((r.total_frequency if r else 0) / max((r.max_stream_coverage if r else 1), 1))
    numerator = sum(a * b for a, b in zip(left_vector, right_vector))
    left_norm = math.sqrt(sum(a * a for a in left_vector))
    right_norm = math.sqrt(sum(b * b for b in right_vector))
    if not left_norm or not right_norm:
        return 0.0
    return numerator / (left_norm * right_norm)


def select_peer_order(target: TalentBundle, all_bundles: Dict[str, TalentBundle]) -> List[str]:
    ranked = []
    for other_name, other in all_bundles.items():
        if other_name == target.source.talent:
            continue
        ranked.append((similarity_score(target, other), other_name))
    ranked.sort(reverse=True)
    return [name for _, name in ranked]


def family_stats_for_behavior(
    talent: str,
    shared_behavior_name: str,
    bundle: TalentBundle,
    baseline: dict,
) -> Tuple[List[str], List[str]]:
    shared_rows = baseline["shared_by_talent_behavior"].get((talent, shared_behavior_name), [])
    theme_families = sorted({row["theme_family"] for row in shared_rows})
    code_labels = sorted({row["contributing_open_code"] for row in shared_rows})
    if not theme_families:
        for code_label, code_info in bundle.code_stats.items():
            mapped = baseline["code_behavior_map"].get((talent, code_label), set())
            if shared_behavior_name in mapped:
                code_labels.append(code_label)
                theme_families.append(code_info["theme_family"])
    return sorted(set(theme_families)), sorted(set(code_labels))


def best_outside_families(bundle: TalentBundle, baseline: dict) -> List[FamilyStat]:
    candidates: List[FamilyStat] = []
    for theme_family, stat in bundle.family_stats.items():
        bucket = family_bucket(theme_family)
        if bucket == "shared_or_other":
            continue
        if bucket == "money":
            continue
        if bucket not in baseline["non_baseline_markers"]:
            continue
        if stat.total_frequency < 8 and stat.max_stream_coverage < 5:
            continue
        candidates.append(stat)
    candidates.sort(
        key=lambda stat: (
            OUTSIDE_FAMILY_ORDER.index(family_bucket(stat.theme_family)),
            -stat.max_stream_coverage,
            -stat.total_frequency,
        )
    )
    return candidates


def shared_candidate_order(bundle: TalentBundle, baseline: dict) -> List[str]:
    behavior_map = baseline["matrix_by_talent"][bundle.source.talent]

    def score(shared_behavior_name: str) -> Tuple[int, int, int]:
        row = behavior_map.get(shared_behavior_name)
        if not row:
            return (-1, -1, -1)
        support_bonus = {"strong": 2, "moderate": 1}.get((row.get("support_level") or "").lower(), 0)
        class_bonus = 2 if row.get("classification") == "shared_but_distinctive_in_form" else 1
        families, _ = family_stats_for_behavior(bundle.source.talent, shared_behavior_name, bundle, baseline)
        family_score = sum(bundle.family_stats[family].total_frequency for family in families if family in bundle.family_stats)
        return (class_bonus, support_bonus, family_score)

    available = [name for name in BEHAVIOR_ORDER if name in behavior_map]
    return sorted(available, key=score, reverse=True)


def build_peer_comparison_snippet(
    target_talent: str,
    shared_behavior_name: str,
    baseline: dict,
    peer_order: Sequence[str],
) -> str:
    target_row = baseline["matrix_by_talent"][target_talent].get(shared_behavior_name)
    if not target_row:
        return "Peer comparison unavailable."
    snippets = []
    for index, peer in enumerate(prioritized_peers(target_talent, peer_order)):
        peer_row = baseline["matrix_by_talent"].get(peer, {}).get(shared_behavior_name)
        if not peer_row:
            continue
        peer_version = peer_row["talent_specific_version"].rstrip(".").lower()
        target_version = target_row["talent_specific_version"].rstrip(".").lower()
        if can_name_peer(target_talent, peer):
            snippets.append(
                f"Could be confused with {strip_variant_suffix(peer)} on {BASELINE_LABELS[shared_behavior_name]}, but differs in that {short_talent_name(target_talent)} {target_version} rather than {peer_version}."
            )
        else:
            lead = "Could be confused with other talents in the dataset" if index == 0 else "Could also be confused with peer streamers in this corpus"
            snippets.append(
                f"{lead} on {BASELINE_LABELS[shared_behavior_name]}, but differs in that {short_talent_name(target_talent)} {target_version} rather than the more {peer_version} version visible elsewhere."
            )
        if len(snippets) >= 2:
            break
    return " ".join(snippets) if snippets else "Peer comparison unavailable."


def build_shared_dimension(
    bundle: TalentBundle,
    shared_behavior_name: str,
    baseline: dict,
    peer_order: Sequence[str],
) -> Dimension:
    matrix_row = baseline["matrix_by_talent"][bundle.source.talent][shared_behavior_name]
    version_text = matrix_row["talent_specific_version"]
    dimension_name = infer_shared_dimension_name(shared_behavior_name, version_text)
    theme_families, code_labels = family_stats_for_behavior(
        bundle.source.talent,
        shared_behavior_name,
        bundle,
        baseline,
    )
    evidence_pool = family_rows(bundle, theme_families, code_labels)
    keywords = distinctive_keywords(version_text, dimension_name, code_labels)
    supporting_rows = pick_rows(evidence_pool, keywords=keywords, desired=3)

    contributing_codes = []
    for theme_family in theme_families:
        stat = bundle.family_stats.get(theme_family)
        if not stat:
            continue
        contributing_codes.extend(code for code, _, _ in stat.codes[:3])
    contributing_codes = list(dict.fromkeys(contributing_codes))[:5]

    coverage_bits = []
    for theme_family in theme_families:
        stat = bundle.family_stats.get(theme_family)
        if stat:
            coverage_bits.append(
                f"{theme_family} (freq={stat.total_frequency}, streams={stat.max_stream_coverage})"
            )
    summary_codes = bundle.summary_context.get("retained_codes") or []
    summary_note = ""
    if summary_codes:
        summary_note = f" Summary classification also retains {', '.join(summary_codes[:5])}."

    behavior_row = baseline["behavior_codebook"][shared_behavior_name]
    why_unique = (
        f"{short_talent_name(bundle.source.talent)} does not merely show {BASELINE_LABELS[shared_behavior_name]}; "
        f"{version_text.rstrip('.')} in a way that remains identifiable even against the shared baseline. "
        f"{build_peer_comparison_snippet(bundle.source.talent, shared_behavior_name, baseline, peer_order)}"
    )
    shared_baseline_comparison = (
        f"The shared baseline treats {shared_behavior_name} as {behavior_row['classification']}. "
        f"{behavior_row['baseline_relevance_note']}"
    )
    evidence_pattern = (
        f"Primary support comes from {', '.join(coverage_bits)}. "
        f"Quotes were drawn from {len({row.video_id for row in supporting_rows})} different streams."
        f"{summary_note}"
    )
    limits = (
        "This is a text-visible style claim, not a claim about tone, facial expression, or off-stream intent."
        if matrix_row.get("support_level") == "strong"
        else "Evidence is present but thinner than this talent's strongest recurring families, so the contrast should be read cautiously."
    )
    weakness_bucket = (
        "weakly_shared_pattern"
        if matrix_row.get("classification") == "shared_but_distinctive_in_form"
        else "shared_baseline_variant"
    )
    return Dimension(
        dimension_name=dimension_name,
        uniqueness_type="distinctive_version_of_shared_behavior",
        shared_behavior_name=shared_behavior_name,
        why_unique=why_unique,
        shared_baseline_comparison=shared_baseline_comparison,
        contributing_open_codes=contributing_codes,
        theme_families=theme_families,
        evidence_pattern=evidence_pattern,
        supporting_rows=supporting_rows,
        limits=limits,
        source_kind="shared_behavior",
        weakness_bucket=weakness_bucket,
    )


def build_outside_dimension(bundle: TalentBundle, family_stat: FamilyStat, baseline: dict) -> Dimension:
    dimension_name = infer_outside_dimension_name(bundle, family_stat)
    codes = [code for code, _, _ in family_stat.codes[:5]]
    rows = family_rows(bundle, [family_stat.theme_family], codes)
    keywords = distinctive_keywords(" ".join(codes), dimension_name, codes)
    supporting_rows = pick_rows(rows, keywords=keywords, desired=3)

    bucket = family_bucket(family_stat.theme_family)
    comparison_lookup = {
        "guardrails": "The shared baseline explicitly left guardrail enforcement outside the retained baseline because it was too sparse and context-bound across the dataset.",
        "co_regulation": "The shared baseline marked explicit co-regulation loops as too weakly shared to retain as common baseline behavior.",
        "money": "The shared baseline treated money-linked framing as too weakly shared to stabilize a common baseline.",
    }
    why_unique = (
        f"{dimension_name} matters here because the pattern recurs within {short_talent_name(bundle.source.talent)}'s own evidence "
        f"({family_stat.total_frequency} retained instances across up to {family_stat.max_stream_coverage} streams) even though it sits outside the shared baseline."
    )
    shared_baseline_comparison = comparison_lookup.get(bucket, "This family sits outside the retained shared baseline.")
    evidence_pattern = (
        f"Primary support comes from {family_stat.theme_family} with top retained codes "
        f"{', '.join(codes[:4])}. Quotes were drawn from {len({row.video_id for row in supporting_rows})} different streams where possible."
    )
    limits = (
        "This remains smaller than the talent's main shared-behavior families, so it should be treated as a secondary but sharper uniqueness marker."
    )
    return Dimension(
        dimension_name=dimension_name,
        uniqueness_type="outside_shared_baseline",
        shared_behavior_name="",
        why_unique=why_unique,
        shared_baseline_comparison=shared_baseline_comparison,
        contributing_open_codes=codes,
        theme_families=[family_stat.theme_family],
        evidence_pattern=evidence_pattern,
        supporting_rows=supporting_rows,
        limits=limits,
        source_kind="outside_family",
        weakness_bucket="outside_shared_baseline",
    )


def build_combination_dimension(
    bundle: TalentBundle,
    selected_dimensions: Sequence[Dimension],
    baseline: dict,
    peer_order: Sequence[str],
) -> Dimension:
    shared_map = baseline["matrix_by_talent"][bundle.source.talent]
    version_texts = " ".join(row["talent_specific_version"] for row in shared_map.values()).lower()
    tag = "generic"
    if any(keyword in version_texts for keyword in ["mock-ominous", "theatrical", "ceremonial", "darker"]):
        tag = "theatrical"
    elif any(keyword in version_texts for keyword in ["abrasive", "deadpan", "sardonic", "command-style", "blunt"]):
        tag = "abrasive"
    elif any(keyword in version_texts for keyword in ["warm", "blessing", "nurturing", "protective", "coaching"]):
        tag = "protective"
    elif any(keyword in version_texts for keyword in ["communal", "reciprocal", "worth-focused", "gentler", "check-in"]):
        tag = "communal"

    if tag == "theatrical":
        dimension_name = "Ceremonial performance fused with exposed strain"
        why_unique = (
            f"What makes {short_talent_name(bundle.source.talent)} especially non-interchangeable is the way ceremonial welcome language, stop-start staging, and exposed anxiety/discomfort travel together. "
            f"Peers may share one piece of that package, but not this exact mix of performance, tension, and control."
        )
    elif tag == "abrasive":
        dimension_name = "Abrasive control fused with complaint-forward candor"
        why_unique = (
            f"{short_talent_name(bundle.source.talent)}'s strongest uniqueness signal is the bundle rather than any single code: blunt redirect control, insult-flavored banter, and irritation-shaped self-disclosure repeatedly arrive in the same interactional register."
        )
    elif tag == "protective":
        dimension_name = "Protective coaching as a relationship style"
        why_unique = (
            f"{short_talent_name(bundle.source.talent)} becomes non-interchangeable through the combination of rallying pace control, openly nurturing reassurance, and affectionate protective escalation. "
            f"The baseline contains each ingredient separately, but not this exact maternal-protective blend."
        )
    elif tag == "communal":
        dimension_name = "Communal self-check-in intimacy"
        why_unique = (
            f"{short_talent_name(bundle.source.talent)} stands out less through hard theatricality and more through a repeated package of communal gratitude, soft overwhelm disclosure, and worth-affirming care. "
            f"That combination makes the relationship stance feel reciprocal without giving up streamer control."
        )
    else:
        dimension_name = "Distinctive interaction package"
        why_unique = (
            f"{short_talent_name(bundle.source.talent)} is best understood through the combination of the retained dimensions below rather than any single trait in isolation."
        )

    selected_families = []
    selected_codes = []
    shared_behaviors = []
    pool = []
    for dimension in selected_dimensions[:3]:
        selected_families.extend(dimension.theme_families)
        selected_codes.extend(dimension.contributing_open_codes[:2])
        if dimension.shared_behavior_name:
            shared_behaviors.append(dimension.shared_behavior_name)
        pool.extend(dimension.supporting_rows[:2])
    supporting_rows = pick_rows(pool, keywords=distinctive_keywords(why_unique, dimension_name, selected_codes), desired=3)

    peer_bits = []
    for index, peer in enumerate(prioritized_peers(bundle.source.talent, peer_order)[:2]):
        peer_map = baseline["matrix_by_talent"].get(peer, {})
        overlaps = [behavior for behavior in shared_behaviors if behavior in peer_map]
        if not overlaps:
            continue
        focus_behavior = overlaps[0]
        if can_name_peer(bundle.source.talent, peer):
            peer_bits.append(
                f"Against {strip_variant_suffix(peer)}, the difference is not whether {focus_behavior} exists, but how it combines with {', '.join(strip_variant_suffix(name).lower() for name in selected_families[:2])}."
            )
        else:
            lead = "Against other talents in the dataset" if index == 0 else "Against peer streamers in this corpus"
            peer_bits.append(
                f"{lead}, the difference is not whether {focus_behavior} exists, but how it combines with {', '.join(strip_variant_suffix(name).lower() for name in selected_families[:2])}."
            )
    shared_baseline_comparison = (
        "The shared baseline already captures several ingredients individually, but their combination here remains more talent-specific than the individual pieces. "
        + " ".join(peer_bits)
    ).strip()
    evidence_pattern = (
        f"This synthesis draws across {', '.join(dict.fromkeys(selected_families))} and uses quotes from "
        f"{len({row.video_id for row in supporting_rows})} streams to show the combination rather than a one-off moment."
    )
    limits = (
        "Because this is a combination claim, it synthesizes repeated families rather than pointing to one exclusive code; it should be read as a high-level interaction pattern."
    )
    return Dimension(
        dimension_name=dimension_name,
        uniqueness_type="distinctive_combination_of_traits",
        shared_behavior_name="; ".join(dict.fromkeys(shared_behaviors)),
        why_unique=why_unique,
        shared_baseline_comparison=shared_baseline_comparison,
        contributing_open_codes=list(dict.fromkeys(selected_codes))[:6],
        theme_families=list(dict.fromkeys(selected_families))[:6],
        evidence_pattern=evidence_pattern,
        supporting_rows=supporting_rows,
        limits=limits,
        source_kind="combination",
        weakness_bucket="weakly_shared_pattern",
    )


def build_spec_dimension(bundle: TalentBundle, spec: dict) -> Dimension:
    theme_families = list(spec.get("theme_families") or [])
    code_labels = list(spec.get("code_labels") or [])
    pool = [
        row
        for row in family_rows(bundle, theme_families, code_labels)
        if (not code_labels or row.code_label in code_labels or row.theme_family in theme_families)
    ]
    supporting_rows = pick_rows(
        pool,
        keywords=list(spec.get("keywords") or []) + code_labels,
        desired=3,
    )
    summary_codes = bundle.summary_context.get("retained_codes") or []
    coverage_bits = []
    for theme_family in theme_families:
        stat = bundle.family_stats.get(theme_family)
        if stat:
            coverage_bits.append(
                f"{theme_family} (freq={stat.total_frequency}, streams={stat.max_stream_coverage})"
            )
    evidence_pattern = (
        f"Primary support comes from {', '.join(coverage_bits)}. "
        f"Quotes were drawn from {len({row.video_id for row in supporting_rows})} different streams."
    )
    if summary_codes:
        evidence_pattern += f" Summary classification also retains {', '.join(summary_codes[:5])}."
    return Dimension(
        dimension_name=spec["dimension_name"],
        uniqueness_type=spec["uniqueness_type"],
        shared_behavior_name=spec.get("shared_behavior_name", ""),
        why_unique=spec["why_unique"],
        shared_baseline_comparison=spec["shared_baseline_comparison"],
        contributing_open_codes=code_labels,
        theme_families=theme_families,
        evidence_pattern=evidence_pattern,
        supporting_rows=supporting_rows,
        limits=spec["limits"],
        source_kind=spec.get("source_kind", "shared_behavior"),
        weakness_bucket=spec.get("weakness_bucket", "weakly_shared_pattern"),
    )


def build_dimensions(
    bundle: TalentBundle,
    all_bundles: Dict[str, TalentBundle],
    baseline: dict,
) -> List[Dimension]:
    if bundle.source.talent in SPECIAL_DIMENSION_SPECS:
        return [build_spec_dimension(bundle, spec) for spec in SPECIAL_DIMENSION_SPECS[bundle.source.talent]]

    peer_order = select_peer_order(bundle, all_bundles)
    selected: List[Dimension] = []
    used_behavior_names = set()

    ordered_shared = shared_candidate_order(bundle, baseline)
    priority_behaviors = [
        behavior
        for behavior in [
            "performative teasing and welcome escalation",
            "audience-facing self-disclosure windows",
        ]
        if behavior in ordered_shared
    ]
    for behavior in ordered_shared:
        if behavior not in priority_behaviors:
            priority_behaviors.append(behavior)

    for behavior in priority_behaviors:
        if len(selected) >= 2:
            break
        matrix_row = baseline["matrix_by_talent"][bundle.source.talent].get(behavior)
        if not matrix_row:
            continue
        if matrix_row.get("support_level") not in {"strong", "moderate"}:
            continue
        selected.append(build_shared_dimension(bundle, behavior, baseline, peer_order))
        used_behavior_names.add(behavior)

    outside_candidates = best_outside_families(bundle, baseline)
    if outside_candidates:
        selected.append(build_outside_dimension(bundle, outside_candidates[0], baseline))

    fallback_shared = [
        behavior for behavior in ordered_shared if behavior not in used_behavior_names
    ]
    for behavior in fallback_shared:
        if len(selected) >= TARGET_DIMENSION_COUNT - 1:
            break
        selected.append(build_shared_dimension(bundle, behavior, baseline, peer_order))
        used_behavior_names.add(behavior)

    selected.append(build_combination_dimension(bundle, selected, baseline, peer_order))

    if len(selected) > TARGET_DIMENSION_COUNT:
        keep = selected[: TARGET_DIMENSION_COUNT - 1] + [selected[-1]]
        selected = keep
    return selected


def render_quote_lines(rows: Sequence[EvidenceRow]) -> str:
    lines = []
    for row in rows:
        lines.append(
            f"- [{row.video_id} {row.timecode}] {json.dumps(shorten_quote(row.quote), ensure_ascii=False)}"
        )
    return "\n".join(lines)


def build_baseline_comparison_notes(bundle: TalentBundle, dimensions: Sequence[Dimension], baseline: dict) -> str:
    shared_map = baseline["matrix_by_talent"][bundle.source.talent]
    shared_behaviors = [
        BASELINE_LABELS[name]
        for name, row in shared_map.items()
        if row.get("support_level") in {"strong", "moderate"} and row.get("classification") == "shared_baseline"
    ]
    distinctive_shared = [dim.dimension_name for dim in dimensions if dim.uniqueness_type == "distinctive_version_of_shared_behavior"]
    outside = [dim.dimension_name for dim in dimensions if dim.uniqueness_type == "outside_shared_baseline"]
    if not outside:
        outside_note = "No strong non-baseline family cleared the evidence threshold beyond smaller boundary or money traces."
    else:
        outside_note = ", ".join(outside)
    return (
        f"{short_talent_name(bundle.source.talent)} clearly shares the dataset baseline through {', '.join(shared_behaviors[:4])}. "
        f"The distinctive versions that matter most here are {', '.join(distinctive_shared[:3])}. "
        f"The strongest traits that sit outside or weaker than the baseline are {outside_note}."
    )


def build_signature_section(bundle: TalentBundle, dimensions: Sequence[Dimension]) -> str:
    strongest = ", ".join(dim.dimension_name for dim in dimensions[:3])
    weak_count = sum(1 for dim in dimensions if dim.weakness_bucket in {"weakly_shared_pattern", "outside_shared_baseline"})
    confidence = "High" if weak_count >= 2 and len(dimensions) >= 4 else "Medium"
    descriptor_text = ", ".join(bundle.summary_context.get("relationship_descriptors")[:2]) or "text-visible"
    paragraph_one = (
        f"{strip_variant_suffix(bundle.source.talent)} is most distinctive in this dataset through {strongest}. "
        f"Relative to the shared baseline, the channel keeps recurring common behaviors but bends them into a more {descriptor_text} relationship stance that peers do not perform in the same combination."
    )
    paragraph_two = (
        f"The strongest uniqueness signal is not a single adjective but the package formed by {dimensions[0].dimension_name.lower()}, "
        f"{dimensions[1].dimension_name.lower()}, and {dimensions[-1].dimension_name.lower()}. "
        f"Confidence: {confidence}. Text-only evidence cannot establish visual affect, full vocal prosody, or off-stream motives."
    )
    return paragraph_one + "\n\n" + paragraph_two


def build_distinctive_shared_section(bundle: TalentBundle, dimensions: Sequence[Dimension], baseline: dict, all_bundles: Dict[str, TalentBundle]) -> str:
    peer_order = select_peer_order(bundle, all_bundles)
    shared_dims = [dim for dim in dimensions if dim.shared_behavior_name]
    sentences = []
    for dimension in shared_dims[:3]:
        behavior_names = [
            name
            for name in split_behavior_names(dimension.shared_behavior_name)
            if name in baseline["matrix_by_talent"][bundle.source.talent]
        ]
        if not behavior_names:
            continue
        focus_behavior = behavior_names[0]
        sentence = (
            f"{dimension.dimension_name} is the talent-specific version of {focus_behavior}. "
            f"{build_peer_comparison_snippet(bundle.source.talent, focus_behavior, baseline, peer_order)}"
        )
        sentences.append(sentence)
    return "\n\n".join(sentences)


def build_non_baseline_section(dimensions: Sequence[Dimension]) -> str:
    outside_dims = [dim for dim in dimensions if dim.uniqueness_type == "outside_shared_baseline"]
    if not outside_dims:
        return "insufficient evidence for a strong non-baseline recurring marker beyond smaller boundary or money traces."
    lines = []
    for dim in outside_dims:
        lines.append(
            f"{dim.dimension_name}: {dim.why_unique} {dim.shared_baseline_comparison}"
        )
    return "\n\n".join(lines)


def build_relational_uniqueness(bundle: TalentBundle, dimensions: Sequence[Dimension]) -> str:
    reciprocity = dominant_label(bundle.summary_context.get("reciprocity", {}), "unclear")
    pacing = dominant_label(bundle.summary_context.get("pacing", {}), "unclear")
    top_names = [dim.dimension_name.lower() for dim in dimensions[:3]]
    return (
        f"With chat, {short_talent_name(bundle.source.talent)} reads as {', '.join(bundle.summary_context.get('relationship_descriptors')[:2]) or 'interactionally legible'} in text. "
        f"Closeness is built through {top_names[0]} and {top_names[1]}, while authority usually shows up through pacing and boundary moves rather than constant distance. "
        f"Summary-classification context suggests reciprocity is most often {reciprocity} and pacing is most often {pacing}; the retained evidence adds that teasing, reassurance, and audience management are shaped by {top_names[-1]} rather than by a generic streamer warmth."
    )


def build_cross_talent_contrast(
    bundle: TalentBundle,
    dimensions: Sequence[Dimension],
    baseline: dict,
    all_bundles: Dict[str, TalentBundle],
) -> str:
    peer_order = select_peer_order(bundle, all_bundles)
    lines = []
    shared_dims = [dim for dim in dimensions if dim.shared_behavior_name]
    for dimension in shared_dims[:2]:
        behavior_names = [
            name
            for name in split_behavior_names(dimension.shared_behavior_name)
            if name in baseline["matrix_by_talent"][bundle.source.talent]
        ]
        if not behavior_names:
            continue
        snippet = build_peer_comparison_snippet(
            bundle.source.talent,
            behavior_names[0],
            baseline,
            peer_order,
        )
        if snippet == "Peer comparison unavailable.":
            continue
        lines.append(f"- {snippet}")

    return "\n".join(lines)


def build_money_section(bundle: TalentBundle) -> str:
    money_family = next(
        (stat for family, stat in bundle.family_stats.items() if family_bucket(family) == "money"),
        None,
    )
    money_rows = [row for row in bundle.evidence_rows if family_bucket(row.theme_family) == "money" or row.monetary_context != "none"]
    if not money_family or money_family.max_stream_coverage < 10 or len(money_rows) < 3:
        return "insufficient evidence"
    rows = pick_rows(money_rows, keywords=["member", "gift", "super chat", "support", "donation"], desired=2)
    quotes = "; ".join(
        f"[{row.video_id} {row.timecode}] {json.dumps(shorten_quote(row.quote), ensure_ascii=False)}"
        for row in rows
    )
    return (
        f"Money-linked evidence is present but still secondary. It mostly reinforces the existing relationship style rather than creating a separate uniqueness claim "
        f"(money framing freq={money_family.total_frequency}, streams={money_family.max_stream_coverage}). Example support: {quotes}."
    )


def build_validity_section(dimensions: Sequence[Dimension]) -> str:
    strongest = [f"- {dim.dimension_name}: {dim.why_unique}" for dim in dimensions[:3]]
    uncertainty = [
        "- Some retained families mix interactional style with situational context, so a recurring tone should not be mistaken for a universal rule.",
        "- Text-only evidence cannot recover facial expression, timing nuance, or full vocal warmth/harshness.",
        "- Lower-frequency boundary or money families are analytically useful but still smaller than the channel's main interaction families.",
        "- Risk note: common streamer behaviors like thanking, pacing, or joking with chat can look unique unless their talent-specific form is specified.",
        "- Text-only modality note: these conclusions describe visible language patterns, not diagnoses, motives, or off-stream personality.",
    ]
    return "\n".join(
        ["3 strongest evidence-backed uniqueness conclusions:"] + strongest
        + ["", "3 uncertainty points:"]
        + uncertainty[:3]
        + ["", uncertainty[3], uncertainty[4]]
    )


def render_markdown(
    bundle: TalentBundle,
    dimensions: Sequence[Dimension],
    baseline: dict,
    all_bundles: Dict[str, TalentBundle],
) -> str:
    lines = [
        f"Analysis conducted: {ANALYSIS_STAMP}",
        f"Talent: {bundle.source.talent}",
        f"Primary talent source: {bundle.source.source_label}",
        "Shared baseline source used: yes",
        f"Summary-classification source used: {'yes' if bundle.summary_context.get('used') else 'no'}",
        "",
        "## 1) Unique Personality Signature",
        build_signature_section(bundle, dimensions),
        "",
        "## 2) Baseline Comparison Notes",
        build_baseline_comparison_notes(bundle, dimensions, baseline),
        "",
        "## 3) Major Uniqueness Dimensions",
    ]
    for dimension in dimensions:
        lines.extend(
            [
                "",
                f"### {dimension.dimension_name}",
                f"- dimension_name: {dimension.dimension_name}",
                f"- uniqueness_type: {dimension.uniqueness_type}",
                f"- why this is unique in the dataset: {dimension.why_unique}",
                f"- shared baseline comparison: {dimension.shared_baseline_comparison}",
                f"- contributing open codes: {', '.join(dimension.contributing_open_codes) or 'insufficient evidence'}",
                f"- evidence pattern: {dimension.evidence_pattern}",
                "- supporting quotes:",
                render_quote_lines(dimension.supporting_rows) if dimension.supporting_rows else "- insufficient evidence",
                f"- limits / counterevidence: {dimension.limits}",
            ]
        )

    lines.extend(
        [
            "",
            "## 4) Distinctive Versions of Shared Behaviors",
            build_distinctive_shared_section(bundle, dimensions, baseline, all_bundles),
            "",
            "## 5) Non-Baseline Markers and Rituals",
            build_non_baseline_section(dimensions),
            "",
            "## 6) Relational Uniqueness With Chat",
            build_relational_uniqueness(bundle, dimensions),
            "",
            "## 7) Cross-Talent Contrast",
            build_cross_talent_contrast(bundle, dimensions, baseline, all_bundles),
            "",
            "## 8) Optional Money-Linked Distinctiveness",
            build_money_section(bundle),
            "",
            "## 9) Validity, Limits, and Uncertainty",
            build_validity_section(dimensions),
            "",
        ]
    )
    return "\n".join(lines)


def build_evidence_log_rows(bundle: TalentBundle, dimensions: Sequence[Dimension]) -> List[dict]:
    rows = []
    for dimension in dimensions:
        for row in dimension.supporting_rows:
            rows.append(
                {
                    "talent": bundle.source.talent,
                    "dimension_name": dimension.dimension_name,
                    "uniqueness_type": dimension.uniqueness_type,
                    "shared_behavior_name": dimension.shared_behavior_name,
                    "contributing_open_code": row.code_label,
                    "theme_family": row.theme_family,
                    "video_id": row.video_id,
                    "time_in_seconds": f"{row.time_in_seconds:.3f}" if not math.isnan(row.time_in_seconds) else "",
                    "timecode": row.timecode,
                    "source": row.source,
                    "speaker": row.speaker,
                    "quote": row.quote,
                    "evidence_role": row.evidence_role,
                    "why_this_supports_uniqueness": shorten_quote(dimension.why_unique, limit=220),
                }
            )
    return rows


def build_state(bundle: TalentBundle, dimensions: Sequence[Dimension], snapshot_path: Path, evidence_rows_used: int) -> dict:
    return {
        "talent": bundle.source.talent,
        "analysis_conducted_at": ANALYSIS_STAMP,
        "primary_talent_source": bundle.source.source_label,
        "shared_baseline_source_used": True,
        "summary_classification_source_used": bool(bundle.summary_context.get("used")),
        "uniqueness_dimension_count": len(dimensions),
        "latest_snapshot_path": str(snapshot_path),
        "notes": [
            f"Evidence rows used: {evidence_rows_used}",
            "Money context was treated only as a secondary supplement.",
            "Legacy personality reports were not used as evidence.",
        ],
    }


def render_snapshot(markdown: str, prompt_text: str) -> str:
    return "\n".join(
        [
            f"Snapshot recorded: {ANALYSIS_STAMP}",
            "",
            "## Current Markdown Content",
            "",
            markdown.rstrip(),
            "",
            "## Prompt Text Used For Run",
            "",
            prompt_text.rstrip(),
            "",
        ]
    )


def run() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--talent", dest="talent_scope", help="Process only the exact talent folder name.")
    args = parser.parse_args()

    prompt_text = PROMPT_PATH.read_text(encoding="utf-8")
    baseline = load_baseline()
    target_sources = discover_talents(talent_scope=args.talent_scope)
    if not target_sources:
        raise SystemExit("No eligible talents found.")

    comparison_sources = discover_talents()
    source_map = {source.talent: source for source in comparison_sources}
    for source in target_sources:
        source_map[source.talent] = source

    bundles = {source.talent: load_talent_bundle(source) for source in source_map.values()}
    summaries = []

    for talent in sorted(source.talent for source in target_sources):
        bundle = bundles[talent]
        dimensions = build_dimensions(bundle, bundles, baseline)
        markdown = render_markdown(bundle, dimensions, baseline, bundles)
        evidence_rows = build_evidence_log_rows(bundle, dimensions)

        output_root = (
            bundle.source.root
            / "stream_summaries"
            / "overall_themes"
            / "personality_unique_features"
        )
        current_dir = output_root / "current"
        snapshot_dir = output_root / "snapshots"
        current_md = current_dir / "unique_personality_profile_codex.md"
        current_csv = current_dir / "unique_personality_evidence_log_codex.csv"
        current_state = current_dir / "unique_personality_state.json"
        snapshot_md = snapshot_dir / f"unique_personality_profile_{SNAPSHOT_STAMP}.md"

        write_text(current_md, markdown)
        write_csv(
            current_csv,
            [
                "talent",
                "dimension_name",
                "uniqueness_type",
                "shared_behavior_name",
                "contributing_open_code",
                "theme_family",
                "video_id",
                "time_in_seconds",
                "timecode",
                "source",
                "speaker",
                "quote",
                "evidence_role",
                "why_this_supports_uniqueness",
            ],
            evidence_rows,
        )
        write_text(snapshot_md, render_snapshot(markdown, prompt_text))
        state = build_state(bundle, dimensions, snapshot_md, len(evidence_rows))
        write_text(current_state, json.dumps(state, indent=2, ensure_ascii=False) + "\n")

        summaries.append(
            {
                "talent": talent,
                "source": bundle.source.source_label,
                "dimensions": [dimension.dimension_name for dimension in dimensions],
                "evidence_rows": len(evidence_rows),
                "current_md": str(current_md),
                "current_csv": str(current_csv),
                "current_state": str(current_state),
                "snapshot_md": str(snapshot_md),
            }
        )

    for summary in summaries:
        print(f"Talent: {summary['talent']}")
        print(f"  primary talent source: {summary['source']}")
        print("  shared baseline used: yes")
        print(f"  uniqueness dimensions retained: {', '.join(summary['dimensions'])}")
        print(f"  evidence rows used: {summary['evidence_rows']}")
        print(f"  outputs:")
        print(f"    - {summary['current_md']}")
        print(f"    - {summary['current_csv']}")
        print(f"    - {summary['current_state']}")
        print(f"    - {summary['snapshot_md']}")


if __name__ == "__main__":
    run()
