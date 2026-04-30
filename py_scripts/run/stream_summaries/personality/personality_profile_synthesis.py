#!/usr/bin/env python3
"""Build monthly and overall personality synthesis reports from open-coding outputs."""

from __future__ import annotations

import argparse
import csv
import json
import math
import os
import re
from collections import Counter, defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Tuple


DATA_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
REPO_ROOT = Path(__file__).resolve().parents[4]
NOW = datetime.now().astimezone()
ANALYSIS_STAMP = NOW.strftime("%Y-%m-%d %H:%M %Z")

MONTH_DIR = Path("stream_summaries/overall_themes/personality_profile/monthly")
OVERALL_DIR = Path("stream_summaries/overall_themes/personality_profile/overall")

APPRECIATION_RE = re.compile(r"\b(thank(?:s| you)?|appreciate|grateful|bless you)\b", re.I)
MONEY_RE = re.compile(r"\b(member(?:ship)?s?|gift(?:ed)?|super ?chat|donat(?:e|ion)|support|subs?)\b", re.I)
GUIDANCE_RE = re.compile(
    r"\b(hold on|wait|listen|mods\b|focus|stay on topic|moving on|move on|next up|next|we'?re going|we are going|let'?s|keep it|no spoilers|don't ask|do not ask|please stop|lock in)\b",
    re.I,
)
BOUNDARY_RE = re.compile(
    r"\b(no spoilers|don't ask|do not ask|don't spam|do not spam|please stop|keep it respectful|stop asking|stop spamming|not okay)\b",
    re.I,
)
REASSURANCE_RE = re.compile(
    r"\b(it'?s okay|its okay|don't worry|do not worry|no worries|take care|be safe|get some rest|rest well|drink water|you got this|i believe in you|proud of you|deep breath|you're okay|you are okay)\b",
    re.I,
)
THEATRICAL_RE = re.compile(
    r"\b(welcome to|right side of history|baby|darling|queen|blood|mortals?|ceremony|bozo|clown|bite you|eat you|kill you|my dear|yandere|goblin|gremlin|menace)\b",
    re.I,
)
VULNERABILITY_RE = re.compile(
    r"\b(i'?m|i am|i feel|i felt|i'?ve been|i have been|mental health|burnout|anxious|anxiety|overwhelmed|stressed|scared|worried|broke|poor|crying|rough day|exhausted|tired|not okay|struggling|panic|ashamed)\b",
    re.I,
)
STRONG_VULNERABILITY_RE = re.compile(
    r"\b(i'?m (?:so )?(?:anxious|overwhelmed|stressed|scared|worried|tired|exhausted|broke|poor)|i feel (?:anxious|overwhelmed|stressed|scared|worried|awful)|mental health|burnout|rough day|i'?ve been struggling|i have been struggling|i cried|i was crying|panic attack)\b",
    re.I,
)
EMOTION_WORD_RE = re.compile(
    r"\b(anxious|overwhelmed|stressed|scared|worried|tired|exhausted|sad|upset|rough|struggling|broke|poor|crying)\b",
    re.I,
)
AUDIENCE_RE = re.compile(r"\b(chat|everyone|everybody|you guys|you all|y'?all|mods|folks)\b", re.I)
HANDLE_RE = re.compile(r"[^a-z0-9]+")
VIDEO_ID_RE = re.compile(r"([A-Za-z0-9_-]{11})")
YMD_RE = re.compile(r"(20\d{2})[-_ ](0[1-9]|1[0-2])[-_ ]([0-3]\d)")
MDY_RE = re.compile(r"\b(0?[1-9]|1[0-2])[-_ /](0?[1-9]|[12]\d|3[01])[-_ /](20\d{2})\b")
YM_RE = re.compile(r"(20\d{2})[-_ ](0[1-9]|1[0-2])")
MONTH_NAME_RE = re.compile(
    r"\b(January|February|March|April|May|June|July|August|September|October|November|December)\s+([0-3]?\d),\s*(20\d{2})\b",
    re.I,
)

SIGNAL_ORDER = ["appreciation", "guidance", "theatrical", "reassurance", "vulnerability"]
SIGNAL_LABELS = {
    "appreciation": "ritualized appreciation",
    "guidance": "pace steering",
    "theatrical": "theatrical play",
    "reassurance": "reassurance",
    "vulnerability": "strain disclosure",
}

SUMMARY_THEME_MAP = {
    "appreciation": {"support-driven momentum", "active chat acknowledgment"},
    "guidance": {"rapid flow steering", "gentle pace steering", "active chat acknowledgment"},
    "theatrical": {"performative hype and bits"},
    "reassurance": {"warm reassurance"},
    "vulnerability": set(),
}


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
    year_month: str = ""
    signals: Tuple[str, ...] = field(default_factory=tuple)


@dataclass
class Dimension:
    key: str
    name: str
    rows: List[EvidenceRow]
    quotes: List[EvidenceRow]
    contributing_codes: List[str]
    theme_families: List[str]
    why: str
    distinct_note: str
    evidence_pattern: str
    limit_note: str


@dataclass
class MarkerEntry:
    name: str
    note: str
    quotes: List[EvidenceRow]


def csv_rows(path: Path) -> List[dict]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def normalize_token(text: str) -> str:
    return HANDLE_RE.sub("", (text or "").lower())


def talent_tokens(talent_name: str) -> List[str]:
    base = re.sub(r"[【\[].*?[】\]]", " ", talent_name)
    base = re.sub(r"[^A-Za-z0-9 ]+", " ", base)
    tokens = [normalize_token(part) for part in base.split() if part.strip()]
    extras = []
    if tokens:
        extras.append("".join(tokens[:2]))
    return [token for token in tokens + extras if token]


def is_streamer_speaker(talent_name: str, speaker: str) -> bool:
    norm = normalize_token(speaker.lstrip("@"))
    if norm in {"stream", "streamer", "host"}:
        return True
    if not norm:
        return False
    return any(token and token in norm for token in talent_tokens(talent_name))


def parse_float(value: str) -> float:
    try:
        return float(str(value).strip())
    except Exception:
        return math.nan


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def parse_summary_context(md_path: Optional[Path], state_path: Optional[Path]) -> dict:
    info = {
        "used": False,
        "retained_codes": [],
        "relationship_descriptors": [],
        "reciprocity": {},
        "pacing": {},
        "summary_note": "Summary classification not available for this talent, so open-coding evidence carried the synthesis.",
    }
    if not md_path or not md_path.exists():
        return info
    text = read_text(md_path)
    info["used"] = True
    info["summary_note"] = "Summary classification available but not parsed."
    if state_path and state_path.exists():
        try:
            state = json.loads(read_text(state_path))
            info["retained_codes"] = list(state.get("retained_code_names") or [])
        except Exception:
            pass

    match = re.search(
        r"text-visible relationship with chat is most often described as ([^.]+)\.",
        text,
        re.I,
    )
    if match:
        info["relationship_descriptors"] = [part.strip() for part in match.group(1).split(",") if part.strip()]

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

    if info["retained_codes"]:
        info["summary_note"] = "Summary classification mostly reinforced the same channel-level interaction tendencies."
    return info


def dominant_label(counts: Dict[str, int], default: str) -> str:
    if not counts:
        return default
    return max(counts.items(), key=lambda item: item[1])[0]


def choose_open_coding_source(talent_dir: Path) -> Optional[dict]:
    v3 = talent_dir / "stream_summaries/overall_themes/personality_open_coding/v3/current"
    v2 = talent_dir / "stream_summaries/overall_themes/personality_open_coding/v2"
    candidates = [
        (
            "v3 current",
            v3 / "open_codebook_v3.csv",
            v3 / "open_coding_evidence_v3.csv",
            v3 / "personality_profile_v3_open_coding.md",
        ),
        (
            "v2 fallback",
            v2 / "open_codebook_v2.csv",
            v2 / "open_coding_evidence_v2.csv",
            v2 / "personality_profile_v2_open_coding.md",
        ),
    ]
    for label, codebook_path, evidence_path, profile_path in candidates:
        if codebook_path.exists() and evidence_path.exists() and profile_path.exists():
            return {
                "label": label,
                "codebook_path": codebook_path,
                "evidence_path": evidence_path,
                "profile_path": profile_path,
            }
    return None


def overall_channel_summary_paths(talent_dir: Path) -> Tuple[Optional[Path], Optional[Path]]:
    current = talent_dir / "stream_summaries/overall_channel_summary/current"
    markdown = current / "overall_channel_summary.md"
    state = current / "overall_channel_summary_state.json"
    if markdown.exists():
        return markdown, state

    legacy_current = talent_dir / "stream_summaries/overall_themes/summary_classification/current"
    legacy_md = legacy_current / "overall_themes_codex.md"
    legacy_state = legacy_current / "summary_classification_state.json"
    if legacy_md.exists():
        return legacy_md, legacy_state
    return None, None


def discover_talents(root: Path, talent_slug: Optional[str] = None) -> List[Path]:
    talents = []
    for child in sorted(root.iterdir(), key=lambda path: path.name):
        if not child.is_dir():
            continue
        if child.name == "VarianceProject":
            continue
        if talent_slug and child.name != talent_slug:
            continue
        summary_md, _ = overall_channel_summary_paths(child)
        personality_dir = child / "stream_summaries/overall_themes/personality_open_coding"
        if personality_dir.exists() and (summary_md is None or summary_md.exists() or talent_slug):
            talents.append(child)
    return talents


def extract_video_id_from_name(path: Path) -> Optional[str]:
    match = VIDEO_ID_RE.search(path.name)
    return match.group(1) if match else None


def parse_date_to_month(text: str) -> Optional[str]:
    if not text:
        return None
    match = YMD_RE.search(text)
    if match:
        year, month, _ = match.groups()
        return f"{year}-{month}"
    match = MDY_RE.search(text)
    if match:
        month, _, year = match.groups()
        return f"{year}-{int(month):02d}"
    match = MONTH_NAME_RE.search(text)
    if match:
        month_name, _, year = match.groups()
        month_num = datetime.strptime(month_name[:3], "%b").month
        return f"{year}-{month_num:02d}"
    match = YM_RE.search(text)
    if match:
        year, month = match.groups()
        return f"{year}-{month}"
    return None


def build_video_month_map(talent_dir: Path, video_ids: Iterable[str]) -> Tuple[Dict[str, str], List[str]]:
    pending = set(video_ids)
    mapping: Dict[str, str] = {}
    analytics_dir = talent_dir / "raw_data/video_analytics"
    if analytics_dir.exists():
        for csv_path in sorted(analytics_dir.glob("*.csv")):
            with csv_path.open("r", encoding="utf-8-sig", newline="") as handle:
                reader = csv.DictReader(handle)
                for row in reader:
                    video_id = (row.get("Video ID") or row.get("video") or row.get("VideoId") or "").strip()
                    if video_id not in pending:
                        continue
                    published = (row.get("Published At") or "").strip()
                    month = parse_date_to_month(published)
                    if month and video_id not in mapping:
                        mapping[video_id] = month
    unresolved = [video_id for video_id in pending if video_id not in mapping]
    for video_id in list(unresolved):
        file_hits = sorted(talent_dir.rglob(f"*{video_id}*"))
        for file_path in file_hits:
            month = parse_date_to_month(file_path.name)
            if month:
                mapping[video_id] = month
                break
    unresolved = [video_id for video_id in pending if video_id not in mapping]
    for video_id in list(unresolved):
        summary_hits = sorted((talent_dir / "stream_summaries/stream_summary_codex").glob(f"*{video_id}*.md"))
        for summary_path in summary_hits:
            month = parse_date_to_month(summary_path.name)
            if month:
                mapping[video_id] = month
                break
            text = summary_path.read_text(encoding="utf-8")
            month = parse_date_to_month(text[:2500])
            if month:
                mapping[video_id] = month
                break
    unresolved = sorted(video_id for video_id in pending if video_id not in mapping)
    return mapping, unresolved


def make_evidence_rows(talent_name: str, evidence_path: Path, month_map: Dict[str, str]) -> List[EvidenceRow]:
    rows = []
    for raw in csv_rows(evidence_path):
        video_id = (raw.get("video_id") or "").strip()
        row = EvidenceRow(
            talent=talent_name,
            code_label=(raw.get("code_label") or "").strip(),
            theme_family=(raw.get("theme_family") or "").strip(),
            video_id=video_id,
            time_in_seconds=parse_float(raw.get("time_in_seconds", "")),
            timecode=(raw.get("timecode") or "").strip(),
            source=(raw.get("source") or "").strip(),
            speaker=(raw.get("speaker") or "").strip(),
            quote=(raw.get("quote") or "").strip(),
            evidence_role=(raw.get("evidence_role") or "").strip(),
            monetary_context=(raw.get("monetary_context") or "").strip() or "none",
            incorporated_in_run=(raw.get("incorporated_in_run") or "").strip(),
            year_month=month_map.get(video_id, ""),
        )
        row.signals = classify_signals(row)
        rows.append(row)
    return rows


def classify_signals(row: EvidenceRow) -> Tuple[str, ...]:
    quote = row.quote.lower()
    theme_family = row.theme_family.lower()
    code_label = row.code_label.lower()
    signals: List[str] = []
    if APPRECIATION_RE.search(quote) or ("thank recognition loops" in theme_family and MONEY_RE.search(quote)):
        signals.append("appreciation")
    guidance_hit = GUIDANCE_RE.search(quote) or BOUNDARY_RE.search(quote)
    if not guidance_hit and ("pacing control" in theme_family or "guardrails" in theme_family or code_label.endswith(" pace")):
        guidance_hit = bool(AUDIENCE_RE.search(quote) and re.search(r"\b(hold on|wait|let's|next|focus|mods|please|don't|chat)\b", quote, re.I))
    if guidance_hit:
        signals.append("guidance")
    theatrical_hit = bool(THEATRICAL_RE.search(quote))
    if theatrical_hit and re.search(r"\byou'?re welcome to\b", quote, re.I):
        theatrical_hit = False
    if not theatrical_hit and "bit escalation" in theme_family:
        roots = [part for part in code_label.replace(" bit", "").split() if part not in {"welcome"}]
        theatrical_hit = any(root and re.search(rf"\b{re.escape(root)}\b", quote, re.I) for root in roots)
    if theatrical_hit:
        signals.append("theatrical")
    reassurance_hit = bool(REASSURANCE_RE.search(quote))
    if not reassurance_hit and ("reassurance stance" in theme_family or "co-regulation loops" in theme_family):
        roots = [part for part in code_label.replace(" care", "").replace(" loop", "").split()]
        reassurance_hit = any(root and re.search(rf"\b{re.escape(root)}\b", quote, re.I) for root in roots)
    if reassurance_hit:
        signals.append("reassurance")
    if ("self-disclosure" in theme_family and STRONG_VULNERABILITY_RE.search(quote)) or (
        STRONG_VULNERABILITY_RE.search(quote) and EMOTION_WORD_RE.search(quote)
    ):
        signals.append("vulnerability")
    return tuple(sorted(set(signals), key=SIGNAL_ORDER.index))


def quote_noise_penalty(quote: str) -> float:
    penalty = 0.0
    lower = quote.lower()
    if "[ __ ]" in quote:
        penalty += 2.0
    if sum(1 for token in quote.split() if len(token) == 1 and token.isalpha()) >= 4:
        penalty += 1.0
    if quote.count("...") >= 2:
        penalty += 0.5
    if lower in {"queen", "lmao", "skill issue"}:
        penalty += 3.0
    return penalty


def quote_cleanliness(row: EvidenceRow, signal: str) -> float:
    quote = row.quote.strip()
    score = 0.0
    if 24 <= len(quote) <= 180:
        score += 2.0
    elif 12 <= len(quote) <= 220:
        score += 1.0
    if signal == "appreciation" and APPRECIATION_RE.search(quote):
        score += 2.5
    if signal == "guidance" and GUIDANCE_RE.search(quote):
        score += 2.5
    if signal == "theatrical" and THEATRICAL_RE.search(quote):
        score += 2.5
    if signal == "reassurance" and REASSURANCE_RE.search(quote):
        score += 2.5
    if signal == "vulnerability" and STRONG_VULNERABILITY_RE.search(quote):
        score += 3.0
    if signal == "guidance" and BOUNDARY_RE.search(quote):
        score += 1.0
    if signal == "appreciation" and (row.monetary_context != "none" or MONEY_RE.search(quote)):
        score += 0.75
    if signal == "reassurance" and AUDIENCE_RE.search(quote):
        score += 1.0
    if signal == "theatrical" and "welcome to" in quote:
        score += 1.0
    if signal == "vulnerability" and "i need" in quote and not EMOTION_WORD_RE.search(quote):
        score -= 2.0
    if signal == "reassurance" and quote.lower() == "it's okay.":
        score -= 1.0
    score -= quote_noise_penalty(quote)
    return score


def select_quotes(rows: Sequence[EvidenceRow], signal: str, desired: int = 4) -> List[EvidenceRow]:
    ranked = sorted(
        rows,
        key=lambda row: (
            quote_cleanliness(row, signal),
            row.video_id,
            -(row.time_in_seconds if not math.isnan(row.time_in_seconds) else 0.0),
        ),
        reverse=True,
    )
    selected: List[EvidenceRow] = []
    used_videos = set()
    for distinct_only in (True, False):
        for row in ranked:
            if row in selected:
                continue
            if distinct_only and row.video_id in used_videos:
                continue
            if quote_cleanliness(row, signal) < 0.5:
                continue
            selected.append(row)
            used_videos.add(row.video_id)
            if len(selected) >= desired:
                return selected
    return selected


def contributing_codes(rows: Sequence[EvidenceRow], max_codes: int = 5) -> List[str]:
    counts = Counter(row.code_label for row in rows)
    return [code for code, _ in counts.most_common(max_codes)]


def dimension_specific_rows(rows: Sequence[EvidenceRow], signal: str) -> List[EvidenceRow]:
    out = []
    for row in rows:
        code = row.code_label.lower()
        family = row.theme_family.lower()
        if signal == "appreciation" and ("thank" in code or "appreciate" in code or "grateful" in code or "thank recognition" in family):
            out.append(row)
        elif signal == "guidance" and ("pace" in code or "guardrail" in code or "spoiler" in code or "mods" in code or "pacing" in family):
            out.append(row)
        elif signal == "theatrical" and (" bit" in code or family.endswith("bit escalation") or "bit escalation" in family):
            out.append(row)
        elif signal == "reassurance" and ("care" in code or "worry" in code or "reassurance" in family or "co-regulation" in family):
            out.append(row)
        elif signal == "vulnerability" and ("disclosure" in code or "self-disclosure" in family):
            out.append(row)
    return out


def theme_families(rows: Sequence[EvidenceRow], max_families: int = 4) -> List[str]:
    counts = Counter(row.theme_family for row in rows)
    return [family for family, _ in counts.most_common(max_families)]


def code_rate_map(codebook_rows: Sequence[dict]) -> Dict[str, float]:
    total = sum(int((row.get("frequency_count") or "0").strip() or "0") for row in codebook_rows) or 1
    return {
        (row.get("code_label") or "").strip(): int((row.get("frequency_count") or "0").strip() or "0") / total
        for row in codebook_rows
        if (row.get("code_label") or "").strip()
    }


def code_distinctiveness_note(
    codes: Sequence[str],
    target_rates: Dict[str, float],
    peer_rate_maps: Sequence[Dict[str, float]],
) -> str:
    distinctive = []
    for code in codes:
        target = target_rates.get(code, 0.0)
        peer_avg = sum(peer.get(code, 0.0) for peer in peer_rate_maps) / max(1, len(peer_rate_maps))
        if target >= 0.005 and (peer_avg == 0.0 or target >= peer_avg * 1.75):
            distinctive.append(code)
    if distinctive:
        return "Compared with peers in this dataset, the pattern is unusually visible through " + ", ".join(distinctive[:3]) + "."
    return "This pattern also exists elsewhere in the dataset, so the distinctiveness claim rests on combination and emphasis more than on a single exclusive code."


def signal_rows(rows: Sequence[EvidenceRow], signal: str) -> List[EvidenceRow]:
    return [row for row in rows if signal in row.signals]


def summary_support_note(summary_context: dict, signal: str) -> str:
    retained = {value.lower() for value in summary_context.get("retained_codes", [])}
    overlap = SUMMARY_THEME_MAP.get(signal, set()) & retained
    if overlap:
        return " Summary classification also retained " + ", ".join(sorted(overlap)) + "."
    if signal == "vulnerability":
        return " Summary classification compressed this area, so disclosure nuance rests mostly on the open-coding evidence."
    return " Summary classification did not add much signal beyond the open-coding layer here."


def dimension_template(
    signal: str,
    rows: Sequence[EvidenceRow],
    summary_context: dict,
    month_scope: bool,
) -> Tuple[str, str, str]:
    pace_mode = dominant_label(summary_context.get("pacing", {}), "rapid")
    has_money = any(row.monetary_context != "none" or MONEY_RE.search(row.quote) for row in rows)
    has_boundary = any(BOUNDARY_RE.search(row.quote) for row in rows)
    if signal == "appreciation":
        name = "ritualized appreciation as community glue" if has_money else "ritualized appreciation as audience upkeep"
        why = (
            "Recognition turns keep viewers socially visible and often convert support moments into an ongoing relationship ritual."
        )
        pattern = "Thank-you or appreciation lines recur across multiple streams"
        if has_money:
            pattern += ", with especially visible clustering around member, gifted, or donation moments"
        pattern += "." + summary_support_note(summary_context, signal)
        return name, why, pattern
    if signal == "guidance":
        if pace_mode == "slow":
            name = "gentle pace steering with soft redirects"
        elif has_boundary:
            name = "host-led pacing with explicit redirect"
        else:
            name = "continuous flow steering through chat management"
        why = "The streamer repeatedly decides when chat can redirect the flow and when the segment keeps moving on the host's terms."
        pattern = "Pacing language appears as turn-taking control, topic routing, and occasional rule-setting rather than passive commentary."
        pattern += summary_support_note(summary_context, signal)
        return name, why, pattern
    if signal == "theatrical":
        name = "mock-ceremonial escalation as social play" if any("welcome to" in row.quote.lower() for row in rows) else "theatrical escalation as social play"
        why = "Bits, exaggerated address, and heightened phrases make audience contact feel performed rather than merely conversational."
        pattern = "The effect comes less from one catchphrase than from repeated dramatic framing across different streams."
        pattern += summary_support_note(summary_context, signal)
        return name, why, pattern
    if signal == "reassurance":
        if pace_mode == "rapid":
            name = "protective reassurance under fast pacing"
        else:
            name = "quick reassurance as a reset habit"
        why = "Short calming phrases function as small de-escalation moves, either toward chat or toward the moment's tension."
        pattern = "These reassurance lines tend to appear when the stream risks wobbling, frustration rises, or someone in chat needs a softer landing."
        pattern += summary_support_note(summary_context, signal)
        return name, why, pattern
    name = "selective strain disclosure with self-reset"
    why = "The streamer occasionally lets pressure, fatigue, or emotional strain become text-visible, but usually in brief windows instead of full confessional stretches."
    pattern = "Only emotionally legible disclosure lines were retained for this dimension because the broader disclosure bucket also contains routine logistics."
    pattern += summary_support_note(summary_context, signal)
    return name, why, pattern


def limit_note(signal: str, rows: Sequence[EvidenceRow]) -> str:
    counts_by_video = Counter(row.video_id for row in rows)
    dominant_video_share = counts_by_video.most_common(1)[0][1] / max(1, len(rows))
    if signal == "appreciation":
        if dominant_video_share > 0.5:
            return "Much of this evidence clusters in a small number of streams, so the ritual is visible but unevenly distributed."
        return "Many of these lines happen around support acknowledgments, so part of the pattern may reflect platform affordances rather than a fully generalized trait."
    if signal == "guidance":
        return "A portion of the pacing evidence is logistical or gameplay-management language, so this speaks to visible stream control more than to off-stream assertiveness."
    if signal == "theatrical":
        return "Some of the strongest lexical markers are recurring bits, so the conclusion rests on their interactional function, not on the repeated words alone."
    if signal == "reassurance":
        return "Short phrases like \"it's okay\" can be self-directed repair as much as viewer comfort, so the caring stance should not be overstated."
    return "Disclosure evidence is thinner than the raw code counts suggest because many \"I need\" lines are practical rather than emotionally revealing."


def build_dimensions(
    rows: Sequence[EvidenceRow],
    summary_context: dict,
    min_rows: int,
    month_scope: bool,
    target_code_rates: Dict[str, float],
    peer_code_rates: Sequence[Dict[str, float]],
) -> List[Dimension]:
    dimensions: List[Dimension] = []
    for signal in SIGNAL_ORDER:
        candidates = signal_rows(rows, signal)
        distinct_videos = len({row.video_id for row in candidates})
        if len(candidates) < min_rows and distinct_videos < 2:
            continue
        code_rows = dimension_specific_rows(candidates, signal) or list(candidates)
        selected = select_quotes(code_rows, signal)
        if not selected:
            continue
        name, why, pattern = dimension_template(signal, candidates, summary_context, month_scope)
        codes = contributing_codes(code_rows)
        dimensions.append(
            Dimension(
                key=signal,
                name=name,
                rows=list(candidates),
                quotes=selected,
                contributing_codes=codes,
                theme_families=theme_families(candidates),
                why=why,
                distinct_note=code_distinctiveness_note(codes, target_code_rates, peer_code_rates),
                evidence_pattern=f"Visible in {len(candidates)} selected evidence rows across {distinct_videos} videos. {pattern}",
                limit_note=limit_note(signal, candidates),
            )
        )
    dimensions.sort(
        key=lambda dimension: (
            len(dimension.rows),
            len({row.video_id for row in dimension.rows}),
            SIGNAL_ORDER.index(dimension.key) * -1,
        ),
        reverse=True,
    )
    return dimensions[:5 if month_scope else 6]


def render_quote(row: EvidenceRow) -> str:
    return f'[{row.timecode}] {row.speaker} ({row.video_id}): "{row.quote}"'


def confidence_label(video_count: int, dimensions: Sequence[Dimension], row_count: int) -> str:
    if video_count >= 6 and len(dimensions) >= 4 and row_count >= 24:
        return "High"
    if video_count >= 2 and len(dimensions) >= 2 and row_count >= 10:
        return "Medium"
    return "Low"


def top_dimension_phrase(dimensions: Sequence[Dimension], max_items: int = 4) -> str:
    names = [dimension.name for dimension in dimensions[:max_items]]
    if not names:
        return "insufficient evidence"
    if len(names) == 1:
        return names[0]
    return ", ".join(names[:-1]) + f", and {names[-1]}"


def markers_from_dimensions(dimensions: Sequence[Dimension]) -> List[Tuple[str, str]]:
    markers: List[Tuple[str, str]] = []
    seen = set()
    for dimension in dimensions:
        for code in dimension.contributing_codes:
            code_norm = code.lower()
            if code_norm in seen:
                continue
            if "thank" in code_norm or "appreciate" in code_norm or "grateful" in code_norm:
                markers.append((code, "Used to keep viewers individually acknowledged and to turn support into ongoing relationship work."))
            elif "welcome" in code_norm:
                markers.append((code, "Turns arrivals or support events into staged entry moments rather than neutral acknowledgments."))
            elif "care" in code_norm or "worry" in code_norm:
                markers.append((code, "Functions as quick emotional down-regulation when chat or the stream moment starts to wobble."))
            elif "pace" in code_norm or "guardrail" in code_norm or "mods" in code_norm:
                markers.append((code, "Signals who is steering the current segment and how much redirect power chat has."))
            elif "bit" in code_norm:
                markers.append((code, "Makes recurring joke language socially legible as a persona performance instead of isolated phrasing."))
            if len(markers) >= 4:
                return markers
            seen.add(code_norm)
    return markers


def marker_note_for_code(code: str) -> Optional[str]:
    code_norm = code.lower()
    if "thank" in code_norm or "appreciate" in code_norm or "grateful" in code_norm:
        return "Used to keep viewers individually acknowledged and to turn support into ongoing relationship work."
    if "welcome" in code_norm:
        return "Turns arrivals or support events into staged entry moments rather than neutral acknowledgments."
    if "care" in code_norm or "worry" in code_norm:
        return "Functions as quick emotional down-regulation when chat or the stream moment starts to wobble."
    if "pace" in code_norm or "guardrail" in code_norm or "mods" in code_norm:
        return "Signals who is steering the current segment and how much redirect power chat has."
    if "bit" in code_norm:
        return "Makes recurring joke language socially legible as a persona performance instead of isolated phrasing."
    return None


def marker_entries_from_dimensions(dimensions: Sequence[Dimension], desired_quotes: int = 2) -> List[MarkerEntry]:
    entries: List[MarkerEntry] = []
    seen = set()
    for dimension in dimensions:
        for code in dimension.contributing_codes:
            code_norm = code.lower()
            if code_norm in seen:
                continue
            note = marker_note_for_code(code)
            if not note:
                continue
            matching_rows = [row for row in dimension.rows if row.code_label == code]
            quotes = select_quotes(matching_rows, dimension.key, desired=desired_quotes) if matching_rows else []
            if not quotes:
                quotes = dimension.quotes[:desired_quotes]
            entries.append(MarkerEntry(name=code, note=note, quotes=quotes))
            seen.add(code_norm)
            if len(entries) >= 4:
                return entries
    return entries


def dimension_lookup(dimensions: Sequence[Dimension]) -> Dict[str, Dimension]:
    return {dimension.key: dimension for dimension in dimensions}


def signal_scores(rows: Sequence[EvidenceRow]) -> Dict[str, int]:
    scores = {}
    for signal in SIGNAL_ORDER:
        scores[signal] = len(signal_rows(rows, signal))
    return scores


def signal_rates(rows: Sequence[EvidenceRow]) -> Dict[str, float]:
    base = max(1, len(rows))
    return {signal: len(signal_rows(rows, signal)) / base for signal in SIGNAL_ORDER}


def classify_month_shift(month_rates: Dict[str, float], overall_rates: Dict[str, float], month_count: int) -> str:
    if month_count < 8:
        return "showed insufficient evidence for meaningful change"
    stronger = []
    for signal, rate in month_rates.items():
        baseline = overall_rates.get(signal, 0.0)
        if baseline > 0 and rate >= baseline * 1.3 and rate - baseline >= 0.015:
            stronger.append(signal)
    if stronger:
        return "intensified a known trait"
    active = [signal for signal, rate in month_rates.items() if rate >= 0.03 and overall_rates.get(signal, 0.0) >= 0.03]
    if len(active) >= 2:
        return "reinforced the baseline"
    episodic = [signal for signal, rate in month_rates.items() if rate >= 0.03 and overall_rates.get(signal, 0.0) < 0.02]
    if episodic:
        return "revealed a temporary emphasis"
    return "showed insufficient evidence for meaningful change"


def overall_shift_summary(month_scores_by_month: Dict[str, Dict[str, int]], month_rates_by_month: Dict[str, Dict[str, float]]) -> Dict[str, List[str]]:
    months = sorted(month_rates_by_month)
    if not months:
        return {
            "stable": ["insufficient evidence"],
            "strengthened": ["insufficient evidence"],
            "weakened": ["insufficient evidence"],
            "episodic": ["insufficient evidence"],
        }
    stable = []
    strengthened = []
    weakened = []
    episodic = []
    for signal in SIGNAL_ORDER:
        series = [month_rates_by_month[month].get(signal, 0.0) for month in months]
        active_months = sum(1 for value in series if value >= 0.03)
        if active_months >= max(2, math.ceil(len(months) * 0.6)):
            stable.append(signal)
        prior_mean = sum(series[:-1]) / max(1, len(series) - 1) if len(series) > 1 else series[-1]
        if len(series) >= 2 and series[-1] >= 0.04 and prior_mean > 0 and series[-1] >= prior_mean * 1.35 and series[-1] - prior_mean >= 0.015:
            strengthened.append(signal)
        if len(series) >= 3 and max(series[:-2] or series[:-1]) >= 0.04 and prior_mean > 0 and series[-1] <= prior_mean * 0.65 and series[-1] <= 0.025:
            weakened.append(signal)
        if 1 <= active_months <= 2:
            episodic.append(signal)
    return {
        "stable": stable or ["profile appears mostly stable over time"],
        "strengthened": strengthened or ["no robust recently strengthened trait"],
        "weakened": weakened or ["no robust weakened trait"],
        "episodic": episodic or ["no clear episodic dimension beyond normal month-to-month variation"],
    }


def markdown_list(lines: Sequence[str]) -> str:
    return "\n".join(f"- {line}" for line in lines)


def relationship_style_text(summary_context: dict, dimensions: Sequence[Dimension], month_scope: bool) -> str:
    pace = dominant_label(summary_context.get("pacing", {}), "rapid")
    reciprocity = dominant_label(summary_context.get("reciprocity", {}), "high")
    dimension_names = {dimension.key for dimension in dimensions}
    closeness = "high-closeness" if "appreciation" in dimension_names else "more task-focused"
    reassurance = "reassuring" if "reassurance" in dimension_names else "less overtly reassuring"
    teasing = "teasing and performed" if "theatrical" in dimension_names else "comparatively plainspoken"
    control = "host-led" if "guidance" in dimension_names else "loosely steered"
    scope = "this month" if month_scope else "in the long run"
    return (
        f"{scope.capitalize()}, the chat relationship reads as {closeness}, {control}, and {teasing}. "
        f"Summary classification points to {pace} pacing and {reciprocity} reciprocity, while the selected evidence suggests that closeness is managed through appreciation rituals and {reassurance} resets rather than through open-ended disclosure alone."
    )


def append_dimension_section(lines: List[str], dimensions: Sequence[Dimension], monthly: bool) -> None:
    for dimension in dimensions:
        lines.append(f"### {dimension.name}")
        lines.append(f"- dimension_name: {dimension.name}")
        key = "why this mattered this month" if monthly else "why this is a stable personality dimension"
        lines.append(f"- {key}: {dimension.why}")
        if not monthly:
            lines.append(f"- why this is this streamer's version of the pattern: {dimension.distinct_note}")
        lines.append("- contributing open codes: " + ", ".join(dimension.contributing_codes))
        lines.append("- evidence pattern: " + dimension.evidence_pattern)
        lines.append("- supporting quotes:")
        for row in dimension.quotes:
            lines.append(f"- {render_quote(row)}")
        lines.append("- limits / counterevidence: " + dimension.limit_note)


def write_csv(path: Path, rows: Sequence[dict], fieldnames: Sequence[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def log_rows_for_dimensions(talent: str, report_window: str, dimensions: Sequence[Dimension]) -> List[dict]:
    records = []
    seen = set()
    for dimension in dimensions:
        for row in dimension.quotes:
            key = (dimension.name, row.video_id, row.timecode, row.quote)
            if key in seen:
                continue
            seen.add(key)
            records.append(
                {
                    "talent": talent,
                    "report_window": report_window,
                    "dimension_name": dimension.name,
                    "contributing_open_code": row.code_label,
                    "theme_family": row.theme_family,
                    "video_id": row.video_id,
                    "year_month": row.year_month,
                    "time_in_seconds": f"{row.time_in_seconds:.3f}" if not math.isnan(row.time_in_seconds) else "",
                    "timecode": row.timecode,
                    "source": row.source,
                    "speaker": row.speaker,
                    "quote": row.quote,
                    "evidence_role": "supporting_quote",
                    "personality_relevance": dimension.why,
                }
            )
    return records


def log_rows_for_markers(talent: str, report_window: str, markers: Sequence[MarkerEntry]) -> List[dict]:
    records = []
    seen = set()
    for marker in markers:
        for row in marker.quotes:
            key = (marker.name, row.video_id, row.timecode, row.quote)
            if key in seen:
                continue
            seen.add(key)
            records.append(
                {
                    "talent": talent,
                    "report_window": report_window,
                    "dimension_name": f"marker::{marker.name}",
                    "contributing_open_code": row.code_label,
                    "theme_family": row.theme_family,
                    "video_id": row.video_id,
                    "year_month": row.year_month,
                    "time_in_seconds": f"{row.time_in_seconds:.3f}" if not math.isnan(row.time_in_seconds) else "",
                    "timecode": row.timecode,
                    "source": row.source,
                    "speaker": row.speaker,
                    "quote": row.quote,
                    "evidence_role": "marker_quote",
                    "personality_relevance": marker.note,
                }
            )
    return records


def conclusion_lines(dimensions: Sequence[Dimension]) -> List[str]:
    out = []
    for dimension in dimensions[:3]:
        out.append(f"{dimension.name}: supported by {len(dimension.quotes)} quoted examples across {len({row.video_id for row in dimension.quotes})} streams.")
    return out


def uncertainty_lines(unresolved_count: int) -> List[str]:
    return [
        "Repeated lexical markers were only treated as personality evidence when their interactional function was visible in context.",
        "Text-only evidence cannot recover facial expression, body language, or full vocal prosody.",
        "Open-coding disclosure buckets include noisy logistical lines, so vulnerability claims were restricted to clearly emotional excerpts.",
        f"{unresolved_count} videos could not be assigned to a month confidently and were excluded from monthly slicing.",
    ]


def build_monthly_markdown(
    talent_name: str,
    source_label: str,
    summary_used: bool,
    month: str,
    month_dimensions: Sequence[Dimension],
    overall_dimensions: Sequence[Dimension],
    month_rows: Sequence[EvidenceRow],
    resolved_videos: int,
    unresolved_videos: int,
    summary_context: dict,
    month_scores: Dict[str, int],
    overall_scores: Dict[str, int],
    month_rates: Dict[str, float],
    overall_rates: Dict[str, float],
) -> str:
    distinct_dimension = next((dimension for dimension in month_dimensions if "Compared with peers" in dimension.distinct_note), month_dimensions[0] if month_dimensions else None)
    lines = [
        f"Analysis conducted: {ANALYSIS_STAMP}",
        f"Talent: {talent_name}",
        f"Report window: {month}",
        f"Primary personality source: {source_label}",
        f"Summary-classification source used: {'yes' if summary_used else 'no'}",
        f"Resolved videos in month: {resolved_videos}",
        f"Videos excluded from month due to unresolved date: {unresolved_videos}",
        "",
        "## 1) This Month's Personality Highlights",
    ]
    confidence = confidence_label(resolved_videos, month_dimensions, len(month_rows))
    lines.append(
        f"{month} is most visibly shaped by {top_dimension_phrase(month_dimensions)}. The strongest evidence this month comes from {len(month_rows)} selected streamer-visible rows across {resolved_videos} resolved videos, so the personality picture reads as a windowed interaction pattern rather than a timeless essence."
    )
    lines.append(
        f"What feels least interchangeable this month is {distinct_dimension.name if distinct_dimension else 'insufficient evidence'}: {distinct_dimension.distinct_note if distinct_dimension else 'insufficient evidence'}. Confidence: {confidence}."
    )
    lines.extend(
        [
            "",
            "## 2) Month-Specific Personality Dimensions",
        ]
    )
    append_dimension_section(lines, month_dimensions, monthly=True)
    lines.extend(
        [
            "",
            "## 3) Monthly Markers and Rituals",
        ]
    )
    marker_pairs = markers_from_dimensions(month_dimensions) or [("insufficient evidence", "No marker was repeated enough this month to warrant a stronger claim.")]
    lines.extend(f"- {marker}: {note}" for marker, note in marker_pairs)
    lines.extend(
        [
            "",
            "## 4) Relationship Style This Month",
            relationship_style_text(summary_context, month_dimensions, month_scope=True),
            "",
            "## 5) Shifts Relative to Overall Pattern",
        ]
    )
    shift = classify_month_shift(month_rates, overall_rates, len(month_rows))
    overall_keys = [dimension.key for dimension in overall_dimensions]
    month_keys = [dimension.key for dimension in month_dimensions]
    strengthened = [
        SIGNAL_LABELS[key]
        for key in month_keys
        if overall_rates.get(key, 0.0) > 0 and month_rates.get(key, 0.0) >= overall_rates.get(key, 0.0) * 1.3 and month_rates.get(key, 0.0) - overall_rates.get(key, 0.0) >= 0.015
    ]
    muted = [
        SIGNAL_LABELS[key]
        for key in overall_keys
        if overall_rates.get(key, 0.0) >= 0.03 and month_rates.get(key, 0.0) <= overall_rates.get(key, 0.0) * 0.7 and key not in month_keys[:2]
    ]
    lines.append(f"This month {shift}.")
    if strengthened:
        lines.append("- Intensified traits: " + ", ".join(strengthened))
    if muted:
        lines.append("- More muted than overall: " + ", ".join(muted))
    if not strengthened and not muted:
        lines.append("- The visible pattern mostly reinforced the broader baseline.")
    lines.extend(
        [
            "",
            "## 6) Monthly Limits and Uncertainty",
            f"- Disclosure-heavy code families contain noisy logistical lines, so only emotionally legible excerpts were used when discussing vulnerability.",
            f"- {unresolved_videos} unresolved videos remained outside the monthly slice.",
            f"- Text-only evidence cannot confirm visual demeanor or vocal prosody.",
        ]
    )
    return "\n".join(lines).strip() + "\n"


def compare_talents(profile_summaries: Dict[str, dict], talent_name: str) -> Tuple[str, str]:
    target = profile_summaries[talent_name]
    others = [summary for name, summary in profile_summaries.items() if name != talent_name]
    if len(others) < 2:
        return "insufficient evidence", "Could be confused with insufficient evidence on insufficient evidence, but differs in insufficient evidence."
    def similarity(other: dict) -> float:
        score = 0.0
        for signal in SIGNAL_ORDER:
            score -= abs(target["rates"].get(signal, 0.0) - other["rates"].get(signal, 0.0))
        if target["pace"] == other["pace"]:
            score += 0.4
        return score
    similar = sorted(others, key=similarity, reverse=True)[0]
    contrast = sorted(others, key=lambda other: abs(target["rates"].get("guidance", 0.0) - other["rates"].get("guidance", 0.0)) + abs(target["rates"].get("reassurance", 0.0) - other["rates"].get("reassurance", 0.0)), reverse=True)[0]
    shared_signal = max(
        SIGNAL_ORDER,
        key=lambda signal: min(target["rates"].get(signal, 0.0), similar["rates"].get(signal, 0.0)),
    )
    diff_candidates = [signal for signal in SIGNAL_ORDER if signal != shared_signal]
    diff_signal = max(
        diff_candidates,
        key=lambda signal: abs(target["rates"].get(signal, 0.0) - similar["rates"].get(signal, 0.0)),
    )
    diff_phrase = SIGNAL_LABELS[diff_signal]
    if abs(target["rates"].get(diff_signal, 0.0) - similar["rates"].get(diff_signal, 0.0)) < 0.01:
        if target["pace"] != similar["pace"]:
            diff_phrase = f"{target['pace']} pacing rather than {similar['pace']} pacing"
        elif target["reciprocity"] != similar["reciprocity"]:
            diff_phrase = f"{target['reciprocity']} reciprocity rather than {similar['reciprocity']} reciprocity"
    paragraph = (
        f"Shared ground with {similar['name']} and {contrast['name']} appears in the recurring emphasis on {SIGNAL_LABELS[shared_signal]}. "
        f"What keeps {talent_name} from collapsing into the same profile is the stronger tilt toward {diff_phrase}. "
        f"Compared with {similar['name']}, the shared baseline trait is performed with more host-steered routing. Compared with {contrast['name']}, the difference is not just volume but the combination of staged bits with more active pace control."
    )
    sentence = (
        f"Could be confused with {similar['name']} on {SIGNAL_LABELS[shared_signal]}, "
        f"but differs in {diff_phrase}."
    )
    return paragraph, sentence


def money_section(rows: Sequence[EvidenceRow]) -> Tuple[str, List[EvidenceRow]]:
    money_rows = [
        row
        for row in rows
        if row.monetary_context != "none" and is_streamer_speaker(row.talent, row.speaker) and ("appreciation" in row.signals or "theatrical" in row.signals)
    ]
    money_rows = select_quotes(money_rows, "appreciation") if money_rows else []
    if len(money_rows) < 3:
        return "insufficient evidence", []
    text = (
        "Money-linked moments mostly reveal how the streamer handles support as relationship work: paid events trigger visible acknowledgment, often with heightened appreciation or staged welcomes rather than flat transaction language. "
        "That makes monetization moments informative about reciprocity style, but still secondary to the broader interaction pattern."
    )
    return text, money_rows


def build_overall_markdown(
    talent_name: str,
    source_label: str,
    summary_used: bool,
    overall_dimensions: Sequence[Dimension],
    overall_rows: Sequence[EvidenceRow],
    monthly_reports: Dict[str, dict],
    unresolved_videos: int,
    summary_context: dict,
    retained_code_count: int,
    total_evidence_rows: int,
    profile_summaries: Dict[str, dict],
) -> Tuple[str, List[MarkerEntry], List[EvidenceRow]]:
    month_scores_by_month = {month: report["scores"] for month, report in monthly_reports.items()}
    month_rates_by_month = {month: report["rates"] for month, report in monthly_reports.items()}
    shift_groups = overall_shift_summary(month_scores_by_month, month_rates_by_month)
    confidence = confidence_label(len({row.video_id for row in overall_rows}), overall_dimensions, len(overall_rows))
    relationship_text = relationship_style_text(summary_context, overall_dimensions, month_scope=False)
    money_text, money_rows = money_section(overall_rows)
    marker_entries = marker_entries_from_dimensions(overall_dimensions)
    distinctiveness_paragraph, distinctiveness_sentence = compare_talents(profile_summaries, talent_name)
    summary_note = summary_context.get("summary_note", "Summary classification available.")
    lines = [
        f"Analysis conducted: {ANALYSIS_STAMP}",
        f"Talent: {talent_name}",
        "Report window: overall",
        f"Primary personality source: {source_label}",
        f"Summary-classification source used: {'yes' if summary_used else 'no'}",
        f"Resolved monthly windows: {len(monthly_reports)}",
        f"Videos with unresolved month metadata: {unresolved_videos}",
        "",
        "## 1) Overall Personality Highlights",
        f"{talent_name} is least interchangeable in this dataset where host-led pace control meets theatrical bit escalation and quick reassurance. Across the available text evidence, the profile reads most strongly through {top_dimension_phrase(overall_dimensions)}. These are recurring interaction tendencies grounded in quoted evidence, not fixed archetypes, and they describe how the streamer manages audience contact rather than who they are off-stream.",
        f"The overall picture is built from {len(overall_rows)} selected streamer-visible rows across {len({row.video_id for row in overall_rows})} videos. Confidence: {confidence}. What cannot be known from text-only evidence: facial expression, body language, and the fuller prosodic texture of the performance.",
        "",
        "## 2) Evidence Base and Synthesis Notes",
        f"- open-coding version used: {source_label}",
        f"- total retained codes available: {retained_code_count}",
        f"- total evidence rows reviewed: {total_evidence_rows}",
        f"- summary classification check: {summary_note}",
        f"- resolved monthly windows: {len(monthly_reports)}",
        "",
        "## 3) Stable Core Personality Dimensions",
    ]
    append_dimension_section(lines, overall_dimensions, monthly=False)
    lines.extend(
        [
            "",
            "## 4) Shifts Over Time",
            "- stable traits: " + ", ".join(SIGNAL_LABELS.get(value, value) for value in shift_groups["stable"]),
            "- recently strengthened traits: " + ", ".join(SIGNAL_LABELS.get(value, value) for value in shift_groups["strengthened"]),
            "- weakened or less-visible traits: " + ", ".join(SIGNAL_LABELS.get(value, value) for value in shift_groups["weakened"]),
            "- episodic or month-bound traits: " + ", ".join(SIGNAL_LABELS.get(value, value) for value in shift_groups["episodic"]),
            "",
            "## 5) Relational Style With Chat",
            relationship_text,
            "",
            "## 6) Idiosyncratic Markers and Rituals",
        ]
    )
    if marker_entries:
        for marker in marker_entries:
            lines.append(f"### {marker.name}")
            lines.append(f"- marker_name: {marker.name}")
            lines.append(f"- social function: {marker.note}")
            lines.append("- marker quotes:")
            for row in marker.quotes:
                lines.append(f"- {render_quote(row)}")
    else:
        lines.append("- insufficient evidence: No marker pattern rose above the general dimensions strongly enough to isolate here.")
    lines.extend(
        [
            "",
            "## 7) Personality Around Money Moments",
            money_text,
            "",
            "## 8) Cross-Talent Distinctiveness",
            distinctiveness_paragraph,
            distinctiveness_sentence,
            "",
            "## 9) Validity, Limits, and Uncertainty",
            "- strongest evidence-backed conclusions:",
        ]
    )
    for line in conclusion_lines(overall_dimensions):
        lines.append(f"- {line}")
    lines.append("- uncertainty points:")
    for line in uncertainty_lines(unresolved_videos)[:3]:
        lines.append(f"- {line}")
    lines.append(f"- lexical repetition risk note: repeated markers like `thank`, `welcome`, or `it's okay` were only treated as personality evidence when the surrounding interaction showed a relational function.")
    lines.append("- text-only modality note: the evidence cannot support claims about stable off-stream identity, diagnosis, or visually expressed affect.")
    text = "\n".join(lines).strip() + "\n"
    extra = []
    for row in money_rows:
        extra.append(row)
    return text, marker_entries, extra


def collect_streamer_rows(rows: Sequence[EvidenceRow], talent_name: str) -> List[EvidenceRow]:
    return [
        row
        for row in rows
        if row.quote
        and row.video_id
        and is_streamer_speaker(talent_name, row.speaker)
    ]


def rates_for_rows(rows: Sequence[EvidenceRow]) -> Dict[str, float]:
    base = max(1, len(rows))
    return {signal: len(signal_rows(rows, signal)) / base for signal in SIGNAL_ORDER}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build monthly and overall personality synthesis reports.")
    parser.add_argument("--talent", help="Process only the exact talent folder name.")
    return parser.parse_args()


def run() -> int:
    args = parse_args()
    profile_summaries: Dict[str, dict] = {}
    talent_runs: List[dict] = []
    discovered = discover_talents(DATA_ROOT, talent_slug=args.talent)
    discovered_for_peers = discover_talents(DATA_ROOT)
    peer_codebooks: Dict[str, Dict[str, float]] = {}
    for talent_dir in discovered_for_peers:
        source = choose_open_coding_source(talent_dir)
        if not source:
            continue
        peer_codebooks[talent_dir.name] = code_rate_map(csv_rows(source["codebook_path"]))
    for talent_dir in discovered_for_peers:
        source = choose_open_coding_source(talent_dir)
        if not source:
            continue
        summary_md, summary_state = overall_channel_summary_paths(talent_dir)
        codebook_rows = csv_rows(source["codebook_path"])
        raw_evidence = csv_rows(source["evidence_path"])
        video_ids = sorted({(row.get("video_id") or "").strip() for row in raw_evidence if (row.get("video_id") or "").strip()})
        month_map, unresolved_videos = build_video_month_map(talent_dir, video_ids)
        evidence_rows = make_evidence_rows(talent_dir.name, source["evidence_path"], month_map)
        streamer_rows = collect_streamer_rows(evidence_rows, talent_dir.name)
        usable_rows = [row for row in streamer_rows if row.signals]
        summary_context = parse_summary_context(summary_md, summary_state if summary_state and summary_state.exists() else None)
        target_code_rates = peer_codebooks.get(talent_dir.name, {})
        peer_rate_maps = [rates for name, rates in peer_codebooks.items() if name != talent_dir.name]
        overall_dimensions = build_dimensions(
            usable_rows,
            summary_context,
            min_rows=3,
            month_scope=False,
            target_code_rates=target_code_rates,
            peer_code_rates=peer_rate_maps,
        )
        monthly_reports: Dict[str, dict] = {}
        for month in sorted({row.year_month for row in usable_rows if row.year_month}):
            month_rows = [row for row in usable_rows if row.year_month == month]
            month_dimensions = build_dimensions(
                month_rows,
                summary_context,
                min_rows=2,
                month_scope=True,
                target_code_rates=target_code_rates,
                peer_code_rates=peer_rate_maps,
            )
            if len(month_rows) < 6 or len(month_dimensions) < 2:
                continue
            monthly_reports[month] = {
                "rows": month_rows,
                "dimensions": month_dimensions,
                "scores": signal_scores(month_rows),
                "rates": signal_rates(month_rows),
                "resolved_videos": len({row.video_id for row in month_rows}),
            }
        profile_summaries[talent_dir.name] = {
            "name": talent_dir.name,
            "rates": rates_for_rows(usable_rows),
            "pace": dominant_label(summary_context.get("pacing", {}), "rapid"),
            "reciprocity": dominant_label(summary_context.get("reciprocity", {}), "high"),
        }
        talent_runs.append(
            {
                "dir": talent_dir,
                "source": source,
                "summary_context": summary_context,
                "codebook_rows": codebook_rows,
                "evidence_rows": evidence_rows,
                "usable_rows": usable_rows,
                "overall_dimensions": overall_dimensions,
                "monthly_reports": monthly_reports,
                "unresolved_videos": unresolved_videos,
            }
        )

    report_summary = []
    for run_info in talent_runs:
        talent_dir = run_info["dir"]
        if args.talent and talent_dir.name != args.talent:
            continue
        talent_name = talent_dir.name
        source = run_info["source"]
        summary_context = run_info["summary_context"]
        codebook_rows = run_info["codebook_rows"]
        evidence_rows = run_info["evidence_rows"]
        usable_rows = run_info["usable_rows"]
        overall_dimensions = run_info["overall_dimensions"]
        monthly_reports = run_info["monthly_reports"]
        unresolved_videos = run_info["unresolved_videos"]

        monthly_root = talent_dir / MONTH_DIR
        overall_root = talent_dir / OVERALL_DIR
        overall_scores = signal_scores(usable_rows)
        overall_rates = signal_rates(usable_rows)
        summary_used = bool(summary_context.get("used"))
        monthly_log_total = 0
        monthly_output_paths = []

        for month, report in monthly_reports.items():
            month_dir = monthly_root / month
            markdown_path = month_dir / "personality_monthly_highlights_codex.md"
            csv_path = month_dir / "personality_monthly_evidence_log_codex.csv"
            markdown = build_monthly_markdown(
                talent_name=talent_name,
                source_label=source["label"],
                summary_used=summary_used,
                month=month,
                month_dimensions=report["dimensions"],
                overall_dimensions=overall_dimensions,
                month_rows=report["rows"],
                resolved_videos=report["resolved_videos"],
                unresolved_videos=len(unresolved_videos),
                summary_context=summary_context,
                month_scores=report["scores"],
                overall_scores=overall_scores,
                month_rates=report["rates"],
                overall_rates=overall_rates,
            )
            month_dir.mkdir(parents=True, exist_ok=True)
            markdown_path.write_text(markdown, encoding="utf-8")
            log_rows = log_rows_for_dimensions(talent_name, month, report["dimensions"])
            monthly_log_total += len(log_rows)
            write_csv(
                csv_path,
                log_rows,
                [
                    "talent",
                    "report_window",
                    "dimension_name",
                    "contributing_open_code",
                    "theme_family",
                    "video_id",
                    "year_month",
                    "time_in_seconds",
                    "timecode",
                    "source",
                    "speaker",
                    "quote",
                    "evidence_role",
                    "personality_relevance",
                ],
            )
            monthly_output_paths.append(str(markdown_path))
            monthly_output_paths.append(str(csv_path))

        overall_markdown_path = overall_root / "personality_overall_highlights_codex.md"
        overall_csv_path = overall_root / "personality_overall_evidence_log_codex.csv"
        overall_markdown, marker_entries, extra_money_rows = build_overall_markdown(
            talent_name=talent_name,
            source_label=source["label"],
            summary_used=summary_used,
            overall_dimensions=overall_dimensions,
            overall_rows=usable_rows,
            monthly_reports=monthly_reports,
            unresolved_videos=len(unresolved_videos),
            summary_context=summary_context,
            retained_code_count=len(codebook_rows),
            total_evidence_rows=len(evidence_rows),
            profile_summaries=profile_summaries,
        )
        overall_root.mkdir(parents=True, exist_ok=True)
        overall_markdown_path.write_text(overall_markdown, encoding="utf-8")
        overall_log_rows = log_rows_for_dimensions(talent_name, "overall", overall_dimensions)
        for row in log_rows_for_markers(talent_name, "overall", marker_entries):
            overall_log_rows.append(row)
        seen = {(row["video_id"], row["timecode"], row["quote"]) for row in overall_log_rows}
        for row in extra_money_rows:
            key = (row.video_id, row.timecode, row.quote)
            if key in seen:
                continue
            overall_log_rows.append(
                {
                    "talent": talent_name,
                    "report_window": "overall",
                    "dimension_name": "money-linked relationship work",
                    "contributing_open_code": row.code_label,
                    "theme_family": row.theme_family,
                    "video_id": row.video_id,
                    "year_month": row.year_month,
                    "time_in_seconds": f"{row.time_in_seconds:.3f}" if not math.isnan(row.time_in_seconds) else "",
                    "timecode": row.timecode,
                    "source": row.source,
                    "speaker": row.speaker,
                    "quote": row.quote,
                    "evidence_role": "money_moment",
                    "personality_relevance": "Shows how support moments are folded into the streamer's relationship style.",
                }
            )
            seen.add(key)
        write_csv(
            overall_csv_path,
            overall_log_rows,
            [
                "talent",
                "report_window",
                "dimension_name",
                "contributing_open_code",
                "theme_family",
                "video_id",
                "year_month",
                "time_in_seconds",
                "timecode",
                "source",
                "speaker",
                "quote",
                "evidence_role",
                "personality_relevance",
            ],
        )
        report_summary.append(
            {
                "talent": talent_name,
                "open_coding_source": source["label"],
                "resolved_monthly_windows_written": len(monthly_reports),
                "retained_codes_available": len(codebook_rows),
                "monthly_evidence_rows_used": monthly_log_total,
                "overall_evidence_rows_used": len(overall_log_rows),
                "summary_classification_used": summary_used,
                "output_paths": monthly_output_paths + [str(overall_markdown_path), str(overall_csv_path)],
            }
        )

    print(json.dumps(report_summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(run())
