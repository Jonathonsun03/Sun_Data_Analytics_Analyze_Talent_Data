#!/usr/bin/env python3
"""Incremental per-talent qualitative classification for stream summaries.

This workflow:
- discovers all direct talent folders under the configured data root
- skips the aggregate VarianceProject folder
- uses stream_summary_codex markdown as the primary analytic input
- uses raw CSV logs only to verify direct quotes and note evidence limitations
- updates a cumulative current markdown/state pair plus a dated snapshot
"""

from __future__ import annotations

import csv
import glob
import json
import math
import os
import re
from collections import Counter, defaultdict
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Tuple


DATA_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
REPO_ROOT = Path(__file__).resolve().parents[4]
PROMPT_SPEC_PATH = REPO_ROOT / "prompts" / "stream_summaries" / "summarizing_stream_classification_v2.md"
SKIP_DIRS = {"VarianceProject"}
MAX_CODES = 5
FORCE_REFRESH = os.environ.get("SUMMARY_CLASSIFICATION_FORCE_REFRESH") == "1"

STOPWORDS = {
    "a",
    "an",
    "and",
    "are",
    "as",
    "at",
    "be",
    "been",
    "but",
    "by",
    "for",
    "from",
    "have",
    "i",
    "if",
    "in",
    "into",
    "is",
    "it",
    "its",
    "just",
    "me",
    "my",
    "of",
    "on",
    "or",
    "our",
    "so",
    "that",
    "the",
    "their",
    "them",
    "there",
    "they",
    "this",
    "to",
    "us",
    "was",
    "we",
    "were",
    "with",
    "you",
    "your",
}


@dataclass
class SummaryDoc:
    video_id: str
    title: str
    path: str
    text: str
    overview: str
    descriptors: List[str]
    reciprocity: str
    pacing: str
    content_tags: List[str]
    prompt_version: str
    prompt_label: str


@dataclass
class Event:
    video_id: str
    sec: float
    timecode: str
    source: str
    speaker: str
    text: str
    message_type: str
    paid_amount_text: str
    file_path: str
    file_line: int


@dataclass
class InventoryRow:
    video_id: str
    summary_path: str
    playback_path: str
    chat_path: str
    playback_rows: int = -1
    chat_rows: int = -1


@dataclass
class Example:
    video_id: str
    timecode: str
    speaker: str
    quote: str
    file_path: str
    file_line: int


@dataclass
class CodeResult:
    key: str
    code_name: str
    definition: str
    inclusion_criteria: str
    exclusion_criteria: str
    observed_summary_count: int
    supporting_summary_ids: List[str]
    supporting_new_summary_ids: List[str]
    examples: List[Example]
    new_this_run: bool


CODE_SPECS = [
    {
        "key": "humor_play",
        "default_name": "playful banter",
        "summary_patterns": [
            r"\bcelebratory\b",
            r"\benergetic\b",
            r"\bperformative\b",
            r"\bplayful\b",
            r"\bbanter\b",
            r"\bself-deprecating\b",
            r"\bchaotic\b",
            r"\bhype\b",
            r"\bcozy\b",
            r"\bintimate vibe\b",
        ],
        "event_patterns": [
            r"\b(?:haha|hehe|lol|lmao)\b",
            r"\b(?:cute|baby|bozo|nerd|silly|goober|gremlin|granny)\b.{4,}",
            r"\bwhat(?:'s| is) happening\b",
            r"\b(?:let's go|lets go|woo+|yipee)\b.{0,}",
            r"\b(?:oh my god|oh no)\b",
        ],
        "label_rules": [
            (r"\b(?:performative|celebratory|energetic|theatrical)\b", "performative hype and bits"),
            (r"\b(?:self-deprecating|cozy|grandma|granny|warm)\b", "cozy self-deprecation"),
            (r"\b(?:rant|blunt|sardonic|dry)\b", "dry banter"),
        ],
        "definition": "Recurring use of jokes, teasing, or heightened delivery to keep the interaction lively.",
        "inclusion": "Retain when summaries repeatedly describe the stream as playful, performative, celebratory, or self-deprecating and raw lines show that tone directly.",
        "exclusion": "Do not use for purely informational replies or generic acknowledgements without a playful edge.",
        "speaker_preference": "streamer",
    },
    {
        "key": "chat_attunement",
        "default_name": "active chat acknowledgment",
        "summary_patterns": [
            r"\bconversational\b",
            r"\baudience-aware\b",
            r"\bback-and-forth\b",
            r"\backnowledge(?:s|d)?\b",
            r"\bresponds?\b",
            r"\battentive\b",
            r"\banswers questions\b",
            r"\breciprocity\b",
            r"\bresponsive\b",
        ],
        "event_patterns": [
            r"\b(?:thank you|thanks|welcome in|welcome on|good morning|goodnight)\b",
            r"\b(?:i see you|i hear you|i got you|i know)\b",
            r"\b(?:what do you mean|how are you|how convenient)\b",
            r"\b(?:be here soon|take your time)\b",
        ],
        "label_rules": [
            (r"\b(?:muted|typing responses|working silently|recording constraints)\b", "responsive low-key check-ins"),
        ],
        "definition": "Regular visible acknowledgment of chat presence, questions, or prompts.",
        "inclusion": "Retain when summaries repeatedly note that chat messages are seen, answered, or folded into the flow of the stream.",
        "exclusion": "Do not use for monologue stretches where chat presence is minimal or only implied.",
        "speaker_preference": "streamer",
    },
    {
        "key": "pace_control",
        "default_name": "segment pacing control",
        "summary_patterns": [
            r"\brapid pacing\b",
            r"\bslow pacing\b",
            r"\bmoderate\b",
            r"\bleads the session structure\b",
            r"\bsegment flow\b",
            r"\bbalancing\b",
            r"\bcontinues working\b",
            r"\bdrives the stream\b",
        ],
        "event_patterns": [
            r"\b(?:let's|lets) (?:go|see|do|move|keep)\b",
            r"\b(?:hold on|one second|hang on)\b",
            r"\b(?:we(?:'re| are) going to|i(?:'m| am) going to)\b",
            r"\b(?:starting|moving on|next up|be right back|focus)\b",
        ],
        "label_rules": [
            (r"\brapid pacing\b", "rapid flow steering"),
            (r"\b(?:slow pacing|moderate|working silently|muted)\b", "gentle pace steering"),
        ],
        "definition": "Visible management of tempo, transitions, and when chat can redirect the stream.",
        "inclusion": "Retain when summaries consistently mention pacing or streamer-led structure and raw lines show active transition-setting.",
        "exclusion": "Do not use for isolated filler words or one-off pauses without a broader flow-management pattern.",
        "speaker_preference": "streamer",
    },
    {
        "key": "care_reassurance",
        "default_name": "care and reassurance",
        "summary_patterns": [
            r"\bwarm\b",
            r"\bsupportive\b",
            r"\bencouragement\b",
            r"\bappreciation\b",
            r"\breassur(?:e|ing)\b",
            r"\bcozy\b",
            r"\bbirthday\b",
            r"\bfriendly\b",
        ],
        "event_patterns": [
            r"\b(?:take your time|it's okay|its okay|no worries|rest well|happy birthday|be safe|proud of you)\b",
            r"\b(?:thank you for the membership|thank you so much|thanks for the gift)\b",
            r"\b(?:you got this|all the best)\b",
        ],
        "label_rules": [
            (r"\b(?:warm|supportive|cozy|friendly)\b", "warm reassurance"),
            (r"\b(?:birthday|gift membership|memberships?)\b", "community care rituals"),
        ],
        "definition": "Moments where the relationship is framed through comfort, appreciation, or gentle encouragement.",
        "inclusion": "Retain when summaries repeatedly describe supportive or caring exchanges and raw lines confirm that stance.",
        "exclusion": "Do not use for transactional thank-yous that show no wider supportive or reassuring posture.",
        "speaker_preference": "any",
    },
    {
        "key": "boundary_setting",
        "default_name": "firm conversational guardrails",
        "summary_patterns": [
            r"\bguardrails?\b",
            r"\bboundar(?:y|ies)\b",
            r"\bmoderation\b",
            r"\bfirm\b",
            r"\bkeep it respectful\b",
        ],
        "event_patterns": [
            r"\b(?:no spoilers|no backseating|please stop|don't spam|dont spam)\b",
            r"\b(?:be respectful|behave|calm down|not okay|that's not okay)\b",
            r"\b(?:we're moving on|we are moving on)\b",
            r"\bdon't\b.{4,}",
        ],
        "label_rules": [],
        "definition": "Visible correction or limit-setting that keeps chat within the streamer’s preferred frame.",
        "inclusion": "Retain when summaries or raw evidence show repeated correction, rule-setting, or narrowing of chat behavior.",
        "exclusion": "Do not use for playful mock scolding unless it clearly functions as a real limit or redirect.",
        "speaker_preference": "streamer",
    },
    {
        "key": "support_rituals",
        "default_name": "support as participation ritual",
        "summary_patterns": [
            r"\bfunction primarily as support\b",
            r"\bbonding\b",
            r"\britual participation\b",
            r"\bgoal(?:s|/time extension)?\b",
            r"\bmemberships?\b",
            r"\bgift membership\b",
            r"\btime extension\b",
            r"\bcommunal celebration\b",
        ],
        "event_patterns": [
            r"\b(?:thank you for the membership|gift(?:ed)?|super ?chat|goal started|rum fund)\b",
            r"\b(?:here's to|heres to|happy birthday)\b",
        ],
        "label_rules": [
            (r"\b(?:goal|time extension)\b", "support-driven momentum"),
            (r"\b(?:birthday|gift membership|communal celebration)\b", "support as community ritual"),
        ],
        "definition": "Support moments are framed as communal participation rather than isolated transactions.",
        "inclusion": "Retain when multiple summaries describe paid or gifted moments as shared rituals, celebrations, or momentum cues.",
        "exclusion": "Do not use when summaries explicitly report no paid-message evidence and no communal support pattern appears elsewhere.",
        "speaker_preference": "any",
    },
]


def normalize_text(text: str) -> str:
    s = (text or "").lower().strip()
    s = re.sub(r"https?://\S+", "", s)
    s = re.sub(r"\s+", " ", s)
    return s


def normalize_speaker(speaker: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", (speaker or "").lower().lstrip("@"))


def parse_float(value: str) -> float:
    try:
        return float(str(value).strip())
    except Exception:
        return math.nan


def sec_to_timecode(sec: float) -> str:
    if math.isnan(sec) or sec < 0:
        return "00:00:00"
    total = int(round(sec))
    h = total // 3600
    m = (total % 3600) // 60
    s = total % 60
    return f"{h:02d}:{m:02d}:{s:02d}"


def short_quote(text: str, limit: int = 160) -> str:
    out = (text or "").replace("\n", " ").strip()
    out = re.sub(r"\s+", " ", out)
    if len(out) <= limit:
        return out
    return out[: limit - 3].rstrip() + "..."


def escape_md(text: str) -> str:
    return text.replace("|", "\\|")


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def load_json(path: Path) -> dict:
    if not path.exists():
        return {}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {}


def write_json(path: Path, data: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2), encoding="utf-8")


def nonempty(path: Path) -> bool:
    return path.exists() and path.stat().st_size > 0


def extract_video_id_from_name(path: str) -> str:
    base = os.path.basename(path)
    match = re.search(r"_([A-Za-z0-9_-]{11})(?:_summary\.md|_chat\.csv|\.csv)$", base)
    if match:
        return match.group(1)
    match = re.search(r"([A-Za-z0-9_-]{11})(?:_summary\.md|_chat\.csv|\.csv)$", base)
    if match:
        return match.group(1)
    return ""


def count_csv_rows(path: str) -> int:
    if not path or not os.path.exists(path):
        return 0
    with open(path, "r", encoding="utf-8-sig", newline="") as f:
        return max(0, sum(1 for _ in f) - 1)


def compile_any(patterns: Sequence[str]) -> List[re.Pattern[str]]:
    return [re.compile(p, re.I) for p in patterns]


def discover_talents() -> List[Path]:
    talents = []
    for path in sorted(DATA_ROOT.iterdir()):
        if not path.is_dir():
            continue
        if path.name in SKIP_DIRS:
            continue
        if (path / "stream_summaries").is_dir():
            talents.append(path)
    return talents


def derive_streamer_tokens(talent_name: str) -> List[str]:
    base = re.sub(r"【.*?】", " ", talent_name)
    base = re.sub(r"[^A-Za-z0-9]+", " ", base).strip().lower()
    tokens = []
    for token in base.split():
        if token in {"variance", "project", "ch"}:
            continue
        if len(token) >= 3:
            tokens.append(token)
    return tokens[:4]


def is_streamer(event: Event, talent_name: str, tokens: Iterable[str]) -> bool:
    speaker = normalize_speaker(event.speaker)
    if event.source == "subtitle" and speaker in {"stream", "host", "streamer"}:
        return True
    talent_norm = normalize_speaker(talent_name)
    if talent_norm and talent_norm in speaker:
        return True
    for token in tokens:
        if token and token in speaker:
            return True
    return False


def parse_summary_overview(text: str) -> str:
    match = re.search(r"##\s*1\)\s*Interaction Overview\s*(.*?)(?:\n##\s*2\)|\Z)", text, re.S | re.I)
    if match:
        return match.group(1).strip()
    return text[:1200].strip()


def parse_summary_descriptors(overview: str) -> List[str]:
    matches = re.findall(r"\bis ([^.:\n]+?)(?:, with|\.|\n|;)", overview, re.I)
    if not matches:
        return []
    raw = matches[0]
    parts = re.split(r",| and ", raw)
    out = []
    for part in parts:
        cleaned = re.sub(r"[^A-Za-z0-9 -]+", " ", part).strip().lower()
        cleaned = re.sub(r"\s+", " ", cleaned)
        if cleaned and cleaned not in STOPWORDS:
            out.append(cleaned)
    return out[:6]


def parse_reciprocity(text: str) -> str:
    match = re.search(r"Reciprocity[^:]*:\s*\**(High|Moderate|Low)\**", text, re.I)
    return match.group(1).title() if match else "Unspecified"


def parse_pacing(text: str) -> str:
    for label in ("rapid", "moderate", "slow"):
        if re.search(rf"\b{label} pacing\b", text, re.I):
            return label
    if re.search(r"\bchat momentum:\s*rapid\b", text, re.I):
        return "rapid"
    if re.search(r"\bchat momentum:\s*moderate\b", text, re.I):
        return "moderate"
    if re.search(r"\bchat momentum:\s*slow\b", text, re.I):
        return "slow"
    return "unspecified"


def parse_prompt_metadata(text: str) -> Tuple[str, str]:
    version_patterns = [
        r"source summary prompt version:\s*(.+)",
        r"prompt version:\s*(.+)",
        r"summary prompt version:\s*(.+)",
    ]
    label_patterns = [
        r"source summary prompt path/label:\s*(.+)",
        r"prompt path/label:\s*(.+)",
        r"prompt path:\s*(.+)",
        r"prompt label:\s*(.+)",
    ]
    version = ""
    label = ""
    for pattern in version_patterns:
        match = re.search(pattern, text, re.I)
        if match:
            version = match.group(1).strip()
            break
    for pattern in label_patterns:
        match = re.search(pattern, text, re.I)
        if match:
            label = match.group(1).strip()
            break
    return version, label


def infer_content_tags(title: str, text: str) -> List[str]:
    hay = f"{title} {text}".lower()
    tags = []
    rules = [
        ("karaoke/music", [r"\bkaraoke\b", r"\bsing\b", r"\bconcert\b", r"\bcaroling\b"]),
        ("chatting/zatsu", [r"\bzatsu\b", r"\bafterhours\b", r"\bbar\b", r"\brantcast\b", r"\bchatting\b"]),
        ("working/art", [r"\bworking stream\b", r"\bworking\b", r"\bdrawing\b", r"\bart\b", r"\brecord\b"]),
        ("collab/relay", [r"\bcollab\b", r"\brelay\b", r"\bwith @", r"\bcompetition\b"]),
        ("gaming", [r"\bpt\.?\b", r"\bchapter\b", r"\bgame\b", r"\bpokemon\b", r"\bmario\b", r"\bphoenix wright\b"]),
        ("announcement/milestone", [r"\bannouncement\b", r"\banniversary\b", r"\bbirthday\b", r"\b500 sub\b", r"\bwrap up\b"]),
    ]
    for tag, patterns in rules:
        if any(re.search(pattern, hay, re.I) for pattern in patterns):
            tags.append(tag)
    if not tags:
        tags.append("uncategorized")
    return tags


def load_summaries(summary_paths: List[str]) -> Dict[str, SummaryDoc]:
    docs = {}
    for path in summary_paths:
        video_id = extract_video_id_from_name(path)
        if not video_id:
            continue
        text = read_text(Path(path))
        overview = parse_summary_overview(text)
        version, label = parse_prompt_metadata(text)
        title = os.path.basename(path).rsplit("_summary.md", 1)[0].replace("_", " ")
        docs[video_id] = SummaryDoc(
            video_id=video_id,
            title=title,
            path=path,
            text=text,
            overview=overview,
            descriptors=parse_summary_descriptors(overview),
            reciprocity=parse_reciprocity(text),
            pacing=parse_pacing(text),
            content_tags=infer_content_tags(title, text),
            prompt_version=version,
            prompt_label=label,
        )
    return docs


def load_playback_events(playback_paths: List[str]) -> List[Event]:
    events: List[Event] = []
    for path in playback_paths:
        fallback_video_id = extract_video_id_from_name(path)
        with open(path, "r", encoding="utf-8-sig", newline="") as f:
            reader = csv.DictReader(f)
            for line_no, row in enumerate(reader, start=2):
                text = (row.get("text") or "").strip()
                if not text or text.upper() == "NA":
                    continue
                sec = parse_float(row.get("sec", ""))
                if math.isnan(sec):
                    continue
                video_id = (row.get("video_id") or fallback_video_id or "").strip()
                if not video_id:
                    continue
                source = (row.get("source") or "chat").strip().lower()
                if source not in {"chat", "subtitle"}:
                    source = "chat"
                events.append(
                    Event(
                        video_id=video_id,
                        sec=sec,
                        timecode=(row.get("timecode") or sec_to_timecode(sec)).strip(),
                        source=source,
                        speaker=(row.get("speaker") or "").strip() or "UNKNOWN",
                        text=text,
                        message_type=(row.get("message_type") or "").strip(),
                        paid_amount_text=(row.get("paid_amount_text") or "").strip(),
                        file_path=path,
                        file_line=line_no,
                    )
                )
    return events


def load_chat_events(chat_paths: List[str]) -> List[Event]:
    events: List[Event] = []
    for path in chat_paths:
        fallback_video_id = extract_video_id_from_name(path)
        with open(path, "r", encoding="utf-8-sig", newline="") as f:
            reader = csv.DictReader(f)
            for line_no, row in enumerate(reader, start=2):
                text = (row.get("message") or "").strip()
                if not text or text.upper() == "NA":
                    continue
                sec = parse_float(row.get("time_in_seconds", ""))
                if math.isnan(sec):
                    continue
                video_id = (row.get("video_id") or fallback_video_id or "").strip()
                if not video_id:
                    continue
                paid_text = (row.get("paid_amount_text") or "").strip()
                events.append(
                    Event(
                        video_id=video_id,
                        sec=sec,
                        timecode=sec_to_timecode(sec),
                        source="chat",
                        speaker=(row.get("username") or "").strip() or "UNKNOWN",
                        text=text,
                        message_type=(row.get("message_type") or "").strip(),
                        paid_amount_text=paid_text,
                        file_path=path,
                        file_line=line_no,
                    )
                )
    return events


def dedupe_events(playback_events: List[Event], chat_events: List[Event]) -> Dict[str, List[Event]]:
    merged = list(playback_events)
    seen = {
        (
            event.video_id,
            int(round(event.sec)),
            normalize_speaker(event.speaker),
            normalize_text(event.text),
            event.source,
        )
        for event in playback_events
    }
    for event in chat_events:
        key = (
            event.video_id,
            int(round(event.sec)),
            normalize_speaker(event.speaker),
            normalize_text(event.text),
            event.source,
        )
        if key in seen:
            continue
        seen.add(key)
        merged.append(event)
    merged.sort(key=lambda e: (e.video_id, e.sec, e.file_path, e.file_line))
    out: Dict[str, List[Event]] = defaultdict(list)
    for event in merged:
        out[event.video_id].append(event)
    return out


def build_inventory(talent_dir: Path) -> Tuple[Dict[str, InventoryRow], List[str]]:
    summary_paths = sorted(glob.glob(str(talent_dir / "stream_summaries" / "stream_summary_codex" / "*.md")))
    playback_paths = sorted(glob.glob(str(talent_dir / "text_playback" / "*.csv")))
    chat_paths = sorted(glob.glob(str(talent_dir / "Chat" / "Original" / "*_chat.csv")))

    playback_map = {extract_video_id_from_name(path): path for path in playback_paths if extract_video_id_from_name(path)}
    chat_map = {extract_video_id_from_name(path): path for path in chat_paths if extract_video_id_from_name(path)}
    inventory = {}
    for summary_path in summary_paths:
        video_id = extract_video_id_from_name(summary_path)
        if not video_id:
            continue
        playback_path = playback_map.get(video_id, "")
        chat_path = chat_map.get(video_id, "")
        inventory[video_id] = InventoryRow(
            video_id=video_id,
            summary_path=summary_path,
            playback_path=playback_path,
            chat_path=chat_path,
        )
    raw_only_ids = sorted((set(playback_map) | set(chat_map)) - set(inventory))
    return inventory, raw_only_ids


def ensure_row_counts(inventory: Dict[str, InventoryRow], video_ids: Sequence[str]) -> None:
    for video_id in video_ids:
        row = inventory.get(video_id)
        if not row:
            continue
        if row.playback_rows < 0:
            row.playback_rows = count_csv_rows(row.playback_path)
        if row.chat_rows < 0:
            row.chat_rows = count_csv_rows(row.chat_path)


def aggregate_prompt_metadata(docs: Sequence[SummaryDoc]) -> Tuple[str, str]:
    versions = sorted({doc.prompt_version for doc in docs if doc.prompt_version})
    labels = sorted({doc.prompt_label for doc in docs if doc.prompt_label})
    if not versions:
        version = "unknown"
    elif len(versions) == 1:
        version = versions[0]
    else:
        version = "mixed"
    if not labels:
        label = "unknown"
    elif len(labels) == 1:
        label = labels[0]
    else:
        label = "mixed"
    return version, label


def summary_matches(doc: SummaryDoc, patterns: List[re.Pattern[str]]) -> bool:
    hay = f"{doc.overview}\n{doc.text}"
    return any(pattern.search(hay) for pattern in patterns)


def event_matches(event: Event, patterns: List[re.Pattern[str]]) -> bool:
    text = event.text
    if len(text) < 4:
        return False
    if text.strip().startswith("http"):
        return False
    return any(pattern.search(text) for pattern in patterns)


def choose_label(spec: dict, docs: Sequence[SummaryDoc]) -> str:
    threshold = max(3, int(math.ceil(len(docs) * 0.15))) if docs else 1
    for pattern, label in spec["label_rules"]:
        count = sum(1 for doc in docs if re.search(pattern, doc.text, re.I))
        if count >= threshold:
            return label
    return spec["default_name"]


def rank_examples(
    candidates: List[Event],
    talent_name: str,
    tokens: List[str],
    speaker_preference: str,
    max_examples: int = 3,
) -> List[Example]:
    generic_phrases = {
        "hi",
        "hello",
        "wait",
        "wait.",
        "wait,",
        "stop",
        "stop.",
        "stop,",
        "cute",
        "cute.",
        "baby",
        "baby.",
        "okay",
        "okay.",
    }
    filtered = []
    for event in candidates:
        quote = short_quote(event.text)
        normalized_quote = normalize_text(quote)
        word_count = len([word for word in re.split(r"\s+", normalized_quote) if word])
        if len(quote) < 8:
            continue
        if re.fullmatch(r"[\W_]+", quote):
            continue
        if normalized_quote in generic_phrases:
            continue
        if word_count < 2:
            continue
        filtered.append(event)
    def sort_key(event: Event) -> Tuple[int, int, int, str]:
        streamer = is_streamer(event, talent_name, tokens)
        pref = 0
        if speaker_preference == "streamer":
            pref = 0 if streamer else 1
        elif speaker_preference == "any":
            pref = 0
        quote = short_quote(event.text)
        words = len([word for word in re.split(r"\s+", normalize_text(quote)) if word])
        richness = abs(words - 8)
        return (
            pref,
            richness,
            -len(quote),
            int(round(event.sec)),
            event.video_id,
        )
    filtered.sort(key=sort_key)
    picked: List[Example] = []
    used_videos = set()
    for event in filtered:
        if len(picked) >= max_examples:
            break
        quote = short_quote(event.text)
        if event.video_id in used_videos and len(filtered) > max_examples:
            continue
        used_videos.add(event.video_id)
        picked.append(
            Example(
                video_id=event.video_id,
                timecode=event.timecode,
                speaker=event.speaker,
                quote=quote,
                file_path=event.file_path,
                file_line=event.file_line,
            )
        )
    if len(picked) < min(max_examples, len(filtered)):
        for event in filtered:
            if len(picked) >= max_examples:
                break
            quote = short_quote(event.text)
            marker = (event.video_id, quote)
            if any((ex.video_id, ex.quote) == marker for ex in picked):
                continue
            picked.append(
                Example(
                    video_id=event.video_id,
                    timecode=event.timecode,
                    speaker=event.speaker,
                    quote=quote,
                    file_path=event.file_path,
                    file_line=event.file_line,
                )
            )
    return picked


def select_codes(
    talent_name: str,
    tokens: List[str],
    docs: Dict[str, SummaryDoc],
    analysis_ids: List[str],
    new_ids: List[str],
    events_by_video: Dict[str, List[Event]],
    prior_code_names: Iterable[str],
) -> List[CodeResult]:
    prior_names = {name.lower() for name in prior_code_names}
    selected_docs = [docs[video_id] for video_id in analysis_ids if video_id in docs]
    selected_new_docs = [docs[video_id] for video_id in new_ids if video_id in docs]
    results: List[CodeResult] = []
    for spec in CODE_SPECS:
        summary_patterns = compile_any(spec["summary_patterns"])
        event_patterns = compile_any(spec["event_patterns"])
        supported_docs = [doc for doc in selected_docs if summary_matches(doc, summary_patterns)]
        if not supported_docs:
            continue
        candidate_events = []
        for video_id in analysis_ids:
            for event in events_by_video.get(video_id, []):
                if event_matches(event, event_patterns):
                    candidate_events.append(event)
        examples = rank_examples(candidate_events, talent_name, tokens, spec["speaker_preference"])
        if not examples:
            continue
        code_name = choose_label(spec, supported_docs)
        new_doc_ids = [doc.video_id for doc in selected_new_docs if summary_matches(doc, summary_patterns)]
        new_this_run = code_name.lower() not in prior_names and bool(new_doc_ids or not prior_names)
        results.append(
            CodeResult(
                key=spec["key"],
                code_name=code_name,
                definition=spec["definition"],
                inclusion_criteria=spec["inclusion"],
                exclusion_criteria=spec["exclusion"],
                observed_summary_count=len(supported_docs),
                supporting_summary_ids=[doc.video_id for doc in supported_docs],
                supporting_new_summary_ids=new_doc_ids,
                examples=examples[:3],
                new_this_run=new_this_run,
            )
        )
    results.sort(
        key=lambda item: (
            item.observed_summary_count,
            len({example.video_id for example in item.examples}),
            len(item.examples),
            item.code_name,
        ),
        reverse=True,
    )
    return results[:MAX_CODES]


def reciprocity_summary(docs: Sequence[SummaryDoc]) -> str:
    counts = Counter(doc.reciprocity for doc in docs if doc.reciprocity != "Unspecified")
    if not counts:
        return "reciprocity is not consistently stated in the source summaries"
    parts = [f"{label.lower()} in {count}" for label, count in counts.most_common()]
    return "reciprocity is described as " + ", ".join(parts) + " summaries"


def pacing_summary(docs: Sequence[SummaryDoc]) -> str:
    counts = Counter(doc.pacing for doc in docs if doc.pacing != "unspecified")
    if not counts:
        return "pacing is mixed or unspecified in the available summaries"
    parts = [f"{label} in {count}" for label, count in counts.most_common()]
    return "pacing is most often " + ", ".join(parts) + " summaries"


def descriptor_summary(docs: Sequence[SummaryDoc], limit: int = 4) -> List[str]:
    counter = Counter()
    for doc in docs:
        counter.update(doc.descriptors)
    return [item for item, _ in counter.most_common(limit)]


def build_relationship_overview(
    talent_name: str,
    docs: Sequence[SummaryDoc],
    new_docs: Sequence[SummaryDoc],
    codes: Sequence[CodeResult],
    update_scope: str,
) -> str:
    if not docs:
        return "Insufficient summary evidence was available to build a cumulative overview."
    descriptors = descriptor_summary(docs)
    descriptor_text = ", ".join(descriptors[:3]) if descriptors else "mixed in tone"
    code_names = ", ".join(code.code_name for code in codes[:3]) if codes else "few stable recurring codes"
    first = (
        f"Across {len(docs)} incorporated summaries, {talent_name}'s text-visible relationship with chat is most often described as "
        f"{descriptor_text}. The summaries repeatedly frame the interaction around {code_names}, and {reciprocity_summary(docs)}."
    )
    second = (
        f"The cumulative picture suggests a streamer who uses text interaction to keep viewers socially present even when the stream format changes. "
        f"{pacing_summary(docs)}. Where the evidence is weaker, it is usually because summaries are formulaic or because one raw input class is missing for some videos."
    )
    if update_scope == "initial bootstrap":
        newest_note = "Because this is the initial bootstrap, the current run establishes the baseline cumulative picture rather than revising an earlier one."
    else:
        newly_strengthened = [code.code_name for code in codes if code.supporting_new_summary_ids]
        if any(code.new_this_run for code in codes):
            newest_note = (
                "The newest summaries materially broadened the cumulative picture by adding or clarifying "
                + ", ".join(code.code_name for code in codes if code.new_this_run)
                + "."
            )
        elif newly_strengthened:
            newest_note = "The newest summaries mostly reinforced prior themes, especially " + ", ".join(newly_strengthened[:3]) + "."
        else:
            newest_note = "The newest summaries mostly reinforced prior themes rather than changing the cumulative picture."
    paragraphs = [first, second, newest_note]
    return "\n\n".join(paragraphs)


def build_incremental_notes(
    docs: Sequence[SummaryDoc],
    new_docs: Sequence[SummaryDoc],
    codes: Sequence[CodeResult],
    weak_ids: List[str],
) -> str:
    retained = ", ".join(code.code_name for code in codes) if codes else "none retained"
    strengthened = [code.code_name for code in codes if code.supporting_new_summary_ids]
    newly_added = [code.code_name for code in codes if code.new_this_run]
    weak_note = ", ".join(weak_ids[:8]) if weak_ids else "none"
    lines = [
        f"- new summaries scanned this run: {len(new_docs)} ({', '.join(doc.video_id for doc in new_docs[:10]) or 'none'})",
        f"- cumulative summaries scanned: {len(docs)}",
        f"- recurring themes retained: {retained}",
        f"- newly added or newly strengthened themes this run: {', '.join(newly_added or strengthened or ['none'])}",
        f"- missing or pending raw-verification limitations: {weak_note}",
    ]
    return "\n".join(lines)


def format_examples(examples: Sequence[Example]) -> str:
    if not examples:
        return "- Verified examples: none available from raw CSVs."
    lines = ["- Verified examples:"]
    for example in examples:
        lines.append(
            f'  - [{example.timecode}] {example.speaker} ({example.video_id}): "{escape_md(example.quote)}"'
        )
    return "\n".join(lines)


def build_codebook(codes: Sequence[CodeResult]) -> str:
    if not codes:
        return "No recurring codes met the retention threshold with verified raw examples."
    chunks = []
    for code in codes:
        chunks.append(
            "\n".join(
                [
                    f"### {code.code_name}",
                    f"- `new_this_run: {'yes' if code.new_this_run else 'no'}`",
                    f"- definition: {code.definition}",
                    f"- inclusion_criteria: {code.inclusion_criteria}",
                    f"- exclusion_criteria: {code.exclusion_criteria}",
                    f"- observed_summary_count: {code.observed_summary_count}",
                    format_examples(code.examples),
                ]
            )
        )
    return "\n\n".join(chunks)


def build_pattern_section(codes: Sequence[CodeResult]) -> str:
    by_key = {code.key: code for code in codes}
    humor = by_key.get("humor_play")
    response = by_key.get("chat_attunement")
    pacing = by_key.get("pace_control")
    care = by_key.get("care_reassurance")
    boundary = by_key.get("boundary_setting")

    def sentence(code: Optional[CodeResult], fallback: str) -> str:
        if not code:
            return fallback
        return (
            f"{code.code_name.capitalize()} appears in {code.observed_summary_count} incorporated summaries, and the verified examples suggest that it is a stable interaction habit rather than a one-off moment."
        )

    care_boundary = sentence(care, "Care and reassurance are visible but not strongly enough repeated to retain as a separate code.") + " "
    if boundary:
        care_boundary += sentence(boundary, "Boundary setting is limited in the verified raw examples.")
    else:
        care_boundary += "Boundary-setting evidence is limited, so any firm limits should be treated as tentative rather than dominant."

    paragraphs = [
        f"**Humor/play style:** {sentence(humor, 'Humor is present, but the summaries do not describe a single dominant play style strongly enough to isolate beyond the broader cumulative overview.')}",
        f"**Response to chat:** {sentence(response, 'The summaries indicate recurring chat response, but raw-log examples are too thin to describe a stronger response pattern with confidence.')}",
        f"**Pacing/control:** {sentence(pacing, 'Pacing is discussed in the summaries, but the verified examples are too sparse to make a stronger claim about flow control.')}",
        f"**Care, reassurance, or boundary behavior:** {care_boundary}",
    ]
    return "\n\n".join(paragraphs)


def build_coverage_section(
    docs: Sequence[SummaryDoc],
    inventory: Dict[str, InventoryRow],
    raw_only_ids: List[str],
    analysis_ids: List[str],
    weak_ids: List[str],
) -> str:
    tag_counter = Counter()
    for doc in docs:
        tag_counter.update(doc.content_tags)
    common_tags = ", ".join(f"{tag} ({count})" for tag, count in tag_counter.most_common(5)) or "none"
    weak_detail = ", ".join(weak_ids[:10]) if weak_ids else "none"
    raw_only_detail = ", ".join(raw_only_ids[:10]) if raw_only_ids else "none visible from the current raw-file inventory"
    paragraphs = [
        f"The incorporated summary set is weighted toward these detected stream types: {common_tags}. Categories with very small counts should be treated as underrepresented rather than absent in the channel overall.",
        f"- no summary yet available: {len(raw_only_ids)} raw-video IDs appear in `text_playback` or `Chat/Original` without a matching `stream_summary_codex` markdown ({raw_only_detail})",
        f"- summary available but raw verification weak: {len(weak_ids)} incorporated summaries are missing one raw input class or have very thin raw support ({weak_detail})",
        f"- summary available and incorporated: {len(analysis_ids)} cumulative summary video_ids are currently incorporated",
    ]
    return "\n".join(paragraphs)


def build_limits_section(weak_ids: List[str]) -> str:
    lines = [
        "- This is a summary-driven workflow with raw-log verification, not a raw-log-first full recoding workflow.",
        "- This is text-only evidence and does not include visual cues or full vocal prosody.",
        "- Summary markdowns use partially standardized phrasing, so some stream-specific nuance may be compressed before classification.",
        f"- Raw verification is uneven for some incorporated summaries because one raw input class is missing or thin ({', '.join(weak_ids[:8]) or 'none noted'}).",
    ]
    return "\n".join(lines)


def build_markdown(
    talent_name: str,
    analysis_at: datetime,
    update_scope: str,
    source_version: str,
    source_label: str,
    docs: Sequence[SummaryDoc],
    new_docs: Sequence[SummaryDoc],
    codes: Sequence[CodeResult],
    inventory: Dict[str, InventoryRow],
    raw_only_ids: List[str],
    weak_ids: List[str],
) -> str:
    header = [
        f"Analysis conducted: {analysis_at.strftime('%Y-%m-%d %H:%M %Z')}",
        f"Update scope: {update_scope}",
        f"New summary video_ids incorporated this run: {len(new_docs)}",
        f"Cumulative summary video_ids incorporated: {len(docs)}",
        f"Source summary prompt version: {source_version}",
        f"Source summary prompt path/label: {source_label}",
    ]
    sections = [
        "## 1) Streamer and Chat Relationship Overview",
        build_relationship_overview(talent_name, docs, new_docs, codes, update_scope),
        "## 2) Incremental Summary Classification Notes",
        build_incremental_notes(docs, new_docs, codes, weak_ids),
        "## 3) Qualitative Codebook",
        build_codebook(codes),
        "## 4) Recurring Interaction Patterns",
        build_pattern_section(codes),
        "## 5) Coverage and Gaps",
        build_coverage_section(docs, inventory, raw_only_ids, [doc.video_id for doc in docs], weak_ids),
        "## 6) Evidence Limits and Uncertainty",
        build_limits_section(weak_ids),
    ]
    return "\n".join(header) + "\n\n" + "\n\n".join(sections) + "\n"


def build_snapshot_markdown(
    snapshot_at: datetime,
    current_markdown: str,
    prompt_text: str,
    source_version: str,
    source_label: str,
) -> str:
    lines = [
        f"# Summary Classification Snapshot",
        f"- Snapshot created: {snapshot_at.strftime('%Y-%m-%d %H:%M:%S %Z')}",
        f"- Snapshot ISO timestamp: `{snapshot_at.isoformat()}`",
        f"- Recorded source summary prompt version: `{source_version}`",
        f"- Recorded source summary prompt path/label: `{source_label}`",
        "",
        "## Current cumulative overall_themes_codex.md",
        "",
        current_markdown.strip(),
        "",
        "## Summary-classification prompt used for this run",
        "",
        "```md",
        prompt_text.rstrip(),
        "```",
        "",
    ]
    return "\n".join(lines)


def verify_example_line(example: Example) -> bool:
    try:
        with open(example.file_path, "r", encoding="utf-8-sig", newline="") as f:
            for line_no, line in enumerate(f, start=1):
                if line_no == example.file_line:
                    return example.quote[:40] in line
    except Exception:
        return False
    return False


def process_talent(talent_dir: Path, prompt_text: str) -> dict:
    talent_name = talent_dir.name
    tokens = derive_streamer_tokens(talent_name)
    inventory, raw_only_ids = build_inventory(talent_dir)
    all_summary_ids = sorted(inventory)

    current_dir = talent_dir / "stream_summaries" / "overall_themes" / "summary_classification" / "current"
    snapshots_dir = talent_dir / "stream_summaries" / "overall_themes" / "summary_classification" / "snapshots"
    markdown_path = current_dir / "overall_themes_codex.md"
    state_path = current_dir / "summary_classification_state.json"

    state = load_json(state_path)
    processed_prior = set(state.get("processed_summary_video_ids", []))
    new_ids = [video_id for video_id in all_summary_ids if video_id not in processed_prior]
    update_scope = "incremental update"
    if not state:
        update_scope = "initial bootstrap"
        analysis_ids = list(all_summary_ids)
    else:
        current_missing = not nonempty(markdown_path)
        if current_missing:
            analysis_ids = sorted(processed_prior | set(new_ids))
        else:
            if not new_ids and not FORCE_REFRESH:
                return {
                    "talent": talent_name,
                    "status": "skipped",
                    "reason": "no new summary video_ids and current outputs are already populated",
                }
            analysis_ids = sorted(processed_prior | set(new_ids))
    if not state and not analysis_ids:
        analysis_ids = []
    if not analysis_ids:
        return {
            "talent": talent_name,
            "status": "skipped",
            "reason": "no summary markdown files found",
        }

    summary_paths = [inventory[video_id].summary_path for video_id in analysis_ids if video_id in inventory]
    docs_map = load_summaries(summary_paths)
    docs = [docs_map[video_id] for video_id in analysis_ids if video_id in docs_map]
    new_docs = [docs_map[video_id] for video_id in new_ids if video_id in docs_map]

    ensure_row_counts(inventory, analysis_ids)
    playback_paths = [inventory[video_id].playback_path for video_id in analysis_ids if inventory[video_id].playback_path]
    chat_paths = [inventory[video_id].chat_path for video_id in analysis_ids if inventory[video_id].chat_path]
    playback_events = load_playback_events(playback_paths)
    chat_events = load_chat_events(chat_paths)
    events_by_video = dedupe_events(playback_events, chat_events)

    weak_ids = []
    for video_id in analysis_ids:
        row = inventory[video_id]
        if not row.playback_path or not row.chat_path or row.playback_rows < 3 or row.chat_rows < 3:
            weak_ids.append(video_id)

    source_version, source_label = aggregate_prompt_metadata(new_docs or docs)
    prior_code_names = state.get("retained_code_names", [])
    codes = select_codes(talent_name, tokens, docs_map, analysis_ids, new_ids, events_by_video, prior_code_names)

    analysis_at = datetime.now().astimezone()
    markdown = build_markdown(
        talent_name=talent_name,
        analysis_at=analysis_at,
        update_scope=update_scope,
        source_version=source_version,
        source_label=source_label,
        docs=docs,
        new_docs=new_docs,
        codes=codes,
        inventory=inventory,
        raw_only_ids=raw_only_ids,
        weak_ids=weak_ids,
    )
    write_text(markdown_path, markdown)

    snapshot_at = datetime.now().astimezone()
    snapshot_name = "overall_themes_codex_" + snapshot_at.strftime("%Y-%m-%d_%H-%M-%S_%z") + ".md"
    snapshot_path = snapshots_dir / snapshot_name
    snapshot_markdown = build_snapshot_markdown(snapshot_at, markdown, prompt_text, source_version, source_label)
    write_text(snapshot_path, snapshot_markdown)

    notes = [
        f"raw_only_video_ids_without_summary={len(raw_only_ids)}",
        f"summaries_with_weak_raw_verification={len(weak_ids)}",
        "source_summary_metadata=reliable" if source_version != "unknown" or source_label != "unknown" else "source_summary_metadata=unknown",
    ]
    state_out = {
        "talent": talent_name,
        "analysis_conducted_at": analysis_at.isoformat(),
        "update_scope": update_scope,
        "processed_summary_video_ids": analysis_ids,
        "new_summary_video_ids_this_run": new_ids,
        "cumulative_summaries_scanned": len(analysis_ids),
        "source_summary_prompt_version": source_version,
        "source_summary_prompt_path_or_label": source_label,
        "latest_snapshot_path": str(snapshot_path),
        "notes": notes,
        "retained_code_names": [code.code_name for code in codes],
    }
    write_json(state_path, state_out)

    spot_example = None
    for code in codes:
        for example in code.examples:
            if verify_example_line(example):
                spot_example = example
                break
        if spot_example:
            break

    return {
        "talent": talent_name,
        "status": "written",
        "new_summary_count": len(new_ids),
        "cumulative_summary_count": len(analysis_ids),
        "source_summary_prompt_version": source_version,
        "current_output_paths": [str(markdown_path), str(state_path)],
        "snapshot_path": str(snapshot_path),
        "spot_check": {
            "code_name": next((code.code_name for code in codes if spot_example and spot_example in code.examples), ""),
            "video_id": spot_example.video_id if spot_example else "",
            "quote": spot_example.quote if spot_example else "",
            "file_path": spot_example.file_path if spot_example else "",
            "file_line": spot_example.file_line if spot_example else "",
        },
    }


def print_completion(results: Sequence[dict]) -> None:
    for result in results:
        if result["status"] != "written":
            print(f"Talent: {result['talent']}")
            print(f"Status: {result['status']}")
            print(f"Reason: {result.get('reason', '')}")
            print()
            continue
        print(f"Talent: {result['talent']}")
        print(f"New summary count: {result['new_summary_count']}")
        print(f"Cumulative summary count: {result['cumulative_summary_count']}")
        print(f"Source summary prompt version: {result['source_summary_prompt_version']}")
        print("Current output paths:")
        for path in result["current_output_paths"]:
            print(f"  - {path}")
        print(f"Snapshot path: {result['snapshot_path']}")
        spot = result.get("spot_check") or {}
        if spot.get("file_path"):
            print(
                "Spot-check raw quote: "
                f"{spot.get('quote', '')} "
                f"({spot.get('video_id', '')}) -> {spot.get('file_path', '')}:{spot.get('file_line', '')}"
            )
        else:
            print("Spot-check raw quote: none verified")
        print()


def main() -> None:
    prompt_text = read_text(PROMPT_SPEC_PATH)
    results = []
    for talent_dir in discover_talents():
        results.append(process_talent(talent_dir, prompt_text))
    print_completion(results)


if __name__ == "__main__":
    main()
