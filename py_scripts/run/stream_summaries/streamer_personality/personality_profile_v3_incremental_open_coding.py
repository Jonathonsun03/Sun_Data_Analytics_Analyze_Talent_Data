#!/usr/bin/env python3
"""Incremental v3 open-coding personality profiling for discovered talent folders.

The workflow is stateful:
- bootstrap from all eligible streams when v3 state is missing
- skip talents already up to date
- process only newly eligible video_ids on incremental runs
- repair missing/empty v3 outputs by rebuilding from processed + new ids

Evidence is grounded only in:
- stream_summaries/stream_summary_codex/*.md (context only)
- text_playback/*.csv
- Chat/Original/*_chat.csv
- stream_summaries/overall_themes/money_timestamps.csv

Existing v3 outputs are used only as merge targets / continuity metadata,
never as primary evidence for analytic claims.
"""

from __future__ import annotations

import csv
import glob
import json
import math
import os
import re
import argparse
from collections import Counter, defaultdict
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, Iterable, List, Optional, Tuple


DATA_ROOT = "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data"
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, "..", "..", "..", ".."))
PROMPT_SPEC_PATH = os.path.join(REPO_ROOT, "prompts", "personality", "personality_open_coding.md")
AGGREGATE_TALENT_DIRS = {"VarianceProject"}
TOKEN_STOPWORDS = {
    "ch",
    "channel",
    "official",
    "project",
    "variance",
}
TALENT_TOKEN_OVERRIDES = {
    "Avaritia Hawthorne 【Variance Project】": ("avaritia", "ava"),
    "Katya Sable 【Variance Project】": ("katya", "sable"),
    "Leia Memoria【Variance Project】": ("leia", "memoria"),
    "Terberri Solaris Ch": ("terberri", "solar", "solaris"),
}

MIN_PRIMARY_ROWS_SINGLE_SOURCE = 8
MIN_PRIMARY_ROWS_ANY = 5
FORCE_REBUILD = os.environ.get("PERSONALITY_V3_FORCE_REBUILD") == "1"
FORCE_SCOPE_INITIAL = os.environ.get("PERSONALITY_V3_FORCE_SCOPE_INITIAL") == "1"
TALENT_SLUG = (os.environ.get("TALENT_SLUG") or "").strip()

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
    "had",
    "has",
    "have",
    "he",
    "her",
    "his",
    "i",
    "if",
    "im",
    "in",
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
    "she",
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

THANK_RE = re.compile(r"\b(thank(?:s| you)?|appreciate|grateful|tysm|ty)\b", re.I)
CARE_RE = re.compile(
    r"\b(are you okay|are you ok|it'?s okay|it'?s ok|take care|take your time|rest well|drink water|you got this|be safe|proud of you|no worries|don't worry)\b",
    re.I,
)
BOUNDARY_RE = re.compile(
    r"\b(no spoilers|no backseating|don'?t spam|do not spam|stop spamming|stop asking|don'?t ask|do not ask|not okay|we don'?t do that here|please stop|don'?t be weird)\b",
    re.I,
)
DIRECTIVE_RE = re.compile(
    r"\b(listen up|focus|mods|we'?re moving on|let'?s move on|next up|time to|starting now|keep it respectful|stay on topic|we are moving on)\b",
    re.I,
)
DISCLOSURE_RE = re.compile(
    r"\b(i feel|i'?m (?:tired|stressed|scared|anxious|worried|sad|overwhelmed)|i was (?:stressed|overwhelmed)|i'?ve been struggling|burnout|depress(?:ed|ing)?|anxiety|mental health|i cried|i need|i needed|i was broke|i was poor)\b",
    re.I,
)
THEATRICAL_RE = re.compile(
    r"\b(behold|welcome to|mortals|ladies and gentlemen|my dear|minions|ceremony|queen|goblin|blood|baby|darling)\b",
    re.I,
)
PLAY_RE = re.compile(
    r"\b(chaos|cursed|unhinged|gremlin|goblin|wild|wtf|what is happening|haha|lmao|lol|blood|eat you|kill|bite)\b",
    re.I,
)
LAUGH_RE = re.compile(r"\b(lol|lmao|haha|hehe|rofl|kek|www)\b", re.I)
TEASE_RE = re.compile(r"\b(nerd|bozo|clown|dummy|stinky|coward|skill issue|loser)\b", re.I)
DISTRESS_RE = re.compile(r"\b(scared|sad|anxious|stressed|overwhelmed|tired|confused)\b", re.I)
HYPE_RE = re.compile(r"\b(let'?s go|lets go|hype|pog|we got this|we can do this|woo+|yipee)\b", re.I)
REQUEST_RE = re.compile(r"\b(can you|could you|please|sing|react|play|read this|request)\b", re.I)
CELEBRATE_RE = re.compile(r"\b(congrats|congratulations|happy birthday|anniversary|celebrate|proud of)\b", re.I)
MONEY_RE = re.compile(r"\b(super ?chat|donation|membership|member|gift(?:ed)?|support|monetization)\b", re.I)

SEED_SUFFIX = {
    "ritual": "ritual",
    "care": "care",
    "guardrail": "guardrail",
    "pace": "pace",
    "disclosure": "disclosure",
    "bit": "bit",
    "sync": "loop",
    "money": "frame",
}

FAMILY_TEMPLATES = {
    "ritual": "{} recognition loops",
    "care": "{} reassurance stance",
    "guardrail": "{} guardrails",
    "pace": "{} pacing control",
    "disclosure": "{} self-disclosure windows",
    "bit": "{} bit escalation",
    "sync": "{} co-regulation loops",
    "money": "{} money framing",
}


@dataclass
class Event:
    talent: str
    video_id: str
    sec: float
    timecode: str
    source: str
    speaker: str
    text: str
    message_type: str
    file_path: str
    file_line: int


@dataclass
class VideoInventory:
    video_id: str
    summary_path: str
    playback_path: str
    chat_path: str
    playback_rows: int
    chat_rows: int
    total_rows: int
    eligible: bool
    pending_reason: str


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


def normalize_text(text: str) -> str:
    s = (text or "").lower().strip()
    s = re.sub(r"https?://\S+", "", s)
    s = re.sub(r"[^a-z0-9\s']", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def normalize_speaker(speaker: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", (speaker or "").lower().lstrip("@"))


def tokenize(text: str) -> List[str]:
    return [w for w in normalize_text(text).split() if w]


def talent_tokens_from_name(talent_name: str) -> Tuple[str, ...]:
    override = TALENT_TOKEN_OVERRIDES.get(talent_name)
    if override:
        return override

    words = [
        word
        for word in re.findall(r"[a-z0-9]+", talent_name.lower())
        if len(word) >= 3 and word not in TOKEN_STOPWORDS
    ]
    tokens = []
    seen = set()
    for token in words + (["".join(words)] if len(words) > 1 else []):
        if token and token not in seen:
            tokens.append(token)
            seen.add(token)
    return tuple(tokens)


def discover_talents(talent_filter: str = "") -> List[dict]:
    if not os.path.isdir(DATA_ROOT):
        return []

    talents = []
    for entry in sorted(os.listdir(DATA_ROOT)):
        if entry in AGGREGATE_TALENT_DIRS:
            continue
        if talent_filter and entry != talent_filter:
            continue
        talent_path = os.path.join(DATA_ROOT, entry)
        if not os.path.isdir(talent_path):
            continue

        primary_files = glob.glob(os.path.join(talent_path, "text_playback", "*.csv")) + glob.glob(
            os.path.join(talent_path, "Chat", "Original", "*_chat.csv")
        )
        summary_files = glob.glob(os.path.join(talent_path, "stream_summaries", "stream_summary_codex", "*.md"))
        if not primary_files and not summary_files:
            continue

        talents.append(
            {
                "name": entry,
                "path": talent_path,
                "tokens": talent_tokens_from_name(entry),
            }
        )
    return talents


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Incremental v3 open-coding personality profiling.")
    parser.add_argument(
        "--talent",
        dest="talent",
        default=TALENT_SLUG,
        help="Process only the exact talent folder name.",
    )
    return parser.parse_args()


def short_quote(text: str, limit: int = 240) -> str:
    t = (text or "").replace("\n", " ").strip()
    if len(t) <= limit:
        return t
    return t[: limit - 3] + "..."


def extract_video_id_from_name(path: str) -> str:
    base = os.path.basename(path)
    match = re.search(r"_([A-Za-z0-9_-]{11})(?:_summary\.md|_chat\.csv|\.csv)$", base)
    if match:
        return match.group(1)
    match = re.search(r"([A-Za-z0-9_-]{11})(?:_summary\.md|_chat\.csv|\.csv)$", base)
    if match:
        return match.group(1)
    return ""


def is_streamer(event: Event, tokens: Iterable[str]) -> bool:
    speaker = normalize_speaker(event.speaker)
    if speaker in {"stream", "host", "streamer"}:
        return True
    for token in tokens:
        if token in speaker:
            return True
    return False


def slug_from_phrase(phrase: str, max_words: int = 3) -> str:
    words = [w for w in tokenize(phrase) if w not in STOPWORDS]
    if not words:
        words = tokenize(phrase)
    if not words:
        return "marker"
    return " ".join(words[:max_words])


def code_label_from_anchor(anchor: str, seed: str) -> str:
    root = slug_from_phrase(anchor, max_words=2)
    return f"{root} {SEED_SUFFIX[seed]}".strip()


def family_name_from_codes(seed: str, code_labels: List[str]) -> str:
    roots = []
    for label in code_labels:
        toks = tokenize(label)
        if toks:
            roots.append(toks[0])
    top = Counter(roots).most_common(1)[0][0] if roots else "interaction"
    return FAMILY_TEMPLATES[seed].format(top.title())


def first_existing(values: List[str]) -> str:
    for value in values:
        if value:
            return value
    return ""


def load_state(path: str) -> dict:
    if not os.path.exists(path):
        return {}
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except Exception:
        return {}


def nonempty(path: str) -> bool:
    return os.path.exists(path) and os.path.getsize(path) > 0


def read_text(path: str) -> str:
    with open(path, "r", encoding="utf-8") as f:
        return f.read()


def write_text(path: str, text: str) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        f.write(text)


def build_output_paths(overall_dir: str) -> Dict[str, str]:
    personality_dir = os.path.join(overall_dir, "personality")
    personality_open_coding_v2_dir = os.path.join(overall_dir, "personality_open_coding", "v2")
    personality_open_coding_v3_dir = os.path.join(overall_dir, "personality_open_coding", "v3")
    personality_open_coding_v3_current_dir = os.path.join(personality_open_coding_v3_dir, "current")
    personality_open_coding_v3_snapshots_dir = os.path.join(personality_open_coding_v3_dir, "snapshots")
    return {
        "personality_dir": personality_dir,
        "personality_profile_path": os.path.join(personality_dir, "personality_profile_codex.md"),
        "personality_evidence_path": os.path.join(personality_dir, "personality_evidence_log.csv"),
        "personality_open_coding_v2_dir": personality_open_coding_v2_dir,
        "personality_open_coding_v2_profile_path": os.path.join(personality_open_coding_v2_dir, "personality_profile_v2_open_coding.md"),
        "personality_open_coding_v2_codebook_path": os.path.join(personality_open_coding_v2_dir, "open_codebook_v2.csv"),
        "personality_open_coding_v2_evidence_path": os.path.join(personality_open_coding_v2_dir, "open_coding_evidence_v2.csv"),
        "personality_open_coding_v3_dir": personality_open_coding_v3_dir,
        "personality_open_coding_v3_current_dir": personality_open_coding_v3_current_dir,
        "personality_open_coding_v3_snapshots_dir": personality_open_coding_v3_snapshots_dir,
        "profile_path": os.path.join(personality_open_coding_v3_current_dir, "personality_profile_v3_open_coding.md"),
        "codebook_path": os.path.join(personality_open_coding_v3_current_dir, "open_codebook_v3.csv"),
        "evidence_path": os.path.join(personality_open_coding_v3_current_dir, "open_coding_evidence_v3.csv"),
        "state_path": os.path.join(personality_open_coding_v3_current_dir, "personality_profile_v3_state.json"),
        "money_path": os.path.join(overall_dir, "money_timestamps.csv"),
    }


def migrate_overall_themes_outputs(overall_dir: str) -> Dict[str, str]:
    paths = build_output_paths(overall_dir)
    moves = {
        os.path.join(overall_dir, "personality_profile_codex.md"): paths["personality_profile_path"],
        os.path.join(overall_dir, "personality_evidence_log.csv"): paths["personality_evidence_path"],
        os.path.join(overall_dir, "personality_profile_v2_open_coding.md"): paths["personality_open_coding_v2_profile_path"],
        os.path.join(overall_dir, "open_codebook_v2.csv"): paths["personality_open_coding_v2_codebook_path"],
        os.path.join(overall_dir, "open_coding_evidence_v2.csv"): paths["personality_open_coding_v2_evidence_path"],
        os.path.join(overall_dir, "personality_profile_v3_open_coding.md"): paths["profile_path"],
        os.path.join(overall_dir, "open_codebook_v3.csv"): paths["codebook_path"],
        os.path.join(overall_dir, "open_coding_evidence_v3.csv"): paths["evidence_path"],
        os.path.join(overall_dir, "personality_profile_v3_state.json"): paths["state_path"],
        os.path.join(overall_dir, "open_coding", "v2", "personality_profile_v2_open_coding.md"): paths["personality_open_coding_v2_profile_path"],
        os.path.join(overall_dir, "open_coding", "v2", "open_codebook_v2.csv"): paths["personality_open_coding_v2_codebook_path"],
        os.path.join(overall_dir, "open_coding", "v2", "open_coding_evidence_v2.csv"): paths["personality_open_coding_v2_evidence_path"],
        os.path.join(overall_dir, "open_coding", "v3", "current", "personality_profile_v3_open_coding.md"): paths["profile_path"],
        os.path.join(overall_dir, "open_coding", "v3", "current", "open_codebook_v3.csv"): paths["codebook_path"],
        os.path.join(overall_dir, "open_coding", "v3", "current", "open_coding_evidence_v3.csv"): paths["evidence_path"],
        os.path.join(overall_dir, "open_coding", "v3", "current", "personality_profile_v3_state.json"): paths["state_path"],
    }
    for src, dst in moves.items():
        if os.path.exists(src) and not os.path.exists(dst):
            os.makedirs(os.path.dirname(dst), exist_ok=True)
            os.replace(src, dst)
    old_snapshot_dir = os.path.join(overall_dir, "open_coding", "v3", "snapshots")
    new_snapshot_dir = paths["personality_open_coding_v3_snapshots_dir"]
    if os.path.isdir(old_snapshot_dir):
        os.makedirs(new_snapshot_dir, exist_ok=True)
        for path in sorted(glob.glob(os.path.join(old_snapshot_dir, "*.md"))):
            dst = os.path.join(new_snapshot_dir, os.path.basename(path))
            if not os.path.exists(dst):
                os.replace(path, dst)
    return paths


def load_playback_events(talent_name: str, playback_files: List[str]) -> List[Event]:
    events: List[Event] = []
    for path in playback_files:
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
                source = (row.get("source") or "chat").strip().lower()
                if source not in {"chat", "subtitle"}:
                    source = "chat"
                video_id = (row.get("video_id") or "").strip() or fallback_video_id
                if not video_id:
                    continue
                events.append(
                    Event(
                        talent=talent_name,
                        video_id=video_id,
                        sec=sec,
                        timecode=(row.get("timecode") or sec_to_timecode(sec)).strip(),
                        source=source,
                        speaker=(row.get("speaker") or "").strip() or "UNKNOWN",
                        text=text,
                        message_type=(row.get("message_type") or "").strip(),
                        file_path=path,
                        file_line=line_no,
                    )
                )
    return events


def load_chat_events(talent_name: str, chat_files: List[str]) -> List[Event]:
    events: List[Event] = []
    for path in chat_files:
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
                video_id = (row.get("video_id") or "").strip() or fallback_video_id
                if not video_id:
                    continue
                events.append(
                    Event(
                        talent=talent_name,
                        video_id=video_id,
                        sec=sec,
                        timecode=sec_to_timecode(sec),
                        source="chat",
                        speaker=(row.get("username") or "").strip() or "UNKNOWN",
                        text=text,
                        message_type=(row.get("message_type") or "").strip(),
                        file_path=path,
                        file_line=line_no,
                    )
                )
    return events


def dedupe_and_group(playback_events: List[Event], chat_events: List[Event]) -> Tuple[List[Event], Dict[str, List[Event]]]:
    out: List[Event] = list(playback_events)
    seen = {
        (
            e.video_id,
            int(round(e.sec)),
            normalize_speaker(e.speaker),
            normalize_text(e.text),
            e.source,
        )
        for e in playback_events
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
        out.append(event)
    out.sort(key=lambda e: (e.video_id, e.sec, e.file_path, e.file_line))
    by_video: Dict[str, List[Event]] = defaultdict(list)
    for event in out:
        by_video[event.video_id].append(event)
    return out, by_video


def load_money_rows(path: str) -> List[dict]:
    if not os.path.exists(path):
        return []
    rows = []
    with open(path, "r", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        for line_no, row in enumerate(reader, start=2):
            sec = parse_float(row.get("time_in_seconds", ""))
            if math.isnan(sec):
                continue
            rows.append(
                {
                    "video_id": (row.get("video_id") or "").strip(),
                    "video_title": (row.get("video_title") or "").strip(),
                    "time_in_seconds": sec,
                    "timecode": (row.get("timecode") or sec_to_timecode(sec)).strip(),
                    "username": (row.get("username") or "").strip() or "UNKNOWN",
                    "message_type": (row.get("message_type") or "").strip(),
                    "paid_amount": (row.get("paid_amount") or "").strip(),
                    "paid_currency": (row.get("paid_currency") or "").strip(),
                    "message": (row.get("message") or "").strip(),
                    "file_path": path,
                    "file_line": line_no,
                }
            )
    rows.sort(key=lambda r: (r["video_id"], r["time_in_seconds"]))
    return rows


def sample_evenly(rows: List[dict], n: int) -> List[dict]:
    if len(rows) <= n:
        return list(rows)
    out = []
    for i in range(n):
        idx = int(round(i * (len(rows) - 1) / max(1, n - 1)))
        out.append(rows[idx])
    return out


def build_money_index(money_rows: List[dict]) -> Dict[str, List[float]]:
    by_video: Dict[str, List[float]] = defaultdict(list)
    for row in money_rows:
        by_video[row["video_id"]].append(row["time_in_seconds"])
    for video_id in by_video:
        by_video[video_id].sort()
    return by_video


def infer_monetary_context(video_id: str, sec: float, money_index: Dict[str, List[float]]) -> str:
    times = money_index.get(video_id, [])
    if not times:
        return "none"
    best = None
    for moment in times:
        delta = sec - moment
        if best is None or abs(delta) < abs(best):
            best = delta
    if best is None:
        return "none"
    if -120 <= best < -5:
        return "pre_event"
    if abs(best) <= 5:
        return "event"
    if 5 < best <= 120:
        return "post_event"
    return "none"


def discover_inventory(talent: dict, by_video: Dict[str, List[Event]]) -> Tuple[Dict[str, VideoInventory], List[str], List[str], List[str]]:
    base = talent["path"]
    summary_files = sorted(glob.glob(os.path.join(base, "stream_summaries", "stream_summary_codex", "*.md")))
    playback_files = sorted(glob.glob(os.path.join(base, "text_playback", "*.csv")))
    chat_files = sorted(glob.glob(os.path.join(base, "Chat", "Original", "*_chat.csv")))

    summary_by_id = {}
    playback_by_id = {}
    chat_by_id = {}
    for path in summary_files:
        video_id = extract_video_id_from_name(path)
        if video_id:
            summary_by_id[video_id] = path
    for path in playback_files:
        video_id = extract_video_id_from_name(path)
        if video_id and video_id not in playback_by_id:
            playback_by_id[video_id] = path
    for path in chat_files:
        video_id = extract_video_id_from_name(path)
        if video_id and video_id not in chat_by_id:
            chat_by_id[video_id] = path

    all_ids = sorted(set(summary_by_id) | set(playback_by_id) | set(chat_by_id) | set(by_video))
    inventory = {}
    for video_id in all_ids:
        events = by_video.get(video_id, [])
        playback_rows = sum(1 for e in events if e.source == "subtitle")
        chat_rows = sum(1 for e in events if e.source == "chat")
        total_rows = len(events)
        eligible = False
        pending_reason = ""
        if total_rows < MIN_PRIMARY_ROWS_ANY:
            pending_reason = "too few primary evidence rows"
        elif not (playback_by_id.get(video_id) or chat_by_id.get(video_id)):
            pending_reason = "summary-only discovery with no primary evidence file"
        elif playback_by_id.get(video_id) and chat_by_id.get(video_id):
            eligible = True
        elif total_rows >= MIN_PRIMARY_ROWS_SINGLE_SOURCE:
            eligible = True
        else:
            pending_reason = "single-source evidence below sufficiency threshold"
        inventory[video_id] = VideoInventory(
            video_id=video_id,
            summary_path=summary_by_id.get(video_id, ""),
            playback_path=playback_by_id.get(video_id, ""),
            chat_path=chat_by_id.get(video_id, ""),
            playback_rows=playback_rows,
            chat_rows=chat_rows,
            total_rows=total_rows,
            eligible=eligible,
            pending_reason=pending_reason,
        )
    return inventory, summary_files, playback_files, chat_files


def seed_anchor(text: str, seed: str) -> Optional[str]:
    patterns = {
        "ritual": THANK_RE,
        "care": CARE_RE,
        "guardrail": BOUNDARY_RE,
        "pace": DIRECTIVE_RE,
        "disclosure": DISCLOSURE_RE,
        "bit": THEATRICAL_RE,
        "sync": HYPE_RE,
        "money": MONEY_RE,
    }
    match = patterns[seed].search(text)
    if match:
        return match.group(0)
    return None


def phase1_stream_level_open_coding(talent: dict, by_video: Dict[str, List[Event]]) -> Tuple[List[dict], List[Event], List[str]]:
    raw_hits: List[dict] = []
    streamer_events: List[Event] = []
    memos: List[str] = []
    seen = set()
    tokens = talent["tokens"]

    def push(code_label: str, seed: str, event: Event, role: str, memo: str) -> None:
        key = (
            code_label,
            seed,
            event.video_id,
            round(event.sec, 3),
            normalize_speaker(event.speaker),
            normalize_text(event.text),
            role,
        )
        if key in seen:
            return
        seen.add(key)
        raw_hits.append(
            {
                "talent": talent["name"],
                "raw_label": code_label,
                "seed": seed,
                "video_id": event.video_id,
                "sec": event.sec,
                "timecode": event.timecode or sec_to_timecode(event.sec),
                "source": event.source,
                "speaker": event.speaker,
                "quote": event.text,
                "evidence_role": role,
                "file_path": event.file_path,
                "file_line": event.file_line,
                "memo": memo,
            }
        )

    for video_id, rows in by_video.items():
        video_codes = 0
        for idx, event in enumerate(rows):
            text = event.text
            if not normalize_text(text):
                continue
            streamer = is_streamer(event, tokens)
            if streamer:
                streamer_events.append(event)
            nearby = [x for x in rows[max(0, idx - 6) : min(len(rows), idx + 7)] if x is not event and abs(x.sec - event.sec) <= 45]

            if streamer and THANK_RE.search(text):
                push(code_label_from_anchor(THANK_RE.search(text).group(0), "ritual"), "ritual", event, "primary", "gratitude/recognition language")
                video_codes += 1
            if streamer and CARE_RE.search(text):
                push(code_label_from_anchor(CARE_RE.search(text).group(0), "care"), "care", event, "primary", "reassurance/caretaking language")
                video_codes += 1
            if streamer and BOUNDARY_RE.search(text):
                push(code_label_from_anchor(BOUNDARY_RE.search(text).group(0), "guardrail"), "guardrail", event, "primary", "explicit limit-setting")
                video_codes += 1
            if streamer and DIRECTIVE_RE.search(text):
                push(code_label_from_anchor(DIRECTIVE_RE.search(text).group(0), "pace"), "pace", event, "primary", "agenda/pacing control")
                video_codes += 1
            if streamer and DISCLOSURE_RE.search(text):
                push(code_label_from_anchor(DISCLOSURE_RE.search(text).group(0), "disclosure"), "disclosure", event, "primary", "first-person personal disclosure")
                video_codes += 1
            if streamer and (THEATRICAL_RE.search(text) or (PLAY_RE.search(text) and any(LAUGH_RE.search(x.text) or PLAY_RE.search(x.text) for x in nearby))):
                anchor = THEATRICAL_RE.search(text) or PLAY_RE.search(text)
                push(code_label_from_anchor(anchor.group(0), "bit"), "bit", event, "primary", "dramatic/absurd performative framing")
                video_codes += 1
            if TEASE_RE.search(text) and (LAUGH_RE.search(text) or any(LAUGH_RE.search(x.text) for x in nearby)):
                push(code_label_from_anchor(TEASE_RE.search(text).group(0), "bit"), "bit", event, "primary", "teasing with affiliative uptake")
                video_codes += 1

            if not streamer and REQUEST_RE.search(text):
                for later in rows[idx + 1 : idx + 12]:
                    if later.sec - event.sec > 45:
                        break
                    if is_streamer(later, tokens):
                        label = code_label_from_anchor("request", "pace")
                        push(label, "pace", later, "primary", "chat request followed by host uptake")
                        push(label, "pace", event, "supporting", "originating chat request")
                        video_codes += 1
                        break

            if not streamer and (DISTRESS_RE.search(text) or HYPE_RE.search(text)):
                for later in rows[idx + 1 : idx + 10]:
                    if later.sec - event.sec > 35:
                        break
                    if not is_streamer(later, tokens):
                        continue
                    if CARE_RE.search(later.text) or HYPE_RE.search(later.text) or LAUGH_RE.search(later.text):
                        anchor = CARE_RE.search(later.text) or HYPE_RE.search(later.text) or LAUGH_RE.search(later.text)
                        label = code_label_from_anchor(anchor.group(0), "sync")
                        push(label, "sync", later, "primary", "chat affect followed by host modulation")
                        push(label, "sync", event, "supporting", "chat affect cue")
                        video_codes += 1
                        break

        memos.append(f"{video_id}: stream-level open coding produced {video_codes} raw coded hits from interaction windows.")
    return raw_hits, streamer_events, memos


def phase1_money_probe_open_coding(talent: dict, by_video: Dict[str, List[Event]], money_rows: List[dict]) -> Tuple[List[dict], List[dict]]:
    sampled = sample_evenly(money_rows, 20)
    hits: List[dict] = []
    sample_log: List[dict] = []
    seen = set()
    tokens = talent["tokens"]

    def push(code_label: str, event_like: dict, role: str, memo: str) -> None:
        key = (
            code_label,
            event_like["video_id"],
            round(event_like["sec"], 3),
            normalize_speaker(event_like["speaker"]),
            normalize_text(event_like["quote"]),
            role,
        )
        if key in seen:
            return
        seen.add(key)
        hits.append(
            {
                "talent": talent["name"],
                "raw_label": code_label,
                "seed": "money",
                "video_id": event_like["video_id"],
                "sec": event_like["sec"],
                "timecode": event_like["timecode"],
                "source": event_like["source"],
                "speaker": event_like["speaker"],
                "quote": event_like["quote"],
                "evidence_role": role,
                "file_path": event_like["file_path"],
                "file_line": event_like["file_line"],
                "memo": memo,
            }
        )

    for row in sampled:
        video_id = row["video_id"]
        sec = row["time_in_seconds"]
        message = row["message"] or ""
        label_anchor = "money moment"
        note = "insufficient evidence"
        if CELEBRATE_RE.search(message):
            label_anchor = CELEBRATE_RE.search(message).group(0)
            note = "support framed as celebration"
        elif REQUEST_RE.search(message):
            label_anchor = "request"
            note = "support framed as request leverage"
        elif CARE_RE.search(message):
            label_anchor = CARE_RE.search(message).group(0)
            note = "support framed as care"
        elif PLAY_RE.search(message) or LAUGH_RE.search(message):
            label_anchor = "joke support"
            note = "support framed as play"
        elif MONEY_RE.search(message):
            label_anchor = MONEY_RE.search(message).group(0)
            note = "money explicitly named"
        label = code_label_from_anchor(label_anchor, "money")
        push(
            label,
            {
                "video_id": video_id,
                "sec": sec,
                "timecode": row["timecode"],
                "source": "chat",
                "speaker": row["username"],
                "quote": message or f"paid event {row['paid_amount']}",
                "file_path": row["file_path"],
                "file_line": row["file_line"],
            },
            "primary",
            note,
        )
        post_rows = [e for e in by_video.get(video_id, []) if sec <= e.sec <= sec + 90 and is_streamer(e, tokens)]
        if post_rows:
            response = post_rows[0]
            push(
                label,
                {
                    "video_id": response.video_id,
                    "sec": response.sec,
                    "timecode": response.timecode,
                    "source": response.source,
                    "speaker": response.speaker,
                    "quote": response.text,
                    "file_path": response.file_path,
                    "file_line": response.file_line,
                },
                "supporting",
                f"post-event streamer response: {note}",
            )
        sample_log.append(
            {
                "video_id": video_id,
                "time_in_seconds": sec,
                "timecode": row["timecode"],
                "username": row["username"],
                "message": message,
                "probe_label": note,
            }
        )
    return hits, sample_log


def canonicalize_label(raw_label: str, seed: str) -> str:
    root = slug_from_phrase(raw_label, max_words=3)
    suffix = SEED_SUFFIX[seed]
    if root == suffix or root.endswith(f" {suffix}"):
        return root
    return f"{root} {suffix}".strip()


def phase2_constant_comparison(raw_hits: List[dict], existing_labels: List[str]) -> Tuple[Dict[str, List[dict]], dict, List[str]]:
    merged: Dict[str, List[dict]] = defaultdict(list)
    notes: List[str] = []
    raw_unique = len({row["raw_label"] for row in raw_hits})

    existing_index = defaultdict(list)
    for label in existing_labels:
        toks = tuple(sorted(set(tokenize(label))))
        existing_index[toks].append(label)

    for row in raw_hits:
        canonical = canonicalize_label(row["raw_label"], row["seed"])
        best = canonical
        best_score = 0.0
        new_tokens = set(tokenize(canonical))
        for existing in existing_labels:
            old_tokens = set(tokenize(existing))
            if not old_tokens or not new_tokens:
                continue
            overlap = len(new_tokens & old_tokens) / len(new_tokens | old_tokens)
            if overlap > best_score:
                best_score = overlap
                best = existing
        if best_score >= 0.5:
            if best != canonical:
                notes.append(f"Merged new code variant '{canonical}' into existing cumulative code '{best}'.")
            canonical = best
        row2 = dict(row)
        row2["canonical_label"] = canonical
        merged[canonical].append(row2)

    split_groups: Dict[str, List[dict]] = defaultdict(list)
    split_count = 0
    for label, rows in merged.items():
        raw_counts = Counter(row["raw_label"] for row in rows)
        if len(rows) >= 140 and len(raw_counts) >= 5:
            top = raw_counts.most_common(2)
            if len(top) == 2 and top[1][1] >= max(20, int(0.18 * len(rows))):
                split_count += 1
                seed = rows[0]["seed"]
                keep = {top[0][0], top[1][0]}
                for raw_name in keep:
                    new_label = canonicalize_label(raw_name, seed)
                    split_groups[new_label].extend([row for row in rows if row["raw_label"] == raw_name])
                split_groups[label].extend([row for row in rows if row["raw_label"] not in keep])
                notes.append(f"Split broad code '{label}' using dominant in-vivo variants '{top[0][0]}' and '{top[1][0]}'.")
            else:
                split_groups[label].extend(rows)
        else:
            split_groups[label].extend(rows)

    stats = {
        "raw_codes_generated": raw_unique,
        "merged_codes": len(merged),
        "split_operations": split_count,
    }
    return {k: v for k, v in split_groups.items() if v}, stats, notes


def retain_codes(groups: Dict[str, List[dict]]) -> Tuple[Dict[str, dict], List[str]]:
    kept = {}
    notes = []
    dropped = []
    for label, rows in groups.items():
        primary = [row for row in rows if row["evidence_role"] == "primary"]
        primary_count = len(primary)
        coverage = len({row["video_id"] for row in primary})
        seed = Counter(row["seed"] for row in rows).most_common(1)[0][0]
        concept_important = seed in {"guardrail", "disclosure", "sync", "money"}
        keep = (primary_count >= 6 and coverage >= 2) or (concept_important and primary_count >= 2)
        if keep:
            kept[label] = {
                "label": label,
                "seed": seed,
                "rows": rows,
                "primary_count": primary_count,
                "stream_coverage_count": coverage,
            }
            if concept_important and primary_count < 6:
                notes.append(f"Retained low-frequency but conceptually important code '{label}' (n={primary_count}).")
        else:
            dropped.append((label, primary_count))

    if len(kept) < 6:
        for label, _ in sorted(dropped, key=lambda item: item[1], reverse=True):
            if label in kept:
                continue
            rows = groups[label]
            primary = [row for row in rows if row["evidence_role"] == "primary"]
            kept[label] = {
                "label": label,
                "seed": Counter(row["seed"] for row in rows).most_common(1)[0][0],
                "rows": rows,
                "primary_count": len(primary),
                "stream_coverage_count": len({row["video_id"] for row in primary}),
            }
            notes.append(f"Retained lower-frequency code '{label}' to preserve talent-specific nuance.")
            if len(kept) >= 6:
                break
    return kept, notes


def build_theme_families(retained_codes: Dict[str, dict], existing_theme_map: Dict[str, str]) -> Tuple[Dict[str, str], Dict[str, dict]]:
    label_to_family = {}
    family_meta: Dict[str, dict] = defaultdict(lambda: {"codes": [], "primary_total": 0})
    by_seed: Dict[str, List[str]] = defaultdict(list)

    for label in retained_codes:
        if label in existing_theme_map:
            family = existing_theme_map[label]
            label_to_family[label] = family
            family_meta[family]["codes"].append(label)
            family_meta[family]["primary_total"] += retained_codes[label]["primary_count"]
        else:
            by_seed[retained_codes[label]["seed"]].append(label)

    for seed, labels in by_seed.items():
        family = family_name_from_codes(seed, labels)
        for label in labels:
            label_to_family[label] = family
            family_meta[family]["codes"].append(label)
            family_meta[family]["primary_total"] += retained_codes[label]["primary_count"]
    return label_to_family, dict(family_meta)


def top_examples(rows: List[dict], n: int = 3, prefer_new: bool = False) -> List[dict]:
    ordered = list(rows)
    if prefer_new:
        ordered.sort(key=lambda row: (row.get("incorporated_in_run") != "yes", row["video_id"], float(row["time_in_seconds"])))
    else:
        ordered.sort(key=lambda row: (row["video_id"], float(row["time_in_seconds"])))
    out = []
    seen_video = set()
    for row in ordered:
        if row["video_id"] in seen_video:
            continue
        out.append(row)
        seen_video.add(row["video_id"])
        if len(out) >= n:
            return out
    for row in ordered:
        if len(out) >= n:
            break
        if row not in out:
            out.append(row)
    return out


def inclusion_exclusion_text(seed: str, markers: List[str]) -> Tuple[str, str, str]:
    marker_text = ", ".join(markers[:3]) if markers else "observed interaction markers"
    if seed == "ritual":
        return (
            f"Repeated recognition or gratitude turns anchored by in-vivo phrases such as {marker_text}.",
            f"Include explicit appreciation, name-recognition, or repeated thank-you routines ({marker_text}).",
            "Exclude one-off courtesy that does not recur as a visible interactional pattern.",
        )
    if seed == "care":
        return (
            f"Direct reassurance, soothing, or caretaking stance visible in text patterns such as {marker_text}.",
            f"Include check-ins, calming language, or protective encouragement ({marker_text}).",
            "Exclude neutral logistics without an emotional support component.",
        )
    if seed in {"guardrail", "pace"}:
        return (
            f"Host control over rules, topic boundaries, or pacing through language such as {marker_text}.",
            f"Include clear limits, directives, request-routing, or flow-control statements ({marker_text}).",
            "Exclude loose suggestions without a visible norm-setting or pacing function.",
        )
    if seed == "disclosure":
        return (
            f"Self-revelation with personal stake, using first-person vulnerability markers such as {marker_text}.",
            f"Include admissions of stress, difficulty, need, or emotionally consequential context ({marker_text}).",
            "Exclude generic status updates lacking personal vulnerability or reflection.",
        )
    if seed == "sync":
        return (
            f"Mutual affect modulation between chat and streamer, anchored by markers like {marker_text}.",
            f"Require a chat affect cue and a short-window host response that grounds, amplifies, or redirects it ({marker_text}).",
            "Exclude isolated hype or distress lines that do not form a visible loop.",
        )
    if seed == "money":
        return (
            f"Interpretive framing of paid moments, often using cues such as {marker_text}.",
            f"Include paid-event text plus social meaning such as celebration, request, care, or joking obligation ({marker_text}).",
            "Exclude amount-only mentions without relational interpretation.",
        )
    return (
        f"Playful or theatrical escalation anchored by phrases such as {marker_text}.",
        f"Include absurd bits, dramatic address, teasing, or roleplay-flavored text ({marker_text}).",
        "Exclude plain commentary with no visible performative or playful frame.",
    )


def verify_quote_in_source(path: str, quote: str) -> bool:
    if not path or not quote or not os.path.exists(path):
        return False
    try:
        with open(path, "r", encoding="utf-8-sig", errors="ignore") as f:
            for line in f:
                if quote in line:
                    return True
    except Exception:
        return False
    return False


def build_evidence_rows(
    talent_name: str,
    retained_codes: Dict[str, dict],
    label_to_family: Dict[str, str],
    money_index: Dict[str, List[float]],
    incorporated_in_run: str,
) -> List[dict]:
    rows = []
    for label, meta in retained_codes.items():
        family = label_to_family[label]
        for row in meta["rows"]:
            rows.append(
                {
                    "talent": talent_name,
                    "code_label": label,
                    "theme_family": family,
                    "video_id": row["video_id"],
                    "time_in_seconds": f"{row['sec']:.3f}",
                    "timecode": row["timecode"],
                    "source": row["source"],
                    "speaker": row["speaker"],
                    "quote": row["quote"],
                    "evidence_role": row["evidence_role"],
                    "monetary_context": infer_monetary_context(row["video_id"], row["sec"], money_index),
                    "incorporated_in_run": incorporated_in_run,
                    "file_path": row["file_path"],
                    "file_line": row["file_line"],
                }
            )
    return rows


def add_counterexamples(
    talent_name: str,
    all_events: List[Event],
    code_meta: Dict[str, dict],
    label_to_family: Dict[str, str],
    money_index: Dict[str, List[float]],
    already_have: List[dict],
    incorporated_in_run: str,
) -> Tuple[List[dict], Dict[str, List[dict]]]:
    rows = []
    by_family: Dict[str, List[dict]] = defaultdict(list)
    coded_keys = defaultdict(set)
    family_label = {}
    for label, meta in code_meta.items():
        family = label_to_family[label]
        family_label.setdefault(family, label)
        for row in meta.get("rows", []):
            coded_keys[family].add((row["video_id"], round(row["sec"], 3), normalize_speaker(row["speaker"]), normalize_text(row["quote"])))
    for row in already_have:
        if row["evidence_role"] == "counterexample":
            by_family[row["theme_family"]].append(row)

    for family, label in family_label.items():
        need = max(0, 2 - len(by_family.get(family, [])))
        if need == 0:
            continue
        for event in all_events:
            key = (event.video_id, round(event.sec, 3), normalize_speaker(event.speaker), normalize_text(event.text))
            if key in coded_keys[family]:
                continue
            if len(normalize_text(event.text)) < 15:
                continue
            row = {
                "talent": talent_name,
                "code_label": label,
                "theme_family": family,
                "video_id": event.video_id,
                "time_in_seconds": f"{event.sec:.3f}",
                "timecode": event.timecode or sec_to_timecode(event.sec),
                "source": event.source,
                "speaker": event.speaker,
                "quote": event.text,
                "evidence_role": "counterexample",
                "monetary_context": infer_monetary_context(event.video_id, event.sec, money_index),
                "incorporated_in_run": incorporated_in_run,
                "file_path": event.file_path,
                "file_line": event.file_line,
            }
            rows.append(row)
            by_family[family].append(row)
            need -= 1
            if need == 0:
                break
    return rows, by_family


def load_existing_codebook(path: str) -> List[dict]:
    if not nonempty(path):
        return []
    with open(path, "r", encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def load_existing_evidence(path: str) -> List[dict]:
    if not nonempty(path):
        return []
    rows = []
    with open(path, "r", encoding="utf-8-sig", newline="") as f:
        for row in csv.DictReader(f):
            row["file_path"] = row.get("file_path", "")
            row["file_line"] = row.get("file_line", "")
            rows.append(row)
    return rows


def aggregate_cumulative_codebook(
    talent_name: str,
    combined_evidence: List[dict],
    base_meta: Dict[str, dict],
    new_code_meta: Dict[str, dict],
) -> Tuple[List[dict], Dict[str, dict], Dict[str, str]]:
    grouped: Dict[str, List[dict]] = defaultdict(list)
    for row in combined_evidence:
        grouped[row["code_label"]].append(row)

    label_to_family = {}
    code_meta = {}
    rows_out = []
    for label, rows in sorted(grouped.items()):
        family = rows[0]["theme_family"]
        label_to_family[label] = family
        primary = [row for row in rows if row["evidence_role"] == "primary"]
        seed = None
        if label in new_code_meta:
            seed = new_code_meta[label]["seed"]
        elif label in base_meta:
            seed = base_meta[label].get("seed")
        if not seed:
            family_text = family.lower()
            if "recognition" in family_text:
                seed = "ritual"
            elif "reassurance" in family_text:
                seed = "care"
            elif "guardrail" in family_text:
                seed = "guardrail"
            elif "pacing" in family_text:
                seed = "pace"
            elif "self-disclosure" in family_text:
                seed = "disclosure"
            elif "co-regulation" in family_text:
                seed = "sync"
            elif "money" in family_text:
                seed = "money"
            else:
                seed = "bit"
        markers = []
        if label in new_code_meta:
            markers = [item[0] for item in Counter(r["raw_label"] for r in new_code_meta[label]["rows"]).most_common(4)]
        meta_source = base_meta.get(label, {})
        operational_definition = meta_source.get("operational_definition", "")
        inclusion_criteria = meta_source.get("inclusion_criteria", "")
        exclusion_criteria = meta_source.get("exclusion_criteria", "")
        if not operational_definition:
            operational_definition, inclusion_criteria, exclusion_criteria = inclusion_exclusion_text(seed, markers)
        new_this_run = "yes" if label not in base_meta else "no"
        memo_notes = meta_source.get("memo_notes", "")
        if label in new_code_meta and new_code_meta[label].get("memo_notes"):
            memo_notes = " | ".join([memo_notes, new_code_meta[label]["memo_notes"]]).strip(" |")
        rows_out.append(
            {
                "talent": talent_name,
                "code_label": label,
                "theme_family": family,
                "operational_definition": operational_definition,
                "inclusion_criteria": inclusion_criteria,
                "exclusion_criteria": exclusion_criteria,
                "frequency_count": len(primary),
                "stream_coverage_count": len({row["video_id"] for row in primary}),
                "new_this_run": new_this_run,
                "memo_notes": memo_notes,
            }
        )
        code_meta[label] = {
            "label": label,
            "seed": seed,
            "rows": rows,
            "primary_count": len(primary),
            "stream_coverage_count": len({row["video_id"] for row in primary}),
        }
    rows_out.sort(key=lambda row: (-int(row["frequency_count"]), row["code_label"]))
    return rows_out, code_meta, label_to_family


def build_base_meta(existing_codebook: List[dict]) -> Dict[str, dict]:
    meta = {}
    for row in existing_codebook:
        meta[row["code_label"]] = dict(row)
    return meta


def build_new_codebook_meta(
    retained_codes: Dict[str, dict],
    label_to_family: Dict[str, str],
    notes: List[str],
) -> Dict[str, dict]:
    out = {}
    note_text = " | ".join(notes[:8])
    for label, meta in retained_codes.items():
        markers = [item[0] for item in Counter(r["raw_label"] for r in meta["rows"]).most_common(4)]
        op, inc, exc = inclusion_exclusion_text(meta["seed"], markers)
        out[label] = {
            "seed": meta["seed"],
            "rows": meta["rows"],
            "operational_definition": op,
            "inclusion_criteria": inc,
            "exclusion_criteria": exc,
            "memo_notes": note_text,
            "theme_family": label_to_family[label],
        }
    return out


def build_phrase_counter(events: List[Event]) -> Counter:
    counter = Counter()
    for event in events:
        words = [w for w in tokenize(event.text) if w not in STOPWORDS]
        if len(words) < 3:
            continue
        for n in (2, 3):
            for idx in range(0, len(words) - n + 1):
                phrase = " ".join(words[idx : idx + n])
                if any(ch.isdigit() for ch in phrase):
                    continue
                counter[phrase] += 1
    return counter


def choose_idiosyncratic_markers(talent_name: str, streamer_events: List[Event], all_phrase_counters: Dict[str, Counter]) -> List[dict]:
    own = all_phrase_counters.get(talent_name, Counter())
    others = [name for name in all_phrase_counters if name != talent_name]
    phrase_rows: Dict[str, List[Event]] = defaultdict(list)
    phrase_videos: Dict[str, set] = defaultdict(set)
    for event in streamer_events:
        words = [w for w in tokenize(event.text) if w not in STOPWORDS]
        for n in (2, 3):
            for idx in range(0, len(words) - n + 1):
                phrase = " ".join(words[idx : idx + n])
                phrase_rows[phrase].append(event)
                phrase_videos[phrase].add(event.video_id)
    scored = []
    for phrase, own_count in own.items():
        if own_count < 4 or len(phrase_videos.get(phrase, set())) < 2:
            continue
        other_avg = sum(all_phrase_counters[other].get(phrase, 0) for other in others) / max(1, len(others))
        score = (own_count * len(phrase_videos[phrase])) / (1 + other_avg)
        scored.append((score, phrase))
    scored.sort(reverse=True)
    out = []
    used = set()
    for _, phrase in scored:
        if any(phrase in seen or seen in phrase for seen in used):
            continue
        examples = []
        seen_videos = set()
        for event in phrase_rows[phrase]:
            if event.video_id in seen_videos:
                continue
            examples.append(
                {
                    "video_id": event.video_id,
                    "timecode": event.timecode,
                    "speaker": event.speaker,
                    "quote": short_quote(event.text),
                }
            )
            seen_videos.add(event.video_id)
            if len(examples) >= 3:
                break
        if not examples:
            continue
        out.append(
            {
                "marker": phrase,
                "why": f"Observed {len(phrase_rows[phrase])} times across {len(phrase_videos[phrase])} streams with lower cross-talent prevalence.",
                "examples": examples,
            }
        )
        used.add(phrase)
        if len(out) >= 4:
            break
    return out


def family_distribution(evidence_rows: List[dict]) -> Dict[str, int]:
    counts = Counter()
    for row in evidence_rows:
        if row["evidence_role"] == "primary":
            counts[row["theme_family"]] += 1
    return dict(counts)


def money_pattern_summary(sampled_money: List[dict], evidence_rows: List[dict], current_run_ids: List[str]) -> Tuple[List[str], List[str]]:
    by_video: Dict[str, List[dict]] = defaultdict(list)
    for row in evidence_rows:
        if row["evidence_role"] != "primary":
            continue
        by_video[row["video_id"]].append(row)
    for video_id in by_video:
        by_video[video_id].sort(key=lambda row: float(row["time_in_seconds"]))

    current_notes = []
    cumulative_counts = Counter()
    for row in evidence_rows:
        if row["monetary_context"] != "none" and row["evidence_role"] == "primary":
            cumulative_counts[row["theme_family"]] += 1

    for item in sampled_money[:5]:
        video_id = item["video_id"]
        sec = item["time_in_seconds"]
        rows = by_video.get(video_id, [])
        pre = Counter(r["theme_family"] for r in rows if sec - 90 <= float(r["time_in_seconds"]) < sec - 5)
        post = Counter(r["theme_family"] for r in rows if sec <= float(r["time_in_seconds"]) <= sec + 90)
        if post:
            post_family = post.most_common(1)[0][0]
            current_notes.append(f"[{item['timecode']}] {item['username']} ({video_id}) most strongly surfaces `{post_family}` after the paid moment.")
        else:
            current_notes.append(f"[{item['timecode']}] {item['username']} ({video_id}) has insufficient nearby coded response evidence.")
    if not current_notes:
        current_notes.append("No paid-event sample available in this run.")

    cumulative_notes = []
    if cumulative_counts:
        cumulative_notes.append("Cumulative money-linked patterns most often co-occur with " + ", ".join(f"`{family}` ({count})" for family, count in cumulative_counts.most_common(3)) + ".")
    else:
        cumulative_notes.append("No stable cumulative money-linked pattern is observable from available evidence.")
    return current_notes, cumulative_notes


def build_cross_talent_text(talent_name: str, family_dist_all: Dict[str, Dict[str, int]], run_scope: str) -> List[str]:
    me = family_dist_all.get(talent_name, {})
    others = [name for name in family_dist_all if name != talent_name]
    families = sorted({family for dist in family_dist_all.values() for family in dist})

    def norm(dist: Dict[str, int]) -> Dict[str, float]:
        total = sum(dist.values()) or 1
        return {family: dist.get(family, 0) / total for family in families}

    me_norm = norm(me)
    others_norm = {other: norm(family_dist_all[other]) for other in others}
    nearest = sorted(
        others,
        key=lambda other: sum(abs(me_norm[family] - others_norm[other][family]) for family in families),
    )[:2]
    peer_mean = {family: sum(others_norm[other][family] for other in others) / max(1, len(others)) for family in families}
    distinctive = max(families, key=lambda family: me_norm[family] - peer_mean[family]) if families else "insufficient evidence"
    shared = max(families, key=lambda family: min(me_norm[family], peer_mean[family])) if families else "insufficient evidence"
    resemble = nearest[0] if nearest else "another talent"
    lines = [
        f"{talent_name} is most non-interchangeable through `{distinctive}`, which appears at a higher cumulative share than the peer mean.",
        f"Shared ground with peers is strongest in `{shared}`.",
    ]
    if len(nearest) >= 2:
        lines.append(f"This contrast is drawn most directly against {nearest[0]} and {nearest[1]}.")
    lines.append(f"Can resemble {resemble}, but differs by how `{distinctive}` organizes interaction turns.")
    if run_scope == "initial bootstrap":
        lines.append("This run established the initial contrast baseline rather than sharpening or softening a prior one.")
    else:
        lines.append("Newest-batch effect was checked against cumulative family totals to avoid over-weighting recent streams.")
    return lines


def write_csv(path: str, columns: List[str], rows: List[dict]) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=columns)
        writer.writeheader()
        for row in rows:
            writer.writerow({column: row.get(column, "") for column in columns})


def build_markdown_profile_text(
    talent: dict,
    run_scope: str,
    new_video_ids: List[str],
    current_run_streams: int,
    cumulative_streams: int,
    raw_codes_generated: int,
    final_codes_retained: int,
    new_codes_added: int,
    merged_notes: List[str],
    cumulative_codebook: List[dict],
    combined_evidence: List[dict],
    counter_by_family: Dict[str, List[dict]],
    idio_markers: List[dict],
    money_sampled: List[dict],
    money_current_notes: List[str],
    money_cumulative_notes: List[str],
    cross_talent_lines: List[str],
) -> str:
    analysis_at = datetime.now().astimezone().strftime("%Y-%m-%d %H:%M %Z")
    primary_total = sum(1 for row in combined_evidence if row["evidence_role"] == "primary")
    if primary_total >= 500:
        confidence = "High"
    elif primary_total >= 180:
        confidence = "Medium"
    else:
        confidence = "Low"
    top_codes = cumulative_codebook[:3]
    signature_codes = ", ".join(f"`{row['code_label']}`" for row in top_codes) if top_codes else "insufficient evidence"

    by_code = defaultdict(list)
    for row in combined_evidence:
        by_code[row["code_label"]].append(row)

    family_meta = defaultdict(lambda: {"codes": [], "rows": []})
    for row in cumulative_codebook:
        family_meta[row["theme_family"]]["codes"].append(row["code_label"])
    for row in combined_evidence:
        family_meta[row["theme_family"]]["rows"].append(row)

    strongest_rows = sorted(
        [row for row in combined_evidence if row["evidence_role"] == "primary"],
        key=lambda row: (row.get("incorporated_in_run") != "yes", row["video_id"], float(row["time_in_seconds"])),
    )
    strongest_conclusions = []
    for item in top_codes[:3]:
        strongest_conclusions.append(f"`{item['code_label']}` remains well-supported with {item['frequency_count']} cumulative primary evidence rows.")

    lines = [
        f"Analysis conducted: {analysis_at}",
        f"Incremental update scope: {run_scope}",
        f"New video_ids incorporated this run: {len(new_video_ids)}",
        "",
        "## 1) Personality Signature (Emergent, Cumulative)",
        f"Across the cumulative v3 open coding, {talent['name']} most consistently resolves through {signature_codes}. These patterns are carried by repeated turn-level evidence rather than a preset taxonomy, so the profile emphasizes recurring interactional moves over abstract personality typing.",
        f"The current cumulative signature shows how this streamer handles audience contact, self-disclosure, pacing, and play in text-visible ways. Confidence level: {confidence}. Limitations: this is text-only evidence, so facial expression, camera behavior, body language, and full vocal prosody are unavailable.",
    ]
    if run_scope == "initial bootstrap":
        lines.append("This run established the initial v3 signature baseline rather than revising a prior cumulative signature.")
    else:
        lines.append("This run mainly updates the living cumulative profile by testing whether newly coded streams reinforce or materially shift prior themes.")

    lines.extend(
        [
            "",
            "## 2) Incremental Open Coding Process Notes",
            "New stream cases were coded independently first, then compared for semantic overlap, split when too broad, and clustered into higher-order theme families only after the stream-level pass.",
            f"- new streams scanned this run: {current_run_streams}",
            f"- cumulative streams scanned: {cumulative_streams}",
            f"- raw codes generated this run: {raw_codes_generated}",
            f"- final retained cumulative codes: {final_codes_retained}",
            f"- newly added codes this run: {new_codes_added}",
            f"- merged/retired codes this run: {len([note for note in merged_notes if 'Merged' in note or 'Split' in note])}",
        ]
    )
    for note in merged_notes[:8]:
        lines.append(f"- {note}")

    lines.append("")
    lines.append("## 3) Emergent Codebook (Talent-Specific, Cumulative)")
    for row in cumulative_codebook:
        code = row["code_label"]
        lines.append(f"### {code}")
        lines.append(f"- code_label: {code}")
        lines.append(f"- operational_definition: {row['operational_definition']}")
        lines.append(f"- inclusion_criteria: {row['inclusion_criteria']}")
        lines.append(f"- exclusion_criteria: {row['exclusion_criteria']}")
        lines.append(f"- cumulative frequency_count: {row['frequency_count']}")
        lines.append(f"- cumulative stream_coverage_count: {row['stream_coverage_count']}")
        lines.append(f"- new_this_run: {row['new_this_run']}")
        examples = top_examples([entry for entry in by_code[code] if entry["evidence_role"] == "primary"], n=3, prefer_new=True)
        if examples:
            for example in examples:
                lines.append(f'- [{example["timecode"]}] {example["speaker"]} ({example["video_id"]}): "{short_quote(example["quote"])}"')
        else:
            lines.append("- insufficient evidence")
        lines.append("")

    lines.append("## 4) Emergent Theme Families")
    for family, meta in sorted(family_meta.items(), key=lambda item: (-len(item[1]["rows"]), item[0])):
        member_codes = sorted(set(meta["codes"]))
        primary_rows = [row for row in meta["rows"] if row["evidence_role"] == "primary"]
        support = top_examples(primary_rows, n=2, prefer_new=True)
        status = "reinforced" if any(row.get("incorporated_in_run") == "yes" for row in primary_rows) else "unchanged"
        lines.append(f"### {family}")
        lines.append(f"- family summary: Built from {len(primary_rows)} cumulative primary evidence rows spanning {', '.join(member_codes)}.")
        lines.append(f"- member codes: {', '.join(member_codes)}")
        lines.append("- strongest supporting evidence:")
        if support:
            for example in support:
                lines.append(f'  - [{example["timecode"]}] {example["speaker"]} ({example["video_id"]}): "{short_quote(example["quote"])}"')
        else:
            lines.append("  - insufficient evidence")
        lines.append("- counterexamples/limits:")
        counter_rows = counter_by_family.get(family, [])[:2]
        if counter_rows:
            for example in counter_rows:
                lines.append(f'  - [{example["timecode"]}] {example["speaker"]} ({example["video_id"]}): "{short_quote(example["quote"])}"')
        else:
            lines.append("  - insufficient evidence")
            lines.append("  - insufficient evidence")
        lines.append(f"- current-run effect: {status}")
        lines.append("")

    lines.append("## 5) Idiosyncratic Charm Markers")
    if idio_markers:
        for marker in idio_markers:
            lines.append(f"### {marker['marker']}")
            lines.append(f"- marker note: {marker['why']}")
            lines.append(f"- newly emergent this run: {'yes' if run_scope == 'initial bootstrap' else 'no'}")
            for example in marker["examples"]:
                lines.append(f'  - [{example["timecode"]}] {example["speaker"]} ({example["video_id"]}): "{example["quote"]}"')
    else:
        lines.append("Insufficient repeated phrase-level evidence to retain a stable charm marker.")
    lines.append("")

    lines.append("## 6) Money-Linked Personality Patterns (Supplement, not primary driver)")
    lines.append(f"Current-run money sample size: {len(money_sampled)}.")
    lines.append("- current-run money observations:")
    for note in money_current_notes:
        lines.append(f"  - {note}")
    lines.append("- cumulative money-linked patterns:")
    for note in money_cumulative_notes:
        lines.append(f"  - {note}")
    lines.append("")

    lines.append("## 7) Cross-Talent Distinctiveness")
    for line in cross_talent_lines:
        lines.append(f"- {line}")
    lines.append("")

    lines.append("## 8) Validity and Uncertainty")
    lines.append("- 3 strongest evidence-backed conclusions:")
    for item in strongest_conclusions[:3]:
        lines.append(f"  - {item}")
    lines.append("- 3 uncertainty points:")
    lines.append("  - Text-only evidence cannot directly resolve delivery tone, irony intensity, or off-camera affect.")
    lines.append("  - Streams with only one primary source may under-represent short interaction loops or chat-only context.")
    lines.append("  - The coding pass is cumulative, but vivid recent streams can still feel more salient than quieter older ones.")
    lines.append("- Possible coder bias and mitigation:")
    lines.append("  - Bias risk: overweighting memorable lines or titles. Mitigation: retained claims were tied to timestamped raw rows, checked for stream coverage, and balanced with counterexamples.")
    lines.append("- Incremental-risk bias:")
    lines.append("  - Risk of over-weighting the newest batch was reduced by comparing current-run hits against cumulative evidence totals before updating theme prominence.")

    return "\n".join(lines).strip() + "\n"


def build_run_snapshot_markdown(
    talent_name: str,
    run_scope: str,
    snapshot_created_at: datetime,
    current_profile_path: str,
    prompt_spec_path: str,
    prompt_spec_text: str,
    profile_markdown: str,
) -> str:
    analysis_at = snapshot_created_at.strftime("%Y-%m-%d %H:%M %Z")
    snapshot_iso = snapshot_created_at.isoformat()
    prompt_body = prompt_spec_text.strip()
    profile_body = profile_markdown.strip()
    lines = [
        f"# {talent_name} Personality Open Coding v3 Snapshot",
        "",
        f"- Snapshot created: {analysis_at}",
        f"- Snapshot ISO timestamp: `{snapshot_iso}`",
        f"- Update scope: {run_scope}",
        f"- Current cumulative output path: `{current_profile_path}`",
        f"- Prompt spec source: `{prompt_spec_path}`",
        "",
        "## Current Cumulative Profile",
        "",
        profile_body,
        "",
        "## Prompt Spec Used For This Run",
        "",
        "```md",
        prompt_body,
        "```",
        "",
    ]
    return "\n".join(lines)


def write_profile_snapshot(
    snapshot_dir: str,
    talent_name: str,
    run_scope: str,
    current_profile_path: str,
    prompt_spec_text: str,
    profile_markdown: str,
) -> str:
    snapshot_created_at = datetime.now().astimezone()
    snapshot_name = "personality_profile_v3_open_coding_" + snapshot_created_at.strftime("%Y-%m-%d_%H-%M-%S_%z") + ".md"
    snapshot_path = os.path.join(snapshot_dir, snapshot_name)
    snapshot_markdown = build_run_snapshot_markdown(
        talent_name,
        run_scope,
        snapshot_created_at,
        current_profile_path,
        PROMPT_SPEC_PATH,
        prompt_spec_text,
        profile_markdown,
    )
    write_text(snapshot_path, snapshot_markdown)
    return snapshot_path


def write_state(path: str, payload: dict) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        json.dump(payload, f, ensure_ascii=False, indent=2)
        f.write("\n")


def classify_talent(state_path: str, output_paths: List[str], discovered_ids: List[str], eligible_ids: List[str]) -> str:
    state = load_state(state_path)
    outputs_complete = all(nonempty(path) for path in output_paths)
    if not state:
        return "v3 not started"
    if not outputs_complete:
        return "v3 incomplete/repair needed"
    processed = set(state.get("processed_video_ids", []))
    new_ids = [video_id for video_id in eligible_ids if video_id not in processed]
    if new_ids:
        return "v3 has new eligible videos"
    return "v3 up to date"


def run() -> None:
    args = parse_args()
    talent_filter = (args.talent or "").strip()
    pre_run_status = []
    talent_work = {}
    prompt_spec_text = read_text(PROMPT_SPEC_PATH)
    talents = discover_talents(talent_filter=talent_filter)
    if not talents:
        if talent_filter:
            raise SystemExit(f"No eligible talent folder matched {talent_filter!r} under {DATA_ROOT}")
        raise SystemExit(f"No eligible talent folders found under {DATA_ROOT}")

    for talent in talents:
        overall_dir = os.path.join(talent["path"], "stream_summaries", "overall_themes")
        output_paths = migrate_overall_themes_outputs(overall_dir)
        state_path = output_paths["state_path"]
        profile_path = output_paths["profile_path"]
        codebook_path = output_paths["codebook_path"]
        evidence_path = output_paths["evidence_path"]
        money_path = output_paths["money_path"]

        playback_events = load_playback_events(talent["name"], sorted(glob.glob(os.path.join(talent["path"], "text_playback", "*.csv"))))
        chat_events = load_chat_events(talent["name"], sorted(glob.glob(os.path.join(talent["path"], "Chat", "Original", "*_chat.csv"))))
        all_events, all_by_video = dedupe_and_group(playback_events, chat_events)
        inventory, summary_files, playback_files, chat_files = discover_inventory(talent, all_by_video)
        money_rows = load_money_rows(money_path)

        state = load_state(state_path)
        processed_prior = set(state.get("processed_video_ids", []))
        eligible_ids = sorted([video_id for video_id, item in inventory.items() if item.eligible])
        pending_ids = sorted([video_id for video_id, item in inventory.items() if not item.eligible])
        outputs_complete = all(nonempty(path) for path in [profile_path, codebook_path, evidence_path])
        status = classify_talent(state_path, [profile_path, codebook_path, evidence_path], sorted(inventory), eligible_ids)
        pre_run_status.append({"talent": talent["name"], "status": status})

        run_scope = "weekly update"
        new_ids = [video_id for video_id in eligible_ids if video_id not in processed_prior]
        analysis_ids = []
        repair_mode = FORCE_REBUILD or (bool(state) and not outputs_complete)
        if not state:
            run_scope = "initial bootstrap"
            analysis_ids = list(eligible_ids)
        elif repair_mode:
            analysis_ids = sorted(processed_prior | set(new_ids))
        elif new_ids:
            analysis_ids = list(new_ids)
        if FORCE_SCOPE_INITIAL and analysis_ids:
            run_scope = "initial bootstrap"
            new_ids = list(analysis_ids)

        talent_work[talent["name"]] = {
            "talent": talent,
            "status": status,
            "run_scope": run_scope,
            "repair_mode": repair_mode,
            "state_path": state_path,
            "profile_path": profile_path,
            "codebook_path": codebook_path,
            "evidence_path": evidence_path,
            "snapshot_dir": output_paths["personality_open_coding_v3_snapshots_dir"],
            "inventory": inventory,
            "eligible_ids": eligible_ids,
            "pending_ids": pending_ids,
            "pending_notes": [f"{video_id}: {inventory[video_id].pending_reason}" for video_id in pending_ids if inventory[video_id].pending_reason],
            "analysis_ids": analysis_ids,
            "new_ids": list(new_ids),
            "all_events": all_events,
            "all_by_video": all_by_video,
            "money_rows": money_rows,
            "money_index": build_money_index(money_rows),
            "existing_codebook": load_existing_codebook(codebook_path) if state and outputs_complete and not repair_mode else [],
            "existing_evidence": load_existing_evidence(evidence_path) if state and outputs_complete and not repair_mode else [],
            "summary_files": summary_files,
            "playback_files": playback_files,
            "chat_files": chat_files,
        }

    all_phrase_counters = {}
    report_rows = []

    for talent_name, data in talent_work.items():
        if not data["analysis_ids"] and data["status"] == "v3 up to date":
            snapshot_path = ""
            if nonempty(data["profile_path"]):
                snapshot_path = write_profile_snapshot(
                    data["snapshot_dir"],
                    talent_name,
                    load_state(data["state_path"]).get("update_scope", "weekly update"),
                    data["profile_path"],
                    prompt_spec_text,
                    read_text(data["profile_path"]),
                )
            report_rows.append(
                {
                    "talent": talent_name,
                    "status": data["status"],
                    "new_streams_scanned": 0,
                    "cumulative_streams_scanned": len(load_state(data["state_path"]).get("processed_video_ids", [])),
                    "raw_codes_generated_this_run": 0,
                    "final_codes_retained_cumulative": len(data["existing_codebook"]),
                    "new_codes_added_this_run": 0,
                    "theme_families_cumulative": len({row["theme_family"] for row in data["existing_codebook"]}),
                    "evidence_rows_logged_cumulative": len(data["existing_evidence"]),
                    "money_events_sampled_this_run": 0,
                    "output_paths": [data["profile_path"], data["codebook_path"], data["evidence_path"], snapshot_path],
                    "state_path": data["state_path"],
                    "spotchecks": [],
                }
            )
            continue

        talent = data["talent"]
        analysis_set = set(data["analysis_ids"])
        run_by_video = {video_id: data["all_by_video"][video_id] for video_id in data["analysis_ids"] if video_id in data["all_by_video"]}
        run_events = [event for event in data["all_events"] if event.video_id in analysis_set]
        run_money_rows = [row for row in data["money_rows"] if row["video_id"] in analysis_set]

        raw_hits_stream, streamer_events, phase1_notes = phase1_stream_level_open_coding(talent, run_by_video)
        raw_hits_money, sampled_money = phase1_money_probe_open_coding(talent, run_by_video, run_money_rows)
        raw_hits = raw_hits_stream + raw_hits_money

        existing_labels = [row["code_label"] for row in data["existing_codebook"]]
        groups, phase2_stats, phase2_notes = phase2_constant_comparison(raw_hits, existing_labels)
        retained_codes, retain_notes = retain_codes(groups)
        existing_theme_map = {row["code_label"]: row["theme_family"] for row in data["existing_codebook"]}
        label_to_family, _family_meta_unused = build_theme_families(retained_codes, existing_theme_map)
        new_code_meta = build_new_codebook_meta(retained_codes, label_to_family, phase2_notes + retain_notes)
        new_evidence_rows = build_evidence_rows(
            talent["name"],
            retained_codes,
            label_to_family,
            data["money_index"],
            incorporated_in_run="yes",
        )

        if data["repair_mode"] or data["run_scope"] == "initial bootstrap":
            existing_evidence = []
            base_meta = {}
        else:
            existing_evidence = [dict(row, incorporated_in_run="no") for row in data["existing_evidence"]]
            base_meta = build_base_meta(data["existing_codebook"])

        combined_evidence = existing_evidence + new_evidence_rows
        counter_rows, counter_by_family = add_counterexamples(
            talent["name"],
            run_events if not data["repair_mode"] and data["run_scope"] != "initial bootstrap" else data["all_events"],
            retained_codes if retained_codes else {},
            label_to_family if label_to_family else {},
            data["money_index"],
            combined_evidence,
            incorporated_in_run="yes",
        )
        combined_evidence.extend(counter_rows)

        cumulative_codebook, cumulative_code_meta, cumulative_label_to_family = aggregate_cumulative_codebook(
            talent["name"],
            combined_evidence,
            base_meta,
            new_code_meta,
        )

        if data["repair_mode"] or data["run_scope"] == "initial bootstrap":
            all_streamer_events = [event for event in data["all_events"] if event.video_id in set(data["analysis_ids"])]
        else:
            all_streamer_events = streamer_events
        all_phrase_counters[talent_name] = build_phrase_counter(all_streamer_events)

        data["combined_evidence"] = combined_evidence
        data["cumulative_codebook"] = cumulative_codebook
        data["cumulative_code_meta"] = cumulative_code_meta
        data["counter_by_family"] = counter_by_family
        data["sampled_money"] = sampled_money
        data["phase_notes"] = phase1_notes + phase2_notes + retain_notes + data["pending_notes"]
        data["raw_codes_generated"] = phase2_stats["raw_codes_generated"]
        data["new_codes_added"] = len([row for row in cumulative_codebook if row["new_this_run"] == "yes"])

    family_dist_all = {}
    for talent_name, data in talent_work.items():
        if "combined_evidence" in data:
            family_dist_all[talent_name] = family_distribution(data["combined_evidence"])
        else:
            family_dist_all[talent_name] = family_distribution(data["existing_evidence"])

    for talent_name, data in talent_work.items():
        if data["status"] == "v3 up to date" and not data["analysis_ids"]:
            continue

        idio_markers = choose_idiosyncratic_markers(
            talent_name,
            [event for event in data["all_events"] if event.video_id in set(data["analysis_ids"]) and is_streamer(event, data["talent"]["tokens"])],
            all_phrase_counters,
        )
        money_current_notes, money_cumulative_notes = money_pattern_summary(
            data.get("sampled_money", []),
            data.get("combined_evidence", data.get("existing_evidence", [])),
            data["analysis_ids"],
        )
        cross_talent_lines = build_cross_talent_text(talent_name, family_dist_all, data["run_scope"])

        profile_markdown = build_markdown_profile_text(
            data["talent"],
            data["run_scope"],
            data["new_ids"],
            len(data["analysis_ids"]),
            len(sorted(set(load_state(data["state_path"]).get("processed_video_ids", [])) | set(data["analysis_ids"]))),
            data.get("raw_codes_generated", 0),
            len(data.get("cumulative_codebook", [])),
            data.get("new_codes_added", 0),
            data.get("phase_notes", []),
            data.get("cumulative_codebook", []),
            data.get("combined_evidence", []),
            data.get("counter_by_family", {}),
            idio_markers,
            data.get("sampled_money", []),
            money_current_notes,
            money_cumulative_notes,
            cross_talent_lines,
        )
        write_text(data["profile_path"], profile_markdown)

        snapshot_path = write_profile_snapshot(
            data["snapshot_dir"],
            talent_name,
            data["run_scope"],
            data["profile_path"],
            prompt_spec_text,
            profile_markdown,
        )

        write_csv(
            data["codebook_path"],
            [
                "talent",
                "code_label",
                "theme_family",
                "operational_definition",
                "inclusion_criteria",
                "exclusion_criteria",
                "frequency_count",
                "stream_coverage_count",
                "new_this_run",
                "memo_notes",
            ],
            data.get("cumulative_codebook", []),
        )
        write_csv(
            data["evidence_path"],
            [
                "talent",
                "code_label",
                "theme_family",
                "video_id",
                "time_in_seconds",
                "timecode",
                "source",
                "speaker",
                "quote",
                "evidence_role",
                "monetary_context",
                "incorporated_in_run",
            ],
            data.get("combined_evidence", []),
        )

        processed_after = sorted(set(load_state(data["state_path"]).get("processed_video_ids", [])) | set(data["analysis_ids"]))
        state_payload = {
            "talent": talent_name,
            "analysis_conducted_at": datetime.now().astimezone().isoformat(),
            "update_scope": data["run_scope"],
            "processed_video_ids": processed_after,
            "new_video_ids_this_run": data["new_ids"],
            "pending_video_ids": data["pending_ids"],
            "cumulative_streams_scanned": len(processed_after),
            "raw_codes_generated_this_run": data.get("raw_codes_generated", 0),
            "final_codes_retained_cumulative": len(data.get("cumulative_codebook", [])),
            "theme_families_cumulative": len({row["theme_family"] for row in data.get("cumulative_codebook", [])}),
            "evidence_rows_cumulative": len(data.get("combined_evidence", [])),
            "money_events_sampled_this_run": len(data.get("sampled_money", [])),
            "latest_snapshot_path": snapshot_path,
            "notes": data.get("phase_notes", [])[:30],
        }
        write_state(data["state_path"], state_payload)

        spotchecks = []
        for row in data.get("combined_evidence", []):
            if row["evidence_role"] != "primary":
                continue
            if row.get("incorporated_in_run") != "yes":
                continue
            if verify_quote_in_source(row.get("file_path", ""), row.get("quote", "")):
                spotchecks.append(
                    {
                        "video_id": row["video_id"],
                        "timecode": row["timecode"],
                        "speaker": row["speaker"],
                        "quote": short_quote(row["quote"], 160),
                        "file_path": row.get("file_path", ""),
                        "file_line": row.get("file_line", ""),
                    }
                )
            if len(spotchecks) >= 2:
                break

        report_rows.append(
            {
                "talent": talent_name,
                "status": data["status"],
                "new_streams_scanned": len(data["analysis_ids"]),
                "cumulative_streams_scanned": len(processed_after),
                "raw_codes_generated_this_run": data.get("raw_codes_generated", 0),
                "final_codes_retained_cumulative": len(data.get("cumulative_codebook", [])),
                "new_codes_added_this_run": data.get("new_codes_added", 0),
                "theme_families_cumulative": len({row["theme_family"] for row in data.get("cumulative_codebook", [])}),
                "evidence_rows_logged_cumulative": len(data.get("combined_evidence", [])),
                "money_events_sampled_this_run": len(data.get("sampled_money", [])),
                "output_paths": [data["profile_path"], data["codebook_path"], data["evidence_path"], snapshot_path],
                "state_path": data["state_path"],
                "spotchecks": spotchecks,
            }
        )

    print("PRE_RUN_STATUS")
    for item in pre_run_status:
        print(json.dumps(item, ensure_ascii=False))
    print("RUN_REPORT")
    for row in report_rows:
        print(json.dumps(row, ensure_ascii=False))


if __name__ == "__main__":
    run()
