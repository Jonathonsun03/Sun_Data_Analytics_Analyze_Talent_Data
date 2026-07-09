#!/usr/bin/env python3
"""Incremental per-talent qualitative monetary-relationship analysis from raw text logs only."""

from __future__ import annotations

import csv
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
PROMPT_SPEC_PATH = REPO_ROOT / "prompts" / "Monetary_analysis" / "monetary_summary_classification"
SKIP_DIRS = {"VarianceProject"}
WINDOW_BEFORE_SEC = 90.0
WINDOW_AFTER_SEC = 120.0
GROUP_GAP_SEC = 12.0
FORCE_REFRESH = os.environ.get("MONETARY_SUMMARY_FORCE_REFRESH") == "1"

WEAK_QUERY_RE = re.compile(
    r"\b("
    r"how do i super chat|do you have super chats?|do you see super chats?|you don['’]?t have super chats?|"
    r"what does this do\??|gift gacha|gifted to me|this was gifted to me|game was gifted to me|"
    r"remember to hit that like|support it by wishlisting|support it by buying|i got gifted"
    r")\b",
    re.I,
)
THANK_RE = re.compile(r"\b(thank(?:s| you)?|appreciate|grateful|love)\b", re.I)
MONEY_TEXT_RE = re.compile(
    r"\b("
    r"super ?chat|paid message|donat(?:e|ion|ions|ed)|"
    r"gifted(?:\s+subs?)?|gifting|gift sub|memberships?|member goal|membership goal|"
    r"hype train|subathon|roulette spin|unlocks?|"
    r"\d+\s+gifted|\d+\s+bits|five gifted|ten gifted|hundred bits|thousand bits|"
    r"becoming a member|became a member"
    r")\b",
    re.I,
)
ACKNOWLEDGEMENT_RE = re.compile(r"\b(thank(?:s| you)?(?:\s+so\s+much)?|appreciate|grateful)\b", re.I)
TEXTUAL_MONEY_ACK_RE = re.compile(
    r"("
    r"\b(thank(?:s| you)?|appreciate)\b.{0,50}\b(super ?chat|donat(?:e|ion|ions|ed)|gifted(?:\s+subs?)?|gifting|memberships?|bits|becoming a member|became a member)\b|"
    r"\b(super ?chat|donat(?:e|ion|ions|ed)|gifted(?:\s+subs?)?|gifting|memberships?|bits|becoming a member|became a member)\b.{0,50}\b(thank(?:s| you)?|appreciate)\b|"
    r"\b(\d+|five|ten|fifteen|twenty|fifty|hundred|thousand)\s+(gifted|bits)\b"
    r")",
    re.I,
)
REQUEST_RE = re.compile(r"\b(can you|could you|would you|please|pick|vote|what does|does it|can i)\b|\?", re.I)
CELEBRATION_RE = re.compile(
    r"\b(congrats|congratulations|happy birthday|anniversary|milestone|goal|unlock|celebrat|hype train|level)\b",
    re.I,
)
CARE_RE = re.compile(
    r"\b(take care|take your time|rest well|hydrate|feel better|hope you|proud of you|be safe|you got this)\b",
    re.I,
)
HUMOR_RE = re.compile(
    r"\b(lol|lmao|lmfao|haha|hehe|cackle|chaos|bully|bozo|clown|gremlin|cute|goth|violence|yummy yum|bit)\b",
    re.I,
)
GOAL_RE = re.compile(
    r"\b(goal|member goal|membership goal|every\s+\d+\s+members?|roulette spin|spin|unlocks?|level\s+\d+|subathon)\b",
    re.I,
)
EFFORT_RE = re.compile(
    r"\b(set(?:ting)? up|recording|karaoke|practice|working|work stream|locked in|trying|grind|stream check in)\b",
    re.I,
)
DISCLOSURE_RE = re.compile(
    r"\b(i'?m (?:tired|stressed|anxious|sad|overwhelmed)|burnout|cry|crying|rough day|sorry y['’]?all)\b",
    re.I,
)
HYPE_RE = re.compile(r"\b(oh my god|let'?s go|welcome|good morning|good evening|yo|chaos|stop it stop it)\b", re.I)


@dataclass
class RawEvent:
    video_id: str
    sec: float
    timecode: str
    source: str
    speaker: str
    text: str
    message_type: str
    paid_amount_text: str
    paid_currency: str
    file_path: str
    file_line: int
    ingest_source: str


@dataclass
class MoneyCandidate:
    video_id: str
    sec: float
    source: str
    speaker: str
    text: str
    message_type: str
    paid_amount_text: str
    paid_currency: str
    money_signal_type: str
    file_path: str
    file_line: int
    ingest_source: str
    confidence: str


@dataclass
class MoneyEventGroup:
    event_group_id: str
    video_id: str
    sec: float
    timecode: str
    money_signal_type: str
    candidates: List[MoneyCandidate]
    window: List[RawEvent]
    giver_motive: str
    streamer_attractor: str
    relationship_function: str
    support_level: str
    incorporated_in_run: str


@dataclass
class CodeSpec:
    code_label: str
    code_layer: str
    definition: str
    inclusion_criteria: str
    exclusion_criteria: str
    patterns: Tuple[re.Pattern[str], ...]


def parse_float(value: object) -> float:
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
    out = (text or "").lower().strip()
    out = re.sub(r"https?://\S+", "", out)
    out = re.sub(r"[^a-z0-9\s']", " ", out)
    out = re.sub(r"\s+", " ", out).strip()
    return out


def normalize_speaker(speaker: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", (speaker or "").lower().lstrip("@"))


def markdown_escape(text: str) -> str:
    return (text or "").replace("\\", "\\\\").replace("`", "\\`")


def short_quote(text: str, limit: int = 220) -> str:
    compact = (text or "").replace("\n", " ").strip()
    if len(compact) <= limit:
        return compact
    return compact[: limit - 3].rstrip() + "..."


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def write_json(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def load_json(path: Path) -> dict:
    if not path.exists():
        return {}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {}


def nonempty(path: Path) -> bool:
    return path.exists() and path.stat().st_size > 0


def extract_video_id_from_name(path: str) -> str:
    base = os.path.basename(path)
    match = re.search(r"_([A-Za-z0-9_-]{11})(?:_chat\.csv|\.csv|\.md)$", base)
    if match:
        return match.group(1)
    match = re.search(r"([A-Za-z0-9_-]{11})(?:_chat\.csv|\.csv|\.md)$", base)
    if match:
        return match.group(1)
    return ""


def load_existing_code_labels(path: Path) -> set[str]:
    if not nonempty(path):
        return set()
    try:
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            return {row.get("code_label", "").strip() for row in csv.DictReader(handle) if row.get("code_label", "").strip()}
    except Exception:
        return set()


def detect_talents() -> List[Path]:
    talents: List[Path] = []
    for talent_dir in sorted(DATA_ROOT.iterdir()):
        if not talent_dir.is_dir():
            continue
        if talent_dir.name in SKIP_DIRS:
            continue
        if (talent_dir / "text_playback").is_dir() or (talent_dir / "Chat" / "Original").is_dir():
            talents.append(talent_dir)
    return talents


def build_video_inventory(talent_dir: Path) -> Dict[str, dict]:
    playback_paths = sorted((talent_dir / "text_playback").glob("*.csv"))
    chat_paths = sorted((talent_dir / "Chat" / "Original").glob("*_chat.csv"))
    inventory: Dict[str, dict] = {}
    for path in playback_paths:
        video_id = extract_video_id_from_name(str(path))
        if not video_id:
            continue
        inventory.setdefault(video_id, {"video_id": video_id, "playback_path": None, "chat_path": None})
        inventory[video_id]["playback_path"] = path
    for path in chat_paths:
        video_id = extract_video_id_from_name(str(path))
        if not video_id:
            continue
        inventory.setdefault(video_id, {"video_id": video_id, "playback_path": None, "chat_path": None})
        inventory[video_id]["chat_path"] = path
    return dict(sorted(inventory.items()))


def load_playback_events(paths: Sequence[Path]) -> List[RawEvent]:
    events: List[RawEvent] = []
    for path in paths:
        fallback_video_id = extract_video_id_from_name(str(path))
        if not path.exists():
            continue
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            reader = csv.DictReader(handle)
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
                video_id = (row.get("video_id") or fallback_video_id or "").strip()
                if not video_id:
                    continue
                events.append(
                    RawEvent(
                        video_id=video_id,
                        sec=sec,
                        timecode=(row.get("timecode") or sec_to_timecode(sec)).strip(),
                        source=source,
                        speaker=(row.get("speaker") or "").strip() or "UNKNOWN",
                        text=text,
                        message_type=(row.get("message_type") or "").strip(),
                        paid_amount_text=clean_paid_value(row.get("paid_amount_text")),
                        paid_currency=clean_paid_value(row.get("paid_currency")),
                        file_path=str(path),
                        file_line=line_no,
                        ingest_source="text_playback",
                    )
                )
    return events


def load_chat_events(paths: Sequence[Path]) -> List[RawEvent]:
    events: List[RawEvent] = []
    for path in paths:
        fallback_video_id = extract_video_id_from_name(str(path))
        if not path.exists():
            continue
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            reader = csv.DictReader(handle)
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
                events.append(
                    RawEvent(
                        video_id=video_id,
                        sec=sec,
                        timecode=sec_to_timecode(sec),
                        source="chat",
                        speaker=(row.get("username") or "").strip() or "UNKNOWN",
                        text=text,
                        message_type=(row.get("message_type") or "").strip(),
                        paid_amount_text=clean_paid_value(row.get("paid_amount_text")),
                        paid_currency=clean_paid_value(row.get("paid_currency")),
                        file_path=str(path),
                        file_line=line_no,
                        ingest_source="raw_chat",
                    )
                )
    return events


def clean_paid_value(value: object) -> str:
    text = ("" if value is None else str(value)).strip()
    if not text or text.upper() == "NA":
        return ""
    return text


def dedupe_events(playback_events: Sequence[RawEvent], chat_events: Sequence[RawEvent]) -> Dict[str, List[RawEvent]]:
    merged: List[RawEvent] = list(playback_events)
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
    merged.sort(key=lambda event: (event.video_id, event.sec, event.file_path, event.file_line))
    grouped: Dict[str, List[RawEvent]] = defaultdict(list)
    for event in merged:
        grouped[event.video_id].append(event)
    return grouped


def explicit_money_signal(event: RawEvent) -> Optional[str]:
    message_type = event.message_type.lower().strip()
    if event.paid_amount_text or event.paid_currency:
        if "paid" in message_type:
            return "paid_message"
        if "member" in message_type or "gift" in message_type:
            return "membership_item"
        return "paid_amount_field"
    if message_type in {"paid_message", "paid_sticker"}:
        return "paid_message"
    if message_type == "membership_item":
        return "membership_item"
    if message_type and any(token in message_type for token in ("member", "gift", "super", "donat")):
        return message_type
    return None


def nearby_explicit_signal(video_id: str, sec: float, explicit_secs_by_video: Dict[str, List[float]], tolerance: float = 20.0) -> bool:
    return any(abs(sec - explicit_sec) <= tolerance for explicit_sec in explicit_secs_by_video.get(video_id, []))


def strong_textual_money_signal(event: RawEvent) -> Optional[str]:
    text = event.text
    if event.source != "subtitle":
        return None
    if WEAK_QUERY_RE.search(text):
        return None
    if re.search(r"\b(a little bit|little bit|bit messy|bit more|bit of|bit easier|enjoy a bit)\b", text, re.I):
        return None
    if not TEXTUAL_MONEY_ACK_RE.search(text):
        return None
    if not MONEY_TEXT_RE.search(text):
        return None
    return "streamer_acknowledgement"


def candidate_from_event(event: RawEvent, signal_type: str, confidence: str) -> MoneyCandidate:
    return MoneyCandidate(
        video_id=event.video_id,
        sec=event.sec,
        source=event.source,
        speaker=event.speaker,
        text=event.text,
        message_type=event.message_type,
        paid_amount_text=event.paid_amount_text,
        paid_currency=event.paid_currency,
        money_signal_type=signal_type,
        file_path=event.file_path,
        file_line=event.file_line,
        ingest_source=event.ingest_source,
        confidence=confidence,
    )


def group_money_candidates(candidates: Sequence[MoneyCandidate]) -> Dict[str, List[MoneyCandidate]]:
    per_video: Dict[str, List[MoneyCandidate]] = defaultdict(list)
    for candidate in sorted(candidates, key=lambda item: (item.video_id, item.sec, item.file_path, item.file_line)):
        group = per_video[candidate.video_id]
        if not group:
            group.append(candidate)
            continue
        previous = group[-1]
        same_text = normalize_text(previous.text) == normalize_text(candidate.text)
        same_amount = bool(previous.paid_amount_text and previous.paid_amount_text == candidate.paid_amount_text)
        if candidate.sec - previous.sec <= GROUP_GAP_SEC and (
            same_text
            or same_amount
            or previous.money_signal_type == candidate.money_signal_type
            or {previous.source, candidate.source} == {"chat", "subtitle"}
        ):
            group.append(candidate)
            continue
        group.append(candidate)
    grouped: Dict[str, List[MoneyCandidate]] = defaultdict(list)
    for video_id, video_candidates in per_video.items():
        clusters: List[List[MoneyCandidate]] = []
        for candidate in sorted(video_candidates, key=lambda item: (item.sec, item.file_path, item.file_line)):
            if not clusters:
                clusters.append([candidate])
                continue
            current = clusters[-1]
            previous = current[-1]
            same_text = normalize_text(previous.text) == normalize_text(candidate.text)
            same_amount = bool(previous.paid_amount_text and previous.paid_amount_text == candidate.paid_amount_text)
            if candidate.sec - previous.sec <= GROUP_GAP_SEC and (
                same_text
                or same_amount
                or previous.money_signal_type == candidate.money_signal_type
                or {previous.source, candidate.source} == {"chat", "subtitle"}
            ):
                current.append(candidate)
            else:
                clusters.append([candidate])
        for index, cluster in enumerate(clusters, start=1):
            grouped[f"{video_id}__{index:03d}"] = cluster
    return grouped


def choose_primary_signal(candidates: Sequence[MoneyCandidate]) -> Tuple[str, float]:
    priority = {
        "paid_message": 5,
        "membership_item": 4,
        "paid_amount_field": 3,
        "streamer_acknowledgement": 2,
    }
    primary = sorted(candidates, key=lambda item: (-priority.get(item.money_signal_type, 0), item.sec, item.file_line))[0]
    anchor_sec = min(item.sec for item in candidates if item.confidence == "explicit") if any(item.confidence == "explicit" for item in candidates) else primary.sec
    return primary.money_signal_type, anchor_sec


def window_for_event(events_by_video: Dict[str, List[RawEvent]], video_id: str, anchor_sec: float) -> List[RawEvent]:
    return [
        event
        for event in events_by_video.get(video_id, [])
        if anchor_sec - WINDOW_BEFORE_SEC <= event.sec <= anchor_sec + WINDOW_AFTER_SEC
    ]


def support_level_for_window(window: Sequence[RawEvent]) -> str:
    has_streamer = any(event.source == "subtitle" for event in window)
    has_chat = any(event.source == "chat" for event in window)
    if has_streamer and has_chat and len(window) >= 6:
        return "money event visible and well-supported"
    return "money event visible but context window weak"


MOTIVE_SPECS: List[CodeSpec] = [
    CodeSpec(
        code_label="belonging ritual through memberships/gifts",
        code_layer="giver motive",
        definition="Money or gifting appears tied to joining, extending, or circulating membership as a community ritual.",
        inclusion_criteria="Use when the visible event centers on gifted memberships, membership renewals, or chat treating access/membership as the point of the contribution.",
        exclusion_criteria="Exclude when the paid line is mainly a question, joke, or compliment without visible membership circulation.",
        patterns=(re.compile(r"\b(membership|member(?:ship)?s?|gift(?:ed|ing)?(?:\s+subs?)?|welcome in|another amazing month|gifties)\b", re.I),),
    ),
    CodeSpec(
        code_label="request leverage or bid for attention",
        code_layer="giver motive",
        definition="Money appears to be used to ask a question, make a request, or secure focused attention.",
        inclusion_criteria="Use when a paid message contains a clear question, request, vote, or prompt directed at the streamer.",
        exclusion_criteria="Exclude when a question is incidental and the visible point is celebration, support, or belonging.",
        patterns=(REQUEST_RE,),
    ),
    CodeSpec(
        code_label="celebration and milestone marking",
        code_layer="giver motive",
        definition="Money appears tied to celebrating a milestone, goal, birthday, anniversary, or hype moment.",
        inclusion_criteria="Use when the event text or nearby window references goals, milestones, birthdays, anniversaries, levels, or celebration language.",
        exclusion_criteria="Exclude when a goal is only background chatter and the paid message is mainly a request or joke.",
        patterns=(CELEBRATION_RE, GOAL_RE),
    ),
    CodeSpec(
        code_label="care and comfort support",
        code_layer="giver motive",
        definition="Money appears to offer comfort, check in on the streamer, or reinforce a caring atmosphere.",
        inclusion_criteria="Use when the paid line or nearby reaction includes visible reassurance, care, or wellbeing language.",
        exclusion_criteria="Exclude when the tone is primarily humorous, transactional, or focused on a content request.",
        patterns=(CARE_RE,),
    ),
    CodeSpec(
        code_label="shared joke and bit participation",
        code_layer="giver motive",
        definition="Money appears to participate in an ongoing joke, tease, or performative bit with the streamer.",
        inclusion_criteria="Use when the paid line is visibly comedic, teasing, or escalates a running gag in the window.",
        exclusion_criteria="Exclude when humor is incidental and the visible aim is a request or generic support.",
        patterns=(HUMOR_RE,),
    ),
    CodeSpec(
        code_label="reciprocity for streamer effort",
        code_layer="giver motive",
        definition="Money appears framed as paying back visible work, preparation, or performance effort.",
        inclusion_criteria="Use when the paid line or context refers to setup, recording, karaoke, work, or sustained effort on stream.",
        exclusion_criteria="Exclude when effort is not visible in the immediate window and support is otherwise unspecified.",
        patterns=(EFFORT_RE,),
    ),
    CodeSpec(
        code_label="appreciation and general support",
        code_layer="giver motive",
        definition="Money appears to function as straightforward appreciation or encouragement without a sharper motive signal.",
        inclusion_criteria="Use when visible text shows thanks, praise, good luck, or positive support but not a clearer request, ritual, or joke frame.",
        exclusion_criteria="Exclude when evidence supports a more specific motive or when motive is unclear from visible text.",
        patterns=(THANK_RE, re.compile(r"\b(good luck|have fun|all the best|support|appreciate you)\b", re.I)),
    ),
]

ATTRACTOR_SPECS: List[CodeSpec] = [
    CodeSpec(
        code_label="direct acknowledgment and responsive attention",
        code_layer="streamer attractor/trigger",
        definition="The streamer visibly reads, thanks, welcomes, or responds to chat in a way that makes paid support feel personally noticed.",
        inclusion_criteria="Use when nearby streamer lines explicitly thank, welcome, answer, or react to donors or gifters.",
        exclusion_criteria="Exclude when the streamer barely reacts or the event is only visible from chat metadata.",
        patterns=(ACKNOWLEDGEMENT_RE,),
    ),
    CodeSpec(
        code_label="goal framing and communal milestones",
        code_layer="streamer attractor/trigger",
        definition="The streamer links contributions to goals, unlocks, roulette spins, or other visible communal thresholds.",
        inclusion_criteria="Use when the window references member goals, levels, unlocks, spins, or milestone targets as the frame for giving.",
        exclusion_criteria="Exclude when a goal is only briefly mentioned and not visibly tied to the contribution.",
        patterns=(GOAL_RE,),
    ),
    CodeSpec(
        code_label="comedic escalation and bit play",
        code_layer="streamer attractor/trigger",
        definition="The streamer’s joking, teasing, or chaotic delivery appears to attract or shape the contribution.",
        inclusion_criteria="Use when the streamer is visibly riffing, teasing, or amplifying a joke around the paid moment.",
        exclusion_criteria="Exclude when humor comes only from chat while the streamer response is flat or unrelated.",
        patterns=(HUMOR_RE,),
    ),
    CodeSpec(
        code_label="visible effort or labor on stream",
        code_layer="streamer attractor/trigger",
        definition="The contribution appears attached to visible work, setup, performance, or effort underway on stream.",
        inclusion_criteria="Use when the window shows recording, setup, karaoke, work-stream labor, or gameplay effort being rewarded.",
        exclusion_criteria="Exclude when effort is generic background context rather than visibly connected to the paid moment.",
        patterns=(EFFORT_RE,),
    ),
    CodeSpec(
        code_label="care and reassurance stance",
        code_layer="streamer attractor/trigger",
        definition="The streamer’s warm, reassuring, or gentle interaction style appears to shape support moments.",
        inclusion_criteria="Use when the streamer is visibly comforting, checking in, or maintaining a caring tone around the event.",
        exclusion_criteria="Exclude when the exchange is primarily a joke, goal push, or blunt request-response.",
        patterns=(CARE_RE,),
    ),
    CodeSpec(
        code_label="celebratory hype and momentum",
        code_layer="streamer attractor/trigger",
        definition="The streamer’s excited, welcoming, or high-energy framing appears to pull contributions into a momentum burst.",
        inclusion_criteria="Use when the window is visibly high-energy, welcoming, or hype-oriented around the paid moment.",
        exclusion_criteria="Exclude when excitement is absent or the event is mostly quiet acknowledgment.",
        patterns=(HYPE_RE, CELEBRATION_RE),
    ),
    CodeSpec(
        code_label="emotional openness or vulnerability",
        code_layer="streamer attractor/trigger",
        definition="The streamer’s visible emotional disclosure or vulnerability appears to shape support.",
        inclusion_criteria="Use when the streamer is visibly discussing stress, fatigue, or emotional strain around the contribution.",
        exclusion_criteria="Exclude when no such disclosure is present in the interaction window.",
        patterns=(DISCLOSURE_RE,),
    ),
]

FUNCTION_SPECS: List[CodeSpec] = [
    CodeSpec(
        code_label="marking belonging and access",
        code_layer="relationship function",
        definition="The paid moment visibly marks who is inside the community, who gets access, or who is welcomed in.",
        inclusion_criteria="Use when gifted memberships, membership renewals, welcomes, or access talk are central in the window.",
        exclusion_criteria="Exclude when the event is mainly a question or one-off compliment without visible belonging work.",
        patterns=(re.compile(r"\b(membership|member(?:ship)?s?|gift(?:ed|ing)?(?:\s+subs?)?|welcome in|enjoy)\b", re.I),),
    ),
    CodeSpec(
        code_label="prompting interaction and prioritized attention",
        code_layer="relationship function",
        definition="The paid moment buys or intensifies direct exchange between streamer and chat.",
        inclusion_criteria="Use when a paid line clearly asks something, prompts a response, or pulls the streamer into direct interaction.",
        exclusion_criteria="Exclude when the visible function is mainly celebration, belonging, or diffuse support.",
        patterns=(REQUEST_RE, ACKNOWLEDGEMENT_RE),
    ),
    CodeSpec(
        code_label="driving collective momentum",
        code_layer="relationship function",
        definition="The paid moment pushes the room toward a goal, threshold, or heightened shared tempo.",
        inclusion_criteria="Use when goals, levels, roulette spins, or hype-train style escalation are visible in the window.",
        exclusion_criteria="Exclude when momentum language is absent and the event is mainly personal thanks.",
        patterns=(GOAL_RE, HYPE_RE),
    ),
    CodeSpec(
        code_label="rewarding streamer labor",
        code_layer="relationship function",
        definition="The paid moment visibly rewards work, performance, or effort the streamer is doing.",
        inclusion_criteria="Use when the contribution is tied to visible setup, work, karaoke, gameplay effort, or sustained labor.",
        exclusion_criteria="Exclude when effort is not visible in the window and support stays unspecific.",
        patterns=(EFFORT_RE,),
    ),
    CodeSpec(
        code_label="stabilizing care and reassurance",
        code_layer="relationship function",
        definition="The paid moment helps maintain a caring, steady, or reassuring relationship atmosphere.",
        inclusion_criteria="Use when the window visibly carries care, checking-in, or soothing language around the contribution.",
        exclusion_criteria="Exclude when the main function is hype, joking, or access marking.",
        patterns=(CARE_RE,),
    ),
    CodeSpec(
        code_label="escalating a shared joke or ceremony",
        code_layer="relationship function",
        definition="The paid moment extends a joke, ritualized tease, or performative ceremony between streamer and chat.",
        inclusion_criteria="Use when the paid moment visibly amplifies a running bit, playful bullying, or ritualized comedic exchange.",
        exclusion_criteria="Exclude when the tone is earnest support or a straightforward request.",
        patterns=(HUMOR_RE,),
    ),
    CodeSpec(
        code_label="marking milestones and celebration",
        code_layer="relationship function",
        definition="The paid moment serves as a textual marker of celebration or communal milestone recognition.",
        inclusion_criteria="Use when birthdays, anniversaries, goals, or milestone language clearly frame what the moment is doing relationally.",
        exclusion_criteria="Exclude when those references are weak and another function is more visible.",
        patterns=(CELEBRATION_RE,),
    ),
]


def classify_with_specs(text: str, specs: Sequence[CodeSpec]) -> Optional[CodeSpec]:
    scored: List[Tuple[int, int, CodeSpec]] = []
    for index, spec in enumerate(specs):
        score = sum(len(pattern.findall(text)) for pattern in spec.patterns)
        if score > 0:
            scored.append((score, -index, spec))
    if not scored:
        return None
    scored.sort(reverse=True)
    return scored[0][2]


def streamer_window_text(window: Sequence[RawEvent], anchor_sec: float) -> str:
    relevant = [
        event.text
        for event in window
        if event.source == "subtitle" and anchor_sec - 30 <= event.sec <= anchor_sec + 60
    ]
    return "\n".join(relevant)


def donor_window_text(window: Sequence[RawEvent], candidates: Sequence[MoneyCandidate], anchor_sec: float) -> str:
    donor_speakers = {normalize_speaker(candidate.speaker) for candidate in candidates if candidate.source == "chat"}
    relevant: List[str] = [candidate.text for candidate in candidates if candidate.source == "chat"]
    for event in window:
        if event.source != "chat":
            continue
        speaker = normalize_speaker(event.speaker)
        if donor_speakers and speaker in donor_speakers and anchor_sec - 15 <= event.sec <= anchor_sec + 30:
            relevant.append(event.text)
    return "\n".join(dict.fromkeys(text for text in relevant if text))


def infer_giver_motive(candidates: Sequence[MoneyCandidate], window: Sequence[RawEvent], anchor_sec: float) -> str:
    donor_text = donor_window_text(window, candidates, anchor_sec)
    if not donor_text:
        fallback = classify_with_specs("\n".join(event.text for event in window), [MOTIVE_SPECS[0]])
        return fallback.code_label if fallback else "unclear from visible text"
    match = classify_with_specs(donor_text, MOTIVE_SPECS)
    return match.code_label if match else "unclear from visible text"


def infer_streamer_attractor(window: Sequence[RawEvent], anchor_sec: float) -> str:
    text = streamer_window_text(window, anchor_sec)
    if not text:
        return "unclear from visible text"
    match = classify_with_specs(text, ATTRACTOR_SPECS)
    return match.code_label if match else "unclear from visible text"


def infer_relationship_function(
    giver_motive: str,
    streamer_attractor: str,
    candidates: Sequence[MoneyCandidate],
    window: Sequence[RawEvent],
) -> str:
    if giver_motive == "belonging ritual through memberships/gifts":
        return "marking belonging and access"
    if giver_motive == "request leverage or bid for attention":
        return "prompting interaction and prioritized attention"
    if giver_motive == "celebration and milestone marking":
        return "marking milestones and celebration"
    if giver_motive == "care and comfort support":
        return "stabilizing care and reassurance"
    if giver_motive == "shared joke and bit participation":
        return "escalating a shared joke or ceremony"
    if giver_motive == "reciprocity for streamer effort":
        return "rewarding streamer labor"
    combined = "\n".join([candidate.text for candidate in candidates] + [event.text for event in window])
    match = classify_with_specs(combined, FUNCTION_SPECS)
    if match:
        return match.code_label
    if streamer_attractor == "goal framing and communal milestones":
        return "driving collective momentum"
    if streamer_attractor == "direct acknowledgment and responsive attention":
        return "prompting interaction and prioritized attention"
    return "showing support and intensifying attention"


def build_money_event_groups(
    candidates: Sequence[MoneyCandidate],
    events_by_video: Dict[str, List[RawEvent]],
    new_ids: set[str],
) -> List[MoneyEventGroup]:
    grouped = group_money_candidates(candidates)
    events: List[MoneyEventGroup] = []
    for event_group_id, cluster in sorted(grouped.items()):
        video_id = cluster[0].video_id
        signal_type, anchor_sec = choose_primary_signal(cluster)
        window = window_for_event(events_by_video, video_id, anchor_sec)
        giver_motive = infer_giver_motive(cluster, window, anchor_sec)
        streamer_attractor = infer_streamer_attractor(window, anchor_sec)
        relationship_function = infer_relationship_function(giver_motive, streamer_attractor, cluster, window)
        events.append(
            MoneyEventGroup(
                event_group_id=event_group_id,
                video_id=video_id,
                sec=anchor_sec,
                timecode=sec_to_timecode(anchor_sec),
                money_signal_type=signal_type,
                candidates=list(cluster),
                window=window,
                giver_motive=giver_motive,
                streamer_attractor=streamer_attractor,
                relationship_function=relationship_function,
                support_level=support_level_for_window(window),
                incorporated_in_run="yes" if video_id in new_ids else "no",
            )
        )
    return events


def build_codebook_specs() -> Dict[Tuple[str, str], CodeSpec]:
    specs: Dict[Tuple[str, str], CodeSpec] = {}
    for spec in MOTIVE_SPECS + ATTRACTOR_SPECS + FUNCTION_SPECS:
        specs[(spec.code_layer, spec.code_label)] = spec
    return specs


def evidence_lines_for_event(event: MoneyEventGroup) -> List[RawEvent]:
    lines: List[RawEvent] = []
    anchor_refs = {(candidate.file_path, candidate.file_line) for candidate in event.candidates}
    window_sorted = sorted(event.window, key=lambda row: (abs(row.sec - event.sec), row.sec, row.file_line))
    keyword_window = [
        row
        for row in window_sorted
        if row.source == "subtitle" and (ACKNOWLEDGEMENT_RE.search(row.text) or MONEY_TEXT_RE.search(row.text))
    ]
    for row in event.window:
        if (row.file_path, row.file_line) in anchor_refs:
            lines.append(row)
    for row in keyword_window[:4]:
        if (row.file_path, row.file_line) not in {(line.file_path, line.file_line) for line in lines}:
            lines.append(row)
    if not lines and event.window:
        lines.extend(sorted(event.window, key=lambda row: (abs(row.sec - event.sec), row.file_line))[:3])
    lines.sort(key=lambda row: (row.sec, row.file_path, row.file_line))
    return lines


def display_line_for_event(event: MoneyEventGroup) -> Optional[RawEvent]:
    anchors = sorted(
        event.candidates,
        key=lambda item: (
            item.money_signal_type == "paid_message",
            item.money_signal_type == "membership_item",
            len(item.text),
            -item.sec,
        ),
        reverse=True,
    )
    if anchors:
        anchor = anchors[0]
        return RawEvent(
            video_id=anchor.video_id,
            sec=anchor.sec,
            timecode=sec_to_timecode(anchor.sec),
            source=anchor.source,
            speaker=anchor.speaker,
            text=anchor.text,
            message_type=anchor.message_type,
            paid_amount_text=anchor.paid_amount_text,
            paid_currency=anchor.paid_currency,
            file_path=anchor.file_path,
            file_line=anchor.file_line,
            ingest_source=anchor.ingest_source,
        )
    lines = evidence_lines_for_event(event)
    return lines[0] if lines else None


def build_evidence_rows(talent_name: str, events: Sequence[MoneyEventGroup]) -> List[dict]:
    rows: List[dict] = []
    for event in events:
        evidence_lines = evidence_lines_for_event(event)
        layers = [
            ("giver motive", event.giver_motive),
            ("streamer attractor/trigger", event.streamer_attractor),
            ("relationship function", event.relationship_function),
        ]
        for line in evidence_lines:
            role = "event_anchor" if any(candidate.file_path == line.file_path and candidate.file_line == line.file_line for candidate in event.candidates) else ("pre_window" if line.sec < event.sec else "post_window")
            for code_layer, code_label in layers:
                rows.append(
                    {
                        "talent": talent_name,
                        "video_id": event.video_id,
                        "time_in_seconds": f"{line.sec:.3f}",
                        "timecode": line.timecode,
                        "source": line.source,
                        "speaker": line.speaker,
                        "quote": line.text,
                        "window_role": role,
                        "money_signal_type": event.money_signal_type,
                        "code_layer": code_layer,
                        "code_label": code_label,
                        "event_group_id": event.event_group_id,
                        "inferred_giver_motive": event.giver_motive,
                        "inferred_streamer_attractor": event.streamer_attractor,
                        "inferred_relationship_function": event.relationship_function,
                        "incorporated_in_run": event.incorporated_in_run,
                        "file_path": line.file_path,
                        "file_line": str(line.file_line),
                    }
                )
    return rows


def write_csv(path: Path, fieldnames: Sequence[str], rows: Sequence[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def build_codebook_rows(
    talent_name: str,
    events: Sequence[MoneyEventGroup],
    prior_labels: set[str],
    new_ids: set[str],
) -> List[dict]:
    specs = build_codebook_specs()
    rows: List[dict] = []
    grouped_events: Dict[Tuple[str, str], List[MoneyEventGroup]] = defaultdict(list)
    for event in events:
        grouped_events[("giver motive", event.giver_motive)].append(event)
        grouped_events[("streamer attractor/trigger", event.streamer_attractor)].append(event)
        grouped_events[("relationship function", event.relationship_function)].append(event)
    for key in sorted(grouped_events, key=lambda item: (item[0], item[1])):
        code_layer, code_label = key
        bucket = grouped_events[key]
        spec = specs.get(key)
        evidence_count = len(bucket)
        stream_coverage_count = len({event.video_id for event in bucket})
        new_this_run = "yes" if code_label not in prior_labels and any(event.video_id in new_ids for event in bucket) else "no"
        memo = []
        if code_label == "unclear from visible text":
            memo.append("Retained only because visible text supports a money event but not a stronger inference.")
        if any(event.support_level == "money event visible but context window weak" for event in bucket):
            memo.append("Includes some weak-context windows.")
        rows.append(
            {
                "talent": talent_name,
                "code_label": code_label,
                "code_layer": code_layer,
                "definition": spec.definition if spec else "Visible recurring pattern retained from raw money-event windows.",
                "inclusion_criteria": spec.inclusion_criteria if spec else "Retain when repeated event windows visibly support this interpretation.",
                "exclusion_criteria": spec.exclusion_criteria if spec else "Exclude when visible text supports a more specific alternative code.",
                "evidence_count": str(evidence_count),
                "stream_coverage_count": str(stream_coverage_count),
                "new_this_run": new_this_run,
                "memo_notes": " ".join(memo).strip(),
            }
        )
    return rows


def strongest_items(counter: Counter[str], limit: int = 3) -> List[Tuple[str, int]]:
    return [(label, count) for label, count in counter.most_common() if label][:limit]


def quoted_examples_for_code(events: Sequence[MoneyEventGroup], limit: int = 3) -> List[str]:
    examples: List[str] = []
    for event in sorted(events, key=lambda item: (item.support_level != "money event visible and well-supported", item.sec)):
        line = display_line_for_event(event)
        if not line:
            continue
        examples.append(
            f"- `{event.video_id}` {event.timecode}: \"{markdown_escape(short_quote(line.text, 180))}\" "
            f"([{Path(line.file_path).name}:{line.file_line}])"
        )
        if len(examples) >= limit:
            break
    return examples


def representative_events(events: Sequence[MoneyEventGroup], limit: int = 5) -> List[MoneyEventGroup]:
    chosen: List[MoneyEventGroup] = []
    seen_signatures: set[Tuple[str, str, str]] = set()
    for event in sorted(
        events,
        key=lambda item: (
            item.support_level != "money event visible and well-supported",
            item.giver_motive == "unclear from visible text",
            item.sec,
        ),
    ):
        signature = (event.giver_motive, event.streamer_attractor, event.relationship_function)
        if signature in seen_signatures and len(chosen) >= 2:
            continue
        chosen.append(event)
        seen_signatures.add(signature)
        if len(chosen) >= limit:
            break
    return chosen


def build_overview_paragraphs(events: Sequence[MoneyEventGroup]) -> List[str]:
    if not events:
        return [
            "No visible money events were detected in the available raw text_playback or raw chat files, so the monetary relationship picture is textually weak.",
            "That result only means no paid-support moment was clearly visible in these logs; it does not rule out off-screen, metadata-missing, or platform-hidden support activity.",
        ]
    motive_counts = Counter(event.giver_motive for event in events)
    attractor_counts = Counter(event.streamer_attractor for event in events)
    function_counts = Counter(event.relationship_function for event in events)
    support_counts = Counter(event.support_level for event in events)
    motive_bits = ", ".join(f"{label} ({count})" for label, count in strongest_items(motive_counts))
    attractor_bits = ", ".join(f"{label} ({count})" for label, count in strongest_items(attractor_counts))
    function_bits = ", ".join(f"{label} ({count})" for label, count in strongest_items(function_counts))
    paragraphs = [
        f"Across the visible money-event windows, chat most often appears to give for {motive_bits or 'unclear reasons'}, while streamer-side triggers most often look like {attractor_bits or 'unclear from visible text'}.",
        f"Relationally, the paid moments most often function as {function_bits or 'general support signals'}. {support_counts.get('money event visible and well-supported', 0)} events are well-supported by both chat and streamer text, while {support_counts.get('money event visible but context window weak', 0)} rely on thinner windows or one-sided visibility.",
    ]
    if motive_counts.get("unclear from visible text"):
        paragraphs.append(
            "A meaningful share of events still leave giver motive underdetermined because the visible text shows support happening without enough donor-side explanation to say more."
        )
    return paragraphs


def build_codebook_section(events: Sequence[MoneyEventGroup], prior_labels: set[str], new_ids: set[str]) -> str:
    grouped_events: Dict[Tuple[str, str], List[MoneyEventGroup]] = defaultdict(list)
    specs = build_codebook_specs()
    for event in events:
        grouped_events[("giver motive", event.giver_motive)].append(event)
        grouped_events[("streamer attractor/trigger", event.streamer_attractor)].append(event)
        grouped_events[("relationship function", event.relationship_function)].append(event)
    if not grouped_events:
        return "No retained codes. No visible money events were detected."
    parts: List[str] = []
    for code_layer, code_label in sorted(grouped_events, key=lambda item: (item[0], item[1])):
        bucket = grouped_events[(code_layer, code_label)]
        spec = specs.get((code_layer, code_label))
        evidence_count = len(bucket)
        stream_coverage_count = len({event.video_id for event in bucket})
        new_this_run = "yes" if code_label not in prior_labels and any(event.video_id in new_ids for event in bucket) else "no"
        parts.extend(
            [
                f"### {code_label}",
                f"- code_layer: `{code_layer}`",
                f"- definition: {spec.definition if spec else 'Visible recurring pattern retained from raw money-event windows.'}",
                f"- inclusion_criteria: {spec.inclusion_criteria if spec else 'Retain when repeated event windows visibly support this interpretation.'}",
                f"- exclusion_criteria: {spec.exclusion_criteria if spec else 'Exclude when a more specific visible interpretation fits better.'}",
                f"- evidence_count: `{evidence_count}`",
                f"- stream_coverage_count: `{stream_coverage_count}`",
                f"- new_this_run: `{new_this_run}`",
            ]
        )
        examples = quoted_examples_for_code(bucket)
        if examples:
            parts.append("- examples:")
            parts.extend(examples)
        parts.append("")
    return "\n".join(parts).strip()


def build_representative_section(events: Sequence[MoneyEventGroup]) -> str:
    if not events:
        return "No representative windows are available because no visible money events were retained."
    parts: List[str] = []
    for event in representative_events(events):
        parts.append(f"### {event.video_id} {event.timecode} ({event.money_signal_type})")
        lines: List[RawEvent] = []
        primary = display_line_for_event(event)
        if primary:
            lines.append(primary)
        seen_refs = {(line.file_path, line.file_line) for line in lines}
        for line in evidence_lines_for_event(event):
            ref = (line.file_path, line.file_line)
            if ref in seen_refs:
                continue
            lines.append(line)
            seen_refs.add(ref)
            if len(lines) >= 4:
                break
        for line in lines:
            parts.append(
                f"- {line.timecode} {line.speaker}: \"{markdown_escape(short_quote(line.text, 180))}\" "
                f"([{Path(line.file_path).name}:{line.file_line}])"
            )
        parts.append(
            f"- Interpretation: giver motive appears as `{event.giver_motive}`, the streamer-side attractor appears as `{event.streamer_attractor}`, and the relationship function appears as `{event.relationship_function}`."
        )
        parts.append("")
    return "\n".join(parts).strip()


def build_coverage_section(
    inventory: Dict[str, dict],
    analysis_ids: Sequence[str],
    events: Sequence[MoneyEventGroup],
) -> str:
    covered_ids = {event.video_id for event in events}
    no_event_ids = [video_id for video_id in analysis_ids if video_id not in covered_ids]
    weak_count = sum(1 for event in events if event.support_level == "money event visible but context window weak")
    lines = [
        f"- money event visible and well-supported: `{sum(1 for event in events if event.support_level == 'money event visible and well-supported')}`",
        f"- money event visible but context window weak: `{weak_count}`",
        f"- no visible money events in available text: `{len(no_event_ids)}`",
    ]
    if no_event_ids:
        sample = ", ".join(no_event_ids[:8])
        suffix = " ..." if len(no_event_ids) > 8 else ""
        lines.append(f"- video_ids with no visible money events detected in available text: `{sample}{suffix}`")
    missing_chat = [video_id for video_id in analysis_ids if not inventory.get(video_id, {}).get("chat_path")]
    missing_playback = [video_id for video_id in analysis_ids if not inventory.get(video_id, {}).get("playback_path")]
    if missing_chat:
        lines.append(f"- chat coverage missing for `{len(missing_chat)}` incorporated video_ids.")
    if missing_playback:
        lines.append(f"- text_playback coverage missing for `{len(missing_playback)}` incorporated video_ids.")
    lines.append("- Platform limits: YouTube chat exports expose explicit `paid_message` / `membership_item` rows unevenly, so some money events are only visible through streamer acknowledgements in subtitles.")
    return "\n".join(lines)


def build_limits_section(events: Sequence[MoneyEventGroup]) -> str:
    unclear = sum(1 for event in events if event.giver_motive == "unclear from visible text")
    weak = sum(1 for event in events if event.support_level == "money event visible but context window weak")
    lines = [
        "- This is a raw-text-only workflow and does not use visual cues, tone of voice, off-platform context, or downstream revenue tables.",
        "- Motive inference is constrained to what can be seen in the text interaction window around each retained money event.",
        "- Some paid moments are likely missing because platform exports do not always preserve paid metadata, especially for gifted memberships and replay-only acknowledgements.",
    ]
    if unclear:
        lines.append(f"- `{unclear}` retained events still have giver motive marked `unclear from visible text` because the donor-side reason was not stated in text.")
    if weak:
        lines.append(f"- `{weak}` retained events rely on weak windows where only one side of the interaction or only a brief acknowledgement was visible.")
    return "\n".join(lines)


def build_markdown(
    talent_name: str,
    analysis_at: datetime,
    update_scope: str,
    new_ids: Sequence[str],
    analysis_ids: Sequence[str],
    events_this_run: Sequence[MoneyEventGroup],
    cumulative_events: Sequence[MoneyEventGroup],
    inventory: Dict[str, dict],
    prior_labels: set[str],
) -> str:
    header = [
        f"Analysis conducted: {analysis_at.strftime('%Y-%m-%d %H:%M %Z')}",
        f"Update scope: {update_scope}",
        f"New video_ids incorporated this run: {len(new_ids)}",
        f"Cumulative video_ids incorporated: {len(analysis_ids)}",
        f"Money events reviewed this run: {len(events_this_run)}",
        f"Cumulative money events reviewed: {len(cumulative_events)}",
        "Evidence basis: raw text_playback + raw chat only",
    ]
    if not cumulative_events:
        overview = [
            "No visible money events were detected in the incorporated raw files, so the monetary relationship picture remains textually underdetermined.",
            "The available raw logs can still be incomplete around paid support, especially when gifted memberships or platform-native monetization cues are visible only in audiovisual overlays or incomplete chat exports.",
        ]
    else:
        overview = build_overview_paragraphs(cumulative_events)
    motive_counts = Counter(event.giver_motive for event in cumulative_events)
    attractor_counts = Counter(event.streamer_attractor for event in cumulative_events)
    function_counts = Counter(event.relationship_function for event in cumulative_events)
    sections = [
        "## 1) Monetary Relationship Overview\n\n" + "\n\n".join(overview),
        "## 2) What Chat Appears To Be Paying For\n\n"
        + (
            "No visible money events detected."
            if not cumulative_events
            else "Strongest recurring giver-motive patterns: "
            + ", ".join(f"`{label}` ({count})" for label, count in strongest_items(motive_counts, 6))
            + "."
            + (" A subset remains `unclear from visible text` where donor reasoning is not stated." if motive_counts.get("unclear from visible text") else "")
        ),
        "## 3) What Streamer Behavior Appears To Attract Money\n\n"
        + (
            "No visible money events detected."
            if not cumulative_events
            else "Recurring streamer-side attractors in visible windows: "
            + ", ".join(f"`{label}` ({count})" for label, count in strongest_items(attractor_counts, 6))
            + "."
        ),
        "## 4) Relational Functions Of Paid Moments\n\n"
        + (
            "No visible money events detected."
            if not cumulative_events
            else "The paid moments most often function as "
            + ", ".join(f"`{label}` ({count})" for label, count in strongest_items(function_counts, 6))
            + "."
        ),
        "## 5) Emergent Codebook\n\n" + build_codebook_section(cumulative_events, prior_labels, set(new_ids)),
        "## 6) Representative Money-Event Windows\n\n" + build_representative_section(cumulative_events),
        "## 7) Coverage and Gaps\n\n" + build_coverage_section(inventory, analysis_ids, cumulative_events),
        "## 8) Evidence Limits and Uncertainty\n\n" + build_limits_section(cumulative_events),
    ]
    return "\n".join(header) + "\n\n" + "\n\n".join(sections) + "\n"


def build_snapshot_markdown(snapshot_at: datetime, current_markdown: str, prompt_text: str) -> str:
    return "\n".join(
        [
            "# Monetary Summary Classification Snapshot",
            f"- Snapshot created: {snapshot_at.strftime('%Y-%m-%d %H:%M:%S %Z')}",
            f"- Snapshot ISO timestamp: `{snapshot_at.isoformat()}`",
            "",
            "## Current cumulative monetary_summary_classification.md",
            "",
            current_markdown.strip(),
            "",
            "## Monetary-analysis prompt used for this run",
            "",
            "```md",
            prompt_text.rstrip(),
            "```",
            "",
        ]
    )


def verify_quote(file_path: str, file_line: int, quote: str) -> bool:
    try:
        with open(file_path, "r", encoding="utf-8-sig", newline="") as handle:
            for line_no, line in enumerate(handle, start=1):
                if line_no == file_line:
                    return quote[:40] in line
    except Exception:
        return False
    return False


def process_talent(talent_dir: Path, prompt_text: str) -> dict:
    talent_name = talent_dir.name
    inventory = build_video_inventory(talent_dir)
    all_video_ids = sorted(inventory)
    if not all_video_ids:
        return {"talent": talent_name, "status": "skipped", "reason": "no eligible raw playback/chat files found"}

    current_dir = talent_dir / "stream_summaries" / "overall_themes" / "monetary_analysis" / "current"
    snapshots_dir = talent_dir / "stream_summaries" / "overall_themes" / "monetary_analysis" / "snapshots"
    markdown_path = current_dir / "monetary_summary_classification.md"
    state_path = current_dir / "monetary_summary_classification_state.json"
    codebook_path = current_dir / "monetary_motive_codebook.csv"
    evidence_path = current_dir / "monetary_event_evidence.csv"

    state = load_json(state_path)
    processed_prior = set(state.get("processed_video_ids", []))
    new_ids = [video_id for video_id in all_video_ids if video_id not in processed_prior]
    outputs_complete = all(nonempty(path) for path in (markdown_path, state_path, codebook_path, evidence_path))
    if not state:
        update_scope = "initial bootstrap"
        analysis_ids = list(all_video_ids)
    else:
        update_scope = "incremental update"
        if not outputs_complete:
            analysis_ids = sorted(processed_prior | set(new_ids))
        else:
            if not new_ids and not FORCE_REFRESH:
                return {
                    "talent": talent_name,
                    "status": "skipped",
                    "reason": "no new video_ids and current outputs are already populated",
                }
            analysis_ids = sorted(processed_prior | set(new_ids))

    playback_paths = [inventory[video_id]["playback_path"] for video_id in analysis_ids if inventory[video_id].get("playback_path")]
    chat_paths = [inventory[video_id]["chat_path"] for video_id in analysis_ids if inventory[video_id].get("chat_path")]
    playback_events = load_playback_events(playback_paths)
    chat_events = load_chat_events(chat_paths)
    events_by_video = dedupe_events(playback_events, chat_events)

    all_events = [event for event in (playback_events + chat_events) if event.video_id in set(analysis_ids)]
    explicit_candidates: List[MoneyCandidate] = []
    explicit_secs_by_video: Dict[str, List[float]] = defaultdict(list)
    for event in all_events:
        signal_type = explicit_money_signal(event)
        if not signal_type:
            continue
        explicit_candidates.append(candidate_from_event(event, signal_type, "explicit"))
        explicit_secs_by_video[event.video_id].append(event.sec)

    contextual_candidates: List[MoneyCandidate] = []
    for event in all_events:
        signal_type = strong_textual_money_signal(event)
        if not signal_type:
            continue
        if nearby_explicit_signal(event.video_id, event.sec, explicit_secs_by_video):
            continue
        contextual_candidates.append(candidate_from_event(event, signal_type, "contextual"))

    candidates = explicit_candidates + contextual_candidates
    cumulative_events = build_money_event_groups(candidates, events_by_video, set(new_ids))
    events_this_run = [event for event in cumulative_events if event.video_id in set(new_ids)]

    prior_labels = set() if not state else load_existing_code_labels(codebook_path)
    analysis_at = datetime.now().astimezone()
    markdown = build_markdown(
        talent_name=talent_name,
        analysis_at=analysis_at,
        update_scope=update_scope,
        new_ids=new_ids,
        analysis_ids=analysis_ids,
        events_this_run=events_this_run,
        cumulative_events=cumulative_events,
        inventory=inventory,
        prior_labels=prior_labels,
    )
    write_text(markdown_path, markdown)

    evidence_rows = build_evidence_rows(talent_name, cumulative_events)
    write_csv(
        evidence_path,
        [
            "talent",
            "video_id",
            "time_in_seconds",
            "timecode",
            "source",
            "speaker",
            "quote",
            "window_role",
            "money_signal_type",
            "code_layer",
            "code_label",
            "event_group_id",
            "inferred_giver_motive",
            "inferred_streamer_attractor",
            "inferred_relationship_function",
            "incorporated_in_run",
            "file_path",
            "file_line",
        ],
        evidence_rows,
    )

    codebook_rows = build_codebook_rows(talent_name, cumulative_events, prior_labels, set(new_ids))
    write_csv(
        codebook_path,
        [
            "talent",
            "code_label",
            "code_layer",
            "definition",
            "inclusion_criteria",
            "exclusion_criteria",
            "evidence_count",
            "stream_coverage_count",
            "new_this_run",
            "memo_notes",
        ],
        codebook_rows,
    )

    snapshot_at = datetime.now().astimezone()
    snapshot_name = "monetary_summary_classification_" + snapshot_at.strftime("%Y-%m-%d_%H-%M-%S_%z") + ".md"
    snapshot_path = snapshots_dir / snapshot_name
    write_text(snapshot_path, build_snapshot_markdown(snapshot_at, markdown, prompt_text))

    notes = []
    if not cumulative_events:
        notes.append("no visible money events detected")
    if not outputs_complete and state:
        notes.append("rebuild triggered because one or more current outputs were missing or empty")
    if any(event.giver_motive == "unclear from visible text" for event in cumulative_events):
        notes.append("some giver motives remain unclear from visible text")
    if any(event.support_level == "money event visible but context window weak" for event in cumulative_events):
        notes.append("some money-event windows remain weak")

    state_out = {
        "talent": talent_name,
        "analysis_conducted_at": analysis_at.isoformat(),
        "update_scope": update_scope,
        "processed_video_ids": analysis_ids,
        "new_video_ids_this_run": new_ids,
        "money_events_reviewed_this_run": len(events_this_run),
        "money_events_reviewed_cumulative": len(cumulative_events),
        "latest_snapshot_path": str(snapshot_path),
        "notes": notes,
    }
    write_json(state_path, state_out)

    spot_check = {}
    if cumulative_events:
        for event in representative_events(cumulative_events, limit=8):
            candidate_lines = [display_line_for_event(event)] + evidence_lines_for_event(event)
            for line in candidate_lines:
                if not line:
                    continue
                if verify_quote(line.file_path, line.file_line, line.text):
                    spot_check = {
                        "video_id": event.video_id,
                        "quote": line.text,
                        "file_path": line.file_path,
                        "file_line": line.file_line,
                    }
                    break
            if spot_check:
                break

    return {
        "talent": talent_name,
        "status": "written",
        "new_video_count": len(new_ids),
        "cumulative_video_count": len(analysis_ids),
        "money_events_reviewed_this_run": len(events_this_run),
        "money_events_reviewed_cumulative": len(cumulative_events),
        "current_output_paths": [
            str(markdown_path),
            str(state_path),
            str(codebook_path),
            str(evidence_path),
        ],
        "snapshot_path": str(snapshot_path),
        "spot_check": spot_check,
    }


def print_completion(results: Sequence[dict]) -> None:
    for result in results:
        print(f"Talent: {result['talent']}")
        if result["status"] != "written":
            print(f"Status: {result['status']}")
            print(f"Reason: {result.get('reason', '')}")
            print()
            continue
        print(f"New video count: {result['new_video_count']}")
        print(f"Cumulative video count: {result['cumulative_video_count']}")
        print(f"Money events reviewed this run: {result['money_events_reviewed_this_run']}")
        print(f"Money events reviewed cumulative: {result['money_events_reviewed_cumulative']}")
        print("Current output paths:")
        for path in result["current_output_paths"]:
            print(f"  - {path}")
        print(f"Snapshot path: {result['snapshot_path']}")
        spot = result.get("spot_check") or {}
        if spot.get("file_path"):
            print(
                "Spot-check raw quote: "
                f"{spot.get('quote', '')} "
                f"-> {spot.get('file_path', '')}:{spot.get('file_line', '')}"
            )
        else:
            print("Spot-check raw quote: none verified")
        print()


def main() -> None:
    prompt_text = read_text(PROMPT_SPEC_PATH)
    results = [process_talent(talent_dir, prompt_text) for talent_dir in detect_talents()]
    print_completion(results)


if __name__ == "__main__":
    main()
