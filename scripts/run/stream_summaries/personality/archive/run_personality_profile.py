#!/usr/bin/env python3
"""Generate personality profile + evidence log for 4 hardcoded talents.

This script follows the prompt spec in:
prompts/stream_summaries/personality/profile.md
"""

from __future__ import annotations

import csv
import glob
import math
import os
import re
from collections import Counter, defaultdict
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, Iterable, List, Tuple


DATA_ROOT = "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data"
TALENTS = [
    {
        "name": "Avaritia Hawthorne 【Variance Project】",
        "path": os.path.join(DATA_ROOT, "Avaritia Hawthorne 【Variance Project】"),
        "tokens": ("avaritia", "ava"),
    },
    {
        "name": "Katya Sable 【Variance Project】",
        "path": os.path.join(DATA_ROOT, "Katya Sable 【Variance Project】"),
        "tokens": ("katya", "sable"),
    },
    {
        "name": "Leia Memoria【Variance Project】",
        "path": os.path.join(DATA_ROOT, "Leia Memoria【Variance Project】"),
        "tokens": ("leia", "memoria"),
    },
    {
        "name": "Terberri Solaris Ch",
        "path": os.path.join(DATA_ROOT, "Terberri Solaris Ch"),
        "tokens": ("terberri", "solar"),
    },
]

BASE_CODES = {
    "PLAYFUL_CHAOS": {
        "definition": "A shared comedic mode where streamer/chat intentionally produce non-literal mismatch text that disrupts literal conversation for playful bonding.",
        "inclusion": [
            "Require at least 1 non-literal mismatch marker: category collision, impossible/fantastical claim used jokingly, deliberate contradiction, or mock-serious language for trivial events.",
            "Plus at least 1 uptake marker: callback/one-up by another speaker, laughter marker (`lol`, `lmao`, `haha`), emote-like playful response, or explicit \"chaos/what is happening\" framing.",
        ],
        "exclusion": [
            "Simple excitement only (e.g., `LET'S GO!!!`) without non-literal mismatch markers.",
            "Routine off-topic chat with no joke continuation.",
            "Hostile conflict without affiliative play cues.",
        ],
    },
    "WARM_NURTURANCE": {
        "definition": "Caretaking, reassurance, tenderness, or protective framing toward chat/community.",
        "inclusion": [
            "At least 1 care marker (`it's okay`, `take your time`, `proud of you`, `you're fine`, `be safe`) directed to a person/group.",
            "Or concern-check + comfort sequence (`are you okay?` followed by reassurance).",
        ],
        "exclusion": [
            "Generic politeness (`thanks`, `hi`) without explicit care content.",
            "Pure logistics/mod instructions with no emotional support intent.",
        ],
    },
    "INTIMATE_SELF_DISCLOSURE": {
        "definition": "Personal revelation of feelings, vulnerabilities, private experiences, or identity-significant reflections.",
        "inclusion": [
            "First-person statement + internal-state marker (`I feel`, `I'm scared`, `I've been struggling`, `I was stressed`, `I cried`, `I needed`) with personal stakes.",
            "Life-context details that make the statement personally consequential (health, finances, burnout, family, identity, grief, insecurity).",
        ],
        "exclusion": [
            "Surface status updates (`starting soon`, `be right back`, `mic issue`).",
            "Lore/performance lines that do not disclose real felt experience.",
        ],
    },
    "PERFORMATIVE_THEATRICALITY": {
        "definition": "Deliberate roleplay/performance voice, dramatic framing, or stage-like presentation style.",
        "inclusion": [
            "At least 1 stylization marker: mock proclamation, character voice, ceremonial phrasing, scripted bit framing, or dramatic register shift.",
            "Often includes performative audience address (`ladies and gentlemen`, `welcome to`, `behold`, etc.) or text stage cues.",
        ],
        "exclusion": [
            "Plain commentary/instruction without roleplay or dramatic register.",
            "Excitement alone without theatrical framing.",
        ],
    },
    "COMMANDING_HOST_ENERGY": {
        "definition": "Strong host control over pace, norms, agenda, and audience direction.",
        "inclusion": [
            "Explicit directive or norm statement (`no spoilers`, `listen up`, `we're moving on`, `do X now`, `stop Y`).",
            "Agenda-control markers: segment transitions, priority setting, pacing calls, rule reinforcement.",
        ],
        "exclusion": [
            "Suggestions phrased as low-commitment options.",
            "Chat-led drift where streamer does not assert direction.",
        ],
    },
    "CO_REGULATION_WITH_CHAT": {
        "definition": "Streamer and chat mutually modulate emotional intensity (calming, hyping, grounding, recovering).",
        "inclusion": [
            "Minimum 2-turn reciprocal pattern: one side signals distress/hype/confusion, other side responds with calming/grounding/amplifying language.",
            "Evidence of adjustment in subsequent turns (tone softens, focus returns, hype synchronizes, conflict de-escalates).",
        ],
        "exclusion": [
            "One-sided venting/hype with no visible response shift.",
            "Single comfort/hype line not embedded in an interaction loop.",
        ],
    },
    "BOUNDARY_SETTING": {
        "definition": "Explicit limit-setting around behavior, topics, expectations, or parasocial conduct.",
        "inclusion": [
            "Clear limit language (`do not`, `don't`, `not okay`, `I won't`, `we don't do that here`).",
            "Norm enforcement or consent boundary with target behavior/topic specified.",
        ],
        "exclusion": [
            "Soft preferences (`I'd rather not`) without enforceable limit.",
            "General policy text not tied to interaction context.",
        ],
    },
    "TEASING_BANTER": {
        "definition": "Playful mockery, friendly roasting, or ironic challenge exchanges that signal relational familiarity.",
        "inclusion": [
            "Playful jab/roast + affiliative cue (laughter marker, emotes, known in-joke, affectionate handle).",
            "Prefer reciprocal evidence: target responds in-kind or positively.",
        ],
        "exclusion": [
            "Insults/slurs/derogation without affiliative cues.",
            "Mockery followed by defensiveness, silence, or conflict escalation.",
        ],
    },
    "APPRECIATION_RITUAL": {
        "definition": "Repeated gratitude scripts and recognition routines that reinforce community belonging.",
        "inclusion": [
            "Recurrent gratitude template across multiple streams (same or similar phrasing patterns).",
            "Recognition routines: donor/member/name callouts, milestone thanks, closing thank-you sequences.",
        ],
        "exclusion": [
            "Single isolated `thank you` with no repeated structure.",
            "Pure acknowledgement that lacks gratitude language.",
        ],
    },
    "MONETARY_MEANING_FRAME": {
        "definition": "The social meaning assigned to money moments (care, joke, obligation, request leverage, celebration, tax, etc.).",
        "inclusion": [
            "Monetary cue (`paid_message`, amount mention, membership gift) plus meaning cue (e.g., support, joke-tax, apology, celebration, request, duty).",
            "Streamer or chat explicitly interprets why money is being sent or how it should be understood.",
        ],
        "exclusion": [
            "Amount-only mention with no interpretive framing.",
            "Purely technical payment processing comments.",
        ],
    },
}

CODE_ORDER = [
    "PLAYFUL_CHAOS",
    "WARM_NURTURANCE",
    "INTIMATE_SELF_DISCLOSURE",
    "PERFORMATIVE_THEATRICALITY",
    "COMMANDING_HOST_ENERGY",
    "CO_REGULATION_WITH_CHAT",
    "BOUNDARY_SETTING",
    "TEASING_BANTER",
    "APPRECIATION_RITUAL",
    "MONETARY_MEANING_FRAME",
]


LAUGH_RE = re.compile(r"\b(lol|lmao|rofl|haha|hehe|kek|www)\b", re.I)
CHAOS_RE = re.compile(
    r"\b(chaos|cursed|unhinged|gremlin|goblin|sacrifice|eat you|blood|apocalypse|world burn|what is happening|wtf)\b",
    re.I,
)
CARE_RE = re.compile(
    r"\b(it'?s ok(?:ay)?|take your time|proud of you|you(?:'re| are) fine|be safe|are you ok|are you okay|rest well|drink water|you got this|no worries|take care)\b",
    re.I,
)
STAKE_RE = re.compile(
    r"\b(health|money|finance|rent|burnout|family|grief|insecure|identity|stress|anxiety|depress|mental)\b",
    re.I,
)
THEATRICAL_RE = re.compile(
    r"\b(ladies and gentlemen|welcome to|behold|i proclaim|my dear|minions|mortals|ceremony|grand)\b",
    re.I,
)
DIRECTIVE_RE = re.compile(
    r"\b("
    r"no spoilers|listen up|we('?| a)re moving on|mods\b|focus up|rule reminder|move on|drop it|be respectful|"
    r"keep it (civil|respectful)|no backseating|please stop|"
    r"stop (spamming|backseating|fighting)|"
    r"don't (spam|backseat|be weird|do that)|"
    r"do not (spam|backseat|do that)"
    r")\b",
    re.I,
)
BOUNDARY_RE = re.compile(
    r"\b("
    r"not okay|i won't (talk|do|engage|answer)|we don't do that here|"
    r"boundary|stop asking|don't ask|no backseating|no spoilers|"
    r"don't (spam|backseat|be weird)|do not (spam|backseat|be weird)"
    r")\b",
    re.I,
)
TEASE_RE = re.compile(r"\b(nerd|bozo|clown|dummy|stinky|loser|skill issue|coward)\b", re.I)
GRAT_RE = re.compile(r"\b(thank you|thanks so much|appreciate (you|it)|tysm|thank u)\b", re.I)
MONEY_CUE_RE = re.compile(r"\b(super ?chat|donation|membership|member|gift(ed)?|money|support)\b", re.I)
REQUEST_RE = re.compile(r"\b(can you|could you|please|sing|play|react|request|read this)\b", re.I)
HYPE_OR_DISTRESS_RE = re.compile(
    r"\b(scared|sad|stressed|anxious|tired|overwhelmed|confused|omg|hype|let's go|lets go)\b",
    re.I,
)
SYNC_RE = re.compile(r"\b(you got this|it's okay|calm|breathe|lets go|let's go|hype|we got this)\b", re.I)
CELEBRATE_RE = re.compile(r"\b(congrats|congratulations|happy|celebrate|bday|birthday|anniversary)\b", re.I)


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


def normalize_speaker(speaker: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", (speaker or "").lower().lstrip("@"))


def normalize_text(text: str) -> str:
    s = (text or "").lower().strip()
    s = re.sub(r"https?://\S+", "", s)
    s = re.sub(r"[^a-z0-9\s']", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def is_streamer(event: Event, tokens: Iterable[str]) -> bool:
    speaker = normalize_speaker(event.speaker)
    if speaker in {"stream", "host", "streamer"}:
        return True
    for token in tokens:
        if token in speaker:
            return True
    return False


def load_playback_events(talent_name: str, playback_files: List[str]) -> Tuple[List[Event], Dict[str, List[Event]]]:
    events: List[Event] = []
    by_video: Dict[str, List[Event]] = defaultdict(list)
    for path in playback_files:
        with open(path, "r", encoding="utf-8-sig", newline="") as f:
            reader = csv.DictReader(f)
            for line_no, row in enumerate(reader, start=2):
                text = (row.get("text") or "").strip()
                if not text or text.upper() == "NA":
                    continue
                vid = (row.get("video_id") or "").strip()
                sec = parse_float(row.get("sec", ""))
                if math.isnan(sec):
                    continue
                source = (row.get("source") or "chat").strip().lower()
                if source not in {"chat", "subtitle"}:
                    source = "chat"
                ev = Event(
                    talent=talent_name,
                    video_id=vid,
                    sec=sec,
                    timecode=(row.get("timecode") or sec_to_timecode(sec)).strip(),
                    source=source,
                    speaker=(row.get("speaker") or "").strip() or "UNKNOWN",
                    text=text,
                    message_type=(row.get("message_type") or "").strip(),
                    file_path=path,
                    file_line=line_no,
                )
                events.append(ev)
                by_video[vid].append(ev)
    for vid in by_video:
        by_video[vid].sort(key=lambda e: e.sec)
    return events, by_video


def load_money_rows(money_path: str) -> List[dict]:
    if not os.path.exists(money_path):
        return []
    rows = []
    with open(money_path, "r", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
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
                }
            )
    rows.sort(key=lambda r: (r["video_id"], r["time_in_seconds"]))
    return rows


def sample_evenly(rows: List[dict], n: int) -> List[dict]:
    if len(rows) <= n:
        return list(rows)
    out = []
    for i in range(n):
        idx = int(round(i * (len(rows) - 1) / (n - 1)))
        out.append(rows[idx])
    return out


def uniq_examples(items: List[dict], n: int = 3) -> List[dict]:
    out = []
    seen = set()
    for item in items:
        vid = item["video_id"]
        if vid not in seen:
            out.append(item)
            seen.add(vid)
        if len(out) >= n:
            return out
    for item in items:
        if len(out) >= n:
            break
        if item not in out:
            out.append(item)
    return out


def short_quote(text: str, limit: int = 240) -> str:
    t = (text or "").replace("\n", " ").strip()
    if len(t) <= limit:
        return t
    return t[: limit - 3] + "..."


def build_marker_label(text: str, max_words: int = 6) -> str:
    words = normalize_text(text).split()
    if not words:
        return "marker"
    return " ".join(words[:max_words])


def detect_code_hits(talent: dict, by_video: Dict[str, List[Event]]) -> Tuple[Dict[str, List[dict]], List[Event]]:
    tokens = talent["tokens"]
    hits: Dict[str, List[dict]] = defaultdict(list)
    seen = set()
    streamer_events: List[Event] = []

    appreciation_candidates: List[Tuple[Event, str]] = []
    appreciation_templates: Dict[str, set] = defaultdict(set)

    def push(code: str, ev: Event, marker: str, role: str) -> None:
        key = (code, ev.video_id, round(ev.sec, 3), ev.speaker, ev.text, marker, role)
        if key in seen:
            return
        seen.add(key)
        hits[code].append(
            {
                "talent": talent["name"],
                "code": code,
                "marker": marker,
                "video_id": ev.video_id,
                "time_in_seconds": f"{ev.sec:.3f}",
                "timecode": ev.timecode or sec_to_timecode(ev.sec),
                "source": ev.source,
                "speaker": ev.speaker,
                "quote": ev.text,
                "evidence_role": role,
                "file_path": ev.file_path,
                "file_line": ev.file_line,
            }
        )

    for video_id, rows in by_video.items():
        for idx, ev in enumerate(rows):
            txt = ev.text
            streamer = is_streamer(ev, tokens)
            if streamer:
                streamer_events.append(ev)
            nearby = rows[max(0, idx - 3) : min(len(rows), idx + 4)]
            nearby = [x for x in nearby if x is not ev and abs(x.sec - ev.sec) <= 20]

            if CHAOS_RE.search(txt):
                if any(LAUGH_RE.search(x.text) or CHAOS_RE.search(x.text) for x in nearby):
                    push("PLAYFUL_CHAOS", ev, "nonliteral+uptake", "primary")

            if CARE_RE.search(txt):
                push("WARM_NURTURANCE", ev, "care-language", "primary")

            strong_disclosure = re.search(
                r"\b(i'?m scared|i am scared|i'?m worried|i am worried|i'?ve been struggling|i was stressed|i cried|i needed|burnout|depressed|anxious)\b",
                txt,
                re.I,
            )
            feel_with_stakes = re.search(r"\bi feel\b", txt, re.I) and STAKE_RE.search(txt)
            if streamer and (strong_disclosure or feel_with_stakes):
                push("INTIMATE_SELF_DISCLOSURE", ev, "first-person-stakes", "primary")

            if THEATRICAL_RE.search(txt) or (
                streamer and txt.count("!") >= 3 and re.search(r"\b(behold|welcome|mortals|minions)\b", txt, re.I)
            ):
                push("PERFORMATIVE_THEATRICALITY", ev, "dramatic-register", "primary")

            if streamer and DIRECTIVE_RE.search(txt):
                push("COMMANDING_HOST_ENERGY", ev, "directive-or-norm", "primary")

            if streamer and BOUNDARY_RE.search(txt):
                push("BOUNDARY_SETTING", ev, "limit-language", "primary")

            if TEASE_RE.search(txt):
                if LAUGH_RE.search(txt) or any(LAUGH_RE.search(x.text) or TEASE_RE.search(x.text) for x in nearby):
                    push("TEASING_BANTER", ev, "playful-jab-with-uptake", "primary")

            if streamer and GRAT_RE.search(txt):
                template = build_marker_label(txt, max_words=7)
                appreciation_candidates.append((ev, template))
                appreciation_templates[template].add(video_id)

            if not streamer and HYPE_OR_DISTRESS_RE.search(txt):
                for later in rows[idx + 1 : idx + 8]:
                    if later.sec - ev.sec > 30:
                        break
                    if is_streamer(later, tokens) and (SYNC_RE.search(later.text) or CARE_RE.search(later.text) or LAUGH_RE.search(later.text)):
                        push("CO_REGULATION_WITH_CHAT", later, "chat-signal->stream-adjustment", "primary")
                        push("CO_REGULATION_WITH_CHAT", ev, "chat-signal", "supporting")
                        break

    for ev, template in appreciation_candidates:
        if len(appreciation_templates[template]) >= 3:
            push("APPRECIATION_RITUAL", ev, f"template:{template}", "primary")

    return hits, streamer_events


def add_monetary_hits(
    talent: dict,
    money_rows: List[dict],
    by_video: Dict[str, List[Event]],
    hits: Dict[str, List[dict]],
) -> Dict[str, int]:
    sampled = sample_evenly(money_rows, 20)
    meaning_counts = Counter()
    seen = set(
        (h["code"], h["video_id"], h["time_in_seconds"], h["speaker"], h["quote"], h["marker"], h["evidence_role"])
        for h in hits["MONETARY_MEANING_FRAME"]
    )

    def push_row(row: dict) -> None:
        key = (row["code"], row["video_id"], row["time_in_seconds"], row["speaker"], row["quote"], row["marker"], row["evidence_role"])
        if key in seen:
            return
        seen.add(key)
        hits["MONETARY_MEANING_FRAME"].append(row)

    tokens = talent["tokens"]
    for m in sampled:
        video_id = m["video_id"]
        t = m["time_in_seconds"]
        donation_msg = m["message"] or ""
        rows = by_video.get(video_id, [])
        pre = [r for r in rows if t - 120 <= r.sec < t - 5]
        post = [r for r in rows if t <= r.sec <= t + 120]
        streamer_post = [r for r in post if is_streamer(r, tokens)]

        labels = []
        if REQUEST_RE.search(donation_msg):
            labels.append("support-as-request")
        if CARE_RE.search(donation_msg) or re.search(r"\b(take care|get well|rest|proud)\b", donation_msg, re.I):
            labels.append("support-as-care")
        if CELEBRATE_RE.search(donation_msg):
            labels.append("celebration")
        if CHAOS_RE.search(donation_msg) or LAUGH_RE.search(donation_msg):
            labels.append("support-as-performance")

        if not labels:
            if any(CARE_RE.search(r.text) for r in streamer_post):
                labels.append("support-as-care")
            elif any(CHAOS_RE.search(r.text) or THEATRICAL_RE.search(r.text) for r in streamer_post):
                labels.append("support-as-performance")
            elif any(REQUEST_RE.search(r.text) for r in pre):
                labels.append("support-as-request")
            else:
                labels.append("insufficient evidence")

        for label in labels:
            meaning_counts[label] += 1

        marker = "|".join(labels)
        push_row(
            {
                "talent": talent["name"],
                "code": "MONETARY_MEANING_FRAME",
                "marker": marker,
                "video_id": video_id,
                "time_in_seconds": f"{t:.3f}",
                "timecode": m["timecode"],
                "source": "chat",
                "speaker": m["username"],
                "quote": donation_msg or f"paid event {m['paid_amount']}",
                "evidence_role": "primary",
                "file_path": os.path.join(talent["path"], "stream_summaries", "overall_themes", "money_timestamps.csv"),
                "file_line": 0,
            }
        )

        if streamer_post:
            r = streamer_post[0]
            push_row(
                {
                    "talent": talent["name"],
                    "code": "MONETARY_MEANING_FRAME",
                    "marker": f"post-response:{marker}",
                    "video_id": r.video_id,
                    "time_in_seconds": f"{r.sec:.3f}",
                    "timecode": r.timecode,
                    "source": r.source,
                    "speaker": r.speaker,
                    "quote": r.text,
                    "evidence_role": "supporting",
                    "file_path": r.file_path,
                    "file_line": r.file_line,
                }
            )

    meaning_counts["sampled_events"] = len(sampled)
    meaning_counts["total_events"] = len(money_rows)
    return dict(meaning_counts)


def choose_idiosyncrasies(
    talent_name: str,
    streamer_events: List[Event],
    all_phrase_counts: Dict[str, Counter],
) -> Tuple[List[dict], List[dict]]:
    phrase_rows: Dict[str, List[Event]] = defaultdict(list)
    phrase_vids: Dict[str, set] = defaultdict(set)
    own_counter = all_phrase_counts[talent_name]
    other_avg = Counter()
    other_names = [k for k in all_phrase_counts if k != talent_name]
    for phrase in own_counter:
        vals = [all_phrase_counts[n][phrase] for n in other_names]
        other_avg[phrase] = sum(vals) / max(1, len(vals))

    for ev in streamer_events:
        norm = normalize_text(ev.text)
        words = norm.split()
        if len(words) < 3 or len(words) > 14:
            continue
        if any(w in {"thank", "you", "guys", "chat"} for w in words[:2]):
            continue
        phrase = " ".join(words[:6])
        phrase_rows[phrase].append(ev)
        phrase_vids[phrase].add(ev.video_id)

    ranked = []
    for phrase, rows in phrase_rows.items():
        if len(rows) < 3 or len(phrase_vids[phrase]) < 2:
            continue
        score = (own_counter[phrase] + len(rows)) * (len(phrase_vids[phrase])) - other_avg[phrase]
        ranked.append((score, phrase))
    ranked.sort(reverse=True)

    selected = [p for _, p in ranked[:3]]
    if len(selected) < 3:
        fallback = []
        for ev in streamer_events:
            if ev.text.count("!") >= 2:
                fallback.append(ev)
        if fallback:
            selected.append("high-exclamation-delivery")
            phrase_rows["high-exclamation-delivery"] = fallback
            phrase_vids["high-exclamation-delivery"] = {x.video_id for x in fallback}
    selected = selected[:3]

    markers = []
    evidence_rows = []
    for phrase in selected:
        rows = phrase_rows.get(phrase, [])
        examples = uniq_examples(
            [
                {
                    "video_id": r.video_id,
                    "timecode": r.timecode,
                    "speaker": r.speaker,
                    "quote": short_quote(r.text),
                }
                for r in rows
            ],
            n=3,
        )
        if not examples:
            continue
        marker_label = phrase
        distinct_note = (
            f"Appears {len(rows)} times across {len(phrase_vids.get(phrase, []))} streams; lower frequency in peer corpora."
        )
        markers.append(
            {
                "marker": marker_label,
                "why": distinct_note,
                "examples": examples,
            }
        )
        for r in rows[:25]:
            evidence_rows.append(
                {
                    "talent": talent_name,
                    "code": "IDIOSYNCRASY",
                    "marker": marker_label,
                    "video_id": r.video_id,
                    "time_in_seconds": f"{r.sec:.3f}",
                    "timecode": r.timecode,
                    "source": r.source,
                    "speaker": r.speaker,
                    "quote": r.text,
                    "evidence_role": "primary",
                    "file_path": r.file_path,
                    "file_line": r.file_line,
                }
            )
    return markers, evidence_rows


def build_phrase_counter(streamer_events: List[Event]) -> Counter:
    counter = Counter()
    for ev in streamer_events:
        norm = normalize_text(ev.text)
        words = norm.split()
        if len(words) < 3:
            continue
        phrase = " ".join(words[:6])
        counter[phrase] += 1
    return counter


def code_counts_from_hits(hits: Dict[str, List[dict]]) -> Dict[str, int]:
    out = {}
    for code in CODE_ORDER:
        out[code] = sum(1 for h in hits.get(code, []) if h["evidence_role"] == "primary")
    return out


def top_strengths(code_counts: Dict[str, int]) -> List[Tuple[str, int]]:
    pairs = sorted(code_counts.items(), key=lambda x: x[1], reverse=True)
    return [p for p in pairs if p[1] > 0][:3]


def confidence_label(total_primary: int) -> str:
    if total_primary >= 200:
        return "High"
    if total_primary >= 80:
        return "Medium"
    return "Low"


def format_example_row(row: dict) -> str:
    return f'[{row["timecode"]}] {row["speaker"]} ({row["video_id"]}): "{short_quote(row["quote"])}"'


def write_evidence_csv(path: str, rows: List[dict]) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    columns = [
        "talent",
        "code",
        "marker",
        "video_id",
        "time_in_seconds",
        "timecode",
        "source",
        "speaker",
        "quote",
        "evidence_role",
    ]
    with open(path, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=columns)
        writer.writeheader()
        for r in rows:
            writer.writerow({k: r.get(k, "") for k in columns})


def build_distinctiveness_section(
    talent_name: str,
    code_counts: Dict[str, Dict[str, int]],
) -> str:
    me = code_counts[talent_name]
    others = [n for n in code_counts if n != talent_name]
    ranked = sorted(me.items(), key=lambda x: x[1], reverse=True)
    top_two = [x for x in ranked if x[1] > 0][:2]
    compare_targets = others[:2]

    lines = []
    for code, count in top_two:
        other_bits = []
        for o in compare_targets:
            other_bits.append(f"{o}: {code_counts[o].get(code, 0)}")
        lines.append(f"- `{code}` appears at {count} primary hits ({'; '.join(other_bits)}).")

    if not lines:
        lines.append("- Insufficient evidence for robust cross-streamer contrast from text-only signals.")

    nearest = min(
        others,
        key=lambda o: sum(abs(me[c] - code_counts[o].get(c, 0)) for c in CODE_ORDER),
    )
    diff_code = max(CODE_ORDER, key=lambda c: abs(me[c] - code_counts[nearest].get(c, 0)))
    lines.append(
        f'- Could be confused with {nearest}, but differs on `{diff_code}` ({talent_name}: {me[diff_code]} vs {nearest}: {code_counts[nearest].get(diff_code, 0)}).'
    )
    return "\n".join(lines)


def write_profile_markdown(
    path: str,
    talent: dict,
    code_hits: Dict[str, List[dict]],
    idio_markers: List[dict],
    money_summary: Dict[str, int],
    code_counts: Dict[str, Dict[str, int]],
    streams_scanned: int,
) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    analysis_conducted_at = datetime.now().astimezone().strftime("%Y-%m-%d %H:%M %Z")
    counts = code_counts[talent["name"]]
    strengths = top_strengths(counts)
    strength_txt = ", ".join(f"{c} ({n})" for c, n in strengths) if strengths else "insufficient evidence"
    conf = confidence_label(sum(counts.values()))

    lines = []
    lines.append(f"Analysis conducted: {analysis_conducted_at}")
    lines.append("")
    lines.append("## 1) Streamer Personality Signature (text-derived)")
    lines.append(
        f"Across {streams_scanned} streams, the strongest text-derived signature is: {strength_txt}. Confidence: {conf}. This profile is constrained to text evidence only; visual expression, body language, and full vocal prosody cannot be directly observed, so affect intensity and performative intent may be under- or over-estimated in edge cases."
    )
    lines.append("")
    lines.append("## 2) Affective-Relational Codebook (Predefined)")
    for code in CODE_ORDER:
        meta = BASE_CODES[code]
        primary_rows = [r for r in code_hits.get(code, []) if r["evidence_role"] == "primary"]
        ex = uniq_examples(primary_rows, 3)
        lines.append(f"### {code}")
        lines.append(f"- Baseline definition: {meta['definition']}")
        lines.append("- Inclusion criteria:")
        for item in meta["inclusion"]:
            lines.append(f"  - {item}")
        lines.append("- Exclusion criteria:")
        for item in meta["exclusion"]:
            lines.append(f"  - {item}")
        lines.append(f"- Observed count: {len(primary_rows)}")
        if ex:
            lines.append("- Examples:")
            for row in ex:
                lines.append(f"  - {format_example_row(row)}")
        else:
            lines.append("- Examples: no qualifying evidence")
        lines.append("")

    lines.append("## 3) Idiosyncrasies and Unique Charm Markers")
    if idio_markers:
        for marker in idio_markers:
            lines.append(f"### {marker['marker']}")
            lines.append(f"- Why distinctive: {marker['why']}")
            lines.append("- Supporting examples:")
            for ex in marker["examples"]:
                lines.append(
                    f'  - [{ex["timecode"]}] {ex["speaker"]} ({ex["video_id"]}): "{short_quote(ex["quote"])}"'
                )
    else:
        lines.append("No stable recurring marker met the repetition threshold; insufficient evidence.")
    lines.append("")

    sampled = money_summary.get("sampled_events", 0)
    total_money = money_summary.get("total_events", 0)
    care = money_summary.get("support-as-care", 0)
    perf = money_summary.get("support-as-performance", 0)
    req = money_summary.get("support-as-request", 0)
    cele = money_summary.get("celebration", 0)
    insuff = money_summary.get("insufficient evidence", 0)
    lines.append("## 4) Personality-Monetary Interaction Link")
    lines.append(
        f"Sampled paid events: {sampled} of {total_money} total. Pre-window (120s), donation moment, and post-window (120s) were compared for each sample."
    )
    lines.append(
        f"Recurring patterns: support-as-care={care}, support-as-performance={perf}, support-as-request={req}, celebration={cele}, insufficient-evidence={insuff}."
    )
    lines.append(
        "Interpretation: support-as-care is inferred when donations or immediate responses include reassurance/caretaking language; support-as-performance when joke/theatrical framing dominates; support-as-request when donor asks for action and interaction pivots around that request."
    )
    lines.append("")

    lines.append("## 5) Distinctiveness Test (cross-streamer)")
    lines.append(build_distinctiveness_section(talent["name"], code_counts))
    lines.append("")

    lines.append("## 6) Validity and Uncertainty Notes")
    lines.append("- Strongest evidence-backed conclusions:")
    strong = top_strengths(counts)
    if strong:
        for code, n in strong:
            lines.append(f"  - `{code}` is repeatedly observed ({n} primary evidence rows).")
    else:
        lines.append("  - Insufficient evidence for strong repeated conclusions.")
    lines.append("- Uncertainty points (text-only limitations):")
    lines.append("  - Sarcasm vs sincerity can be ambiguous without prosody/visual cues.")
    lines.append("  - Emotional intensity calibration is uncertain without vocal dynamics.")
    lines.append("  - Some relational cues may be artifacts of chat pace or transcript sparsity.")

    with open(path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines).strip() + "\n")


def collect_stream_counts(
    summary_files: List[str], playback_files: List[str], chat_files: List[str]
) -> int:
    ids = set()
    for path in summary_files + playback_files + chat_files:
        base = os.path.basename(path)
        base = re.sub(r"_summary\.md$", "", base)
        base = re.sub(r"_chat\.csv$", "", base)
        base = re.sub(r"\.csv$", "", base)
        ids.add(base)
    return len(ids)


def run() -> None:
    talent_data = {}
    all_phrase_counts = {}

    for talent in TALENTS:
        base = talent["path"]
        summary_files = sorted(glob.glob(os.path.join(base, "stream_summaries", "stream_summary_codex", "*.md")))
        playback_files = sorted(glob.glob(os.path.join(base, "text_playback", "*.csv")))
        chat_files = sorted(glob.glob(os.path.join(base, "Chat", "Original", "*_chat.csv")))
        money_path = os.path.join(base, "stream_summaries", "overall_themes", "money_timestamps.csv")

        events, by_video = load_playback_events(talent["name"], playback_files)
        money_rows = load_money_rows(money_path)
        code_hits, streamer_events = detect_code_hits(talent, by_video)
        money_summary = add_monetary_hits(talent, money_rows, by_video, code_hits)
        all_phrase_counts[talent["name"]] = build_phrase_counter(streamer_events)

        talent_data[talent["name"]] = {
            "talent": talent,
            "summary_files": summary_files,
            "playback_files": playback_files,
            "chat_files": chat_files,
            "money_rows": money_rows,
            "money_summary": money_summary,
            "events": events,
            "by_video": by_video,
            "code_hits": code_hits,
            "streamer_events": streamer_events,
            "streams_scanned": collect_stream_counts(summary_files, playback_files, chat_files),
        }

    code_counts = {}
    for name, data in talent_data.items():
        code_counts[name] = code_counts_from_hits(data["code_hits"])

    summary_rows = []
    for name, data in talent_data.items():
        talent = data["talent"]
        code_hits = data["code_hits"]
        idio_markers, idio_rows = choose_idiosyncrasies(name, data["streamer_events"], all_phrase_counts)
        code_hits["IDIOSYNCRASY"] = idio_rows
        all_evidence = []
        for code in CODE_ORDER:
            all_evidence.extend(code_hits.get(code, []))
        all_evidence.extend(code_hits.get("IDIOSYNCRASY", []))

        out_dir = os.path.join(talent["path"], "stream_summaries", "overall_themes")
        profile_path = os.path.join(out_dir, "personality_profile_codex.md")
        evidence_path = os.path.join(out_dir, "personality_evidence_log.csv")

        write_profile_markdown(
            profile_path,
            talent,
            code_hits,
            idio_markers,
            data["money_summary"],
            code_counts,
            data["streams_scanned"],
        )
        write_evidence_csv(evidence_path, all_evidence)

        summary_rows.append(
            {
                "talent": name,
                "streams_scanned": data["streams_scanned"],
                "evidence_rows": len(all_evidence),
                "money_sampled": data["money_summary"].get("sampled_events", 0),
                "profile_path": profile_path,
                "evidence_path": evidence_path,
            }
        )

    print("PERSONALITY_RUN_SUMMARY")
    for row in summary_rows:
        print(
            f"{row['talent']}|streams={row['streams_scanned']}|evidence_rows={row['evidence_rows']}|money_sampled={row['money_sampled']}|profile={row['profile_path']}|evidence={row['evidence_path']}"
        )


if __name__ == "__main__":
    run()
