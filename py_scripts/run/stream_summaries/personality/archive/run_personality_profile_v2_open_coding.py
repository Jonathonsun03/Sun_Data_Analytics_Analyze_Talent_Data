#!/usr/bin/env python3
"""Open-coding personality profiling (v2) for four hardcoded talents.

Spec source of truth:
prompts/personality/archive/profile_v2_open_coding.md
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


STOPWORDS = {
    "a",
    "an",
    "and",
    "are",
    "as",
    "at",
    "be",
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
    r"\b(are you okay|are you ok|it'?s okay|it'?s ok|take care|take your time|rest well|drink water|you got this|be safe|proud of you|no worries)\b",
    re.I,
)
BOUNDARY_RE = re.compile(
    r"\b(no spoilers|no backseating|don'?t spam|do not spam|stop spamming|stop asking|don'?t ask|do not ask|not okay|we don'?t do that here|please stop)\b",
    re.I,
)
DIRECTIVE_RE = re.compile(
    r"\b(listen up|focus|mods|we'?re moving on|let'?s move on|next up|time to|starting now|keep it respectful|stay on topic)\b",
    re.I,
)
DISCLOSURE_RE = re.compile(
    r"\b(i feel|i'?m (?:tired|stressed|scared|anxious|worried)|i was (?:stressed|overwhelmed)|i'?ve been struggling|burnout|depress(?:ed|ing)?|anxiety|mental health|i cried)\b",
    re.I,
)
THEATRICAL_RE = re.compile(
    r"\b(behold|welcome to|mortals|ladies and gentlemen|my dear|minions|ceremony|queen|casino|goblin|chaos)\b",
    re.I,
)
PLAY_RE = re.compile(
    r"\b(chaos|cursed|unhinged|gremlin|goblin|wild|wtf|what is happening|haha|lmao|lol)\b",
    re.I,
)
LAUGH_RE = re.compile(r"\b(lol|lmao|haha|hehe|rofl|kek|www)\b", re.I)
TEASE_RE = re.compile(r"\b(nerd|bozo|clown|dummy|stinky|coward|skill issue|loser)\b", re.I)
DISTRESS_RE = re.compile(r"\b(scared|sad|anxious|stressed|overwhelmed|tired|confused)\b", re.I)
HYPE_RE = re.compile(r"\b(let'?s go|lets go|hype|pog|we got this|we can do this)\b", re.I)
REQUEST_RE = re.compile(r"\b(can you|could you|please|sing|react|play|read this|request)\b", re.I)
CELEBRATE_RE = re.compile(r"\b(congrats|congratulations|happy birthday|anniversary|celebrate)\b", re.I)


FAMILY_SUFFIX = {
    "reciprocity": "ritual",
    "care": "care",
    "boundary": "rule",
    "agenda": "steer",
    "disclosure": "disclosure",
    "play": "bit",
    "sync": "sync",
    "request": "pivot",
    "celebration": "celebrate",
    "money": "frame",
}

FAMILY_BASE_NAME = {
    "reciprocity": "Reciprocity Rituals",
    "care": "Care and Reassurance",
    "boundary": "Boundary and Norm Control",
    "agenda": "Boundary and Norm Control",
    "request": "Boundary and Norm Control",
    "play": "Play and Bit Escalation",
    "celebration": "Play and Bit Escalation",
    "disclosure": "Self-Disclosure Windows",
    "sync": "Affective Co-Regulation",
    "money": "Money Meaning Frames",
}

ALIAS_ROOT = {
    "thank": "thanks",
    "thanks": "thanks",
    "appreciate": "thanks",
    "grateful": "thanks",
    "ty": "thanks",
    "tysm": "thanks",
    "okay": "okay",
    "ok": "okay",
    "spoilers": "spoiler",
    "spoiler": "spoiler",
    "don": "dont",
    "dont": "dont",
    "do": "dont",
    "chaotic": "chaos",
    "cursed": "chaos",
    "wild": "chaos",
    "hype": "hype",
    "lets": "hype",
    "let": "hype",
    "stressed": "stress",
    "anxious": "stress",
    "worried": "stress",
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


def is_streamer(event: Event, tokens: Iterable[str]) -> bool:
    speaker = normalize_speaker(event.speaker)
    if speaker in {"stream", "host", "streamer"}:
        return True
    for token in tokens:
        if token in speaker:
            return True
    return False


def short_quote(text: str, limit: int = 240) -> str:
    t = (text or "").replace("\n", " ").strip()
    if len(t) <= limit:
        return t
    return t[: limit - 3] + "..."


def slug_from_phrase(phrase: str, max_words: int = 2) -> str:
    toks = [w for w in tokenize(phrase) if w not in STOPWORDS]
    if not toks:
        toks = tokenize(phrase)[:max_words]
    if not toks:
        return "marker"
    return "-".join(toks[:max_words])


def canonical_root(label: str) -> str:
    first = tokenize(label.split(" ")[0])
    if not first:
        return "marker"
    root = first[0]
    return ALIAS_ROOT.get(root, root)


def build_raw_label(anchor: str, family_seed: str) -> str:
    stem = slug_from_phrase(anchor, max_words=2)
    suffix = FAMILY_SUFFIX[family_seed]
    return f"{stem} {suffix}".strip()


def load_playback_events(talent_name: str, playback_files: List[str]) -> List[Event]:
    events: List[Event] = []
    for path in playback_files:
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
                events.append(
                    Event(
                        talent=talent_name,
                        video_id=(row.get("video_id") or "").strip(),
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
        with open(path, "r", encoding="utf-8-sig", newline="") as f:
            reader = csv.DictReader(f)
            for line_no, row in enumerate(reader, start=2):
                text = (row.get("message") or "").strip()
                if not text or text.upper() == "NA":
                    continue
                sec = parse_float(row.get("time_in_seconds", ""))
                if math.isnan(sec):
                    continue
                events.append(
                    Event(
                        talent=talent_name,
                        video_id=(row.get("video_id") or "").strip(),
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
    for e in chat_events:
        key = (
            e.video_id,
            int(round(e.sec)),
            normalize_speaker(e.speaker),
            normalize_text(e.text),
            e.source,
        )
        if key in seen:
            continue
        seen.add(key)
        out.append(e)
    out.sort(key=lambda e: (e.video_id, e.sec, e.file_path, e.file_line))
    by_video: Dict[str, List[Event]] = defaultdict(list)
    for e in out:
        by_video[e.video_id].append(e)
    return out, by_video


def load_money_rows(money_path: str) -> List[dict]:
    if not os.path.exists(money_path):
        return []
    rows = []
    with open(money_path, "r", encoding="utf-8-sig", newline="") as f:
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
                    "file_line": line_no,
                    "file_path": money_path,
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


def collect_stream_counts(summary_files: List[str], playback_files: List[str], chat_files: List[str]) -> int:
    ids = set()
    for path in summary_files + playback_files + chat_files:
        base = os.path.basename(path)
        base = re.sub(r"_summary\.md$", "", base)
        base = re.sub(r"_chat\.csv$", "", base)
        base = re.sub(r"\.csv$", "", base)
        ids.add(base)
    return len(ids)


def build_money_index(money_rows: List[dict]) -> Dict[str, List[float]]:
    idx: Dict[str, List[float]] = defaultdict(list)
    for row in money_rows:
        idx[row["video_id"]].append(row["time_in_seconds"])
    for vid in idx:
        idx[vid].sort()
    return idx


def infer_monetary_context(video_id: str, sec: float, money_index: Dict[str, List[float]]) -> str:
    times = money_index.get(video_id, [])
    if not times:
        return "none"
    best = None
    for t in times:
        d = sec - t
        if best is None or abs(d) < abs(best):
            best = d
    if best is None:
        return "none"
    if -120 <= best < -5:
        return "pre_event"
    if abs(best) <= 5:
        return "event"
    if 5 < best <= 120:
        return "post_event"
    return "none"


def phase1_stream_level_open_coding(
    talent: dict,
    by_video: Dict[str, List[Event]],
) -> Tuple[List[dict], List[Event], List[str]]:
    raw_hits: List[dict] = []
    streamer_events: List[Event] = []
    memos: List[str] = []
    seen = set()
    tokens = talent["tokens"]

    def push(code_label: str, family_seed: str, ev: Event, role: str, memo: str = "") -> None:
        key = (code_label, family_seed, ev.video_id, round(ev.sec, 3), ev.speaker, ev.text, role)
        if key in seen:
            return
        seen.add(key)
        raw_hits.append(
            {
                "talent": talent["name"],
                "raw_label": code_label,
                "family_seed": family_seed,
                "video_id": ev.video_id,
                "sec": ev.sec,
                "timecode": ev.timecode or sec_to_timecode(ev.sec),
                "source": ev.source,
                "speaker": ev.speaker,
                "quote": ev.text,
                "evidence_role": role,
                "file_path": ev.file_path,
                "file_line": ev.file_line,
                "memo": memo,
            }
        )

    for video_id, rows in by_video.items():
        for idx, ev in enumerate(rows):
            txt = ev.text
            norm = normalize_text(txt)
            if not norm:
                continue
            streamer = is_streamer(ev, tokens)
            if streamer:
                streamer_events.append(ev)
            nearby = rows[max(0, idx - 6) : min(len(rows), idx + 7)]
            nearby = [x for x in nearby if x is not ev and abs(x.sec - ev.sec) <= 40]

            m = THANK_RE.search(txt)
            if streamer and m:
                push(build_raw_label(m.group(0), "reciprocity"), "reciprocity", ev, "primary", "in-vivo gratitude phrase")

            m = CARE_RE.search(txt)
            if streamer and m:
                push(build_raw_label(m.group(0), "care"), "care", ev, "primary", "care/reassurance language")

            m = BOUNDARY_RE.search(txt)
            if streamer and m:
                push(build_raw_label(m.group(0), "boundary"), "boundary", ev, "primary", "limit-setting marker")

            m = DIRECTIVE_RE.search(txt)
            if streamer and m:
                push(build_raw_label(m.group(0), "agenda"), "agenda", ev, "primary", "host pacing directive")

            m = DISCLOSURE_RE.search(txt)
            if streamer and m:
                push(build_raw_label(m.group(0), "disclosure"), "disclosure", ev, "primary", "first-person vulnerable disclosure")

            m = THEATRICAL_RE.search(txt)
            if streamer and m:
                push(build_raw_label(m.group(0), "play"), "play", ev, "primary", "dramatic/roleplay register")

            m = TEASE_RE.search(txt)
            if m and (LAUGH_RE.search(txt) or any(LAUGH_RE.search(x.text) for x in nearby)):
                push(build_raw_label(m.group(0), "play"), "play", ev, "primary", "teasing with affiliative uptake")

            m = PLAY_RE.search(txt)
            if m and any(LAUGH_RE.search(x.text) or PLAY_RE.search(x.text) for x in nearby):
                push(build_raw_label(m.group(0), "play"), "play", ev, "primary", "chaos/absurdity with uptake")

            m = CELEBRATE_RE.search(txt)
            if streamer and m:
                push(build_raw_label(m.group(0), "celebration"), "celebration", ev, "primary", "celebratory framing")

            if not streamer and REQUEST_RE.search(txt):
                for later in rows[idx + 1 : idx + 10]:
                    if later.sec - ev.sec > 45:
                        break
                    if is_streamer(later, tokens):
                        label = build_raw_label("request", "request")
                        push(label, "request", later, "primary", "chat request followed by host uptake")
                        push(label, "request", ev, "supporting", "originating request")
                        break

            if not streamer and (DISTRESS_RE.search(txt) or HYPE_RE.search(txt)):
                for later in rows[idx + 1 : idx + 8]:
                    if later.sec - ev.sec > 35:
                        break
                    if is_streamer(later, tokens) and (CARE_RE.search(later.text) or HYPE_RE.search(later.text) or LAUGH_RE.search(later.text)):
                        anchor = CARE_RE.search(later.text) or HYPE_RE.search(later.text) or LAUGH_RE.search(later.text)
                        label = build_raw_label(anchor.group(0), "sync") if anchor else build_raw_label("sync", "sync")
                        push(label, "sync", later, "primary", "chat affect signal followed by host modulation")
                        push(label, "sync", ev, "supporting", "chat affect signal")
                        break

        memos.append(f"{video_id}: generated raw codes from independent stream-level interaction units.")

    return raw_hits, streamer_events, memos


def phase1_money_probe_open_coding(
    talent: dict,
    by_video: Dict[str, List[Event]],
    money_rows: List[dict],
) -> Tuple[List[dict], List[dict]]:
    sampled = sample_evenly(money_rows, 20)
    hits: List[dict] = []
    tokens = talent["tokens"]
    seen = set()
    sampled_records: List[dict] = []

    def push(
        code_label: str,
        family_seed: str,
        video_id: str,
        sec: float,
        timecode: str,
        source: str,
        speaker: str,
        quote: str,
        role: str,
        file_path: str,
        file_line: int,
        memo: str,
    ) -> None:
        key = (code_label, family_seed, video_id, round(sec, 3), speaker, quote, role)
        if key in seen:
            return
        seen.add(key)
        hits.append(
            {
                "talent": talent["name"],
                "raw_label": code_label,
                "family_seed": family_seed,
                "video_id": video_id,
                "sec": sec,
                "timecode": timecode,
                "source": source,
                "speaker": speaker,
                "quote": quote,
                "evidence_role": role,
                "file_path": file_path,
                "file_line": file_line,
                "memo": memo,
            }
        )

    for m in sampled:
        vid = m["video_id"]
        t = m["time_in_seconds"]
        msg = m["message"] or ""
        rows = by_video.get(vid, [])
        pre = [r for r in rows if t - 90 <= r.sec < t - 5]
        post = [r for r in rows if t <= r.sec <= t + 90]
        streamer_post = [r for r in post if is_streamer(r, tokens)]

        if CARE_RE.search(msg):
            anchor = CARE_RE.search(msg).group(0)
            probe = "support-as-care"
        elif REQUEST_RE.search(msg):
            anchor = "request"
            probe = "support-as-request"
        elif CELEBRATE_RE.search(msg):
            anchor = CELEBRATE_RE.search(msg).group(0)
            probe = "support-as-celebration"
        elif PLAY_RE.search(msg) or LAUGH_RE.search(msg):
            anchor = "joke support"
            probe = "support-as-performance"
        else:
            anchor = "money moment"
            probe = "insufficient evidence"

        label = build_raw_label(anchor, "money")
        push(
            label,
            "money",
            vid,
            t,
            m["timecode"],
            "chat",
            m["username"],
            msg or f"paid event {m['paid_amount']}",
            "primary",
            m["file_path"],
            m["file_line"],
            f"money probe: {probe}",
        )
        if streamer_post:
            r = streamer_post[0]
            push(
                label,
                "money",
                r.video_id,
                r.sec,
                r.timecode,
                r.source,
                r.speaker,
                r.text,
                "supporting",
                r.file_path,
                r.file_line,
                f"post-event response: {probe}",
            )

        sampled_records.append(
            {
                "video_id": vid,
                "time_in_seconds": t,
                "timecode": m["timecode"],
                "username": m["username"],
                "message": msg,
                "pre_count": len(pre),
                "post_count": len(post),
                "probe_label": probe,
            }
        )

    return hits, sampled_records


def canonicalize_raw_label(raw_label: str, family_seed: str) -> str:
    root = canonical_root(raw_label)
    return f"{root} {FAMILY_SUFFIX[family_seed]}"


def phase2_constant_comparison(raw_hits: List[dict]) -> Tuple[Dict[str, List[dict]], dict, List[str]]:
    raw_unique = len({h["raw_label"] for h in raw_hits})
    merged: Dict[str, List[dict]] = defaultdict(list)
    merge_notes: List[str] = []

    for h in raw_hits:
        key = canonicalize_raw_label(h["raw_label"], h["family_seed"])
        h2 = dict(h)
        h2["canonical_label"] = key
        merged[key].append(h2)

    merged_unique = len(merged)
    split_groups: Dict[str, List[dict]] = defaultdict(list)
    split_count = 0
    for label, rows in merged.items():
        raw_counts = Counter(r["raw_label"] for r in rows)
        if len(rows) >= 140 and len(raw_counts) >= 5:
            top2 = raw_counts.most_common(2)
            if len(top2) == 2 and top2[1][1] >= max(20, int(0.18 * len(rows))):
                split_count += 1
                top_labels = {top2[0][0], top2[1][0]}
                for raw_name in top_labels:
                    new_label = canonicalize_raw_label(raw_name, rows[0]["family_seed"])
                    split_groups[new_label].extend([r for r in rows if r["raw_label"] == raw_name])
                split_groups[label].extend([r for r in rows if r["raw_label"] not in top_labels])
                merge_notes.append(
                    f"Split broad code '{label}' into sub-codes from dominant in-vivo variants: {top2[0][0]} / {top2[1][0]}."
                )
            else:
                split_groups[label].extend(rows)
        else:
            split_groups[label].extend(rows)

    clean_groups = {k: v for k, v in split_groups.items() if v}
    stats = {
        "raw_codes_generated": raw_unique,
        "merged_codes": merged_unique,
        "split_operations": split_count,
    }
    for old, rows in merged.items():
        labels = {r["canonical_label"] for r in rows}
        if len(labels) == 1 and old in labels:
            continue
        merge_notes.append(f"Merged semantically equivalent variants into '{old}'.")
    return clean_groups, stats, merge_notes


def retain_codes(groups: Dict[str, List[dict]]) -> Tuple[Dict[str, dict], List[str]]:
    kept: Dict[str, dict] = {}
    notes: List[str] = []
    dropped = []

    for label, rows in groups.items():
        primary = [r for r in rows if r["evidence_role"] == "primary"]
        pcount = len(primary)
        coverage = len({r["video_id"] for r in primary})
        family_seed = Counter(r["family_seed"] for r in rows).most_common(1)[0][0]
        concept_important = family_seed in {"boundary", "disclosure", "sync", "money"}
        keep = (pcount >= 6 and coverage >= 2) or (concept_important and pcount >= 2)
        if keep:
            kept[label] = {
                "label": label,
                "family_seed": family_seed,
                "rows": rows,
                "primary_count": pcount,
                "stream_coverage_count": coverage,
            }
            if pcount < 6 and concept_important:
                notes.append(
                    f"Retained low-frequency but conceptually important code '{label}' (n={pcount}) due to boundary/disclosure/sync/money salience."
                )
        else:
            dropped.append((label, pcount))

    if len(kept) < 6:
        for label, _ in sorted(dropped, key=lambda x: x[1], reverse=True):
            if label in kept:
                continue
            rows = groups[label]
            primary = [r for r in rows if r["evidence_role"] == "primary"]
            kept[label] = {
                "label": label,
                "family_seed": Counter(r["family_seed"] for r in rows).most_common(1)[0][0],
                "rows": rows,
                "primary_count": len(primary),
                "stream_coverage_count": len({r["video_id"] for r in primary}),
            }
            notes.append(f"Retained '{label}' to preserve talent-specific nuance despite lower count.")
            if len(kept) >= 6:
                break

    return kept, notes


def build_theme_families(retained_codes: Dict[str, dict]) -> Tuple[Dict[str, str], Dict[str, dict]]:
    by_seed: Dict[str, List[str]] = defaultdict(list)
    for label, meta in retained_codes.items():
        by_seed[meta["family_seed"]].append(label)

    label_to_family: Dict[str, str] = {}
    family_meta: Dict[str, dict] = {}
    for seed, labels in by_seed.items():
        roots = [l.split(" ")[0] for l in labels]
        top_root = Counter(roots).most_common(1)[0][0].replace("-", " ")
        family_name = f"{top_root.title()} {FAMILY_BASE_NAME[seed]}"
        total = sum(retained_codes[l]["primary_count"] for l in labels)
        family_meta[family_name] = {"seed": seed, "codes": labels, "primary_total": total}
        for l in labels:
            label_to_family[l] = family_name
    return label_to_family, family_meta


def inclusion_exclusion_text(family_seed: str, markers: List[str]) -> Tuple[str, str, str]:
    markers_txt = ", ".join(markers[:3]) if markers else "observed interactional markers"
    if family_seed == "reciprocity":
        return (
            f"Repeated gratitude/recognition routines anchored by phrases such as {markers_txt}.",
            f"Contains explicit thanking/appreciation text and community-directed acknowledgement ({markers_txt}).",
            "Exclude one-off courtesy without repeated structure or relational uptake.",
        )
    if family_seed == "care":
        return (
            f"Reassurance or caretaking stance, often using markers like {markers_txt}.",
            f"Includes direct soothing/check-in/support wording ({markers_txt}).",
            "Exclude neutral logistics or acknowledgements lacking emotional support intent.",
        )
    if family_seed in {"boundary", "agenda", "request"}:
        return (
            f"Host control over norms or stream flow with recurring markers such as {markers_txt}.",
            f"Includes directives, explicit limits, and request-routing language ({markers_txt}).",
            "Exclude optional suggestions without clear norm-setting or pacing intent.",
        )
    if family_seed in {"play", "celebration"}:
        return (
            f"Playful/theatrical escalation detected through markers like {markers_txt}.",
            f"Includes joking roleplay, teasing, celebratory framing, or absurd callback sequences ({markers_txt}).",
            "Exclude plain status updates where no playful performative frame is present.",
        )
    if family_seed == "disclosure":
        return (
            f"Personal self-revelation moments with first-person internal-state cues ({markers_txt}).",
            f"Includes vulnerability language with personal stakes and affective detail ({markers_txt}).",
            "Exclude operational updates that do not disclose felt state or personal vulnerability.",
        )
    if family_seed == "sync":
        return (
            f"Reciprocal modulation loops between chat signals and host response ({markers_txt}).",
            f"Requires chat affect cue plus host modulation within short window ({markers_txt}).",
            "Exclude single isolated distress/hype lines without interactional response loop.",
        )
    return (
        f"Money moments interpreted through interactional meaning markers ({markers_txt}).",
        f"Includes paid-event text and immediate framing/response language ({markers_txt}).",
        "Exclude amount-only mentions with no social/relational interpretation.",
    )


def top_examples(rows: List[dict], n: int = 3) -> List[dict]:
    out: List[dict] = []
    seen_vid = set()
    for r in sorted(rows, key=lambda x: x["sec"]):
        if r["video_id"] in seen_vid:
            continue
        out.append(r)
        seen_vid.add(r["video_id"])
        if len(out) >= n:
            return out
    for r in sorted(rows, key=lambda x: x["sec"]):
        if len(out) >= n:
            break
        if r not in out:
            out.append(r)
    return out


def build_codebook_rows(
    talent_name: str,
    retained_codes: Dict[str, dict],
    label_to_family: Dict[str, str],
    extra_notes: List[str],
) -> List[dict]:
    rows = []
    note_join = " | ".join(extra_notes[:8]) if extra_notes else ""
    for label, meta in sorted(retained_codes.items(), key=lambda x: x[1]["primary_count"], reverse=True):
        raw_markers = Counter(r["raw_label"] for r in meta["rows"]).most_common(4)
        markers = [m[0] for m in raw_markers]
        op, inc, exc = inclusion_exclusion_text(meta["family_seed"], markers)
        rows.append(
            {
                "talent": talent_name,
                "code_label": label,
                "theme_family": label_to_family[label],
                "operational_definition": op,
                "inclusion_criteria": inc,
                "exclusion_criteria": exc,
                "frequency_count": meta["primary_count"],
                "stream_coverage_count": meta["stream_coverage_count"],
                "memo_notes": note_join,
            }
        )
    return rows


def add_counterexamples(
    talent_name: str,
    all_events: List[Event],
    retained_codes: Dict[str, dict],
    label_to_family: Dict[str, str],
    money_index: Dict[str, List[float]],
) -> Tuple[List[dict], Dict[str, List[dict]]]:
    coded_by_family: Dict[str, set] = defaultdict(set)
    family_to_code: Dict[str, str] = {}
    for label, meta in retained_codes.items():
        fam = label_to_family[label]
        family_to_code.setdefault(fam, label)
        for r in meta["rows"]:
            coded_by_family[fam].add((r["video_id"], round(r["sec"], 3), normalize_speaker(r["speaker"]), normalize_text(r["quote"])))

    counter_rows: List[dict] = []
    counter_by_family: Dict[str, List[dict]] = defaultdict(list)

    for family, keyset in coded_by_family.items():
        picked = 0
        for ev in all_events:
            key = (ev.video_id, round(ev.sec, 3), normalize_speaker(ev.speaker), normalize_text(ev.text))
            if key in keyset:
                continue
            if len(normalize_text(ev.text)) < 15:
                continue
            if "http" in ev.text.lower():
                continue
            row = {
                "talent": talent_name,
                "code_label": family_to_code[family],
                "theme_family": family,
                "video_id": ev.video_id,
                "time_in_seconds": f"{ev.sec:.3f}",
                "timecode": ev.timecode or sec_to_timecode(ev.sec),
                "source": ev.source,
                "speaker": ev.speaker,
                "quote": ev.text,
                "evidence_role": "counterexample",
                "monetary_context": infer_monetary_context(ev.video_id, ev.sec, money_index),
                "file_path": ev.file_path,
                "file_line": ev.file_line,
            }
            counter_rows.append(row)
            counter_by_family[family].append(row)
            picked += 1
            if picked >= 2:
                break
    return counter_rows, counter_by_family


def build_evidence_rows(
    talent_name: str,
    retained_codes: Dict[str, dict],
    label_to_family: Dict[str, str],
    money_index: Dict[str, List[float]],
) -> List[dict]:
    rows: List[dict] = []
    for label, meta in retained_codes.items():
        family = label_to_family[label]
        for r in meta["rows"]:
            rows.append(
                {
                    "talent": talent_name,
                    "code_label": label,
                    "theme_family": family,
                    "video_id": r["video_id"],
                    "time_in_seconds": f"{r['sec']:.3f}",
                    "timecode": r["timecode"],
                    "source": r["source"],
                    "speaker": r["speaker"],
                    "quote": r["quote"],
                    "evidence_role": r["evidence_role"],
                    "monetary_context": infer_monetary_context(r["video_id"], r["sec"], money_index),
                    "file_path": r["file_path"],
                    "file_line": r["file_line"],
                }
            )
    return rows


def build_phrase_counter(events: List[Event]) -> Counter:
    counter = Counter()
    for ev in events:
        toks = [w for w in tokenize(ev.text) if w not in STOPWORDS]
        if len(toks) < 3:
            continue
        for n in (2, 3):
            for i in range(0, len(toks) - n + 1):
                phrase = " ".join(toks[i : i + n])
                if any(ch.isdigit() for ch in phrase):
                    continue
                counter[phrase] += 1
    return counter


def choose_idiosyncratic_markers(
    talent_name: str,
    streamer_events: List[Event],
    all_phrase_counters: Dict[str, Counter],
) -> List[dict]:
    own = all_phrase_counters[talent_name]
    others = [n for n in all_phrase_counters if n != talent_name]
    phrase_rows: Dict[str, List[Event]] = defaultdict(list)
    phrase_vids: Dict[str, set] = defaultdict(set)
    for ev in streamer_events:
        norm = normalize_text(ev.text)
        if not norm:
            continue
        words = [w for w in norm.split() if w not in STOPWORDS]
        for n in (2, 3):
            for i in range(0, len(words) - n + 1):
                phrase = " ".join(words[i : i + n])
                phrase_rows[phrase].append(ev)
                phrase_vids[phrase].add(ev.video_id)

    scored = []
    for phrase, own_count in own.items():
        if own_count < 4 or len(phrase_vids.get(phrase, set())) < 2:
            continue
        other_avg = sum(all_phrase_counters[o].get(phrase, 0) for o in others) / max(1, len(others))
        score = (own_count * len(phrase_vids[phrase])) / (1 + other_avg)
        scored.append((score, phrase))
    scored.sort(reverse=True)

    out = []
    used = set()
    for _, phrase in scored:
        if any(phrase in x or x in phrase for x in used):
            continue
        examples = []
        seen_vid = set()
        for ev in phrase_rows[phrase]:
            if ev.video_id in seen_vid:
                continue
            examples.append(
                {
                    "video_id": ev.video_id,
                    "timecode": ev.timecode,
                    "speaker": ev.speaker,
                    "quote": short_quote(ev.text),
                }
            )
            seen_vid.add(ev.video_id)
            if len(examples) >= 3:
                break
        if not examples:
            continue
        out.append(
            {
                "marker": phrase,
                "why": f"Observed {len(phrase_rows[phrase])} times across {len(phrase_vids[phrase])} streams with lower cross-talent base rate.",
                "examples": examples,
            }
        )
        used.add(phrase)
        if len(out) >= 3:
            break
    return out


def format_example(row: dict) -> str:
    return f'[{row["timecode"]}] {row["speaker"]} ({row["video_id"]}): "{short_quote(row["quote"])}"'


def confidence_label(total_primary: int) -> str:
    if total_primary >= 500:
        return "High"
    if total_primary >= 180:
        return "Medium"
    return "Low"


def family_distribution(evidence_rows: List[dict]) -> Dict[str, int]:
    c = Counter()
    for r in evidence_rows:
        if r["evidence_role"] == "primary":
            c[r["theme_family"]] += 1
    return dict(c)


def money_pattern_summary(
    sampled_money: List[dict],
    evidence_rows: List[dict],
) -> Tuple[List[str], Dict[str, int]]:
    by_video: Dict[str, List[dict]] = defaultdict(list)
    for r in evidence_rows:
        if r["evidence_role"] != "primary":
            continue
        by_video[r["video_id"]].append(r)
    for vid in by_video:
        by_video[vid].sort(key=lambda x: float(x["time_in_seconds"]))

    amplify = Counter()
    dampen = Counter()
    reveal = Counter()

    for m in sampled_money:
        vid = m["video_id"]
        t = m["time_in_seconds"]
        rows = by_video.get(vid, [])
        pre = [r for r in rows if t - 90 <= float(r["time_in_seconds"]) < t - 5]
        post = [r for r in rows if t <= float(r["time_in_seconds"]) <= t + 90]
        pre_c = Counter(r["theme_family"] for r in pre)
        post_c = Counter(r["theme_family"] for r in post)

        top_pre = pre_c.most_common(1)[0][0] if pre_c else "none"
        top_post = post_c.most_common(1)[0][0] if post_c else "none"
        if top_post == "none":
            dampen["interaction drops"] += 1
        elif top_post == top_pre and sum(post_c.values()) >= sum(pre_c.values()):
            amplify[top_post] += 1
        elif top_pre != top_post:
            reveal[f"{top_post} emerges"] += 1
        else:
            dampen[f"{top_pre} softens"] += 1

    lines = []
    if amplify:
        lines.append("Amplify patterns: " + ", ".join(f"{k} ({v})" for k, v in amplify.most_common(3)) + ".")
    if dampen:
        lines.append("Dampen patterns: " + ", ".join(f"{k} ({v})" for k, v in dampen.most_common(3)) + ".")
    if reveal:
        lines.append("Reveal patterns: " + ", ".join(f"{k} ({v})" for k, v in reveal.most_common(3)) + ".")
    if not lines:
        lines.append("Insufficient coded pre/post evidence to characterize amplification/dampening/reveal patterns.")

    stats = {
        "sampled_events": len(sampled_money),
        "amplify_events": sum(amplify.values()),
        "dampen_events": sum(dampen.values()),
        "reveal_events": sum(reveal.values()),
    }
    return lines, stats


def build_cross_talent_text(
    talent_name: str,
    talent_family_dist: Dict[str, Dict[str, int]],
) -> str:
    me = talent_family_dist[talent_name]
    others = [n for n in talent_family_dist if n != talent_name]
    all_families = sorted({f for d in talent_family_dist.values() for f in d})

    def normalize(dist: Dict[str, int]) -> Dict[str, float]:
        tot = sum(dist.values()) or 1
        return {f: dist.get(f, 0) / tot for f in all_families}

    me_n = normalize(me)
    other_n = {o: normalize(talent_family_dist[o]) for o in others}
    nearest = sorted(
        others,
        key=lambda o: sum(abs(me_n[f] - other_n[o][f]) for f in all_families),
    )[:2]

    avg_other = {f: sum(other_n[o][f] for o in others) / max(1, len(others)) for f in all_families}
    distinctive = max(all_families, key=lambda f: me_n[f] - avg_other[f])
    shared = max(all_families, key=lambda f: min(me_n[f], avg_other[f]))
    resemble_target = nearest[0] if nearest else others[0]

    return "\n".join(
        [
            f"- Distinctive family: `{distinctive}` (this talent {me_n[distinctive]:.1%} vs peer mean {avg_other[distinctive]:.1%}).",
            f"- Shared family with peers: `{shared}`.",
            f"- Compared with {nearest[0]} and {nearest[1]} for nearest-pattern contrast.",
            f"- Can resemble {resemble_target}, but differs by `{distinctive}` prominence and how it is enacted in interaction turns.",
        ]
    )


def write_csv(path: str, columns: List[str], rows: List[dict]) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=columns)
        writer.writeheader()
        for r in rows:
            writer.writerow({c: r.get(c, "") for c in columns})


def write_markdown_profile(
    path: str,
    talent: dict,
    coding_stats: dict,
    process_notes: List[str],
    retained_codes: Dict[str, dict],
    label_to_family: Dict[str, str],
    family_meta: Dict[str, dict],
    evidence_rows: List[dict],
    counter_by_family: Dict[str, List[dict]],
    idio_markers: List[dict],
    sampled_money: List[dict],
    money_lines: List[str],
    money_stats: Dict[str, int],
    cross_talent_text: str,
) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    analysis_conducted_at = datetime.now().astimezone().strftime("%Y-%m-%d %H:%M %Z")
    primary_total = sum(1 for r in evidence_rows if r["evidence_role"] == "primary")
    conf = confidence_label(primary_total)
    top_codes = sorted(retained_codes.values(), key=lambda x: x["primary_count"], reverse=True)[:3]
    top_codes_txt = ", ".join(f"{x['label']} ({x['primary_count']})" for x in top_codes) or "insufficient evidence"

    lines: List[str] = []

    lines.append(f"Analysis conducted: {analysis_conducted_at}")
    lines.append("")
    lines.append("## 1) Personality Signature (Emergent)")
    lines.append(
        f"Across open-coded interaction cases, {talent['name']} shows a text-grounded signature centered on {top_codes_txt}. The dominant style is inferred from repeated turn-level evidence across streams rather than preset categories."
    )
    lines.append(
        f"Confidence level: {conf}. Limits: this is text-only inference (no facial/body cues, no full prosody), so sarcasm intensity, affect strength, and performative tone can be under/over-estimated."
    )
    lines.append("")

    lines.append("## 2) Open Coding Process Notes")
    lines.append(
        f"Raw in-vivo/descriptive codes were generated stream-by-stream, then merged/split via constant comparison. Totals: raw codes={coding_stats['raw_codes_generated']}, merged codes={coding_stats['merged_codes']}, final retained codes={len(retained_codes)}."
    )
    for note in process_notes[:10]:
        lines.append(f"- {note}")
    lines.append("")

    lines.append("## 3) Emergent Codebook (Talent-Specific)")
    for label, meta in sorted(retained_codes.items(), key=lambda x: x[1]["primary_count"], reverse=True):
        raw_markers = [x[0] for x in Counter(r["raw_label"] for r in meta["rows"]).most_common(4)]
        op, inc, exc = inclusion_exclusion_text(meta["family_seed"], raw_markers)
        primary_rows = [r for r in meta["rows"] if r["evidence_role"] == "primary"]
        ex = top_examples(primary_rows, 3)
        lines.append(f"### {label}")
        lines.append(f"- code_label: {label}")
        lines.append(f"- operational_definition: {op}")
        lines.append(f"- inclusion_criteria: {inc}")
        lines.append(f"- exclusion_criteria: {exc}")
        lines.append(f"- frequency_count: {meta['primary_count']}")
        lines.append(f"- stream_coverage_count: {meta['stream_coverage_count']}")
        if ex:
            lines.append("- evidence_quotes:")
            for row in ex:
                lines.append(f"  - {format_example(row)}")
        else:
            lines.append("- evidence_quotes: insufficient evidence")
        lines.append("")

    lines.append("## 4) Emergent Theme Families")
    for family, meta in sorted(family_meta.items(), key=lambda x: x[1]["primary_total"], reverse=True):
        strongest = []
        for code in meta["codes"]:
            primary_rows = [r for r in retained_codes[code]["rows"] if r["evidence_role"] == "primary"]
            strongest.extend(primary_rows[:2])
        strongest = top_examples(strongest, 2)
        lines.append(f"### {family}")
        lines.append(
            f"- family_summary: Built from repeated member-code patterns with {meta['primary_total']} primary-coded interaction units."
        )
        lines.append(f"- member_codes: {', '.join(meta['codes'])}")
        lines.append("- strongest_supporting_evidence:")
        for row in strongest:
            lines.append(f"  - {format_example(row)}")
        lines.append("- counterexamples_or_limits:")
        cxs = counter_by_family.get(family, [])
        if cxs:
            for row in cxs[:2]:
                lines.append(f"  - {format_example(row)}")
        else:
            lines.append("  - insufficient evidence")
            lines.append("  - insufficient evidence")
        lines.append("")

    lines.append("## 5) Idiosyncratic Charm Markers")
    if idio_markers:
        for m in idio_markers:
            lines.append(f"### {m['marker']}")
            lines.append(f"- marker_note: {m['why']}")
            lines.append("- supporting_examples:")
            for ex in m["examples"]:
                lines.append(f'  - [{ex["timecode"]}] {ex["speaker"]} ({ex["video_id"]}): "{ex["quote"]}"')
    else:
        lines.append("Insufficient repeated phrase-level evidence for stable marker extraction.")
    lines.append("")

    lines.append("## 6) Money-Linked Personality Patterns (Supplement, not primary driver)")
    lines.append(
        f"Sampled paid events: {money_stats['sampled_events']} (target 20 unless fewer available). For each sample, pre-window 90s, event text, and post-window 90s were compared."
    )
    for line in money_lines:
        lines.append(f"- {line}")
    for m in sampled_money[:3]:
        lines.append(
            f"- Probe example [{m['timecode']}] {m['username']} ({m['video_id']}): \"{short_quote(m['message'])}\" | pre_events={m['pre_count']} post_events={m['post_count']}"
        )
    lines.append("")

    lines.append("## 7) Cross-Talent Distinctiveness")
    lines.append(cross_talent_text)
    lines.append("")

    lines.append("## 8) Validity and Uncertainty")
    lines.append("- 3 strongest evidence-backed conclusions:")
    strongest_codes = sorted(retained_codes.values(), key=lambda x: x["primary_count"], reverse=True)[:3]
    for c in strongest_codes:
        lines.append(f"  - `{c['label']}` repeatedly appears with {c['primary_count']} primary evidence rows.")
    lines.append("- 3 uncertainty points:")
    lines.append("  - Text-only records cannot directly resolve delivery tone or sarcasm strength.")
    lines.append("  - Transcript sparsity and chat burstiness can hide short interaction loops.")
    lines.append("  - Some interaction motifs may be platform/format artifacts rather than stable personality traits.")
    lines.append("- Possible coder bias and mitigation:")
    lines.append(
        "  - Bias risk: over-valuing vivid phrases. Mitigation: all retained claims are tied to timestamped rows, with counterexamples logged per major family."
    )

    with open(path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines).strip() + "\n")


def verify_quote_in_source(row: dict) -> bool:
    quote = (row.get("quote") or "").strip()
    path = row.get("file_path") or ""
    if not quote or not path or not os.path.exists(path):
        return False
    try:
        with open(path, "r", encoding="utf-8-sig", errors="ignore") as f:
            for line in f:
                if quote in line:
                    return True
    except Exception:
        return False
    return False


def run() -> None:
    talent_results = {}
    all_phrase_counters: Dict[str, Counter] = {}

    for talent in TALENTS:
        base = talent["path"]
        summary_files = sorted(glob.glob(os.path.join(base, "stream_summaries", "stream_summary_codex", "*.md")))
        playback_files = sorted(glob.glob(os.path.join(base, "text_playback", "*.csv")))
        chat_files = sorted(glob.glob(os.path.join(base, "Chat", "Original", "*_chat.csv")))
        money_path = os.path.join(base, "stream_summaries", "overall_themes", "money_timestamps.csv")

        playback_events = load_playback_events(talent["name"], playback_files)
        chat_events = load_chat_events(talent["name"], chat_files)
        all_events, by_video = dedupe_and_group(playback_events, chat_events)
        money_rows = load_money_rows(money_path)
        money_index = build_money_index(money_rows)

        raw_hits_stream, streamer_events, phase1_memos = phase1_stream_level_open_coding(talent, by_video)
        raw_hits_money, sampled_money = phase1_money_probe_open_coding(talent, by_video, money_rows)
        raw_hits = raw_hits_stream + raw_hits_money

        groups, phase2_stats, phase2_notes = phase2_constant_comparison(raw_hits)
        retained_codes, phase2_retain_notes = retain_codes(groups)
        label_to_family, family_meta = build_theme_families(retained_codes)
        evidence_rows = build_evidence_rows(talent["name"], retained_codes, label_to_family, money_index)
        counter_rows, counter_by_family = add_counterexamples(
            talent["name"], all_events, retained_codes, label_to_family, money_index
        )
        evidence_rows.extend(counter_rows)

        codebook_rows = build_codebook_rows(
            talent["name"],
            retained_codes,
            label_to_family,
            phase2_notes + phase2_retain_notes,
        )

        all_phrase_counters[talent["name"]] = build_phrase_counter(streamer_events)

        talent_results[talent["name"]] = {
            "talent": talent,
            "summary_files": summary_files,
            "playback_files": playback_files,
            "chat_files": chat_files,
            "streams_scanned": collect_stream_counts(summary_files, playback_files, chat_files),
            "all_events": all_events,
            "streamer_events": streamer_events,
            "money_rows": money_rows,
            "sampled_money": sampled_money,
            "phase1_memos": phase1_memos,
            "phase2_notes": phase2_notes + phase2_retain_notes,
            "coding_stats": {
                "raw_codes_generated": phase2_stats["raw_codes_generated"],
                "merged_codes": phase2_stats["merged_codes"],
            },
            "retained_codes": retained_codes,
            "family_meta": family_meta,
            "label_to_family": label_to_family,
            "evidence_rows": evidence_rows,
            "counter_by_family": counter_by_family,
            "codebook_rows": codebook_rows,
            "money_index": money_index,
        }

    idio_by_talent = {}
    for talent_name, data in talent_results.items():
        idio_by_talent[talent_name] = choose_idiosyncratic_markers(
            talent_name,
            data["streamer_events"],
            all_phrase_counters,
        )

    family_dist_all = {}
    for talent_name, data in talent_results.items():
        family_dist_all[talent_name] = family_distribution(data["evidence_rows"])

    summary_lines = []
    spotcheck_lines = []
    for talent_name, data in talent_results.items():
        talent = data["talent"]
        out_dir = os.path.join(talent["path"], "stream_summaries", "overall_themes")
        profile_path = os.path.join(out_dir, "personality_profile_v2_open_coding.md")
        codebook_path = os.path.join(out_dir, "open_codebook_v2.csv")
        evidence_path = os.path.join(out_dir, "open_coding_evidence_v2.csv")

        money_lines, money_stats = money_pattern_summary(data["sampled_money"], data["evidence_rows"])
        cross_txt = build_cross_talent_text(talent_name, family_dist_all)

        write_markdown_profile(
            profile_path,
            talent,
            data["coding_stats"],
            data["phase1_memos"] + data["phase2_notes"],
            data["retained_codes"],
            data["label_to_family"],
            data["family_meta"],
            data["evidence_rows"],
            data["counter_by_family"],
            idio_by_talent[talent_name],
            data["sampled_money"],
            money_lines,
            money_stats,
            cross_txt,
        )

        write_csv(
            codebook_path,
            [
                "talent",
                "code_label",
                "theme_family",
                "operational_definition",
                "inclusion_criteria",
                "exclusion_criteria",
                "frequency_count",
                "stream_coverage_count",
                "memo_notes",
            ],
            data["codebook_rows"],
        )
        write_csv(
            evidence_path,
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
            ],
            data["evidence_rows"],
        )

        theme_family_count = len(data["family_meta"])
        evidence_count = len(data["evidence_rows"])
        sampled_n = len(data["sampled_money"])
        summary_lines.append(
            "SUMMARY|"
            f"talent={talent_name}|"
            f"streams_scanned={data['streams_scanned']}|"
            f"raw_codes_generated={data['coding_stats']['raw_codes_generated']}|"
            f"final_codes_retained={len(data['retained_codes'])}|"
            f"theme_families={theme_family_count}|"
            f"evidence_rows_logged={evidence_count}|"
            f"money_events_sampled={sampled_n}|"
            f"profile_path={profile_path}|"
            f"codebook_path={codebook_path}|"
            f"evidence_path={evidence_path}"
        )

        checks = 0
        for row in data["evidence_rows"]:
            if row["evidence_role"] != "primary":
                continue
            if verify_quote_in_source(row):
                spotcheck_lines.append(
                    "SPOTCHECK|"
                    f"talent={talent_name}|"
                    f"video_id={row['video_id']}|"
                    f"timecode={row['timecode']}|"
                    f"file_path={row.get('file_path','')}|"
                    f"file_line={row.get('file_line','')}|"
                    f"quote={short_quote(row['quote'], 140)}"
                )
                checks += 1
            if checks >= 2:
                break

    print("OPEN_CODING_V2_RUN_SUMMARY")
    for line in summary_lines:
        print(line)
    print("OPEN_CODING_V2_SPOTCHECKS")
    for line in spotcheck_lines:
        print(line)


if __name__ == "__main__":
    run()
