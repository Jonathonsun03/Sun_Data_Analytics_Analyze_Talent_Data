#!/usr/bin/env python3
"""Build final chat personality profile outputs for a talent."""

from __future__ import annotations

import argparse
import csv
import json
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Iterable
from zoneinfo import ZoneInfo


TIMEZONE = ZoneInfo("America/New_York")
TALENT_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
LOG_ROOT = Path(
    "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/chat_personality/chat_personality_profile"
)


@dataclass(frozen=True)
class PatternSpec:
    code_id: str
    pattern_name: str
    what_chat_does: str
    interactional_function: str
    how_common: str
    evidence_notes: str


PATTERN_SPECS = {
    "CHAT_PC_01": PatternSpec(
        code_id="CHAT_PC_01",
        pattern_name="Chant-led room assembly",
        what_chat_does=(
            "Chat repeatedly uses slogan-like invocations such as `AVA LA AVALUTION` and `CLARITY` "
            "to greet, rally, and visibly join the room."
        ),
        interactional_function=(
            "Builds belonging quickly and turns arrival or emphasis into a recognizable communal ritual."
        ),
        how_common="Very common across the corpus.",
        evidence_notes=(
            "The pattern appears across many stream types, not only one event format, which makes it more than a one-off meme."
        ),
    ),
    "CHAT_PC_02": PatternSpec(
        code_id="CHAT_PC_02",
        pattern_name="Synchronized hype pile-ons",
        what_chat_does=(
            "Chat stacks short high-intensity reactions like `LET'S GO`, `HELL YEAH`, elongated vowels, "
            "and alarmed commentary when a moment lands."
        ),
        interactional_function=(
            "Creates a visible crowd pulse that marks shared excitement, surprise, or mock-despair in real time."
        ),
        how_common="Very common across the corpus.",
        evidence_notes=(
            "These bursts tend to cluster around reveals, big notes, close calls, and escalation beats."
        ),
    ),
    "CHAT_PC_03": PatternSpec(
        code_id="CHAT_PC_03",
        pattern_name="Affection-forward validation swells",
        what_chat_does=(
            "Chat often answers strong or vulnerable moments with overt praise, gratitude, `Ava cute` language, "
            "and direct reassurance."
        ),
        interactional_function=(
            "Publicly affirms the performer-facing moment while reinforcing a warm norm of visible appreciation."
        ),
        how_common="Common and spread across many files, though less constant than chants or hype bursts.",
        evidence_notes=(
            "The support is often personal and explicit rather than generic cheering, which makes the affective norm easy to see in text."
        ),
    ),
    "CHAT_PC_04": PatternSpec(
        code_id="CHAT_PC_04",
        pattern_name="Playful escalation pressure",
        what_chat_does=(
            "Chat pushes for repeats, wheel spins, dares, penalties, and more committed follow-through with lines like "
            "`COWARD`, `SPIN THAT WHEEL`, and `Do it again!`."
        ),
        interactional_function=(
            "Lets the audience co-author stakes and comedic tension by publicly nudging the stream toward escalation."
        ),
        how_common="Moderately common, but recurring often enough to be a stable interaction style.",
        evidence_notes=(
            "The stronger signal is repeated pressure toward bigger payoff, not isolated requests by themselves."
        ),
    ),
}


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open("r", newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, fieldnames: Iterable[str], rows: list[dict[str, str]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(fieldnames))
        writer.writeheader()
        writer.writerows(rows)


def load_json(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def clean(text: str) -> str:
    return " ".join((text or "").replace("\n", " ").split())


def quote_line(row: dict[str, str]) -> str:
    source_file = row["source_file"]
    row_number = row.get("row_number", "") or "NA"
    video_id = row.get("video_id", "") or "NA"
    timestamp = row.get("timestamp_or_sec", "") or "NA"
    text = clean(row["text"])
    return f'- "{text}" ({source_file}; row {row_number}; video_id {video_id}; time {timestamp})'


def current_paths(talent: str) -> dict[str, Path]:
    talent_dir = TALENT_ROOT / talent
    output_root = talent_dir / "qualitative coding" / "chat data"
    return {
        "talent_dir": talent_dir,
        "source_root": talent_dir / "text_playback",
        "output_root": output_root,
        "open_state": output_root / "chat_personality_open_coding" / "current" / "chat_open_coding_state.json",
        "open_codebook": output_root / "chat_personality_open_coding" / "current" / "chat_open_codebook.csv",
        "open_evidence": output_root / "chat_personality_open_coding" / "current" / "chat_open_coding_evidence.csv",
        "profile_md": output_root / "chat_personality_profile.md",
        "evidence_csv": output_root / "chat_personality_evidence_log.csv",
        "state_json": output_root / "chat_personality_state.json",
        "snapshot_dir": output_root / "snapshots",
    }


def build_profile(
    talent: str,
    analysis_stamp: str,
    state: dict,
    codebook_rows: list[dict[str, str]],
    evidence_rows: list[dict[str, str]],
    output_root: Path,
    source_root: Path,
) -> str:
    codebook = {row["code_id"]: row for row in codebook_rows}
    evidence_by_code: dict[str, list[dict[str, str]]] = {}
    for row in evidence_rows:
        evidence_by_code.setdefault(row["code_id"], []).append(row)

    confidence = "Medium"
    highlights: list[str] = []
    for code_id in ("CHAT_PC_01", "CHAT_PC_02", "CHAT_PC_03", "CHAT_PC_04"):
        for row in evidence_by_code.get(code_id, [])[:3]:
            highlights.append(quote_line(row))
    highlights = highlights[:12]

    code_counts = {item["code_id"]: item for item in state["codes_retained"]}
    lines: list[str] = [
        f"Analysis conducted: {analysis_stamp}",
        f"Talent: {talent}",
        f"Source root: {source_root}/",
        f"Output root: {output_root}/",
        f"Replay files inspected: {state['files_inspected']}",
        f"Chat rows considered: {state['chat_rows_considered']}",
        f"Streamer/context rows used only as context: {state['context_rows_used']}",
        f"Confidence: {confidence}",
        "",
        "## 1) Chat Community Personality Summary",
        (
            "This chat reads like a room that performs membership out loud. The most visible habit is chant-based assembly: "
            "viewers do not only say hello or react, they reuse recurring slogans such as `AVA LA AVALUTION` and `CLARITY` "
            "to announce presence, sync energy, and show that they know the room's language."
        ),
        (
            "Once a moment starts landing, the community moves fast into synchronized hype. Short bursts like `LET'S GO`, "
            "elongated cheering, and stacked reaction posts make the room feel collectively keyed to the same beat rather than "
            "like a series of isolated comments."
        ),
        (
            "A second strong tendency is affection-forward validation. Praise is often direct and explicit: chat tells Ava she is "
            "`cute`, says `love you`, voices pride, and uses reassurance when she sounds tired or exposed. That means support here "
            "is not just generic applause; it is openly relational and publicly ratifies the moment."
        ),
        (
            "The community also likes to participate by pushing the bit further. Repeated calls to spin the wheel, do something again, "
            "or stop being a `COWARD` show a room that co-authors stakes through teasing escalation pressure. Taken together, the most "
            "distinctive tendencies are chant-led room assembly, synchronized hype pile-ons, affection-forward validation, and playful "
            "pressure toward bigger payoffs."
        ),
        "",
        "## 2) Major Chat Interaction Patterns",
    ]

    for code_id in ("CHAT_PC_01", "CHAT_PC_02", "CHAT_PC_03", "CHAT_PC_04"):
        spec = PATTERN_SPECS[code_id]
        retained = code_counts[code_id]
        lines.extend(
            [
                f"### {spec.pattern_name}",
                f"- pattern_name: {spec.pattern_name}",
                f"- what chat does: {spec.what_chat_does}",
                f"- interactional function: {spec.interactional_function}",
                f"- how common it appears: {spec.how_common} Open-coding retained {retained['row_count']} supporting rows across {retained['source_count']} source files.",
                f"- evidence notes: {spec.evidence_notes}",
                "",
            ]
        )

    lines.extend(
        [
            "## 3) Relationship To Streamer Context",
            (
                "Chat's visible style appears highly prompt-sensitive. Chant language often appears at openings, rallies, or named-room moments; "
                "hype bursts flare when songs, reveals, jokes, or risky gameplay beats land; and affection-heavy validation frequently follows "
                "moments where the streamer sounds tired, proud, flustered, or emotionally open."
            ),
            (
                "The escalation pattern also looks context-bound rather than random. Viewers most often push for repeats, punishments, wheel spins, "
                "or continued bits after the streamer introduces a challenge, a high note, an embarrassing moment, or some stated boundary that becomes "
                "playfully contestable in chat. That supports reading these behaviors as audience response habits, not as direct evidence about the streamer's stable personality."
            ),
            "",
            "## 4) Recurring Language, Rituals, And In-Jokes",
            "- `AVA LA AVALUTION`: A slogan-like room marker that works as greeting, rallying cry, and in-group membership signal.",
            "- `CLARITY`: A repeated chant used to intensify a moment and show synchronized uptake.",
            "- `Ava cute`: A compact recurring praise formula that turns affection into a recognizable group response.",
            "- `SPIN THAT WHEEL`: A coordination phrase for escalating stakes when a punishment or choice mechanic is on the table.",
            "- `Do it again` and `COWARD`: Teasing pressure phrases that publicly invite encore behavior or mock resistance.",
            "",
            "## 5) Boundaries And Risk Notes",
            (
                "The strongest boundary pattern is playful testing rather than outright disregard. Chat frequently nudges for more commitment, another repeat, or a harsher payoff, "
                "but the available text alone does not show whether every instance lands comfortably in the room."
            ),
            (
                "Affectionate praise and teasing escalation can sit close together, which means individual lines are easy to over-read if separated from their immediate stream context. "
                "A single flirty, sexual, or confrontational message should not be treated as representative unless it recurs as a broader community practice."
            ),
            (
                "There is also some risk of mixing real audience behavior with automation or account-owned chat prompts. The final profile therefore treats clearly audience-like recurring patterns "
                "as primary and avoids building claims on isolated CTA-style lines."
            ),
            "",
            "## 6) Evidence Highlights",
            *highlights,
        ]
    )
    return "\n".join(lines) + "\n"


def build_evidence_log(
    talent: str,
    evidence_rows: list[dict[str, str]],
) -> list[dict[str, str]]:
    output: list[dict[str, str]] = []
    for row in evidence_rows:
        spec = PATTERN_SPECS[row["code_id"]]
        evidence_strength = "high" if row["code_id"] != "CHAT_PC_04" else "medium"
        output.append(
            {
                "talent": talent,
                "source_file": row["source_file"],
                "video_id": row.get("video_id", ""),
                "row_number": row.get("row_number", ""),
                "timestamp_or_sec": row.get("timestamp_or_sec", ""),
                "chat_identifier": row.get("chat_identifier", ""),
                "text": clean(row.get("text", "")),
                "pattern_name": spec.pattern_name,
                "interactional_function": spec.interactional_function,
                "streamer_context_summary": clean(row.get("streamer_context_summary", "")),
                "evidence_strength": evidence_strength,
                "notes": f"Derived from {row['code_id']} open-coding evidence.",
            }
        )
    return output


def build_state(
    talent: str,
    analysis_stamp: str,
    source_root: Path,
    output_root: Path,
    state: dict,
) -> dict:
    patterns = []
    for code_id in ("CHAT_PC_01", "CHAT_PC_02", "CHAT_PC_03", "CHAT_PC_04"):
        retained = next(item for item in state["codes_retained"] if item["code_id"] == code_id)
        spec = PATTERN_SPECS[code_id]
        patterns.append(
            {
                "pattern_name": spec.pattern_name,
                "code_id": code_id,
                "interactional_function": spec.interactional_function,
                "how_common_it_appears": spec.how_common,
                "supporting_source_files": retained["source_count"],
                "supporting_rows": retained["row_count"],
                "confidence": retained["confidence"],
            }
        )

    return {
        "talent": talent,
        "analysis_conducted": analysis_stamp,
        "source_root": str(source_root),
        "output_root": str(output_root),
        "files_inspected": state["files_inspected"],
        "chat_rows_considered": state["chat_rows_considered"],
        "context_rows_used": state["context_rows_used"],
        "patterns": patterns,
        "limitations": state["limitations"],
    }


def maybe_write_snapshot(profile_path: Path, snapshot_dir: Path, new_text: str, now: datetime) -> Path | None:
    if not profile_path.exists():
        return None
    prior_text = profile_path.read_text(encoding="utf-8")
    if prior_text == new_text:
        return None
    snapshot_dir.mkdir(parents=True, exist_ok=True)
    snapshot_path = snapshot_dir / f"chat_personality_profile_{now.strftime('%Y-%m-%d_%H-%M-%S_%Z')}.md"
    snapshot_path.write_text(prior_text, encoding="utf-8")
    return snapshot_path


def write_run_note(log_dir: Path, now: datetime, talent: str, outputs: list[Path], snapshot: Path | None) -> Path:
    log_dir.mkdir(parents=True, exist_ok=True)
    note_path = log_dir / f"{now.strftime('%Y-%m-%d_%H-%M-%S_%Z')}_{talent}_final_message.md"
    touched = "\n".join(f"- {path}" for path in outputs)
    snapshot_line = f"\n- {snapshot}" if snapshot else ""
    note_path.write_text(
        (
            f"Analysis conducted: {now.strftime('%Y-%m-%d %H:%M %Z')}\n\n"
            f"Processed talent: {talent}\n\n"
            f"Outputs touched:\n{touched}\n"
            f"{'Snapshot written:' + snapshot_line if snapshot else 'Snapshot written: none'}\n"
        ),
        encoding="utf-8",
    )
    return note_path


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--talent", required=True)
    args = parser.parse_args()

    now = datetime.now(TIMEZONE)
    analysis_stamp = now.strftime("%Y-%m-%d %H:%M %Z")
    paths = current_paths(args.talent)
    paths["output_root"].mkdir(parents=True, exist_ok=True)

    state = load_json(paths["open_state"])
    codebook_rows = read_csv(paths["open_codebook"])
    evidence_rows = read_csv(paths["open_evidence"])

    profile_text = build_profile(
        talent=args.talent,
        analysis_stamp=analysis_stamp,
        state=state,
        codebook_rows=codebook_rows,
        evidence_rows=evidence_rows,
        output_root=paths["output_root"],
        source_root=paths["source_root"],
    )
    snapshot = maybe_write_snapshot(paths["profile_md"], paths["snapshot_dir"], profile_text, now)
    paths["profile_md"].write_text(profile_text, encoding="utf-8")

    evidence_fieldnames = [
        "talent",
        "source_file",
        "video_id",
        "row_number",
        "timestamp_or_sec",
        "chat_identifier",
        "text",
        "pattern_name",
        "interactional_function",
        "streamer_context_summary",
        "evidence_strength",
        "notes",
    ]
    final_evidence = build_evidence_log(args.talent, evidence_rows)
    write_csv(paths["evidence_csv"], evidence_fieldnames, final_evidence)

    final_state = build_state(
        talent=args.talent,
        analysis_stamp=analysis_stamp,
        source_root=paths["source_root"],
        output_root=paths["output_root"],
        state=state,
    )
    paths["state_json"].write_text(json.dumps(final_state, indent=2) + "\n", encoding="utf-8")

    outputs = [paths["profile_md"], paths["evidence_csv"], paths["state_json"]]
    write_run_note(LOG_ROOT, now, args.talent, outputs, snapshot)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
