#!/usr/bin/env python3
"""Build talent-specific unique chat personality outputs from open-coding and baseline artifacts."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Iterable
from zoneinfo import ZoneInfo


THIS_FILE = Path(__file__).resolve()
PY_SCRIPTS_ROOT = next(
    parent
    for parent in [*THIS_FILE.parents]
    if (parent / "lib" / "utils" / "repo_paths.py").exists()
)
LIB_ROOT = PY_SCRIPTS_ROOT / "lib"
if str(LIB_ROOT) not in sys.path:
    sys.path.insert(0, str(LIB_ROOT))

from utils.repo_paths import find_repo_root


REPO_ROOT = find_repo_root(Path(__file__))
TALENT_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
BASELINE_ROOT = Path(
    "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/concept_areas/interaction_views/chat_shared/current"
)
TIMEZONE = ZoneInfo("America/New_York")


@dataclass(frozen=True)
class CandidatePattern:
    name: str
    supporting_code_ids: tuple[str, ...]
    distinctive_why: str
    shared_variant_note: str
    evidence_note: str
    not_unique_note: str


CANDIDATE_PATTERNS = (
    CandidatePattern(
        name="Bestie-coded room membership",
        supporting_code_ids=("CP06", "CP01"),
        distinctive_why=(
            "The room repeatedly marks participation as membership rather than passive viewership. "
            "Attendance language and explicit `bestie` address combine to make arrival itself part of the group's social performance."
        ),
        shared_variant_note=(
            "Attendance and in-group framing are baseline-level behaviors in the current artifacts, but Nova's room keeps turning them into explicit bestie/member talk."
        ),
        evidence_note=(
            "Evidence rows repeatedly show greetings and check-ins phrased as `bestie/besties`, not just neutral hello/good evening messages."
        ),
        not_unique_note=(
            "Simple greetings on their own are too common across chats to count as unique without the bestie/in-group framing."
        ),
    ),
    CandidatePattern(
        name="Game-literate co-pilot chat",
        supporting_code_ids=("CP05", "CP04"),
        distinctive_why=(
            "Chat often operates like a fast expert bench: viewers answer prompts, give build advice, and supply rules or timing reminders in compressed, decision-ready form."
        ),
        shared_variant_note=(
            "Prompted consensus and peer coaching are shared behavior families in the baseline, but Nova's evidence keeps tying them to trading-card, gacha, and game-system literacy."
        ),
        evidence_note=(
            "Representative rows include deck advice, mechanic clarifications, maintenance reminders, and short yes/no or option-answer swarms during decisions."
        ),
        not_unique_note=(
            "Generic yes/no replies alone are too common to treat as distinctive; the stronger signal is the combination of consensus replies with topic-expert coaching."
        ),
    ),
    CandidatePattern(
        name="Fast support-and-reaction pile-ons",
        supporting_code_ids=("CP02", "CP03"),
        distinctive_why=(
            "The room flips quickly between cheering, groaning, and laughing in tightly synchronized bursts, giving high-variance moments a visibly communal pulse."
        ),
        shared_variant_note=(
            "Support bursts and swarm reactions are both present in the baseline. Nova's local version appears especially tuned to swing moments in pulls, deck lines, and other abrupt success/failure beats."
        ),
        evidence_note=(
            "The retained evidence shows encouragement and celebration living next to dense `oh no`, `LMAO`, and similar one-line swarm reactions."
        ),
        not_unique_note=(
            "Short laughter or hype messages by themselves are not unique enough; they matter here only as part of repeated synchronized support/reaction pivots."
        ),
    ),
)

TALENT_CANDIDATE_PATTERNS: dict[str, tuple[CandidatePattern, ...]] = {
    "Avaritia Hawthorne 【Variance Project】": (
        CandidatePattern(
            name="Chant-led room assembly",
            supporting_code_ids=("CHAT_PC_01",),
            distinctive_why=(
                "This room does not just mark presence through ordinary greetings. "
                "It repeatedly assembles itself through slogan and chant language, so joining chat often looks like stepping into an already-running communal refrain."
            ),
            shared_variant_note=(
                "Room-presence rituals and in-group identity framing are baseline-level behaviors across eligible talents, but Avaritia's room expresses them in a notably chant-heavy, creed-like form."
            ),
            evidence_note=(
                "Representative rows show repeated `AVA LA AVALUTION` and `CLARITY` invocations functioning as crowd-synchronizing room markers rather than isolated jokes."
            ),
            not_unique_note=(
                "Generic greetings or welcome messages are too common across chats to count as distinctive without the repeated chant/slogan structure."
            ),
        ),
        CandidatePattern(
            name="Escalation-as-participation",
            supporting_code_ids=("CHAT_PC_04", "CHAT_PC_02"),
            distinctive_why=(
                "Chat often participates by pushing the bit further: asking for repeats, penalties, encore moments, or more committed follow-through. "
                "The room's co-shaping function is less about neutral advice and more about publicly urging escalation for comedic payoff."
            ),
            shared_variant_note=(
                "Participatory co-shaping is part of the shared baseline, but Avaritia's local version stands out for how often crowd input takes the form of teasing dare-pushes and escalation requests."
            ),
            evidence_note=(
                "Retained rows include `COWARD`, `SPIN THAT WHEEL`, `Do it again!`, and encore-style prompting, often surrounded by synchronized hype or mock-pressure."
            ),
            not_unique_note=(
                "Simple advice or one-off requests are too baseline-like on their own; the stronger local signal is repeated pressure toward higher stakes or repeated performance."
            ),
        ),
        CandidatePattern(
            name="Affection-forward validation swells",
            supporting_code_ids=("CHAT_PC_03", "CHAT_PC_02"),
            distinctive_why=(
                "Support in this room regularly arrives as overt praise, gratitude, and affectionate appraisal layered into crowd-reaction moments. "
                "The result is a chat norm where excitement and personal affirmation frequently land together."
            ),
            shared_variant_note=(
                "Community support and synchronized reaction are both baseline families, but Avaritia's evidence shows a particularly overt blend of chantable hype with direct `Ava cute`, `love you`, and proud-of-you validation."
            ),
            evidence_note=(
                "Representative rows pair collective energy with explicit affection, thanks, and appearance/performance praise rather than limiting support to generic cheering."
            ),
            not_unique_note=(
                "Short hype posts or isolated compliments alone are not distinctive enough; the more defensible local pattern is their repeated overlap as one crowd response style."
            ),
        ),
    ),
    "Leia Memoria【Variance Project】": (
        CandidatePattern(
            name="Owlcolyte room-assembly rituals",
            supporting_code_ids=("C01", "C04"),
            distinctive_why=(
                "The room does more than greet and say goodbye. It repeatedly assembles itself by naming both Leia and the Owlcolytes, "
                "so room presence is performed as a specific community identity rather than generic livestream politeness."
            ),
            shared_variant_note=(
                "Room-presence rituals and in-group identity work both appear in the shared baseline, but Leia's room keeps binding them together through Owlcolyte naming, "
                "raid welcomes, and callback-rich transition language."
            ),
            evidence_note=(
                "Representative rows greet Leia and the Owlcolytes together, welcome raiders into the room, and reuse callback phrases during transitions and exits."
            ),
            not_unique_note=(
                "Plain greetings or farewells alone are too common across chats to count as unique without the Owlcolyte/community-label layer."
            ),
        ),
        CandidatePattern(
            name="Cross-context co-steering",
            supporting_code_ids=("C02",),
            distinctive_why=(
                "Chat acts as a live co-decision layer across multiple stream modes rather than only inside gameplay. "
                "The same steering function shows up in puzzle solving, drawing decisions, and general content-shaping suggestions."
            ),
            shared_variant_note=(
                "Participatory co-steering is part of the shared baseline, but Leia's local version is distinctive for how consistently it travels across game, art, and talk/work contexts."
            ),
            evidence_note=(
                "Retained evidence includes troubleshooting directions, collective wording around choices, and concrete suggestions that shape drawings or what Leia checks next."
            ),
            not_unique_note=(
                "Advice or backseating by itself is too common to treat as unique; the stronger local signal is how broadly the co-steering function appears across content types."
            ),
        ),
        CandidatePattern(
            name="Warm pressure-regulating support",
            supporting_code_ids=("C05",),
            distinctive_why=(
                "Support in this room is not just celebratory. Chat repeatedly monitors workload, fatigue, and emotional pressure, and often folds care language into greetings, check-ins, and permission to step away."
            ),
            shared_variant_note=(
                "Community support and caretaking are shared baseline behaviors, but Leia's room shows a particularly explicit pressure-regulating form centered on wellbeing checks, calm reassurance, and workload-sensitive encouragement."
            ),
            evidence_note=(
                "Representative rows wish Leia well, tell her to take care of herself, and convert ordinary check-ins into explicit wellbeing monitoring."
            ),
            not_unique_note=(
                "Generic encouragement or hype should not be treated as unique on its own; the stronger pattern is explicit care-taking around pressure, fatigue, and recovery."
            ),
        ),
    ),
}

TALENT_SHARED_LOCAL_VARIANTS: dict[str, list[str]] = {
    "Avaritia Hawthorne 【Variance Project】": [
        "Shared room-presence behavior takes a chant-led form here, with Avalution and Clarity slogans turning ordinary arrival or emphasis into visible membership performance.",
        "Shared co-shaping behavior often becomes escalation pressure, where chat pushes for repeats, penalties, wheel spins, or more committed follow-through instead of staying at the level of neutral advice.",
        "Shared support behavior frequently arrives as overt affectionate appraisal, so praise, gratitude, and `Ava cute` style validation often ride the same wave as hype bursts.",
    ],
    "Leia Memoria【Variance Project】": [
        "Shared room-presence behavior takes a more explicitly named community form here, with Owlcolytes language and raid welcomes making attendance feel like room re-assembly.",
        "Shared co-steering behavior extends beyond gameplay into drawing, working, and story-navigation moments, so chat acts like a portable co-navigation layer across formats.",
        "Shared support behavior often arrives as pressure regulation rather than just celebration, with viewers explicitly checking workload, fatigue, and whether Leia should step away.",
    ],
}

TALENT_NOT_UNIQUE_ENOUGH: dict[str, list[str]] = {
    "Avaritia Hawthorne 【Variance Project】": [
        "Simple hype bursts such as `let's go`, `woo`, or `oh no` are too common across livestream chats to count as unique without the stronger chant or escalation context.",
        "Basic compliments or thank-you messages are not distinctive on their own; the analytically stronger claim is about how often affection becomes a visible room-level norm.",
        "Any single slogan term by itself should be treated cautiously, because the distinctiveness claim rests on the repeated chant-led assembly pattern rather than one isolated phrase.",
    ],
    "Leia Memoria【Variance Project】": [
        "Basic greetings, farewells, and welcome messages are too common across livestream chats to count as unique without the Owlcolyte/community-label framing.",
        "Short reaction bursts such as `omg`, `yippee`, or `lol` are too baseline-like to support a distinctive claim on their own.",
        "Local callback phrases are analytically important, but the current baseline does not yet support claiming any single callback as cross-talent unique by itself.",
    ],
}

TALENT_LIMITATIONS: dict[str, list[str]] = {
    "Avaritia Hawthorne 【Variance Project】": [
        "All four retained local chat codes map onto shared cross-talent baseline families, so this profile emphasizes distinctive execution rather than claiming cleanly unique behavior categories.",
        "The current baseline spans 3 eligible talents; additional upstream coding from other talents could weaken or sharpen the distinctiveness of Avaritia's chant-heavy and escalation-heavy local forms.",
        "This profile treats the analytic object as chat/community behavior only; it does not make off-stream personality claims about Avaritia.",
        "Evidence is text-visible only and cannot fully recover delivery, irony, moderation context, or audiovisual timing that may affect how teasing pressure or affectionate validation lands in the room.",
    ],
    "Leia Memoria【Variance Project】": [
        "The current shared baseline only covers the talents with usable upstream chat open-coding outputs, so distinctiveness claims remain comparative within that currently available set.",
        "This profile treats the analytic object as chat/community behavior only; it does not make off-stream personality claims about Leia.",
        "Evidence is text-visible only and cannot fully recover tone, pacing, moderation context, or visual timing that may affect how a moment lands in chat.",
        "Because the shared families are present across the currently eligible talents, this output emphasizes distinctive local form rather than strong uniqueness claims.",
    ],
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build a conservative talent-specific unique chat personality profile."
    )
    parser.add_argument("--talent-name", required=True)
    parser.add_argument("--talent-root", type=Path, default=TALENT_ROOT)
    parser.add_argument("--baseline-root", type=Path, default=BASELINE_ROOT)
    return parser.parse_args()


def current_open_coding_dir(talent_dir: Path) -> Path:
    return (
        talent_dir
        / "qualitative coding"
        / "chat data"
        / "chat_personality_open_coding"
        / "current"
    )


def unique_output_root(talent_dir: Path) -> Path:
    return (
        talent_dir
        / "qualitative coding"
        / "chat data"
        / "chat_personality_unique_features"
    )


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, fieldnames: list[str], rows: Iterable[dict[str, object]]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def stable_hash(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def maybe_write_snapshot(snapshot_dir: Path, current_markdown_path: Path, markdown: str, stamp: str) -> str | None:
    prior_text = current_markdown_path.read_text(encoding="utf-8") if current_markdown_path.exists() else ""
    if stable_hash(prior_text) == stable_hash(markdown):
        return None

    snapshot_dir.mkdir(parents=True, exist_ok=True)
    snapshot_path = snapshot_dir / f"unique_chat_personality_profile_{stamp}.md"
    snapshot_path.write_text(markdown, encoding="utf-8")
    return str(snapshot_path)


def baseline_is_provisional_single_talent(
    baseline_rows: list[dict[str, str]],
    matrix_rows: list[dict[str, str]],
    talent_name: str,
) -> bool:
    strengths = {
        row.get("baseline_strength", "").strip()
        for row in baseline_rows
        if row.get("baseline_strength", "").strip()
    }
    observed_talents = {
        row.get("talent", "").strip()
        for row in matrix_rows
        if row.get("present", "").strip().lower() == "yes" and row.get("talent", "").strip()
    }
    if strengths == {"provisional_single_talent_only"}:
        return True
    return observed_talents == {talent_name}


def pick_evidence_rows(
    evidence_rows: list[dict[str, str]],
    supporting_code_ids: tuple[str, ...],
    max_rows: int = 3,
) -> list[dict[str, str]]:
    selected: list[dict[str, str]] = []
    for code_id in supporting_code_ids:
        for row in evidence_rows:
            if row.get("code_id") == code_id:
                selected.append(row)
                break
    if len(selected) < max_rows:
        for row in evidence_rows:
            if row.get("code_id") in supporting_code_ids and row not in selected:
                selected.append(row)
            if len(selected) >= max_rows:
                break
    return selected[:max_rows]


def supporting_code_names(code_ids: tuple[str, ...], codebook_by_id: dict[str, dict[str, str]]) -> list[str]:
    return [
        f"{code_id} {codebook_by_id[code_id]['code_name']}"
        for code_id in code_ids
        if code_id in codebook_by_id
    ]


def build_profile_markdown(
    talent_name: str,
    analysis_stamp: str,
    confidence: str,
    provisional_baseline: bool,
    pattern_entries: list[dict[str, object]],
    shared_local_variants: list[str],
    not_unique_enough: list[str],
    limitations: list[str],
) -> str:
    lines: list[str] = [
        f"Analysis conducted: {analysis_stamp}",
        f"Talent: {talent_name}",
        "Primary source: chat_personality_open_coding",
        "Shared baseline source used: yes",
        f"Confidence: {confidence}",
        "",
        "## 1) What Makes This Chat Community Distinctive",
    ]

    if provisional_baseline:
        lines.extend(
            [
                "No high-confidence cross-talent uniqueness can be confirmed from the current shared-baseline artifacts because the baseline is still effectively provisional and single-talent-backed.",
                "The strongest candidate distinctiveness signals are a bestie-coded membership frame, a game-literate co-pilot style, and fast support-to-reaction pile-ons. These are useful local markers, but they should be read as low-confidence candidate variants rather than confirmed cross-talent outliers.",
            ]
        )
    else:
        lines.append(
            "The strongest distinctive tendencies are the patterns below, which remain less interchangeable than the broader shared chat baseline."
        )

    lines.extend(["", "## 2) Unique Or Distinctive Chat Patterns"])
    if not pattern_entries:
        lines.append(
            "No pattern cleared the threshold for a defensible distinctive claim after baseline comparison."
        )
    else:
        for entry in pattern_entries:
            lines.extend(
                [
                    "",
                    f"### {entry['pattern_name']}",
                    f"- pattern_name: {entry['pattern_name']}",
                    f"- baseline comparison: {entry['baseline_comparison']}",
                    f"- why this is distinctive: {entry['why_distinctive']}",
                    f"- supporting local codes: {entry['supporting_local_codes']}",
                    f"- evidence notes: {entry['evidence_notes']}",
                    f"- confidence: {entry['confidence']}",
                ]
            )

    lines.extend(["", "## 3) Shared Behaviors With Local Flavor"])
    for item in shared_local_variants:
        lines.append(f"- {item}")

    lines.extend(["", "## 4) Not Unique Enough"])
    for item in not_unique_enough:
        lines.append(f"- {item}")

    lines.extend(["", "## 5) Limitations"])
    for item in limitations:
        lines.append(f"- {item}")

    return "\n".join(lines) + "\n"


def main() -> int:
    args = parse_args()

    talent_dir = args.talent_root / args.talent_name
    if not talent_dir.exists():
        raise FileNotFoundError(f"Talent folder not found: {talent_dir}")

    open_coding_dir = current_open_coding_dir(talent_dir)
    codebook_path = open_coding_dir / "chat_open_codebook.csv"
    evidence_path = open_coding_dir / "chat_open_coding_evidence.csv"
    memo_path = open_coding_dir / "chat_open_coding_memo.md"

    baseline_md_path = args.baseline_root / "chat_shared_behavior_baseline.md"
    baseline_codebook_path = args.baseline_root / "chat_shared_behavior_codebook.csv"
    matrix_path = args.baseline_root / "talent_chat_behavior_matrix.csv"
    baseline_evidence_path = args.baseline_root / "chat_shared_behavior_evidence.csv"

    required_paths = [
        codebook_path,
        evidence_path,
        memo_path,
        baseline_md_path,
        baseline_codebook_path,
        matrix_path,
        baseline_evidence_path,
    ]
    missing = [str(path) for path in required_paths if not path.exists() or path.stat().st_size == 0]
    if missing:
        raise FileNotFoundError(f"Missing required input files: {missing}")

    codebook_rows = read_csv_rows(codebook_path)
    evidence_rows = read_csv_rows(evidence_path)
    baseline_rows = read_csv_rows(baseline_codebook_path)
    matrix_rows = read_csv_rows(matrix_path)

    codebook_by_id = {row["code_id"]: row for row in codebook_rows}
    provisional_baseline = baseline_is_provisional_single_talent(
        baseline_rows=baseline_rows,
        matrix_rows=matrix_rows,
        talent_name=args.talent_name,
    )

    analysis_dt = datetime.now(TIMEZONE)
    analysis_stamp = analysis_dt.strftime("%Y-%m-%d %H:%M %Z")
    snapshot_stamp = analysis_dt.strftime("%Y-%m-%d_%H-%M-%S_%Z")
    overall_confidence = "Low" if provisional_baseline else "Medium"

    candidate_patterns = TALENT_CANDIDATE_PATTERNS.get(args.talent_name, CANDIDATE_PATTERNS)
    pattern_entries: list[dict[str, object]] = []
    evidence_log_rows: list[dict[str, object]] = []

    for pattern in candidate_patterns:
        present_code_ids = tuple(code_id for code_id in pattern.supporting_code_ids if code_id in codebook_by_id)
        if not present_code_ids:
            continue

        if provisional_baseline:
            baseline_comparison = (
                "Shared baseline artifacts were used, but the current baseline is still provisional and effectively single-talent-backed. "
                + pattern.shared_variant_note
            )
        else:
            baseline_comparison = (
                "Shared baseline artifacts were used and the current baseline reflects multiple eligible talents. "
                + pattern.shared_variant_note
            )
        confidence = "Low" if provisional_baseline else "Medium"
        supporting_names = supporting_code_names(present_code_ids, codebook_by_id)
        selected_rows = pick_evidence_rows(evidence_rows, present_code_ids)
        evidence_excerpt = "; ".join(
            f"{row['timestamp_or_sec']} `{row['text']}` ({row['code_id']})" for row in selected_rows
        )

        pattern_entries.append(
            {
                "pattern_name": pattern.name,
                "baseline_comparison": baseline_comparison,
                "why_distinctive": pattern.distinctive_why,
                "supporting_local_codes": "; ".join(supporting_names),
                "evidence_notes": f"{pattern.evidence_note} Sample retained rows: {evidence_excerpt}",
                "confidence": confidence,
            }
        )

        for row in selected_rows:
            evidence_log_rows.append(
                {
                    "talent": args.talent_name,
                    "source_file": row["source_file"],
                    "row_number": row["row_number"],
                    "timestamp_or_sec": row["timestamp_or_sec"],
                    "text": row["text"],
                    "local_code_id": row["code_id"],
                    "local_code_name": row["code_name"],
                    "unique_pattern_name": pattern.name,
                    "baseline_comparison": baseline_comparison,
                    "evidence_strength": row["evidence_strength"],
                    "notes": pattern.evidence_note,
                }
            )

    shared_local_variants = TALENT_SHARED_LOCAL_VARIANTS.get(
        args.talent_name,
        [
            "Attendance rituals often arrive already wrapped in the room's in-group vocabulary, so basic presence-marking regularly doubles as membership performance.",
            "Advice and consensus behavior are unusually game-literate in the retained evidence, with deck, rules, and system reminders showing up as part of normal chat participation.",
            "Support and reaction tend to collapse together around swing moments, so encouragement, groans, and laughter frequently read like one continuous room-level response.",
        ],
    )

    not_unique_enough = TALENT_NOT_UNIQUE_ENOUGH.get(
        args.talent_name,
        [
            "Attendance Rituals on their own are common enough in live chat that they should not be treated as unique without the bestie/in-group overlay.",
            "Swarm Reaction Bursts on their own are too baseline-like across livestream chat to support a unique-profile claim.",
            "Prompted Consensus Replies on their own are also too general; only the way they combine with topic-expert coaching feels potentially distinctive here.",
        ],
    )

    limitations = TALENT_LIMITATIONS.get(
        args.talent_name,
        [
            "The shared baseline was used, but it is still effectively provisional because the current artifacts are single-talent-backed rather than fully cross-talent-confirmed.",
            "This profile treats the analytic object as chat/community behavior only; it does not make off-stream personality claims about the talent.",
            "Evidence is text-visible only and cannot recover tone, moderation context, or visual timing that may sharpen or soften how these interaction patterns land.",
            "The current output is intentionally conservative: patterns are framed as candidate distinctive local variants rather than confirmed unique behaviors.",
        ],
    )

    markdown = build_profile_markdown(
        talent_name=args.talent_name,
        analysis_stamp=analysis_stamp,
        confidence=overall_confidence,
        provisional_baseline=provisional_baseline,
        pattern_entries=pattern_entries,
        shared_local_variants=shared_local_variants,
        not_unique_enough=not_unique_enough,
        limitations=limitations,
    )

    output_root = unique_output_root(talent_dir)
    current_dir = output_root / "current"
    snapshot_dir = output_root / "snapshots"
    current_dir.mkdir(parents=True, exist_ok=True)

    profile_path = current_dir / "unique_chat_personality_profile.md"
    evidence_log_path = current_dir / "unique_chat_personality_evidence_log.csv"
    state_path = current_dir / "unique_chat_personality_state.json"

    snapshot_path = maybe_write_snapshot(
        snapshot_dir=snapshot_dir,
        current_markdown_path=profile_path,
        markdown=markdown,
        stamp=snapshot_stamp,
    )

    profile_path.write_text(markdown, encoding="utf-8")
    write_csv(
        evidence_log_path,
        fieldnames=[
            "talent",
            "source_file",
            "row_number",
            "timestamp_or_sec",
            "text",
            "local_code_id",
            "local_code_name",
            "unique_pattern_name",
            "baseline_comparison",
            "evidence_strength",
            "notes",
        ],
        rows=evidence_log_rows,
    )

    state_payload = {
        "talent": args.talent_name,
        "analysis_conducted": analysis_stamp,
        "source_root": str(open_coding_dir),
        "output_root": str(current_dir),
        "baseline_used": True,
        "unique_patterns": pattern_entries,
        "shared_local_variants": shared_local_variants,
        "limitations": limitations,
    }
    if snapshot_path:
        state_payload["snapshot_written"] = snapshot_path

    state_path.write_text(json.dumps(state_payload, indent=2), encoding="utf-8")

    print(json.dumps(
        {
            "talent": args.talent_name,
            "profile_path": str(profile_path),
            "evidence_log_path": str(evidence_log_path),
            "state_path": str(state_path),
            "snapshot_path": snapshot_path,
            "provisional_baseline": provisional_baseline,
            "pattern_count": len(pattern_entries),
        },
        indent=2,
    ))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
