#!/usr/bin/env python3

from __future__ import annotations

import argparse
import csv
import hashlib
import json
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Iterable
from zoneinfo import ZoneInfo


TALENT_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
OUTPUT_ROOT = Path(
    "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/concept_areas/interaction_views/chat_shared"
)
CURRENT_DIR = OUTPUT_ROOT / "current"
SNAPSHOT_DIR = OUTPUT_ROOT / "snapshots"
TIMEZONE = ZoneInfo("America/New_York")
AGGREGATE_DIR_NAMES = {
    "Northstar Story Lab Demo",
}
SHARED_FAMILIES = [
    {
        "shared_code_id": "SCB01",
        "shared_code_name": "Room Presence Rituals",
        "definition": (
            "Chat publicly marks arrival, departure, raid reception, and room presence so the stream "
            "functions as a socially assembled gathering."
        ),
        "inclusion_criteria": (
            "Greetings, welcome-raider lines, room-addressed check-ins, goodbye formulas, and other "
            "messages that make audience presence visible."
        ),
        "exclusion_criteria": (
            "Pure support messages about outcomes, generic reactions to on-stream moments, and one-off "
            "comments that do not perform room assembly."
        ),
        "common_interactional_function": (
            "Keeps the audience legible to itself as a recurring room rather than a passive comment feed."
        ),
        "mappings": {
            "Avaritia Hawthorne 【Variance Project】": ["CHAT_PC_01"],
            "Nova Aokami Ch": ["CP01"],
            "Leia Memoria【Variance Project】": ["C01"],
        },
    },
    {
        "shared_code_id": "SCB02",
        "shared_code_name": "Participatory Co-Shaping",
        "definition": (
            "Chat acts as a live co-shaping layer by steering choices, offering advice, supplying fast "
            "decision input, or pushing the stream toward escalation when the format invites it."
        ),
        "inclusion_criteria": (
            "Tactical advice, troubleshooting, rule or lore clarification, content suggestions, puzzle/game "
            "directions, short collective answer swarms, and crowd pressure that tries to push the next beat "
            "toward repetition, escalation, or a penalty."
        ),
        "exclusion_criteria": (
            "Pure cheering with no shaping function, generic reactions, and longer commentary that does not "
            "attempt to influence what happens next."
        ),
        "common_interactional_function": (
            "Positions the audience as an active shaping force rather than a purely reactive crowd."
        ),
        "mappings": {
            "Avaritia Hawthorne 【Variance Project】": ["CHAT_PC_04"],
            "Nova Aokami Ch": ["CP04", "CP05"],
            "Leia Memoria【Variance Project】": ["C02"],
        },
    },
    {
        "shared_code_id": "SCB03",
        "shared_code_name": "Synchronized Reaction Bursts",
        "definition": (
            "Chat compresses collective feeling into dense short-form reaction posts that stack around the same moment."
        ),
        "inclusion_criteria": (
            "Low-word-count laughter, alarm, cheers, salutes, and similar burst reactions that mark synchronized uptake."
        ),
        "exclusion_criteria": (
            "Longer evaluative commentary, explicit advice, and lore callbacks whose main function is membership signaling."
        ),
        "common_interactional_function": (
            "Creates a visible shared emotional pulse and demonstrates that the room is tracking the same beat together."
        ),
        "mappings": {
            "Avaritia Hawthorne 【Variance Project】": ["CHAT_PC_02"],
            "Nova Aokami Ch": ["CP03"],
            "Leia Memoria【Variance Project】": ["C03"],
        },
    },
    {
        "shared_code_id": "SCB04",
        "shared_code_name": "Community Support And Caretaking",
        "definition": (
            "Chat repeatedly uses encouragement, reassurance, and care-oriented language to regulate pressure and affirm the talent."
        ),
        "inclusion_criteria": (
            "Good-luck lines, congratulations, calming reassurance, pressure-reducing encouragement, and explicit care-taking language."
        ),
        "exclusion_criteria": (
            "Neutral greetings, purely informational advice, and jokes or reactions with no support or care function."
        ),
        "common_interactional_function": (
            "Builds the room as a supportive social surround that responds to outcomes, nerves, workload, and setbacks."
        ),
        "mappings": {
            "Avaritia Hawthorne 【Variance Project】": ["CHAT_PC_03"],
            "Nova Aokami Ch": ["CP02"],
            "Leia Memoria【Variance Project】": ["C05"],
        },
    },
    {
        "shared_code_id": "SCB05",
        "shared_code_name": "In-Group Identity And Callback Framing",
        "definition": (
            "Chat sustains community identity through repeated in-group labels, stock phrases, and local callback language."
        ),
        "inclusion_criteria": (
            "Community self-labeling, room slogans, recurring shared phrases, and shorthand that depends on ongoing channel memory."
        ),
        "exclusion_criteria": (
            "Generic meme language with no local tie, one-off jokes, and ordinary greetings without a community-identity function."
        ),
        "common_interactional_function": (
            "Signals membership and continuity by letting participation double as belonging performance."
        ),
        "mappings": {
            "Avaritia Hawthorne 【Variance Project】": ["CHAT_PC_01"],
            "Nova Aokami Ch": ["CP06"],
            "Leia Memoria【Variance Project】": ["C04"],
        },
    },
]


@dataclass(frozen=True)
class EligibleTalent:
    talent: str
    root: Path
    codebook_path: Path
    evidence_path: Path
    memo_path: Path


@dataclass(frozen=True)
class SkippedTalent:
    talent: str
    reason: str


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build a provisional cross-talent shared baseline for chat behavior."
    )
    parser.add_argument("--talent-root", type=Path, default=TALENT_ROOT)
    parser.add_argument("--output-root", type=Path, default=OUTPUT_ROOT)
    parser.add_argument("--focused-talent", default="")
    return parser.parse_args()


def current_chat_open_coding_dir(talent_dir: Path) -> Path:
    return (
        talent_dir
        / "qualitative coding"
        / "chat data"
        / "chat_personality_open_coding"
        / "current"
    )


def discover_chat_open_coding(
    talent_root: Path,
) -> tuple[list[EligibleTalent], list[SkippedTalent]]:
    eligible: list[EligibleTalent] = []
    skipped: list[SkippedTalent] = []

    for path in sorted(talent_root.iterdir(), key=lambda value: value.name.lower()):
        if not path.is_dir() or path.name in AGGREGATE_DIR_NAMES:
            continue

        current_dir = current_chat_open_coding_dir(path)
        codebook_path = current_dir / "chat_open_codebook.csv"
        evidence_path = current_dir / "chat_open_coding_evidence.csv"
        memo_path = current_dir / "chat_open_coding_memo.md"

        missing = [
            name
            for name, candidate in [
                ("chat_open_codebook.csv", codebook_path),
                ("chat_open_coding_evidence.csv", evidence_path),
                ("chat_open_coding_memo.md", memo_path),
            ]
            if not candidate.exists() or candidate.stat().st_size == 0
        ]
        if missing:
            skipped.append(
                SkippedTalent(
                    talent=path.name,
                    reason=f"missing usable chat open-coding outputs: {', '.join(missing)}",
                )
            )
            continue

        eligible.append(
            EligibleTalent(
                talent=path.name,
                root=path,
                codebook_path=codebook_path,
                evidence_path=evidence_path,
                memo_path=memo_path,
            )
        )

    return eligible, skipped


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, fieldnames: list[str], rows: Iterable[dict[str, object]]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def slugify_code_id(index: int) -> str:
    return f"SCB{index:02d}"


def join_labels(labels: list[str]) -> str:
    if not labels:
        return ""
    if len(labels) == 1:
        return labels[0]
    if len(labels) == 2:
        return f"{labels[0]} and {labels[1]}"
    return f"{', '.join(labels[:-1])}, and {labels[-1]}"


def local_variant_summary(
    local_codes: list[dict[str, str]],
    focus_match: bool,
) -> str:
    code_names = [row["code_name"] for row in local_codes]
    functions = [row["interactional_function"] for row in local_codes]
    parts = [
        f"Observed locally as {join_labels([f'`{name}`' for name in code_names])}.",
        f"Local interactional emphasis: {join_labels(functions)}.",
    ]
    if len(local_codes) > 1:
        parts.append(
            "This shared family is split across multiple local codes rather than a single exact counterpart."
        )
    if focus_match:
        parts.append("This run's focus note matches this talent.")
    return " ".join(parts)


def render_family_paragraphs(
    shared_rows: list[dict[str, object]],
    family_details: dict[str, dict[str, object]],
) -> str:
    paragraphs = []
    for row in shared_rows:
        detail = family_details[row["shared_code_id"]]
        paragraphs.append(
            "\n".join(
                [
                    f"### {row['shared_code_id']} {row['shared_code_name']}",
                    f"{row['definition']}",
                    f"Observed across {row['talents_observed']}.",
                    f"Shared function: {row['common_interactional_function']}",
                    f"Local variants: {detail['variant_summary']}",
                ]
            )
        )
    return "\n\n".join(paragraphs)


def summarize_shared_scope(shared_rows: list[dict[str, object]], eligible_count: int) -> str:
    shared_count = len(shared_rows)
    if shared_count == 0:
        return (
            "No confirmed cross-talent baseline can be asserted from this run because fewer than two talents "
            "currently align around an evidence-backed shared family."
        )

    across_all = sum(
        1
        for row in shared_rows
        if len(str(row["talents_observed"]).split(", ")) == eligible_count
    )
    across_subset = shared_count - across_all
    parts = [
        f"{shared_count} broad audience-side behavior families recur across the currently eligible chat communities."
    ]
    if across_all:
        parts.append(
            f"{across_all} appear across all {eligible_count} eligible talents."
        )
    if across_subset:
        parts.append(
            f"{across_subset} are shared across a narrower multi-talent subset rather than the full set."
        )
    parts.append(
        "Later profiling work should treat these family-level patterns as baseline community behavior before arguing that a local expression is distinctive."
    )
    return " ".join(parts)


def stable_hash(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def maybe_write_snapshot(snapshot_dir: Path, markdown: str, analysis_dt: datetime) -> str | None:
    current_path = snapshot_dir.parent / "current" / "chat_shared_behavior_baseline.md"
    current_text = current_path.read_text(encoding="utf-8") if current_path.exists() else ""
    if stable_hash(current_text) == stable_hash(markdown):
        return None

    snapshot_dir.mkdir(parents=True, exist_ok=True)
    snapshot_path = snapshot_dir / f"chat_shared_behavior_baseline_{analysis_dt.strftime('%Y-%m-%d_%H-%M-%S_%Z')}.md"
    snapshot_path.write_text(markdown, encoding="utf-8")
    return str(snapshot_path)


def build_outputs(
    eligible: list[EligibleTalent],
    skipped: list[SkippedTalent],
    focused_talent: str,
    output_root: Path,
) -> dict[str, str]:
    analysis_dt = datetime.now(TIMEZONE)
    current_dir = output_root / "current"
    snapshot_dir = output_root / "snapshots"
    current_dir.mkdir(parents=True, exist_ok=True)
    snapshot_dir.mkdir(parents=True, exist_ok=True)

    shared_rows: list[dict[str, object]] = []
    matrix_rows: list[dict[str, object]] = []
    evidence_rows: list[dict[str, object]] = []
    family_details: dict[str, dict[str, object]] = {}

    codebooks_by_talent: dict[str, dict[str, dict[str, str]]] = {}
    evidence_by_talent: dict[str, list[dict[str, str]]] = {}
    evidence_group_counts: dict[tuple[str, str], int] = defaultdict(int)

    for talent in eligible:
        codebook_rows = read_csv_rows(talent.codebook_path)
        evidence_source_rows = read_csv_rows(talent.evidence_path)
        codebooks_by_talent[talent.talent] = {
            row["code_id"]: row
            for row in codebook_rows
        }
        evidence_by_talent[talent.talent] = evidence_source_rows
        for evidence_row in evidence_source_rows:
            evidence_group_counts[(talent.talent, evidence_row["code_id"])] += 1

    eligible_names = {row.talent for row in eligible}
    family_configs = []
    for family in SHARED_FAMILIES:
        observed = [
            talent_name
            for talent_name in family["mappings"]
            if talent_name in eligible_names
        ]
        if len(observed) >= 2:
            family_configs.append(family)

    for family in family_configs:
        shared_code_id = family["shared_code_id"]
        observed_talents: list[str] = []
        variant_chunks: list[str] = []
        total_evidence_count = 0

        observed_talent_set = {
            talent_name
            for talent_name in family["mappings"]
            if talent_name in eligible_names
        }

        for talent_name in sorted(eligible_names):
            local_code_ids = family["mappings"].get(talent_name, [])
            local_code_rows = [
                codebooks_by_talent[talent_name][code_id]
                for code_id in local_code_ids
                if code_id in codebooks_by_talent[talent_name]
            ]
            if not local_code_rows:
                matrix_rows.append(
                    {
                        "talent": talent_name,
                        "shared_code_id": shared_code_id,
                        "shared_code_name": family["shared_code_name"],
                        "present": "no",
                        "local_variant_summary": "",
                        "evidence_count": 0,
                        "confidence": "",
                    }
                )
                continue

            observed_talents.append(talent_name)
            local_evidence_count = sum(
                evidence_group_counts[(talent_name, code_id)]
                for code_id in local_code_ids
            )
            total_evidence_count += local_evidence_count
            summary = local_variant_summary(
                local_code_rows,
                focus_match=focused_talent.strip() == talent_name,
            )
            variant_chunks.append(f"{talent_name}: {summary}")
            matrix_rows.append(
                {
                    "talent": talent_name,
                    "shared_code_id": shared_code_id,
                    "shared_code_name": family["shared_code_name"],
                    "present": "yes",
                    "local_variant_summary": summary,
                    "evidence_count": local_evidence_count,
                    "confidence": join_labels(
                        [row["confidence"] for row in local_code_rows]
                    ),
                }
            )

            code_id_set = set(local_code_ids)
            for evidence_row in evidence_by_talent[talent_name]:
                if evidence_row["code_id"] not in code_id_set:
                    continue
                evidence_rows.append(
                    {
                        "talent": talent_name,
                        "source_file": evidence_row["source_file"],
                        "row_number": evidence_row["row_number"],
                        "timestamp_or_sec": evidence_row["timestamp_or_sec"],
                        "text": evidence_row["text"],
                        "local_code_id": evidence_row["code_id"],
                        "shared_code_id": shared_code_id,
                        "shared_code_name": family["shared_code_name"],
                        "notes": "Mapped from local chat open-coding evidence into a cross-talent shared behavior family.",
                    }
                )

        family_details[shared_code_id] = {
            "variant_summary": " | ".join(variant_chunks),
        }
        shared_rows.append(
            {
                "shared_code_id": shared_code_id,
                "shared_code_name": family["shared_code_name"],
                "definition": family["definition"],
                "inclusion_criteria": family["inclusion_criteria"],
                "exclusion_criteria": family["exclusion_criteria"],
                "common_interactional_function": family["common_interactional_function"],
                "talents_observed": ", ".join(observed_talents),
                "evidence_count": total_evidence_count,
                "baseline_strength": (
                    "shared_across_all_eligible_talents"
                    if observed_talent_set == eligible_names
                    else "shared_across_multiple_talents"
                ),
                "notes": (
                    "Built from mapped local open-coding families. Preserve the listed local variants rather "
                    "than treating the talent-specific expression as interchangeable."
                ),
            }
        )

    skipped_lines = (
        "\n".join(f"- {row.talent}: {row.reason}" for row in skipped)
        if skipped
        else "- None."
    )
    focus_line = (
        f"Focused review note: `{focused_talent}`.\n"
        if focused_talent.strip()
        else ""
    )
    family_block = (
        render_family_paragraphs(shared_rows, family_details)
        if shared_rows
        else "No evidence-backed shared families can be confirmed yet because fewer than two talents are currently eligible."
    )

    if len(shared_rows) >= 1:
        baseline_summary = summarize_shared_scope(shared_rows, len(eligible))
        distinctiveness_guidance = (
            "Treat the shared families in this baseline as generic at the family level. Mark something as "
            "distinctive-in-form when the same broad family is present across talents but the execution differs "
            "materially, such as Avaritia's chant-heavy room assembly and teasing dare-pushes, Nova's compressed "
            "consensus-answer swarms and bestie framing, or Leia's stronger callback-lore and explicit reassurance "
            "layer. Reserve potentially unique claims for patterns that remain outside these shared families after "
            "checking additional eligible talents."
        )
    else:
        baseline_summary = (
            "No confirmed cross-talent baseline can be asserted from this run because fewer than two talents have usable current "
            "chat open-coding outputs. The output should be treated as a provisional candidate baseline only."
        )
        distinctiveness_guidance = (
            "Do not automatically treat any current family as generic across talents yet. Use provisional families only as "
            "comparison checkpoints when additional talent chat open-coding becomes eligible."
        )

    markdown = "\n".join(
        [
            f"Analysis conducted: {analysis_dt.strftime('%Y-%m-%d %H:%M %Z')}",
            f"Eligible talents included: {len(eligible)}",
            "Primary upstream source: chat_personality_open_coding",
            "",
            "## 1) Shared Chat Baseline Summary",
            baseline_summary,
            focus_line.rstrip(),
            "",
            "## 2) Shared Chat Behavior Families",
            family_block,
            "",
            "## 3) Distinctiveness Guidance",
            distinctiveness_guidance,
            "",
            "## 4) Limitations",
            "- Shared-behavior confirmation is limited to talents with current usable chat open-coding outputs; missing or stale upstream work will suppress broader comparison.",
            "- Streamer behavior was not used as evidence; all mappings come from audience chat codebook and evidence rows.",
            "- Shared families were created by analytic mapping across local codebooks, so later updates should re-check family boundaries when more talents become eligible.",
            "- Skipped talents are listed below for transparency.",
            skipped_lines,
        ]
    ).replace("\n\n\n", "\n\n")

    markdown_path = current_dir / "chat_shared_behavior_baseline.md"
    codebook_path = current_dir / "chat_shared_behavior_codebook.csv"
    matrix_path = current_dir / "talent_chat_behavior_matrix.csv"
    evidence_path = current_dir / "chat_shared_behavior_evidence.csv"
    state_path = current_dir / "chat_shared_behavior_state.json"

    snapshot_path = maybe_write_snapshot(snapshot_dir, markdown, analysis_dt)

    markdown_path.write_text(markdown + "\n", encoding="utf-8")
    write_csv(
        codebook_path,
        [
            "shared_code_id",
            "shared_code_name",
            "definition",
            "inclusion_criteria",
            "exclusion_criteria",
            "common_interactional_function",
            "talents_observed",
            "evidence_count",
            "baseline_strength",
            "notes",
        ],
        shared_rows,
    )
    write_csv(
        matrix_path,
        [
            "talent",
            "shared_code_id",
            "shared_code_name",
            "present",
            "local_variant_summary",
            "evidence_count",
            "confidence",
        ],
        matrix_rows,
    )
    write_csv(
        evidence_path,
        [
            "talent",
            "source_file",
            "row_number",
            "timestamp_or_sec",
            "text",
            "local_code_id",
            "shared_code_id",
            "shared_code_name",
            "notes",
        ],
        evidence_rows,
    )

    state = {
        "analysis_conducted_at": analysis_dt.isoformat(),
        "primary_upstream_source": "chat_personality_open_coding",
        "focused_talent": focused_talent or None,
        "eligible_talents": [row.talent for row in eligible],
        "eligible_talent_count": len(eligible),
        "skipped_talents": [
            {"talent": row.talent, "reason": row.reason}
            for row in skipped
        ],
        "shared_code_count": len(shared_rows),
        "evidence_row_count": len(evidence_rows),
        "baseline_status": (
            "confirmed_cross_talent" if len(shared_rows) >= 1 else "provisional_single_talent_only"
        ),
        "latest_snapshot_path": snapshot_path,
    }
    state_path.write_text(
        json.dumps(state, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )

    return {
        "markdown_path": str(markdown_path),
        "codebook_path": str(codebook_path),
        "matrix_path": str(matrix_path),
        "evidence_path": str(evidence_path),
        "state_path": str(state_path),
        "snapshot_path": snapshot_path or "",
    }


def main() -> None:
    args = parse_args()
    eligible, skipped = discover_chat_open_coding(args.talent_root)
    paths = build_outputs(
        eligible=eligible,
        skipped=skipped,
        focused_talent=args.focused_talent,
        output_root=args.output_root,
    )
    result = {
        "eligible_talents": [row.talent for row in eligible],
        "skipped_talents": [row.talent for row in skipped],
        **paths,
    }
    print(json.dumps(result, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
