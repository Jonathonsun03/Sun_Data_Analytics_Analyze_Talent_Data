#!/usr/bin/env python3
"""Build the cumulative chat personality qualitative code log."""

from __future__ import annotations

import csv
import json
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, Iterable, List, Sequence
from zoneinfo import ZoneInfo


TALENT_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
PROCESSED_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data")
QUALITATIVE_CODEBOOKS_ROOT = PROCESSED_ROOT / "Qualitative Codebooks"
SHARED_ROOT = QUALITATIVE_CODEBOOKS_ROOT / "interaction_views" / "chat_shared" / "current"
OUTPUT_ROOT = QUALITATIVE_CODEBOOKS_ROOT / "definitions" / "chat_personality"
CURRENT_DIR = OUTPUT_ROOT / "current"
SNAPSHOT_DIR = OUTPUT_ROOT / "snapshots"
CURRENT_CSV = CURRENT_DIR / "chat_personality_qualitative_code_log.csv"
CURRENT_MD = CURRENT_DIR / "chat_personality_qualitative_code_log.md"
CURRENT_STATE = CURRENT_DIR / "chat_personality_qualitative_code_log_state.json"

TIMEZONE = ZoneInfo("America/New_York")
NOW = datetime.now(TIMEZONE)
CURRENT_DATE = NOW.strftime("%Y-%m-%d")
ANALYSIS_STAMP = NOW.strftime("%Y-%m-%d %H:%M %Z")
SNAPSHOT_PATH = SNAPSHOT_DIR / f"{CURRENT_DATE}_{NOW.strftime('%H-%M-%S')}_chat_personality_qualitative_code_log.md"

FIELDNAMES = [
    "primary_code_id",
    "primary_code",
    "secondary_code_id",
    "secondary_code",
    "definition",
    "inclusion_criteria",
    "exclusion_criteria",
    "interactional_function",
    "examples_from_text",
    "talents_observed",
    "date_added",
    "date_updated",
    "status",
    "notes",
]


@dataclass(frozen=True)
class LocalCode:
    talent: str
    local_code_id: str
    local_code_name: str
    definition: str
    inclusion_criteria: str
    exclusion_criteria: str
    interactional_function: str
    example_quote: str
    confidence: str
    notes: str


@dataclass(frozen=True)
class CodeSpec:
    primary_code_id: str
    primary_code: str
    secondary_code_id: str
    secondary_code: str
    definition: str
    inclusion_criteria: str
    exclusion_criteria: str
    interactional_function: str
    examples_from_text: str
    talents_observed: str
    notes: str


PRIMARY_ORDER = {
    "CPC01": 1,
    "CPC02": 2,
    "CPC03": 3,
    "CPC04": 4,
    "CPC05": 5,
}

SECONDARY_ORDER = {
    "CP01": 1,
    "CP02": 2,
    "CP03": 3,
    "CP04": 4,
    "CP05": 5,
    "CP06": 6,
    "CP07": 7,
    "CP08": 8,
    "CP09": 9,
    "CP10": 10,
    "CP11": 11,
    "CP12": 12,
    "CP13": 13,
    "CP14": 14,
}

LOCAL_MAP = {
    ("Nova Aokami Ch", "CP01"): ("CPC01", "Room Presence Rituals", "CP01"),
    ("Nova Aokami Ch", "CP02"): ("CPC04", "Community Support And Caretaking", "CP02"),
    ("Nova Aokami Ch", "CP03"): ("CPC03", "Synchronized Reaction Bursts", "CP03"),
    ("Nova Aokami Ch", "CP04"): ("CPC02", "Participatory Co-Steering", "CP04"),
    ("Nova Aokami Ch", "CP05"): ("CPC02", "Participatory Co-Steering", "CP05"),
    ("Nova Aokami Ch", "CP06"): ("CPC05", "In-Group Identity And Callback Framing", "CP06"),
    ("Leia Memoria【Variance Project】", "C01"): ("CPC01", "Room Presence Rituals", "CP07"),
    ("Leia Memoria【Variance Project】", "C02"): ("CPC02", "Participatory Co-Steering", "CP08"),
    ("Leia Memoria【Variance Project】", "C03"): ("CPC03", "Synchronized Reaction Bursts", "CP03"),
    ("Leia Memoria【Variance Project】", "C04"): ("CPC05", "In-Group Identity And Callback Framing", "CP09"),
    ("Leia Memoria【Variance Project】", "C05"): ("CPC04", "Community Support And Caretaking", "CP10"),
}


def read_csv(path: Path) -> List[Dict[str, str]]:
    with path.open("r", newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, rows: Sequence[Dict[str, str]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDNAMES)
        writer.writeheader()
        writer.writerows(rows)


def clean_text(text: str) -> str:
    return " ".join(text.replace("\n", " ").replace('"', "'").split())


def load_existing_rows() -> List[Dict[str, str]]:
    if not CURRENT_CSV.exists():
        return []
    return read_csv(CURRENT_CSV)


def existing_date_added_map(rows: Sequence[Dict[str, str]]) -> Dict[str, str]:
    mapping: Dict[str, str] = {}
    for row in rows:
        secondary_id = row["secondary_code_id"]
        if secondary_id:
            mapping[secondary_id] = row["date_added"]
    return mapping


def local_inputs_for_talent(talent: str) -> tuple[Path, Path, Path, Path]:
    talent_dir = TALENT_ROOT / talent
    open_codebook = (
        talent_dir
        / "qualitative coding/chat data/chat_personality_open_coding/current/chat_open_codebook.csv"
    )
    open_evidence = (
        talent_dir
        / "qualitative coding/chat data/chat_personality_open_coding/current/chat_open_coding_evidence.csv"
    )
    unique_profile = (
        talent_dir
        / "qualitative coding/chat data/chat_personality_unique_features/current/unique_chat_personality_profile.md"
    )
    unique_evidence = (
        talent_dir
        / "qualitative coding/chat data/chat_personality_unique_features/current/unique_chat_personality_evidence_log.csv"
    )
    required = [open_codebook, open_evidence, unique_profile, unique_evidence]
    missing = [str(path) for path in required if not path.exists()]
    if missing:
        raise RuntimeError(f"Missing required inputs for {talent}: {missing}")
    return open_codebook, open_evidence, unique_profile, unique_evidence


def load_local_codes(talent: str) -> Dict[str, LocalCode]:
    open_codebook, _, _, _ = local_inputs_for_talent(talent)
    rows = read_csv(open_codebook)
    result: Dict[str, LocalCode] = {}
    for row in rows:
        result[row["code_id"]] = LocalCode(
            talent=talent,
            local_code_id=row["code_id"],
            local_code_name=row["code_name"],
            definition=row["definition"],
            inclusion_criteria=row["inclusion_criteria"],
            exclusion_criteria=row["exclusion_criteria"],
            interactional_function=row["interactional_function"],
            example_quote=row["example_quote"],
            confidence=row.get("confidence", ""),
            notes=row.get("notes", ""),
        )
    return result


def build_specs() -> List[CodeSpec]:
    ava = load_local_codes("Avaritia Hawthorne 【Variance Project】")
    nova = load_local_codes("Nova Aokami Ch")
    leia = load_local_codes("Leia Memoria【Variance Project】")

    shared_reaction_definition = (
        "Chat compresses collective feeling into dense short-form reaction posts that stack around "
        "the same moment."
    )
    shared_reaction_inclusion = (
        "Use for low-word-count laughter, alarm, cheers, salutes, and similar burst reactions that "
        "signal synchronized uptake."
    )
    shared_reaction_exclusion = (
        "Do not use for longer evaluative commentary, explicit advice, or callback language whose "
        "main function is identity signaling."
    )
    shared_reaction_function = (
        "Creates a visible shared emotional pulse and shows that the room is tracking the same beat together."
    )

    specs = [
        CodeSpec(
            primary_code_id="CPC01",
            primary_code="Room Presence Rituals",
            secondary_code_id="CP01",
            secondary_code="Attendance Rituals",
            definition=nova["CP01"].definition,
            inclusion_criteria=nova["CP01"].inclusion_criteria,
            exclusion_criteria=nova["CP01"].exclusion_criteria,
            interactional_function=nova["CP01"].interactional_function,
            examples_from_text="Nova Aokami Ch: hi hi",
            talents_observed="Nova Aokami Ch",
            notes=(
                "Retained from the prior current codebook and re-homed under a shared room-presence family "
                "after multi-talent baseline confirmation."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC01",
            primary_code="Room Presence Rituals",
            secondary_code_id="CP07",
            secondary_code="Community Roll Call And Leave-Taking",
            definition=leia["C01"].definition,
            inclusion_criteria=leia["C01"].inclusion_criteria,
            exclusion_criteria=leia["C01"].exclusion_criteria,
            interactional_function=leia["C01"].interactional_function,
            examples_from_text=(
                "Leia Memoria【Variance Project】: hi Leia hi Owlcolytes | "
                "Leia Memoria【Variance Project】: Welcome Raiders, Hi Panda"
            ),
            talents_observed="Leia Memoria【Variance Project】",
            notes=(
                "Added from Leia's local open coding and unique-features evidence to retain the room-assembly "
                "variant rather than collapsing it into Nova's attendance-only subtype."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC01",
            primary_code="Room Presence Rituals",
            secondary_code_id="CP11",
            secondary_code="Chant-Led Room Assembly",
            definition=(
                "Chat assembles room presence through repeated chant-like slogans that work as crowd markers, "
                "greetings, and participation cues."
            ),
            inclusion_criteria=(
                "Use for repeated communal slogan lines such as Avalution or Clarity chants when they visibly "
                "mark joining, greeting, or rallying the room."
            ),
            exclusion_criteria=(
                "Do not use for one-off meme repetition, generic hype with no room-assembly function, or praise "
                "messages whose main function is affection rather than gathering."
            ),
            interactional_function=(
                "Turns room presence into a visible communal refrain so joining chat feels like stepping into an "
                "already active collective chant."
            ),
            examples_from_text=(
                "Avaritia Hawthorne 【Variance Project】: AVA LA AVALUTION | "
                "Avaritia Hawthorne 【Variance Project】: CLARITY CLARITY CLARITY CLARITY"
            ),
            talents_observed="Avaritia Hawthorne 【Variance Project】",
            notes=(
                "Added from Avaritia's local open coding and unique-features memo because room assembly in this "
                "community is often slogan-led rather than greeting-led, which remains analytically distinct from "
                "the Nova and Leia subtypes."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC02",
            primary_code="Participatory Co-Steering",
            secondary_code_id="CP04",
            secondary_code="Prompted Consensus Replies",
            definition=nova["CP04"].definition,
            inclusion_criteria=nova["CP04"].inclusion_criteria,
            exclusion_criteria=nova["CP04"].exclusion_criteria,
            interactional_function=nova["CP04"].interactional_function,
            examples_from_text="Nova Aokami Ch: yes. | Nova Aokami Ch: NO",
            talents_observed="Nova Aokami Ch",
            notes=(
                "Retained as Nova's compressed decision-surface subtype rather than merging it into the broader "
                "cross-context steering family."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC02",
            primary_code="Participatory Co-Steering",
            secondary_code_id="CP05",
            secondary_code="Peer Coaching And Explaining",
            definition=nova["CP05"].definition,
            inclusion_criteria=nova["CP05"].inclusion_criteria,
            exclusion_criteria=nova["CP05"].exclusion_criteria,
            interactional_function=nova["CP05"].interactional_function,
            examples_from_text=(
                "Nova Aokami Ch: remember tomorrow we get a chance to pull for koyan of light | "
                "Nova Aokami Ch: Just use thief on all bronzor"
            ),
            talents_observed="Nova Aokami Ch",
            notes=(
                "Retained because current evidence still supports a game-literate coaching subtype distinct from "
                "simple short-answer consensus replies."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC02",
            primary_code="Participatory Co-Steering",
            secondary_code_id="CP08",
            secondary_code="Participatory Steering And Co-Decision",
            definition=leia["C02"].definition,
            inclusion_criteria=leia["C02"].inclusion_criteria,
            exclusion_criteria=leia["C02"].exclusion_criteria,
            interactional_function=leia["C02"].interactional_function,
            examples_from_text=(
                "Leia Memoria【Variance Project】: you should probably look through his other texts | "
                "Leia Memoria【Variance Project】: I think we go left"
            ),
            talents_observed="Leia Memoria【Variance Project】",
            notes=(
                "Added from Leia to preserve a broader co-navigation subtype that travels across game, art, "
                "and talk/work contexts."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC02",
            primary_code="Participatory Co-Steering",
            secondary_code_id="CP12",
            secondary_code="Playful Pressure And Dare-Pushing",
            definition=ava["CHAT_PC_04"].definition,
            inclusion_criteria=ava["CHAT_PC_04"].inclusion_criteria,
            exclusion_criteria=ava["CHAT_PC_04"].exclusion_criteria,
            interactional_function=ava["CHAT_PC_04"].interactional_function,
            examples_from_text=(
                "Avaritia Hawthorne 【Variance Project】: COWARD | "
                "Avaritia Hawthorne 【Variance Project】: SPIN THAT WHEEL"
            ),
            talents_observed="Avaritia Hawthorne 【Variance Project】",
            notes=(
                "Added from Avaritia to preserve an escalation-first co-shaping subtype where chat participation "
                "often takes the form of teasing pressure, penalties, repeats, or dare-pushes rather than neutral advice."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC03",
            primary_code="Synchronized Reaction Bursts",
            secondary_code_id="CP03",
            secondary_code="Synchronized Reaction Bursts",
            definition=shared_reaction_definition,
            inclusion_criteria=shared_reaction_inclusion,
            exclusion_criteria=shared_reaction_exclusion,
            interactional_function=shared_reaction_function,
            examples_from_text=(
                "Avaritia Hawthorne 【Variance Project】: let's gooooo | "
                "Leia Memoria【Variance Project】: YIPPEE | Nova Aokami Ch: oh no"
            ),
            talents_observed=(
                "Avaritia Hawthorne 【Variance Project】, Leia Memoria【Variance Project】, Nova Aokami Ch"
            ),
            notes=(
                "Retained as a shared cross-talent family and now confirmed across all currently eligible talents "
                "after Avaritia's matching hype-pile-on evidence aligned with the shared baseline."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC04",
            primary_code="Community Support And Caretaking",
            secondary_code_id="CP02",
            secondary_code="Support And Celebration Bursts",
            definition=nova["CP02"].definition,
            inclusion_criteria=nova["CP02"].inclusion_criteria,
            exclusion_criteria=nova["CP02"].exclusion_criteria,
            interactional_function=nova["CP02"].interactional_function,
            examples_from_text="Nova Aokami Ch: good luck | Nova Aokami Ch: Congrats!",
            talents_observed="Nova Aokami Ch",
            notes=(
                "Retained as Nova's outcome-focused support subtype while moving it under the shared support-and-care "
                "family confirmed across eligible talents."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC04",
            primary_code="Community Support And Caretaking",
            secondary_code_id="CP10",
            secondary_code="Caretaking And Reassurance",
            definition=leia["C05"].definition,
            inclusion_criteria=leia["C05"].inclusion_criteria,
            exclusion_criteria=leia["C05"].exclusion_criteria,
            interactional_function=leia["C05"].interactional_function,
            examples_from_text=(
                "Leia Memoria【Variance Project】: good luck with your recording | "
                "Leia Memoria【Variance Project】: You're ok! Go take care of yourself!"
            ),
            talents_observed="Leia Memoria【Variance Project】",
            notes=(
                "Added from Leia because pressure-regulating care language remains analytically distinct from "
                "Nova's celebration-heavy support bursts."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC04",
            primary_code="Community Support And Caretaking",
            secondary_code_id="CP13",
            secondary_code="Affectionate Appraisal And Thanks",
            definition=ava["CHAT_PC_03"].definition,
            inclusion_criteria=ava["CHAT_PC_03"].inclusion_criteria,
            exclusion_criteria=ava["CHAT_PC_03"].exclusion_criteria,
            interactional_function=ava["CHAT_PC_03"].interactional_function,
            examples_from_text=(
                "Avaritia Hawthorne 【Variance Project】: love you ava | "
                "Avaritia Hawthorne 【Variance Project】: AVA CUTE!"
            ),
            talents_observed="Avaritia Hawthorne 【Variance Project】",
            notes=(
                "Added from Avaritia because this room's support language repeatedly arrives as overt affection, "
                "gratitude, and personal validation, which is not interchangeable with Nova's outcome-focused support "
                "or Leia's caretaking reassurance."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC05",
            primary_code="In-Group Identity And Callback Framing",
            secondary_code_id="CP06",
            secondary_code="Bestie Collective Framing",
            definition=nova["CP06"].definition,
            inclusion_criteria=nova["CP06"].inclusion_criteria,
            exclusion_criteria=nova["CP06"].exclusion_criteria,
            interactional_function=nova["CP06"].interactional_function,
            examples_from_text="Nova Aokami Ch: Bestie time!! | Nova Aokami Ch: HI BESTIE",
            talents_observed="Nova Aokami Ch",
            notes=(
                "Retained as Nova's clearest in-group naming subtype while broadening the parent family to include "
                "callback-driven identity work from additional talents."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC05",
            primary_code="In-Group Identity And Callback Framing",
            secondary_code_id="CP09",
            secondary_code="Shared Callback And In-Group Lore Invocation",
            definition=leia["C04"].definition,
            inclusion_criteria=leia["C04"].inclusion_criteria,
            exclusion_criteria=leia["C04"].exclusion_criteria,
            interactional_function=leia["C04"].interactional_function,
            examples_from_text=(
                "Leia Memoria【Variance Project】: free game btw | "
                "Leia Memoria【Variance Project】: hi Leia hi Owlcolytes"
            ),
            talents_observed="Leia Memoria【Variance Project】",
            notes=(
                "Added from Leia to retain callback-rich identity work that depends on shared room memory rather "
                "than explicit bestie-style self-labeling."
            ),
        ),
        CodeSpec(
            primary_code_id="CPC05",
            primary_code="In-Group Identity And Callback Framing",
            secondary_code_id="CP14",
            secondary_code="Ritualized Slogan And Creed Invocation",
            definition=ava["CHAT_PC_01"].definition,
            inclusion_criteria=ava["CHAT_PC_01"].inclusion_criteria,
            exclusion_criteria=ava["CHAT_PC_01"].exclusion_criteria,
            interactional_function=ava["CHAT_PC_01"].interactional_function,
            examples_from_text=(
                "Avaritia Hawthorne 【Variance Project】: AVA LA AVALUTION | "
                "Avaritia Hawthorne 【Variance Project】: CLARITY CLARITY CLARITY CLARITY"
            ),
            talents_observed="Avaritia Hawthorne 【Variance Project】",
            notes=(
                "Added from Avaritia to retain the identity-facing side of slogan reuse, where repeated creed-like "
                "phrases function as explicit membership performance and community shorthand."
            ),
        ),
    ]
    return specs


def build_rows(existing_rows: Sequence[Dict[str, str]], specs: Sequence[CodeSpec]) -> List[Dict[str, str]]:
    existing_dates = existing_date_added_map(existing_rows)
    rows: List[Dict[str, str]] = []
    for spec in specs:
        rows.append(
            {
                "primary_code_id": spec.primary_code_id,
                "primary_code": spec.primary_code,
                "secondary_code_id": spec.secondary_code_id,
                "secondary_code": spec.secondary_code,
                "definition": spec.definition,
                "inclusion_criteria": spec.inclusion_criteria,
                "exclusion_criteria": spec.exclusion_criteria,
                "interactional_function": spec.interactional_function,
                "examples_from_text": clean_text(spec.examples_from_text),
                "talents_observed": spec.talents_observed,
                "date_added": existing_dates.get(spec.secondary_code_id, CURRENT_DATE),
                "date_updated": CURRENT_DATE,
                "status": "active",
                "notes": clean_text(spec.notes),
            }
        )
    rows.sort(
        key=lambda row: (
            PRIMARY_ORDER[row["primary_code_id"]],
            SECONDARY_ORDER[row["secondary_code_id"]],
        )
    )
    return rows


def shared_state() -> Dict[str, object]:
    state_path = SHARED_ROOT / "chat_shared_behavior_state.json"
    if not state_path.exists():
        raise RuntimeError(f"Missing shared baseline state: {state_path}")
    return json.loads(state_path.read_text(encoding="utf-8"))


def build_markdown(rows: Sequence[Dict[str, str]], prior_rows: Sequence[Dict[str, str]]) -> str:
    prior_secondary_ids = {row["secondary_code_id"] for row in prior_rows}
    active_rows = [row for row in rows if row["status"] == "active"]
    added_ids = [row["secondary_code_id"] for row in active_rows if row["secondary_code_id"] not in prior_secondary_ids]

    code_lines = [
        "Analysis conducted: " + ANALYSIS_STAMP,
        "Primary source layers: chat_personality_open_coding + chat_shared_interactions + chat_personality_unique_features",
        "",
        "## 1) Codebook Summary",
        (
            "This run preserves continuity with the prior current chat-side codebook while incorporating "
            "Avaritia Hawthorne 【Variance Project】 as the focused cross-talent update."
        ),
        (
            f"The active registry now contains {len(active_rows)} secondary codes across five primary families. "
            "Four Avaritia-derived secondary codes were added, the shared reaction family was expanded to three "
            "eligible talents, and no codes were retired."
        ),
        (
            "The five-family structure was retained to match the current shared chat baseline: Room Presence Rituals, "
            "Participatory Co-Steering, Synchronized Reaction Bursts, Community Support And Caretaking, and In-Group "
            "Identity And Callback Framing."
        ),
        "",
        "## 2) Current Chat Personality Codes",
    ]

    families = []
    for primary_id in sorted(PRIMARY_ORDER, key=PRIMARY_ORDER.get):
        family_rows = [row for row in active_rows if row["primary_code_id"] == primary_id]
        if not family_rows:
            continue
        families.append(f"### {primary_id} {family_rows[0]['primary_code']}")
        for row in family_rows:
            families.append(
                f"- {row['secondary_code_id']} {row['secondary_code']}: {row['definition']} "
                f"Talents observed: {row['talents_observed']}."
            )

    evidence_lines = [
        "",
        "## 3) Evidence And Revision Notes",
        (
            "Direct evidence came from the current local open-coding codebooks and unique-features artifacts for "
            "Avaritia Hawthorne 【Variance Project】, Leia Memoria【Variance Project】, and Nova Aokami Ch, with the "
            "shared chat baseline used as the continuity layer for family-level alignment."
        ),
        (
            "Avaritia's unique-features artifacts supported keeping chant-led room assembly, escalation-first "
            "co-shaping, affection-forward support, and ritualized slogan identity work as analytically separate "
            "secondary codes rather than flattening them into the Nova or Leia subtypes."
        ),
        (
            "Existing Nova and Leia codes were retained where the evidence still supported subtype distinctions. "
            "The largest cross-talent revision in this run was updating CP03's observed-talent coverage so the shared "
            "reaction family now explicitly reflects Avaritia, Leia, and Nova."
        ),
        (
            "No merges or retirements were made because the current three-talent evidence base supports broader family "
            "confirmation while still showing meaningful subtype differences within room assembly, steering, support, "
            "and identity work."
        ),
        f"New secondary codes added in this run: {', '.join(added_ids) if added_ids else 'none'}.",
        "",
        "## 4) Limitations",
    ]

    baseline_state = shared_state()
    limitation_lines = [
        "- The cumulative codebook is currently based on three eligible talents with complete current upstream chat inputs.",
        "- Missing eligible current inputs at run time: "
        + ", ".join(item["talent"] for item in baseline_state.get("skipped_talents", []))
        + ".",
        "- Evidence is text-visible only and cannot fully capture timing, delivery, moderation context, or off-screen cues.",
        "- Later cross-talent updates may still require code additions, merges, splits, or retirements once more talent-local inputs become usable.",
    ]

    return "\n".join(code_lines + families + evidence_lines + limitation_lines) + "\n"


def build_state(rows: Sequence[Dict[str, str]]) -> Dict[str, object]:
    baseline_state = shared_state()
    return {
        "analysis_conducted": ANALYSIS_STAMP,
        "source_layers": [
            "chat_personality_open_coding",
            "chat_shared_interactions",
            "chat_personality_unique_features",
        ],
        "active_code_count": len([row for row in rows if row["status"] == "active"]),
        "retired_code_count": len([row for row in rows if row["status"] == "retired"]),
        "talents_included": baseline_state["eligible_talents"],
        "changes_made": [
            "Added Avaritia Hawthorne 【Variance Project】 as an included talent in the cumulative chat-side codebook.",
            "Added Avaritia-derived secondary codes for chant-led room assembly, playful dare-pushing, affectionate appraisal, and ritualized slogan identity work.",
            "Expanded CP03 Synchronized Reaction Bursts to reflect all three currently eligible talents.",
            "Retained the five-family shared baseline structure and preserved prior Nova and Leia subtype continuity where evidence still supports distinct interactional functions.",
        ],
        "limitations": [
            "Only three talents currently have the full eligible local input set for this cumulative codebook.",
            "Several talents still lack usable current chat open-coding artifacts, which limits comparison breadth.",
            "The evidence layer is text-visible only and cannot fully capture timing, delivery, or moderation context.",
        ],
    }


def ensure_dirs() -> None:
    CURRENT_DIR.mkdir(parents=True, exist_ok=True)
    SNAPSHOT_DIR.mkdir(parents=True, exist_ok=True)


def main() -> int:
    ensure_dirs()
    existing_rows = load_existing_rows()
    specs = build_specs()
    rows = build_rows(existing_rows, specs)
    markdown = build_markdown(rows, existing_rows)
    state = build_state(rows)

    write_csv(CURRENT_CSV, rows)
    CURRENT_MD.write_text(markdown, encoding="utf-8")
    CURRENT_STATE.write_text(json.dumps(state, indent=2), encoding="utf-8")
    SNAPSHOT_PATH.write_text(markdown, encoding="utf-8")

    print(f"Wrote CSV: {CURRENT_CSV}")
    print(f"Wrote Markdown: {CURRENT_MD}")
    print(f"Wrote State: {CURRENT_STATE}")
    print(f"Wrote Snapshot: {SNAPSHOT_PATH}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
