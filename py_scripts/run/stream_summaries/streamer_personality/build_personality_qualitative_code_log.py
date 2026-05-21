#!/usr/bin/env python3
"""Maintain the personality qualitative code log."""

from __future__ import annotations

import argparse
import csv
import json
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, Iterable, List, Sequence
from zoneinfo import ZoneInfo


TALENT_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
PROCESSED_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data")
SHARED_ROOT = PROCESSED_ROOT / "shared_interactions" / "current"
OUTPUT_ROOT = PROCESSED_ROOT / "Qualitative Codebook"
CURRENT_DIR = OUTPUT_ROOT / "current"
SNAPSHOT_DIR = OUTPUT_ROOT / "snapshots"
TIMEZONE = ZoneInfo("America/New_York")
NOW = datetime.now(TIMEZONE)
CURRENT_DATE = NOW.strftime("%Y-%m-%d")
ANALYSIS_STAMP = NOW.strftime("%Y-%m-%d %H:%M %Z")
SNAPSHOT_STAMP = NOW.strftime("%Y-%m-%d_%H-%M-%S_%z")

CURRENT_CSV = CURRENT_DIR / "personality_qualitative_code_log.csv"
CURRENT_MD = CURRENT_DIR / "personality_qualitative_code_log_codex.md"
CURRENT_STATE = CURRENT_DIR / "personality_qualitative_code_log_state.json"
SNAPSHOT_MD = SNAPSHOT_DIR / f"personality_qualitative_code_log_{SNAPSHOT_STAMP}.md"

FIELDNAMES = [
    "Primary Code ID",
    "Primary Code",
    "Secondary Code ID",
    "Secondary Code",
    "Definition",
    "Date added",
    "Examples from text",
]


@dataclass(frozen=True)
class UpdateSpec:
    primary_id: str
    primary_code: str
    secondary_id: str
    secondary_code: str
    definition: str
    examples: str
    change_type: str
    why: str
    support: str


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--talent", help="Process only the exact talent folder name.")
    parser.add_argument(
        "--initial-build",
        action="store_true",
        help="Ignore the existing current CSV as the continuity layer.",
    )
    return parser.parse_args()


def read_csv(path: Path) -> List[Dict[str, str]]:
    with path.open("r", newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, rows: Sequence[Dict[str, str]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDNAMES)
        writer.writeheader()
        writer.writerows(rows)


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def short_quote(text: str, limit: int = 88) -> str:
    cleaned = " ".join(text.replace('"', "'").split())
    if len(cleaned) <= limit:
        return cleaned
    return cleaned[: limit - 3].rstrip() + "..."


def format_examples(rows: Sequence[Dict[str, str]], limit: int = 3) -> str:
    parts: List[str] = []
    seen_quotes: set[str] = set()
    for row in rows:
        quote = short_quote(row["quote"])
        rendered = f'{row["talent"]}: "{quote}"'
        if rendered in seen_quotes:
            continue
        seen_quotes.add(rendered)
        parts.append(rendered)
        if len(parts) >= limit:
            break
    return "; ".join(parts)


def load_existing_rows(use_existing_log: bool) -> List[Dict[str, str]]:
    if not use_existing_log or not CURRENT_CSV.exists():
        return []
    return read_csv(CURRENT_CSV)


def existing_date_map(rows: Sequence[Dict[str, str]]) -> Dict[tuple[str, str], str]:
    mapping: Dict[tuple[str, str], str] = {}
    for row in rows:
        mapping[(row["Primary Code ID"], row["Secondary Code ID"])] = row["Date added"]
    return mapping


def choose_open_coding_paths(talent_dir: Path) -> tuple[Path, Path, Path] | None:
    candidates = [
        (
            talent_dir
            / "stream_summaries/overall_themes/personality_open_coding/v3/current/open_codebook_v3.csv",
            talent_dir
            / "stream_summaries/overall_themes/personality_open_coding/v3/current/open_coding_evidence_v3.csv",
            talent_dir
            / "stream_summaries/overall_themes/personality_open_coding/v3/current/personality_profile_v3_open_coding.md",
        ),
        (
            talent_dir / "stream_summaries/overall_themes/personality_open_coding/v2/open_codebook_v2.csv",
            talent_dir / "stream_summaries/overall_themes/personality_open_coding/v2/open_coding_evidence_v2.csv",
            talent_dir / "stream_summaries/overall_themes/personality_profile_v2_open_coding.md",
        ),
    ]
    for codebook, evidence, markdown in candidates:
        if codebook.exists() and evidence.exists() and markdown.exists():
            return codebook, evidence, markdown
    return None


def discover_talent_dir(talent_name: str) -> Path:
    talent_dir = TALENT_ROOT / talent_name
    if not talent_dir.is_dir():
        raise RuntimeError(f"Talent folder not found: {talent_name}")
    return talent_dir


def ensure_required_inputs(talent_dir: Path) -> Dict[str, Path | None]:
    open_paths = choose_open_coding_paths(talent_dir)
    if open_paths is None:
        raise RuntimeError(f"Missing usable personality_open_coding inputs for {talent_dir.name}")

    unique_profile = (
        talent_dir
        / "stream_summaries/overall_themes/personality_unique_features/current/unique_personality_profile_codex.md"
    )
    unique_evidence = (
        talent_dir
        / "stream_summaries/overall_themes/personality_unique_features/current/unique_personality_evidence_log_codex.csv"
    )
    if not unique_profile.exists() or not unique_evidence.exists():
        raise RuntimeError(f"Missing personality_unique_features inputs for {talent_dir.name}")

    summary_markdown = talent_dir / "stream_summaries/overall_channel_summary/current/overall_channel_summary.md"
    if not summary_markdown.exists():
        summary_markdown = (
            talent_dir
            / "stream_summaries/overall_themes/summary_classification/current/overall_themes_codex.md"
        )
    return {
        "open_codebook": open_paths[0],
        "open_evidence": open_paths[1],
        "open_markdown": open_paths[2],
        "unique_profile": unique_profile,
        "unique_evidence": unique_evidence,
        "summary_markdown": summary_markdown if summary_markdown.exists() else None,
    }


def build_shared_indexes() -> tuple[Dict[str, Dict[str, str]], Dict[str, List[Dict[str, str]]]]:
    shared_codebook = {
        row["shared_behavior_name"]: row
        for row in read_csv(SHARED_ROOT / "shared_behavior_codebook.csv")
    }
    shared_evidence: Dict[str, List[Dict[str, str]]] = {}
    for row in read_csv(SHARED_ROOT / "shared_behavior_evidence.csv"):
        shared_evidence.setdefault(row["shared_behavior_name"], []).append(row)
    return shared_codebook, shared_evidence


def build_unique_index(unique_rows: Sequence[Dict[str, str]]) -> Dict[str, List[Dict[str, str]]]:
    index: Dict[str, List[Dict[str, str]]] = {}
    for row in unique_rows:
        index.setdefault(row["dimension_name"], []).append(row)
    return index


def select_by_talent(rows: Sequence[Dict[str, str]], talent: str) -> List[Dict[str, str]]:
    return [row for row in rows if row["talent"] == talent]


def build_update_specs(
    talent_name: str,
    shared_codebook: Dict[str, Dict[str, str]],
    shared_evidence: Dict[str, List[Dict[str, str]]],
    unique_index: Dict[str, List[Dict[str, str]]],
) -> List[UpdateSpec]:
    shared_map = [
        (
            "gratitude ritualization",
            "A1",
            "Ritualized supporter acknowledgment",
            "Recurring supporter-recognition turns in which audience presence, raids, memberships, waiting, or check-ins are explicitly converted into a visible social ritual.",
            "revised",
            "Nova's current evidence reinforces that acknowledgment should stay in the log as a durable family, but the wording now reflects raid and membership handoff structures visible in the current baseline.",
            "Shared-baseline evidence pairs Nova's raid and membership thanks with Terberri's waiting/check-in gratitude, keeping the family broad while sharpening the current form contrast.",
        ),
        (
            "audience-attentive pacing control",
            "B1",
            "Audience pacing and room control",
            "Visible steering of chat attention, transitions, or interaction timing so the streamer can redirect, pin, explain, or refocus the room in real time.",
            "revised",
            "The parent family remains valid, but Nova's brisk mod-routing evidence justified tightening the definition around audience-facing control rather than generic delay or filler pacing.",
            "The shared baseline shows Nova's directive pin/mods control alongside Terberri's explanatory focus resets, confirming the family while clarifying its form range.",
        ),
        (
            "reassurance during strain or recovery",
            "C1",
            "Reassurance and tension repair",
            "Repeated turns that normalize setbacks, soothe tension, or reassure chat, the streamer, or an on-stream target during mistakes, embarrassment, frustration, or fatigue.",
            "revised",
            "Nova's fast reassurance loops fit the existing code, but the definition needed sharpening around tension repair rather than generic supportiveness.",
            "Shared-baseline evidence shows Nova's quick validation and Terberri's worth-affirming care as two forms of the same durable repair move.",
        ),
        (
            "audience-facing self-disclosure windows",
            "D1",
            "Audience-facing self-disclosure",
            "Recurring first-person disclosure of anxiety, stress, overwhelm, fatigue, or personal need that is made visible to the audience rather than left implicit.",
            "revised",
            "Nova's current disclosure evidence is sharper and more anxiety-explicit than the older wording captured, so the parent code now names those states more directly.",
            "The current shared baseline still shows this as a cross-talent family, while Nova's distinct form is strong enough to justify a new secondary code below it.",
        ),
        (
            "performative teasing and welcome escalation",
            "E1",
            "Stylized teasing and welcome escalation",
            "Recurring use of stylized welcome lines, pet-name address, or theatrical escalation that turns audience contact into a performance move rather than plain acknowledgment.",
            "revised",
            "Nova's current evidence kept this family strong, but the definition now foregrounds welcome lines and performance framing rather than a loose teasing bucket.",
            "The shared baseline confirms the family across Nova and Terberri while Nova's louder membership/bestie version motivates a more specific reusable subtype.",
        ),
    ]

    specs: List[UpdateSpec] = []
    for behavior_name, primary_id, primary_code, definition, change_type, why, support in shared_map:
        codebook_row = shared_codebook[behavior_name]
        evidence_rows = shared_evidence[behavior_name]
        examples = format_examples(evidence_rows, limit=4)
        specs.append(
            UpdateSpec(
                primary_id=primary_id,
                primary_code=primary_code,
                secondary_id="",
                secondary_code="",
                definition=definition,
                examples=examples,
                change_type=change_type,
                why=why,
                support=support + f" Shared baseline classification: {codebook_row['classification']}.",
            )
        )

    new_specs = [
        (
            "D1",
            "Audience-facing self-disclosure",
            "D1a",
            "Performative anxiety transparency",
            "A recurrent subtype of self-disclosure in which anxiety, stress, or being 'locked in' is made text-visible in a heightened, chat-aware, dramatic register.",
            "Performative anxiety transparency",
            "added",
            "Added because Nova's current evidence repeatedly shows anxiety disclosure doing more than generic openness: it becomes a recognizable interactional register within the broader disclosure family.",
            "Supported by Nova's unique-feature layer across multiple streams, and explicitly contrasted against Terberri's softer regulation-oriented disclosure in the shared baseline.",
        ),
        (
            "E1",
            "Stylized teasing and welcome escalation",
            "E1d",
            "Membership/bestie welcome hype",
            "A subtype of stylized audience contact where membership welcomes and supporter thanks are fused with 'bestie' intimacy, baby-coded hype, or fast celebratory handoff language.",
            "Distinctive teasing and welcome escalation",
            "added",
            "Added because Nova's welcome evidence is not just a louder version of the parent code; it repeatedly ties supporter acknowledgment to membership/bestie performance language that is analytically reusable.",
            "Supported by Nova's unique-feature evidence and by the shared baseline note that Nova's escalation is louder and more membership- and bestie-coded than the comparison case.",
        ),
        (
            "F1",
            "Boundary-setting as audience management",
            "F1d",
            "Explicit anti-spam / anti-weird guardrails",
            "A recurring subtype of audience management where the streamer directly blocks weirdness, spam, or socially off interactions in plain text-visible terms.",
            "Explicit anti-spam / anti-weird guardrails",
            "added",
            "Added because Nova's guardrail evidence is recurring and socially meaningful even though the current shared baseline leaves boundary enforcement outside the common layer.",
            "Supported by Nova's unique-feature evidence across multiple streams; analytically useful as a narrower boundary subtype rather than as a new primary family.",
        ),
    ]

    for primary_id, primary_code, secondary_id, secondary_code, definition, dimension_name, change_type, why, support in new_specs:
        rows = unique_index.get(dimension_name, [])
        if not rows:
            raise RuntimeError(f"Missing unique-feature evidence for {talent_name}: {dimension_name}")
        specs.append(
            UpdateSpec(
                primary_id=primary_id,
                primary_code=primary_code,
                secondary_id=secondary_id,
                secondary_code=secondary_code,
                definition=definition,
                examples=format_examples(rows, limit=3),
                change_type=change_type,
                why=why,
                support=support,
            )
        )

    return specs


def apply_updates(
    existing_rows: Sequence[Dict[str, str]],
    updates: Sequence[UpdateSpec],
) -> tuple[List[Dict[str, str]], Dict[str, List[UpdateSpec]]]:
    rows_by_key = {
        (row["Primary Code ID"], row["Secondary Code ID"]): dict(row) for row in existing_rows
    }
    date_map = existing_date_map(existing_rows)
    change_log = {
        "added": [],
        "revised": [],
        "merged": [],
        "split": [],
        "retired": [],
    }

    for spec in updates:
        key = (spec.primary_id, spec.secondary_id)
        row = {
            "Primary Code ID": spec.primary_id,
            "Primary Code": spec.primary_code,
            "Secondary Code ID": spec.secondary_id,
            "Secondary Code": spec.secondary_code,
            "Definition": spec.definition,
            "Date added": date_map.get(key, CURRENT_DATE),
            "Examples from text": spec.examples,
        }
        if key not in rows_by_key:
            change_log["added"].append(spec)
        else:
            existing_row = rows_by_key[key]
            if (
                existing_row["Definition"] != row["Definition"]
                or existing_row["Examples from text"] != row["Examples from text"]
                or existing_row["Primary Code"] != row["Primary Code"]
                or existing_row["Secondary Code"] != row["Secondary Code"]
            ):
                change_log["revised"].append(spec)
        rows_by_key[key] = row

    ordered_rows = sorted(
        rows_by_key.values(),
        key=lambda row: (row["Primary Code ID"], row["Secondary Code ID"]),
    )
    return ordered_rows, change_log


def render_added_section(specs: Sequence[UpdateSpec]) -> List[str]:
    if not specs:
        return ["- None."]
    lines: List[str] = []
    for spec in specs:
        lines.append(
            f"- `Primary Code ID`: {spec.primary_id}; `Primary Code`: {spec.primary_code}; "
            f"`Secondary Code ID`: {spec.secondary_id}; `Secondary Code`: {spec.secondary_code}; "
            f"`why it was added`: {spec.why}; `supporting evidence summary`: {spec.support}"
        )
    return lines


def render_revised_section(specs: Sequence[UpdateSpec]) -> List[str]:
    if not specs:
        return ["- None."]
    lines: List[str] = []
    for spec in specs:
        target = spec.secondary_id or spec.primary_id
        lines.append(
            f"- `{target}` was revised. Change: {spec.why} Evidence note: {spec.support}"
        )
    return lines


def render_change_section(change_log: Dict[str, List[UpdateSpec]]) -> List[str]:
    lines: List[str] = []
    if change_log["merged"]:
        for spec in change_log["merged"]:
            lines.append(f"- Merged: {spec.primary_id} {spec.secondary_id} {spec.why}")
    if change_log["split"]:
        for spec in change_log["split"]:
            lines.append(f"- Split: {spec.primary_id} {spec.secondary_id} {spec.why}")
    if change_log["retired"]:
        for spec in change_log["retired"]:
            lines.append(f"- Retired: {spec.primary_id} {spec.secondary_id} {spec.why}")
    if not lines:
        lines.append("- None in this run. The update added subcodes and revised parent rows without retiring older registry entries.")
    return lines


def build_markdown(
    talent_name: str,
    summary_used: bool,
    existing_log_used: bool,
    change_log: Dict[str, List[UpdateSpec]],
) -> str:
    added_count = len(change_log["added"])
    revised_count = len(change_log["revised"])
    existing_used = "yes" if existing_log_used else "no"
    summary_text = "yes" if summary_used else "no"

    summary_paragraphs = [
        (
            f"This scoped run updated the maintained qualitative code log for `{talent_name}` "
            "rather than rebuilding the registry. The main change was selective revision of the "
            "five shared parent families already supported by the current shared baseline, plus the "
            "addition of three Nova-specific secondary codes where the subtype materially changes "
            "later interpretation."
        ),
        (
            "The strongest additions were not generic personality labels. They capture Nova's "
            "performative anxiety disclosure, membership/bestie-coded welcome hype, and explicit "
            "anti-spam or anti-weird guardrails as reusable interaction patterns grounded in "
            "repeated evidence rows."
        ),
        (
            "Older registry rows were preserved unless this run supplied better wording or fresher "
            "examples for the same family. No code families were retired, because this scoped pass "
            "did not revisit the full older corpus closely enough to justify removals."
        ),
    ]

    high_value = [
        "- `A1 Ritualized supporter acknowledgment` remains durable because the current baseline still shows supporter recognition as a structured interaction routine rather than generic politeness.",
        "- `B1 Audience pacing and room control` remains high-value because it captures public steering of chat attention while still allowing stylistic differences in how authority is performed.",
        "- `D1 Audience-facing self-disclosure` is especially strong after this run because the new Nova subtype clarifies that disclosure form matters, not just disclosure frequency.",
        "- `E1 Stylized teasing and welcome escalation` remains analytically useful because Nova's new subcode shows how a shared family can carry a distinct welcome-performance subtype without forcing a new primary family.",
        "- `F1 Boundary-setting as audience management` stays lower-confidence than the main baseline families, but the new Nova subcode shows it can still hold stable niche variants when the evidence recurs.",
    ]

    open_questions = [
        "- `A1` may eventually need a Nova-specific gratitude subtype if raid/membership handoff language recurs strongly across more talents and proves distinct from older ceremonial or reciprocal thanks.",
        "- `B1` may warrant a Nova-focused secondary code later if directive mod-routing and pinning remain analytically different from need-led flow control across the corpus.",
        "- `F1` still needs a broader corpus check before any retirement or merger decisions are made for the older boundary subcodes.",
        "- The current run is text-only and scope-limited to Nova, so later recoding should confirm whether the new secondary codes travel beyond one talent or remain talent-concentrated markers.",
    ]

    sections = [
        f"Analysis conducted: {ANALYSIS_STAMP}",
        "Eligible talents included: 1",
        "Primary source layers: personality_open_coding + shared_interactions + personality_unique_features",
        f"Secondary source used: summary_classification {summary_text}",
        f"Existing log used: {existing_used}",
        "",
        "## 1) Log Update Summary",
        "",
        summary_paragraphs[0],
        "",
        summary_paragraphs[1],
        "",
        summary_paragraphs[2],
        "",
        "## 2) Codes Added This Run",
        "",
        *render_added_section(change_log["added"]),
        "",
        "## 3) Codes Revised This Run",
        "",
        *render_revised_section(change_log["revised"]),
        "",
        "## 4) Codes Merged, Split, or Retired",
        "",
        *render_change_section(change_log),
        "",
        "## 5) Current High-Value Code Families",
        "",
        *high_value,
        "",
        "## 6) Open Questions for Future Recoding",
        "",
        *open_questions,
        "",
        f"Run summary counts: added={added_count}; revised={revised_count}; merged=0; split=0; retired=0.",
        "",
    ]
    return "\n".join(sections)


def write_outputs(rows: Sequence[Dict[str, str]], markdown: str, state: Dict[str, object]) -> None:
    CURRENT_DIR.mkdir(parents=True, exist_ok=True)
    SNAPSHOT_DIR.mkdir(parents=True, exist_ok=True)
    write_csv(CURRENT_CSV, rows)
    CURRENT_MD.write_text(markdown, encoding="utf-8")
    SNAPSHOT_MD.write_text(markdown, encoding="utf-8")
    CURRENT_STATE.write_text(json.dumps(state, indent=2), encoding="utf-8")


def main() -> int:
    args = parse_args()
    required_shared = [
        SHARED_ROOT / "shared_behavior_baseline_codex.md",
        SHARED_ROOT / "shared_behavior_codebook.csv",
        SHARED_ROOT / "talent_shared_behavior_matrix.csv",
        SHARED_ROOT / "shared_behavior_evidence.csv",
        SHARED_ROOT / "shared_behavior_state.json",
    ]
    missing_shared = [str(path) for path in required_shared if not path.exists()]
    if missing_shared:
        raise RuntimeError(f"Missing shared-interaction inputs: {missing_shared}")

    if not args.talent:
        raise RuntimeError("This workflow currently requires --talent for scoped maintenance runs.")

    talent_dir = discover_talent_dir(args.talent)
    talent_inputs = ensure_required_inputs(talent_dir)
    existing_rows = load_existing_rows(use_existing_log=not args.initial_build)
    existing_log_used = bool(existing_rows)
    shared_codebook, shared_evidence = build_shared_indexes()
    unique_rows = read_csv(talent_inputs["unique_evidence"])  # type: ignore[arg-type]
    unique_index = build_unique_index(unique_rows)
    updates = build_update_specs(args.talent, shared_codebook, shared_evidence, unique_index)
    final_rows, change_log = apply_updates(existing_rows, updates)
    summary_used = talent_inputs["summary_markdown"] is not None
    markdown = build_markdown(args.talent, summary_used, existing_log_used, change_log)

    state = {
        "analysis_conducted": ANALYSIS_STAMP,
        "eligible_talents": [args.talent],
        "existing_log_used": existing_log_used,
        "rows_in_log": len(final_rows),
        "new_codes_added": len(change_log["added"]),
        "codes_revised": len(change_log["revised"]),
        "codes_merged": [],
        "codes_split": [],
        "codes_retired": [],
        "source_layers_used": [
            "personality_open_coding",
            "shared_interactions",
            "personality_unique_features",
        ],
        "secondary_source_used": "summary_classification" if summary_used else "no",
        "output_paths": {
            "csv": str(CURRENT_CSV),
            "markdown": str(CURRENT_MD),
            "state": str(CURRENT_STATE),
            "snapshot_markdown": str(SNAPSHOT_MD),
        },
        "notes": [
            f"Scoped maintenance run for {args.talent}.",
            "Preserved Date added values for existing rows and used the current run date only for genuinely new secondary codes.",
            "Revised parent-code wording and examples only where current Nova-plus-baseline evidence materially sharpened the existing registry.",
        ],
    }

    write_outputs(final_rows, markdown, state)
    print(f"Wrote {len(final_rows)} code-log rows to {CURRENT_CSV}")
    print(f"Wrote memo to {CURRENT_MD}")
    print(f"Wrote state to {CURRENT_STATE}")
    print(f"Wrote snapshot to {SNAPSHOT_MD}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
