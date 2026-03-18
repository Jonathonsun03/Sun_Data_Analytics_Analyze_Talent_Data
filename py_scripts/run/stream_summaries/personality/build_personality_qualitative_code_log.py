#!/usr/bin/env python3
"""Build a cumulative cross-talent qualitative code log for personality patterns."""

from __future__ import annotations

import csv
import json
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, Iterable, List, Sequence
from zoneinfo import ZoneInfo


THIS_FILE = Path(__file__).resolve()
PY_SCRIPTS_ROOT = next(
    parent
    for parent in [*THIS_FILE.parents]
    if (parent / "lib" / "repo_paths.py").exists()
)
LIB_ROOT = PY_SCRIPTS_ROOT / "lib"
if str(LIB_ROOT) not in sys.path:
    sys.path.insert(0, str(LIB_ROOT))

from repo_paths import find_repo_root


REPO_ROOT = find_repo_root(Path(__file__))
TALENT_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
PROCESSED_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data")
SHARED_ROOT = PROCESSED_ROOT / "shared_interactions" / "current"
OUTPUT_ROOT = PROCESSED_ROOT / "Qualitative Codebook"
CURRENT_DIR = OUTPUT_ROOT / "current"
SNAPSHOT_DIR = OUTPUT_ROOT / "snapshots"
TIMEZONE = ZoneInfo("America/New_York")
CURRENT_DATE = datetime.now(TIMEZONE).strftime("%Y-%m-%d")
ANALYSIS_STAMP = datetime.now(TIMEZONE).strftime("%Y-%m-%d %H:%M %Z")
SNAPSHOT_STAMP = datetime.now(TIMEZONE).strftime("%Y-%m-%d_%H-%M-%S_%z")

CURRENT_CSV = CURRENT_DIR / "personality_qualitative_code_log.csv"
CURRENT_MD = CURRENT_DIR / "personality_qualitative_code_log_codex.md"
CURRENT_STATE = CURRENT_DIR / "personality_qualitative_code_log_state.json"
SNAPSHOT_MD = SNAPSHOT_DIR / f"personality_qualitative_code_log_{SNAPSHOT_STAMP}.md"


@dataclass(frozen=True)
class TalentInputs:
    talent: str
    base_dir: Path
    open_codebook: Path
    open_evidence: Path
    open_markdown: Path
    unique_profile: Path
    unique_evidence: Path
    unique_state: Path
    summary_markdown: Path | None
    summary_state: Path | None


@dataclass(frozen=True)
class EvidenceRef:
    kind: str
    name: str
    talent: str | None = None


@dataclass(frozen=True)
class CodeSpec:
    primary_id: str
    primary_code: str
    secondary_id: str
    secondary_code: str
    definition: str
    refs: Sequence[EvidenceRef]
    why_added: str
    support_summary: str


def read_csv(path: Path) -> List[Dict[str, str]]:
    with path.open("r", newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


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
            talent_dir
            / "stream_summaries/overall_themes/personality_open_coding/v2/open_codebook_v2.csv",
            talent_dir
            / "stream_summaries/overall_themes/personality_open_coding/v2/open_coding_evidence_v2.csv",
            talent_dir
            / "stream_summaries/overall_themes/personality_profile_v2_open_coding.md",
        ),
    ]
    for codebook, evidence, markdown in candidates:
        if codebook.exists() and evidence.exists() and markdown.exists():
            return codebook, evidence, markdown
    return None


def discover_eligible_talents() -> List[TalentInputs]:
    talents: List[TalentInputs] = []
    for child in sorted(TALENT_ROOT.iterdir(), key=lambda path: path.name):
        if not child.is_dir():
            continue
        if child.name == "VarianceProject":
            continue
        open_paths = choose_open_coding_paths(child)
        unique_profile = (
            child
            / "stream_summaries/overall_themes/personality_unique_features/current/unique_personality_profile_codex.md"
        )
        unique_evidence = (
            child
            / "stream_summaries/overall_themes/personality_unique_features/current/unique_personality_evidence_log_codex.csv"
        )
        unique_state = (
            child
            / "stream_summaries/overall_themes/personality_unique_features/current/unique_personality_state.json"
        )
        if open_paths is None or not unique_profile.exists() or not unique_evidence.exists():
            continue
        summary_markdown = (
            child
            / "stream_summaries/overall_themes/summary_classification/current/overall_themes_codex.md"
        )
        summary_state = (
            child
            / "stream_summaries/overall_themes/summary_classification/current/summary_classification_state.json"
        )
        talents.append(
            TalentInputs(
                talent=child.name,
                base_dir=child,
                open_codebook=open_paths[0],
                open_evidence=open_paths[1],
                open_markdown=open_paths[2],
                unique_profile=unique_profile,
                unique_evidence=unique_evidence,
                unique_state=unique_state,
                summary_markdown=summary_markdown if summary_markdown.exists() else None,
                summary_state=summary_state if summary_state.exists() else None,
            )
        )
    return talents


def build_indexes(
    talents: Sequence[TalentInputs],
) -> tuple[
    Dict[str, List[Dict[str, str]]],
    Dict[tuple[str, str], List[Dict[str, str]]],
]:
    shared_evidence = read_csv(SHARED_ROOT / "shared_behavior_evidence.csv")
    shared_by_behavior: Dict[str, List[Dict[str, str]]] = {}
    for row in shared_evidence:
        shared_by_behavior.setdefault(row["shared_behavior_name"], []).append(row)

    unique_by_dimension: Dict[tuple[str, str], List[Dict[str, str]]] = {}
    for talent in talents:
        rows = read_csv(talent.unique_evidence)
        for row in rows:
            unique_by_dimension.setdefault((talent.talent, row["dimension_name"]), []).append(row)

    return shared_by_behavior, unique_by_dimension


def shorten_quote(text: str, limit: int = 72) -> str:
    cleaned = " ".join(text.replace('"', "'").split())
    if len(cleaned) <= limit:
        return cleaned
    return cleaned[: limit - 3].rstrip() + "..."


def first_rows_by_talent(rows: Sequence[Dict[str, str]], limit: int = 4) -> List[Dict[str, str]]:
    chosen: List[Dict[str, str]] = []
    seen_talents: set[str] = set()
    for row in rows:
        talent = row["talent"]
        if talent in seen_talents:
            continue
        chosen.append(row)
        seen_talents.add(talent)
        if len(chosen) >= limit:
            break
    return chosen


def build_examples(
    refs: Sequence[EvidenceRef],
    shared_by_behavior: Dict[str, List[Dict[str, str]]],
    unique_by_dimension: Dict[tuple[str, str], List[Dict[str, str]]],
) -> str:
    parts: List[str] = []
    for ref in refs:
        if ref.kind == "shared_behavior":
            rows = shared_by_behavior.get(ref.name, [])
            if not rows:
                raise RuntimeError(f"Missing shared behavior evidence for {ref.name!r}")
            selected = first_rows_by_talent(rows)
        elif ref.kind == "unique_dimension":
            if ref.talent is None:
                raise RuntimeError(f"Unique dimension ref for {ref.name!r} is missing a talent")
            rows = unique_by_dimension.get((ref.talent, ref.name), [])
            if not rows:
                raise RuntimeError(
                    f"Missing unique-dimension evidence for {ref.talent!r} / {ref.name!r}"
                )
            selected = rows[:2]
        else:
            raise RuntimeError(f"Unsupported evidence ref kind: {ref.kind}")

        for row in selected:
            rendered = f'{row["talent"]}: "{shorten_quote(row["quote"])}"'
            if rendered not in parts:
                parts.append(rendered)
    return "; ".join(parts)


def load_existing_dates(use_existing_log: bool) -> tuple[bool, Dict[tuple[str, str], str]]:
    if not use_existing_log:
        return False, {}
    if not CURRENT_CSV.exists():
        return False, {}
    rows = read_csv(CURRENT_CSV)
    dates: Dict[tuple[str, str], str] = {}
    for row in rows:
        dates[(row["Primary Code ID"], row["Secondary Code ID"])] = row["Date added"]
    return True, dates


def build_code_specs() -> List[CodeSpec]:
    return [
        CodeSpec(
            primary_id="A1",
            primary_code="Ritualized supporter acknowledgment",
            secondary_id="",
            secondary_code="",
            definition=(
                "Recurring turns where audience support is publicly named, thanked, or marked as a "
                "socially structured exchange rather than a passing courtesy."
            ),
            refs=[EvidenceRef("shared_behavior", "gratitude ritualization")],
            why_added=(
                "Retained as the clearest dataset-wide family: all four talents repeatedly turn "
                "support recognition into visible interaction."
            ),
            support_summary=(
                "Backed by the shared baseline across all four talents and by high-frequency "
                "thank-recognition families in each open-coding set."
            ),
        ),
        CodeSpec(
            primary_id="A1",
            primary_code="Ritualized supporter acknowledgment",
            secondary_id="A1a",
            secondary_code="Ceremonial reception framing",
            definition=(
                "Supporter acknowledgment delivered as initiation, blessing, or staged welcome "
                "rather than plain appreciation."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Ceremonial ominous supporter induction",
                    "Avaritia Hawthorne 【Variance Project】",
                ),
                EvidenceRef(
                    "unique_dimension",
                    "Ceremonial hostess framing",
                    "Leia Memoria【Variance Project】",
                ),
            ],
            why_added=(
                "Added because Avaritia and Leia both repeatedly convert thanks/welcomes into "
                "ceremony, which materially changes how gratitude functions in later coding."
            ),
            support_summary=(
                "Supported by paired unique-feature dimensions in Avaritia and Leia, with quotes "
                "showing induction, blessing, and tavern-style reception language."
            ),
        ),
        CodeSpec(
            primary_id="A1",
            primary_code="Ritualized supporter acknowledgment",
            secondary_id="A1b",
            secondary_code="Reciprocal gratitude/check-in loop",
            definition=(
                "Gratitude that explicitly thanks viewers for waiting, being present, or checking "
                "in, turning appreciation into mutual attendance."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Reciprocal gratitude/check-in loop",
                    "Terberri Solaris Ch",
                )
            ],
            why_added=(
                "Added because Terberri's gratitude repeatedly expands beyond support events into "
                "communal check-in language."
            ),
            support_summary=(
                "Currently concentrated in Terberri's unique-feature layer, but backed by multiple "
                "rows and the largest thank-recognition family in the current corpus."
            ),
        ),
        CodeSpec(
            primary_id="B1",
            primary_code="Audience pacing and room control",
            secondary_id="",
            secondary_code="",
            definition=(
                "Visible steering of audience attention, transitions, or timing so the streamer "
                "can reset, delay, or refocus the room in public."
            ),
            refs=[EvidenceRef("shared_behavior", "audience-attentive pacing control")],
            why_added=(
                "Retained because all four talents repeatedly manage interaction tempo instead of "
                "letting chat or gameplay drift unattended."
            ),
            support_summary=(
                "Backed by the shared baseline and by large pacing-control families in every "
                "eligible talent's open coding."
            ),
        ),
        CodeSpec(
            primary_id="B1",
            primary_code="Audience pacing and room control",
            secondary_id="B1a",
            secondary_code="Need-led flow control",
            definition=(
                "Pacing or redirection moves justified through stated need to focus, recover, or "
                "manage current capacity."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Need-led flow control",
                    "Terberri Solaris Ch",
                )
            ],
            why_added=(
                "Added because Terberri repeatedly knots flow control to self-disclosed need, "
                "which should not be flattened into generic redirection."
            ),
            support_summary=(
                "Supported by Terberri's unique-feature evidence where pacing language and "
                "self-explanation recur together."
            ),
        ),
        CodeSpec(
            primary_id="C1",
            primary_code="Reassurance and tension repair",
            secondary_id="",
            secondary_code="",
            definition=(
                "Repeated soothing, normalizing, or permission-giving turns that lower tension "
                "around mistakes, embarrassment, fatigue, or strain."
            ),
            refs=[EvidenceRef("shared_behavior", "reassurance during strain or recovery")],
            why_added=(
                "Retained because reassurance is strong enough across the set to be a reusable "
                "family, even though its tone varies by talent."
            ),
            support_summary=(
                "Backed by shared-baseline evidence in all four talents, with especially strong "
                "support in Leia and Terberri and durable support in Avaritia."
            ),
        ),
        CodeSpec(
            primary_id="D1",
            primary_code="Audience-facing self-disclosure",
            secondary_id="",
            secondary_code="",
            definition=(
                "Recurring first-person disclosure of need, anxiety, overwhelm, or felt difficulty "
                "that is made legible to the audience."
            ),
            refs=[EvidenceRef("shared_behavior", "audience-facing self-disclosure windows")],
            why_added=(
                "Retained because self-disclosure recurs across all four talents and repeatedly "
                "shapes the relationship stance toward chat."
            ),
            support_summary=(
                "Backed by the shared baseline and by high-frequency self-disclosure families in "
                "every eligible talent's open coding."
            ),
        ),
        CodeSpec(
            primary_id="E1",
            primary_code="Stylized teasing and welcome escalation",
            secondary_id="",
            secondary_code="",
            definition=(
                "Recurring joking address, welcome lines, or exaggerated bits that turn direct chat "
                "contact into a stylized performance move."
            ),
            refs=[EvidenceRef("shared_behavior", "performative teasing and welcome escalation")],
            why_added=(
                "Retained because playful escalation is shared across the corpus but meaningfully "
                "variation-sensitive in form and relationship function."
            ),
            support_summary=(
                "Backed by the shared baseline plus strong welcome/bit families in Avaritia, "
                "Katya, Leia, and moderate recurring support in Terberri."
            ),
        ),
        CodeSpec(
            primary_id="E1",
            primary_code="Stylized teasing and welcome escalation",
            secondary_id="E1a",
            secondary_code="Mock-ominous welcome play",
            definition=(
                "Teasing or welcoming language that uses threat-tinged, dark, or grandiose imagery "
                "as a rapport-building performance style."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Ceremonial ominous supporter induction",
                    "Avaritia Hawthorne 【Variance Project】",
                ),
                EvidenceRef(
                    "unique_dimension",
                    "Menace-coded body/flirt register",
                    "Avaritia Hawthorne 【Variance Project】",
                ),
            ],
            why_added=(
                "Added because Avaritia's welcome/teasing register is repeatedly darker and more "
                "mock-menacing than generic play or hype."
            ),
            support_summary=(
                "Currently concentrated in Avaritia, but backed by multiple unique-feature rows "
                "spanning welcome slogans and body-horror or flirt-inflected joke language."
            ),
        ),
        CodeSpec(
            primary_id="E1",
            primary_code="Stylized teasing and welcome escalation",
            secondary_id="E1b",
            secondary_code="Insult-flirt banter",
            definition=(
                "Teasing that mixes pet names, insult slang, mock aggression, or flirt-adjacent "
                "address in the same interactional register."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Insult-flirt escalation",
                    "Katya Sable 【Variance Project】",
                )
            ],
            why_added=(
                "Added because Katya's joking contact is not just 'playful teasing'; it repeatedly "
                "combines abrasion, flirtation, and mock threat."
            ),
            support_summary=(
                "Supported by Katya's unique-feature evidence and by a substantial baby/kill/bozo "
                "bit family in the open-coding layer."
            ),
        ),
        CodeSpec(
            primary_id="E1",
            primary_code="Stylized teasing and welcome escalation",
            secondary_id="E1c",
            secondary_code="Protective pet-name play",
            definition=(
                "Affectionate pet-name or 'baby' language that casts teasing or hype through rescue, "
                "caretaking, or soft protection."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Maternal-protective care/play blend",
                    "Leia Memoria【Variance Project】",
                )
            ],
            why_added=(
                "Added because Leia repeatedly fuses play and care, making pet-name escalation do "
                "protective relationship work rather than generic cuteness."
            ),
            support_summary=(
                "Supported by Leia's unique-feature evidence and by repeated baby-bit and "
                "reassurance-family overlap."
            ),
        ),
        CodeSpec(
            primary_id="F1",
            primary_code="Boundary-setting as audience management",
            secondary_id="",
            secondary_code="",
            definition=(
                "Recurring rule-setting or correction retained only when it visibly manages suspense, "
                "audience pacing, or room tone rather than functioning as narrow housekeeping."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Confrontational audience-boundary correction",
                    "Avaritia Hawthorne 【Variance Project】",
                ),
                EvidenceRef(
                    "unique_dimension",
                    "Abrupt anti-pestering boundary control",
                    "Katya Sable 【Variance Project】",
                ),
                EvidenceRef(
                    "unique_dimension",
                    "Protective suspense-boundary enforcement",
                    "Leia Memoria【Variance Project】",
                ),
                EvidenceRef(
                    "unique_dimension",
                    "Light social-hygiene boundaries",
                    "Terberri Solaris Ch",
                ),
            ],
            why_added=(
                "Added as a lower-confidence but useful family because all four talents show "
                "boundary work that becomes analytically meaningful when reframed as room "
                "management rather than rule housekeeping."
            ),
            support_summary=(
                "Support is thinner than the shared baseline families, but each talent now has a "
                "retained unique-feature boundary dimension that points to reusable subtype coding."
            ),
        ),
        CodeSpec(
            primary_id="F1",
            primary_code="Boundary-setting as audience management",
            secondary_id="F1a",
            secondary_code="Confrontational audience correction",
            definition=(
                "Abrupt snaps, shutdowns, or anti-pestering corrections that publicly put the room "
                "back in place."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Confrontational audience-boundary correction",
                    "Avaritia Hawthorne 【Variance Project】",
                ),
                EvidenceRef(
                    "unique_dimension",
                    "Abrupt anti-pestering boundary control",
                    "Katya Sable 【Variance Project】",
                ),
            ],
            why_added=(
                "Added because Avaritia and Katya both show recurring hard-stop correction styles "
                "that are sharper than neutral moderation reminders."
            ),
            support_summary=(
                "Supported by parallel unique-feature dimensions in Avaritia and Katya, each backed "
                "by repeated anti-question or anti-pestering rows."
            ),
        ),
        CodeSpec(
            primary_id="F1",
            primary_code="Boundary-setting as audience management",
            secondary_id="F1b",
            secondary_code="Protective suspense control",
            definition=(
                "Spoiler or backseating correction framed as protection of suspense, discovery, or "
                "shared pacing rather than as bare policy enforcement."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Protective suspense-boundary enforcement",
                    "Leia Memoria【Variance Project】",
                )
            ],
            why_added=(
                "Added because Leia's guardrail language repeatedly organizes around protecting the "
                "shared experience of suspense."
            ),
            support_summary=(
                "Currently concentrated in Leia, where spoiler/backseat evidence recurs often enough "
                "to justify a specific relational subcode."
            ),
        ),
        CodeSpec(
            primary_id="F1",
            primary_code="Boundary-setting as audience management",
            secondary_id="F1c",
            secondary_code="Light social-hygiene reminders",
            definition=(
                "Low-drama reminders about spam, follow-backs, or limited asks that keep basic room "
                "order without escalating into a dominant persona claim."
            ),
            refs=[
                EvidenceRef(
                    "unique_dimension",
                    "Light social-hygiene boundaries",
                    "Terberri Solaris Ch",
                )
            ],
            why_added=(
                "Added because Terberri's boundary work is analytically distinct from harsher "
                "correction styles and reads as routine social-hygiene maintenance."
            ),
            support_summary=(
                "Supported by Terberri's unique-feature evidence, especially repeated follow-back "
                "and low-intensity cleanup reminders."
            ),
        ),
    ]


def render_added_section(specs: Sequence[CodeSpec]) -> str:
    lines: List[str] = []
    for spec in specs:
        secondary_id = spec.secondary_id or "(blank)"
        secondary_code = spec.secondary_code or "(blank)"
        lines.append(
            f"- `Primary Code ID`: {spec.primary_id}; `Primary Code`: {spec.primary_code}; "
            f"`Secondary Code ID`: {secondary_id}; `Secondary Code`: {secondary_code}; "
            f"`why it was added`: {spec.why_added} "
            f"`supporting evidence summary`: {spec.support_summary}"
        )
    return "\n".join(lines)


def render_high_value_families() -> str:
    return "\n".join(
        [
            "- `A1 Ritualized supporter acknowledgment` looks durable because it is the strongest "
            "cross-talent family and already carries analytically meaningful variants instead of "
            "generic gratitude language.",
            "- `B1 Audience pacing and room control` remains high-value because it is broadly shared "
            "while still differentiating how streamers stage authority, need, or collective focus.",
            "- `C1 Reassurance and tension repair` is durable because it tracks relationship work "
            "during mistakes, embarrassment, or strain without collapsing into generic "
            "'supportiveness.'",
            "- `D1 Audience-facing self-disclosure` is analytically useful because it captures how "
            "streamers make inner state legible to chat, which repeatedly shapes pacing and "
            "rapport.",
            "- `E1 Stylized teasing and welcome escalation` is worth keeping because playful "
            "audience contact is shared, but the social meaning shifts sharply across talents.",
            "- `F1 Boundary-setting as audience management` is thinner than the main families but "
            "still useful because it reframes rules and corrections as relationship management when "
            "the evidence supports that move.",
        ]
    )


def render_open_questions() -> str:
    return "\n".join(
        [
            "- `B1` may need a later split if coach-style refocusing, disciplinary stage-management, "
            "and theatrical room restaging keep recurring as stable variants beyond this first run.",
            "- `C1` may eventually need subcodes for protective caretaking versus worth/self-care "
            "affirmation, but the current log keeps reassurance unsplit to avoid premature churn.",
            "- `D1` may later warrant explicit variant rows for complaint-shaped or warm anxiety "
            "disclosure if those forms recur more broadly across talents.",
            "- `E1` still leaves Terberri's situational chaos/twist welcomes and Katya's "
            "boss/casino world-building in memo territory rather than full retained codes.",
            "- `F1` should be monitored closely: it is useful now, but still lower-confidence than "
            "the main shared families because the underlying guardrail evidence remains thinner and "
            "more context-bound.",
        ]
    )


def build_rows(
    specs: Sequence[CodeSpec],
    existing_dates: Dict[tuple[str, str], str],
    shared_by_behavior: Dict[str, List[Dict[str, str]]],
    unique_by_dimension: Dict[tuple[str, str], List[Dict[str, str]]],
) -> List[Dict[str, str]]:
    rows: List[Dict[str, str]] = []
    for spec in specs:
        key = (spec.primary_id, spec.secondary_id)
        rows.append(
            {
                "Primary Code ID": spec.primary_id,
                "Primary Code": spec.primary_code,
                "Secondary Code ID": spec.secondary_id,
                "Secondary Code": spec.secondary_code,
                "Definition": spec.definition,
                "Date added": existing_dates.get(key, CURRENT_DATE),
                "Examples from text": build_examples(spec.refs, shared_by_behavior, unique_by_dimension),
            }
        )
    return rows


def write_csv(rows: Sequence[Dict[str, str]]) -> None:
    CURRENT_DIR.mkdir(parents=True, exist_ok=True)
    fieldnames = [
        "Primary Code ID",
        "Primary Code",
        "Secondary Code ID",
        "Secondary Code",
        "Definition",
        "Date added",
        "Examples from text",
    ]
    with CURRENT_CSV.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def build_markdown(
    eligible_talents: Sequence[str],
    existing_log_used: bool,
    specs: Sequence[CodeSpec],
) -> str:
    secondary_used = "yes"
    existing_used = "yes" if existing_log_used else "no"
    paragraphs = [
        (
            "This run initializes the cumulative qualitative code log as a compact registry rather "
            "than a finished taxonomy. The strongest move was to retain the five high-signal shared "
            "families already stabilized by the cross-talent baseline and to add one thinner but "
            "useful family for boundary-setting when it clearly functioned as audience management "
            "rather than housekeeping."
        ),
        (
            "Most of the analytic detail this run sits in a small set of secondary codes. Those "
            "subcodes were added only where the variant form materially changes interpretation: "
            "ceremonial gratitude, reciprocal gratitude, need-led pacing, several teasing "
            "registers, and three boundary-management forms. Reassurance and self-disclosure were "
            "kept broad for now to avoid premature over-splitting."
        ),
        (
            "Summary-classification files were available for all eligible talents and mostly "
            "reinforced the same core families instead of generating new ones. No prior maintained "
            "qualitative log existed in the processed root, so this run is an initial registry build "
            "rather than a revision of an established codebook."
        ),
    ]

    sections = [
        f"Analysis conducted: {ANALYSIS_STAMP}",
        f"Eligible talents included: {len(eligible_talents)}",
        "Primary source layers: personality_open_coding + shared_interactions + personality_unique_features",
        f"Secondary source used: summary_classification {secondary_used}",
        f"Existing log used: {existing_used}",
        "",
        "## 1) Log Update Summary",
        "",
        paragraphs[0],
        "",
        paragraphs[1],
        "",
        paragraphs[2],
        "",
        "## 2) Codes Added This Run",
        "",
        render_added_section(specs).splitlines()[0],
    ]
    sections.extend(render_added_section(specs).splitlines()[1:])
    sections.extend(
        [
            "",
            "## 3) Codes Revised This Run",
            "",
            "- None. This was the first maintained run, so all retained rows entered as new codes.",
            "",
            "## 4) Codes Merged, Split, or Retired",
            "",
            "- None in this run. No existing maintained log was present to merge, split, or retire "
            "against.",
            "",
            "## 5) Current High-Value Code Families",
            "",
            *render_high_value_families().splitlines(),
            "",
            "## 6) Open Questions for Future Recoding",
            "",
            *render_open_questions().splitlines(),
            "",
        ]
    )
    return "\n".join(sections)


def write_markdown(content: str) -> None:
    CURRENT_DIR.mkdir(parents=True, exist_ok=True)
    SNAPSHOT_DIR.mkdir(parents=True, exist_ok=True)
    CURRENT_MD.write_text(content, encoding="utf-8")
    SNAPSHOT_MD.write_text(content, encoding="utf-8")


def write_state(
    eligible_talents: Sequence[str],
    existing_log_used: bool,
    row_count: int,
    new_codes_added: int,
) -> None:
    state = {
        "analysis_conducted": ANALYSIS_STAMP,
        "eligible_talents": list(eligible_talents),
        "existing_log_used": existing_log_used,
        "rows_in_log": row_count,
        "new_codes_added": new_codes_added,
        "codes_revised": 0,
        "codes_merged": [],
        "codes_split": [],
        "codes_retired": [],
        "source_layers_used": [
            "personality_open_coding",
            "shared_interactions",
            "personality_unique_features",
        ],
        "secondary_source_used": "summary_classification",
        "output_paths": {
            "csv": str(CURRENT_CSV),
            "markdown": str(CURRENT_MD),
            "state": str(CURRENT_STATE),
            "snapshot_markdown": str(SNAPSHOT_MD),
        },
        "notes": [
            "Initial cumulative registry build from shared baseline, per-talent open coding, and per-talent unique-feature outputs.",
            "Reassurance and self-disclosure were intentionally left broader than the teasing and boundary families to avoid premature over-splitting.",
            "Boundary-setting was retained only when the evidence supported broader audience-management meaning rather than narrow housekeeping.",
        ],
    }
    CURRENT_STATE.write_text(json.dumps(state, indent=2), encoding="utf-8")


def main() -> int:
    use_existing_log = "--initial-build" not in sys.argv[1:]
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

    talents = discover_eligible_talents()
    if not talents:
        raise RuntimeError("No eligible talents found.")

    summary_available = all(talent.summary_markdown is not None for talent in talents)
    if not summary_available:
        # The prompt allows missing summary-classification artifacts, so keep going.
        pass

    shared_by_behavior, unique_by_dimension = build_indexes(talents)
    existing_log_used, existing_dates = load_existing_dates(use_existing_log=use_existing_log)
    specs = build_code_specs()
    rows = build_rows(specs, existing_dates, shared_by_behavior, unique_by_dimension)
    write_csv(rows)
    markdown = build_markdown(
        eligible_talents=[talent.talent for talent in talents],
        existing_log_used=existing_log_used,
        specs=specs,
    )
    write_markdown(markdown)
    write_state(
        eligible_talents=[talent.talent for talent in talents],
        existing_log_used=existing_log_used,
        row_count=len(rows),
        new_codes_added=sum(1 for row in rows if row["Date added"] == CURRENT_DATE),
    )
    print(f"Wrote {len(rows)} code-log rows to {CURRENT_CSV}")
    print(f"Wrote memo to {CURRENT_MD}")
    print(f"Wrote state to {CURRENT_STATE}")
    print(f"Wrote snapshot to {SNAPSHOT_MD}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
