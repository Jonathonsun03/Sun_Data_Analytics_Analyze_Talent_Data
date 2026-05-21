from __future__ import annotations

import csv
import json
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo


CODEBOOK_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook")
CURRENT_ROOT = CODEBOOK_ROOT / "current"
SNAPSHOT_ROOT = CODEBOOK_ROOT / "snapshots"

CSV_PATH = CURRENT_ROOT / "personality_qualitative_code_log.csv"
MARKDOWN_PATH = CURRENT_ROOT / "personality_qualitative_code_log_codex.md"
STATE_PATH = CURRENT_ROOT / "personality_qualitative_code_log_state.json"

REVISED_DEFINITIONS = {
    ("A1", ""): (
        "Recurring supporter-recognition turns in which audience presence, raids, paid messages, gifted memberships, "
        "supporter goals, waiting, or check-ins are explicitly converted into a visible social ritual. Include "
        "monetary-adjacent streamer turns when prior context clearly establishes a paid or supporter event, even if "
        "the current row does not repeat explicit money terms."
    ),
    ("A1", "A1a"): (
        "Supporter acknowledgment delivered as initiation, blessing, staged welcome, or ritualized reception rather "
        "than plain appreciation. Include paid messages, gifts, memberships, raids, and other supporter events when "
        "the current row ceremonially receives or inducts the supporter/audience action, even without repeated money terms."
    ),
    ("A1", "A1b"): (
        "Gratitude that thanks viewers for waiting, being present, checking in, giving, gifting, joining, or otherwise "
        "supporting, turning appreciation into mutual attendance. The current row may be coded when prior context "
        "clearly anchors the gratitude to a paid/supporter event even if the row itself uses broad thanks rather than "
        "explicit money language."
    ),
    ("E1", ""): (
        "Recurring use of stylized welcome lines, pet-name address, bestie framing, joking escalation, or theatrical "
        "handoff that turns audience contact into a performance move rather than plain acknowledgment. Include "
        "monetary-adjacent supporter moments when a paid message, gift, membership, goal, or donor action is folded "
        "into this stylized interactional register."
    ),
    ("E1", "E1a"): (
        "Teasing or welcoming language that uses threat-tinged, dark, or grandiose imagery as a rapport-building "
        "performance style, including paid/supporter moments that are received through mock-ominous ceremony rather "
        "than plain thanks."
    ),
    ("E1", "E1d"): (
        "A subtype of stylized audience contact where membership welcomes, paid/supporter acknowledgments, and supporter "
        "thanks are fused with bestie intimacy, baby-coded hype, or fast celebratory handoff language. The row can "
        "qualify when prior context establishes the membership/gift/paid-event anchor and the current row performs "
        "the intimacy or hype framing."
    ),
}


def update_csv() -> list[str]:
    with CSV_PATH.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        fieldnames = reader.fieldnames
        if fieldnames is None:
            raise SystemExit(f"Codebook has no header: {CSV_PATH}")
        rows = list(reader)

    changed: list[str] = []
    for row in rows:
        key = (row.get("Primary Code ID", ""), row.get("Secondary Code ID", ""))
        revised = REVISED_DEFINITIONS.get(key)
        if revised is None:
            continue
        row["Definition"] = revised
        changed.append(key[1] or key[0])

    with CSV_PATH.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    return changed


def update_markdown(changed: list[str], stamp: str) -> Path:
    SNAPSHOT_ROOT.mkdir(parents=True, exist_ok=True)
    note = f"""

## Maintenance Note: Monetary-Adjacent Definition Broadening ({stamp})

No codes were added, merged, split, or retired. Existing supporter/welcome definitions were broadened so monetary-conversation coding can capture interactional adjacency: a row may qualify when prior context clearly establishes a paid message, gift, membership, goal, donor action, or streamer monetary acknowledgment and the current row performs the existing code's interactional function without repeating explicit money terms.

Definitions revised: {", ".join(changed)}.

Guardrail retained: generic hype, affection, gratitude, or bestie framing should not be treated as monetary-adjacent unless supplied context makes the monetary/supporter event interactionally relevant and the current row itself performs the coded function.
"""
    current = MARKDOWN_PATH.read_text(encoding="utf-8")
    if "Maintenance Note: Monetary-Adjacent Definition Broadening" not in current:
        MARKDOWN_PATH.write_text(current.rstrip() + note + "\n", encoding="utf-8")

    now_for_name = datetime.now(ZoneInfo("America/New_York"))
    snapshot_path = SNAPSHOT_ROOT / f"personality_qualitative_code_log_{now_for_name:%Y-%m-%d_%H-%M-%S_%z}.md"
    snapshot_path.write_text(MARKDOWN_PATH.read_text(encoding="utf-8"), encoding="utf-8")
    return snapshot_path


def update_state(changed: list[str], stamp: str, snapshot_path: Path) -> None:
    state = json.loads(STATE_PATH.read_text(encoding="utf-8"))
    revised = list(state.get("codes_revised", []))
    for code in changed:
        if code not in revised:
            revised.append(code)
    state["analysis_conducted"] = stamp
    state["codes_revised"] = revised
    notes = list(state.get("notes", []))
    notes.append(
        "Broadened existing supporter/welcome definitions for monetary-adjacent context without adding new code IDs; "
        "explicit money terms are sufficient but not required when prior context clearly anchors the current row to a "
        "paid/supporter event."
    )
    state["notes"] = notes
    state.setdefault("output_paths", {})["snapshot_markdown"] = str(snapshot_path)
    STATE_PATH.write_text(json.dumps(state, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")


def main() -> int:
    now = datetime.now(ZoneInfo("America/New_York"))
    stamp = now.strftime("%Y-%m-%d %H:%M %Z")
    changed = update_csv()
    snapshot_path = update_markdown(changed, stamp)
    update_state(changed, stamp, snapshot_path)
    print(
        json.dumps(
            {
                "csv": str(CSV_PATH),
                "markdown": str(MARKDOWN_PATH),
                "state": str(STATE_PATH),
                "snapshot": str(snapshot_path),
                "revised": changed,
            },
            ensure_ascii=False,
            indent=2,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
