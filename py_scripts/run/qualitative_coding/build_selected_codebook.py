#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import sys
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

SCRIPT_REPO_ROOT = Path(__file__).resolve().parents[3]
if str(SCRIPT_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(SCRIPT_REPO_ROOT))

from py_scripts.lib.qualitative_coding.codebook import first_value, selected_truthy
from py_scripts.lib.qualitative_coding.csv_io import read_csv_dicts
from py_scripts.lib.utils.paths import translate_datalake_path


CODEBOOK_FIELDS = [
    "Primary Code ID",
    "Primary Code",
    "Secondary Code ID",
    "Secondary Code",
    "Definition",
    "Examples from text",
]

SELECTION_FIELDS = [
    "code_id",
    "code_column",
    "analytic_object",
    "row_source_scope",
    "source_group",
    "source_label",
    "source_path",
]


def write_csv(path: Path, fieldnames: list[str], rows: list[dict[str, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def default_output_path(selection_path: Path) -> Path:
    name = selection_path.name
    if name.endswith("_selection.csv"):
        return selection_path.with_name(name.replace("_selection.csv", "_codebook.csv"))
    if name.endswith("_code_selection.csv"):
        return selection_path.with_name(name.replace("_code_selection.csv", "_codebook.csv"))
    return selection_path.with_name(f"{selection_path.stem}_codebook.csv")


def metadata_path_for(codebook_path: Path) -> Path:
    if codebook_path.name.endswith("_codebook.csv"):
        return codebook_path.with_name(codebook_path.name.replace("_codebook.csv", "_selection.csv"))
    return codebook_path.with_name(f"{codebook_path.stem}_selection.csv")


def selected_rows(rows: list[dict[str, str]]) -> list[dict[str, str]]:
    selected = [row for row in rows if selected_truthy(row.get("selected_for_processing", ""))]
    if not selected:
        raise SystemExit("No rows have selected_for_processing set to TRUE.")
    return selected


def build_codebook_rows(rows: list[dict[str, str]]) -> list[dict[str, str]]:
    out: list[dict[str, str]] = []
    seen: set[str] = set()
    for row in rows:
        code_id = first_value(row, "code_id", "secondary_code_id", "primary_code_id")
        code_column = first_value(row, "code_column") or f"code_{code_id}"
        if not code_id or code_column in seen:
            continue
        seen.add(code_column)
        primary_id = first_value(row, "primary_code_id", "Primary Code ID")
        secondary_id = first_value(row, "secondary_code_id", "Secondary Code ID")
        definition = first_value(row, "definition", "Definition")
        inclusion = first_value(row, "inclusion_criteria", "Inclusion Criteria")
        exclusion = first_value(row, "exclusion_criteria", "Exclusion Criteria")
        function = first_value(row, "interactional_function", "Interactional Function")
        definition_parts = [definition]
        if inclusion:
            definition_parts.append(f"Inclusion criteria: {inclusion}")
        if exclusion:
            definition_parts.append(f"Exclusion criteria: {exclusion}")
        if function:
            definition_parts.append(f"Interactional function: {function}")
        rich_definition = " ".join(part for part in definition_parts if part)
        out.append(
            {
                "Primary Code ID": primary_id,
                "Primary Code": first_value(row, "primary_code", "Primary Code", "code_name"),
                "Secondary Code ID": secondary_id,
                "Secondary Code": first_value(row, "secondary_code", "Secondary Code", "code_name"),
                "Definition": rich_definition,
                "Examples from text": first_value(row, "examples_from_text", "Examples from text", "example_quote"),
                "_code_id": code_id,
                "_code_column": code_column,
            }
        )
    if not out:
        raise SystemExit("No usable selected code rows found.")
    return out


def build_metadata_rows(rows: list[dict[str, str]]) -> list[dict[str, str]]:
    out: list[dict[str, str]] = []
    seen: set[str] = set()
    for row in rows:
        code_id = first_value(row, "code_id", "secondary_code_id", "primary_code_id")
        code_column = first_value(row, "code_column") or f"code_{code_id}"
        if not code_id or code_column in seen:
            continue
        seen.add(code_column)
        out.append(
            {
                "code_id": code_id,
                "code_column": code_column,
                "analytic_object": first_value(row, "analytic_object"),
                "row_source_scope": first_value(row, "row_source_scope"),
                "source_group": first_value(row, "source_group"),
                "source_label": first_value(row, "source_label"),
                "source_path": first_value(row, "source_path"),
            }
        )
    return out


def write_manifest(path: Path, selection_path: Path, codebook_path: Path, metadata_path: Path, row_count: int) -> None:
    manifest = {
        "created_at": datetime.now(ZoneInfo("America/New_York")).isoformat(),
        "selection_path": str(selection_path),
        "codebook_path": str(codebook_path),
        "selection_metadata_path": str(metadata_path),
        "selected_code_count": row_count,
    }
    path.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(description="Build a batch-ready codebook CSV from selected qualitative library rows.")
    parser.add_argument("--selection", required=True, help="Qualitative library CSV with selected_for_processing flags.")
    parser.add_argument("--output", default="", help="Output selected codebook CSV. Defaults beside the selection file.")
    parser.add_argument("--overwrite", action="store_true")
    args = parser.parse_args()

    selection_path = translate_datalake_path(args.selection).resolve()
    output_path = translate_datalake_path(args.output).resolve() if args.output else default_output_path(selection_path)
    metadata_path = metadata_path_for(output_path)
    manifest_path = output_path.with_name(f"{output_path.stem}_manifest.json")

    if not selection_path.exists():
        raise SystemExit(f"Selection file not found: {selection_path}")
    for path in (output_path, metadata_path, manifest_path):
        if path.exists() and not args.overwrite:
            raise SystemExit(f"Output already exists; pass --overwrite to replace: {path}")

    fields, rows = read_csv_dicts(selection_path)
    if "selected_for_processing" not in fields:
        raise SystemExit(f"Selection file must contain selected_for_processing: {selection_path}")

    selected = selected_rows(rows)
    codebook_rows = build_codebook_rows(selected)
    metadata_rows = build_metadata_rows(selected)

    write_csv(output_path, CODEBOOK_FIELDS, codebook_rows)
    write_csv(metadata_path, SELECTION_FIELDS, metadata_rows)
    write_manifest(manifest_path, selection_path, output_path, metadata_path, len(codebook_rows))

    print(f"selected_codebook: {output_path}")
    print(f"selection_metadata: {metadata_path}")
    print(f"manifest: {manifest_path}")
    print(f"selected_code_count: {len(codebook_rows)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
