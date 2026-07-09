#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import io
import sys
from pathlib import Path

SCRIPT_REPO_ROOT = Path(__file__).resolve().parents[3]
if str(SCRIPT_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(SCRIPT_REPO_ROOT))

from py_scripts.lib.qualitative_coding.codebook import CodeDef, hierarchy_pairs, load_codebook, resolve_codebook
from py_scripts.lib.qualitative_coding.csv_io import read_csv_dicts, write_csv_dicts
from py_scripts.lib.qualitative_coding.prepared_transcripts import (
    TargetFile,
    filter_transcripts,
    inspect_target,
    list_prepared_transcript_files,
    row_id_for,
    row_needs_coding,
)
from py_scripts.lib.qualitative_coding.text import is_missing, squish
from py_scripts.lib.utils.paths import REPO_ROOT, default_talent_root, resolve_talent_paths


def parse_patch(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    text = path.read_text(encoding="utf-8-sig").strip()
    if text.startswith("```"):
        lines = [line for line in text.splitlines() if not line.strip().startswith("```")]
        text = "\n".join(lines)
    reader = csv.DictReader(io.StringIO(text))
    fieldnames = list(reader.fieldnames or [])
    if not fieldnames or fieldnames[0] != "row_id":
        raise SystemExit("Patch CSV must have row_id as the first column.")
    rows = [{key: (value if value is not None else "") for key, value in row.items()} for row in reader]
    return fieldnames, rows


def prompt_value(row: dict[str, str], key: str) -> str:
    return squish(row.get(key, ""))


def format_prompt_row(path: Path, row: dict[str, str], index: int) -> list[str]:
    fields = ("source", "speaker", "text", "timecode", "paid_amount_text", "paid_amount_value", "paid_currency")
    lines = [f"- row_id: {row_id_for(path, row, index)}", f"  row_number: {index + 1}"]
    for field in fields:
        if field in row:
            lines.append(f"  {field}: {prompt_value(row, field)}")
    return lines


def emit_prompt(
    target: TargetFile,
    code_defs: list[CodeDef],
    template_path: Path,
    batch_size: int,
    context_rows: int,
    reprocess: bool,
) -> None:
    if template_path.exists():
        print(template_path.read_text(encoding="utf-8").rstrip())
        print()
    fields, rows = read_csv_dicts(target.path)
    expected = [code.code_column for code in code_defs]
    missing_columns = tuple(col for col in expected if col not in fields)
    target_indices = [
        i for i, row in enumerate(rows)
        if row_needs_coding(row, expected, missing_columns, reprocess=reprocess)
    ][:batch_size]

    print("CODEBOOK")
    print("code_column,primary_code_id,primary_code,secondary_code_id,secondary_code,definition,examples_from_text")
    for code in code_defs:
        output = io.StringIO()
        writer = csv.writer(output)
        writer.writerow([
            code.code_column,
            code.primary_code_id,
            code.primary_code,
            code.secondary_code_id,
            code.secondary_code,
            code.definition,
            code.examples_from_text,
        ])
        print(output.getvalue().strip())

    print()
    print("TRANSCRIPT ROWS TO CODE")
    for index in target_indices:
        start = max(0, index - context_rows)
        print(f"- row_id: {row_id_for(target.path, rows[index], index)}")
        print(f"  row_number: {index + 1}")
        print("  previous_context_rows:")
        if start == index:
            print("    []")
        else:
            for context_index in range(start, index):
                print("    -")
                for line in format_prompt_row(target.path, rows[context_index], context_index):
                    print(f"      {line[2:] if line.startswith('- ') else line}")
        print("  current_row:")
        for line in format_prompt_row(target.path, rows[index], index):
            print(f"    {line[2:] if line.startswith('- ') else line}")


def apply_patch_to_file(
    target: Path,
    code_defs: list[CodeDef],
    patch_path: Path,
    reprocess: bool,
    smoke_copy: bool,
) -> TargetFile:
    expected = [code.code_column for code in code_defs]
    original_fields, rows = read_csv_dicts(target)
    fields = list(original_fields)
    for col in expected:
        if col not in fields:
            fields.append(col)
            for row in rows:
                row[col] = ""
    original_non_code = [{key: row.get(key, "") for key in original_fields if not key.startswith("code_")} for row in rows]
    row_ids = {row_id_for(target, row, i): i for i, row in enumerate(rows)}
    patch_fields, patch_rows = parse_patch(patch_path)
    patch_codes = patch_fields[1:]
    missing_patch_cols = [col for col in expected if col not in patch_codes]
    extra_patch_cols = [col for col in patch_codes if col not in expected]
    if missing_patch_cols or extra_patch_cols:
        raise SystemExit(
            "Patch columns do not match codebook. "
            f"Missing: {missing_patch_cols or 'none'}; extra: {extra_patch_cols or 'none'}"
        )
    for patch_row in patch_rows:
        rid = squish(patch_row.get("row_id", ""))
        if rid not in row_ids:
            raise SystemExit(f"Patch row_id not found in target file {target}: {rid}")
        row = rows[row_ids[rid]]
        for col in expected:
            value = squish(patch_row.get(col, ""))
            if value not in {"0", "1"}:
                raise SystemExit(f"Invalid patch value for {rid}/{col}: {value!r}")
            if reprocess or is_missing(row.get(col, "")):
                row[col] = value
    for child, parent in hierarchy_pairs(code_defs):
        for row in rows:
            if squish(row.get(child, "")) == "1":
                row[parent] = "1"
    for row in rows:
        for col in expected:
            value = squish(row.get(col, ""))
            if value not in {"0", "1"}:
                raise SystemExit(f"Validation failed: {target} has non-binary or missing value in {col}.")
    after_non_code = [{key: row.get(key, "") for key in original_fields if not key.startswith("code_")} for row in rows]
    if original_non_code != after_non_code:
        raise SystemExit("Validation failed: non-code columns changed in memory.")
    output_path = target.with_name(f"{target.stem}_coded{target.suffix}") if smoke_copy else target
    write_csv_dicts(output_path, fields, rows)
    return inspect_target(output_path, expected)


def print_target_report(targets: list[TargetFile], codebook_selector: str, codebook_path: Path, dry_run: bool) -> None:
    print(f"codebook_selector: {codebook_selector}")
    print(f"codebook_path: {codebook_path}")
    print(f"dry_run: {str(dry_run).lower()}")
    print(f"selected_files: {len(targets)}")
    for target in targets:
        print(f"- csv_path: {target.path}")
        print(f"  row_count: {target.row_count}")
        print(f"  pending_rows: {target.pending_rows}")
        print(f"  missing_code_columns_created_if_run: {len(target.missing_code_columns)}")
        if target.extra_code_columns:
            print(f"  transcript_code_columns_not_in_codebook: {', '.join(target.extra_code_columns)}")


def main() -> int:
    parser = argparse.ArgumentParser(description="Apply qualitative coding CSV patches to prepared transcript CSVs.")
    parser.add_argument("--talent-query", default="Avaritia")
    parser.add_argument("--coding-folder", default="monetary conversation codes")
    parser.add_argument("--codebook", default="current")
    parser.add_argument("--transcript", default="")
    parser.add_argument("--limit", type=int, default=0)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--reprocess", action="store_true")
    parser.add_argument("--patch-file", type=Path)
    parser.add_argument("--smoke-copy", action="store_true", help="Write *_coded.csv instead of overwriting the prepared CSV.")
    parser.add_argument("--emit-prompt", action="store_true", help="Print one coding prompt batch for the selected transcript instead of writing CSVs.")
    parser.add_argument("--batch-size", type=int, default=25)
    parser.add_argument("--context-rows", type=int, default=4)
    parser.add_argument(
        "--prompt-template",
        type=Path,
        default=REPO_ROOT / "prompts" / "qualitative_coding" / "monetary_conversation" / "qualitative_coding_prompt.md",
    )
    args = parser.parse_args()

    talent_root = default_talent_root()
    codebook_path = resolve_codebook(args.codebook, talent_root)
    code_defs = load_codebook(codebook_path)
    code_columns = [code.code_column for code in code_defs]
    talents = resolve_talent_paths(args.talent_query, talent_root, coding_folder=args.coding_folder)
    transcript_files = filter_transcripts(list_prepared_transcript_files(talents), args.transcript or None, talent_root)
    inspected = [inspect_target(path, code_columns) for path in transcript_files]
    pending = [target for target in inspected if args.reprocess or target.pending_rows > 0]
    if args.limit and args.limit > 0:
        pending = pending[: args.limit]

    if args.dry_run:
        print_target_report(pending, args.codebook, codebook_path, dry_run=True)
        return 0
    if args.emit_prompt:
        if len(pending) != 1:
            raise SystemExit("Prompt emission expects exactly one selected pending CSV. Use --limit 1 or --transcript.")
        emit_prompt(pending[0], code_defs, args.prompt_template, args.batch_size, args.context_rows, args.reprocess)
        return 0
    if not pending:
        print_target_report([], args.codebook, codebook_path, dry_run=False)
        return 0
    if not args.patch_file:
        raise SystemExit("--patch-file is required unless --dry-run is set.")
    if len(pending) != 1:
        raise SystemExit("Patch application currently expects exactly one selected pending CSV. Use --limit 1 or --transcript.")
    updated = apply_patch_to_file(pending[0].path, code_defs, args.patch_file, args.reprocess, args.smoke_copy)
    print_target_report([updated], args.codebook, codebook_path, dry_run=False)
    return 0


if __name__ == "__main__":
    sys.exit(main())
