#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import shutil
import sys
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

SCRIPT_REPO_ROOT = Path(__file__).resolve().parents[3]
if str(SCRIPT_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(SCRIPT_REPO_ROOT))

from py_scripts.lib.qualitative_coding.csv_io import read_csv_dicts, write_csv_dicts
from py_scripts.lib.qualitative_coding.prepared_transcripts import row_id_for
from py_scripts.lib.qualitative_coding.review import (
    code_columns_from_compiled_codebook,
    load_compiled_codebook,
)
from py_scripts.lib.qualitative_coding.text import squish


def write_json(path: Path, data: object) -> None:
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def load_json_object(path: Path) -> dict[str, object]:
    if not path.exists():
        return {}
    data = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        raise SystemExit(f"Expected JSON object: {path}")
    return data


def positive_codes(row: dict[str, str], code_columns: list[str]) -> list[str]:
    return [column.removeprefix("code_") for column in code_columns if squish(row.get(column, "")) == "1"]


def safe_backup_name(path: Path, index: int) -> str:
    return f"{index:03d}_{path.name}.before_apply.csv"


def codebook_hierarchy_pairs(compiled_codebook: list[dict[str, str]]) -> list[tuple[str, str]]:
    primary_columns = {
        f"code_{squish(item.get('primary_code_id', ''))}"
        for item in compiled_codebook
        if squish(item.get("primary_code_id", ""))
    }
    pairs: list[tuple[str, str]] = []
    for item in compiled_codebook:
        secondary_id = squish(item.get("secondary_code_id", ""))
        primary_id = squish(item.get("primary_code_id", ""))
        child = squish(item.get("code_column", ""))
        parent = f"code_{primary_id}" if primary_id else ""
        if secondary_id and child and parent in primary_columns:
            pairs.append((child, parent))
    return pairs


def validate_review_rows(
    review_rows: list[dict[str, str]],
    code_columns: list[str],
    allow_chat_codes: bool,
    zero_chat_codes: bool,
) -> tuple[list[str], dict[str, int]]:
    problems: list[str] = []
    counts = {
        "rows": len(review_rows),
        "missing_response_rows": 0,
        "unknown_code_rows": 0,
        "validation_error_rows": 0,
        "invalid_binary_rows": 0,
        "chat_positive_rows": 0,
        "missing_identity_rows": 0,
    }
    for i, row in enumerate(review_rows, start=2):
        row_id = squish(row.get("row_id", ""))
        if not squish(row.get("csv_path", "")) or not row_id:
            counts["missing_identity_rows"] += 1
            problems.append(f"line {i}: missing csv_path or row_id")
        status = squish(row.get("response_status", ""))
        if status != "coded":
            if status == "missing_response":
                counts["missing_response_rows"] += 1
            elif status == "unknown_code":
                counts["unknown_code_rows"] += 1
            problems.append(f"line {i}: response_status={status or '<blank>'}")
        if squish(row.get("validation_error", "")):
            counts["validation_error_rows"] += 1
            problems.append(f"line {i}: validation_error={squish(row.get('validation_error', ''))}")
        for column in code_columns:
            value = squish(row.get(column, ""))
            if value not in {"0", "1"}:
                counts["invalid_binary_rows"] += 1
                problems.append(f"line {i}: invalid value for {column}: {value!r}")
                break
        if squish(row.get("source", "")).lower() == "chat" and positive_codes(row, code_columns):
            counts["chat_positive_rows"] += 1
            if not allow_chat_codes and not zero_chat_codes:
                problems.append(f"line {i}: chat row has positive codes: {row_id}")
    return problems, counts


def build_patch_preview(
    review_rows: list[dict[str, str]],
    code_columns: list[str],
    hierarchy_pairs: list[tuple[str, str]],
    zero_chat_codes: bool,
) -> tuple[list[str], list[dict[str, str]], dict[str, list[dict[str, str]]]]:
    grouped: dict[str, list[dict[str, str]]] = defaultdict(list)
    preview_rows: list[dict[str, str]] = []
    preview_fields = [
        "csv_path",
        "row_number",
        "row_id",
        "source",
        "speaker",
        "old_positive_codes",
        "new_positive_codes",
        "action",
        "text",
    ]
    for row in review_rows:
        patch_row = dict(row)
        if zero_chat_codes and squish(patch_row.get("source", "")).lower() == "chat":
            for column in code_columns:
                patch_row[column] = "0"
            patch_row["positive_codes"] = ""
            patch_row["positive_code_count"] = "0"
        for child, parent in hierarchy_pairs:
            if squish(patch_row.get(child, "")) == "1":
                patch_row[parent] = "1"
        grouped[squish(patch_row.get("csv_path", ""))].append(patch_row)
    for rows in grouped.values():
        rows.sort(key=lambda item: int(squish(item.get("row_number", "0")) or "0"))
    return preview_fields, preview_rows, grouped


def apply_group(
    csv_path: Path,
    patch_rows: list[dict[str, str]],
    code_columns: list[str],
    preview_rows: list[dict[str, str]],
) -> tuple[list[str], list[dict[str, str]], int]:
    fields, rows = read_csv_dicts(csv_path)
    output_fields = list(fields)
    for column in code_columns:
        if column not in output_fields:
            output_fields.append(column)
            for row in rows:
                row[column] = ""

    by_row_id = {row_id_for(csv_path, row, index): index for index, row in enumerate(rows)}
    changed = 0
    for patch_row in patch_rows:
        row_id = squish(patch_row.get("row_id", ""))
        if row_id not in by_row_id:
            raise SystemExit(f"Patch row_id not found in target CSV: {csv_path} :: {row_id}")
        row = rows[by_row_id[row_id]]
        old_codes = positive_codes(row, code_columns)
        new_codes = positive_codes(patch_row, code_columns)
        changed_any = False
        for column in code_columns:
            new_value = squish(patch_row.get(column, ""))
            old_value = squish(row.get(column, ""))
            if old_value != new_value:
                changed_any = True
            row[column] = new_value
        if changed_any:
            changed += 1
        preview_rows.append(
            {
                "csv_path": str(csv_path),
                "row_number": squish(patch_row.get("row_number", "")),
                "row_id": row_id,
                "source": squish(patch_row.get("source", "")),
                "speaker": squish(patch_row.get("speaker", "")),
                "old_positive_codes": ";".join(old_codes),
                "new_positive_codes": ";".join(new_codes),
                "action": "update" if changed_any else "no_change",
                "text": squish(patch_row.get("text", "")),
            }
        )
    return output_fields, rows, changed


def main() -> int:
    parser = argparse.ArgumentParser(description="Apply a qualitative Batch API review CSV back to talent-library coded datasets.")
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--review-csv", type=Path)
    parser.add_argument("--compiled-codebook", type=Path)
    parser.add_argument("--execute", action="store_true", help="Actually write codes back to prepared talent-library CSVs.")
    parser.add_argument("--allow-chat-codes", action="store_true", help="Permit positive codes on source=chat rows.")
    parser.add_argument("--zero-chat-codes", action="store_true", help="Force source=chat rows to all zero code values before applying.")
    args = parser.parse_args()

    run_dir = args.run_dir.resolve()
    review_csv = (args.review_csv or run_dir / "coding_review.csv").resolve()
    compiled_codebook_path = (args.compiled_codebook or run_dir / "compiled_codebook.json").resolve()
    manifest_path = run_dir / "manifest.json"
    preview_path = run_dir / "apply_patch_preview.csv"
    log_path = run_dir / "applied_patch_log.csv"
    summary_path = run_dir / "apply_summary.json"
    backup_dir = run_dir / "backups"

    if not review_csv.exists():
        raise SystemExit(f"Review CSV not found: {review_csv}")
    if not compiled_codebook_path.exists():
        raise SystemExit(f"Compiled codebook not found: {compiled_codebook_path}")

    compiled_codebook = load_compiled_codebook(compiled_codebook_path)
    code_columns = code_columns_from_compiled_codebook(compiled_codebook)
    hierarchy = codebook_hierarchy_pairs(compiled_codebook)
    _, review_rows = read_csv_dicts(review_csv)
    problems, validation_counts = validate_review_rows(
        review_rows,
        code_columns,
        allow_chat_codes=args.allow_chat_codes,
        zero_chat_codes=args.zero_chat_codes,
    )
    preview_fields, preview_rows, grouped = build_patch_preview(
        review_rows,
        code_columns,
        hierarchy,
        zero_chat_codes=args.zero_chat_codes,
    )

    summary: dict[str, object] = {
        "run_dir": str(run_dir),
        "review_csv": str(review_csv),
        "compiled_codebook": str(compiled_codebook_path),
        "execute": args.execute,
        "zero_chat_codes": args.zero_chat_codes,
        "allow_chat_codes": args.allow_chat_codes,
        "validation_counts": validation_counts,
        "target_csv_count": len(grouped),
        "target_row_count": len(review_rows),
        "applied_at": None,
        "apply_status": "dry_run_ready",
        "problem_count": len(problems),
        "problems": problems[:100],
        "artifacts": {
            "preview_csv": str(preview_path),
            "applied_patch_log_csv": str(log_path),
            "backup_dir": str(backup_dir),
            "summary_json": str(summary_path),
        },
    }

    if problems:
        summary["apply_status"] = "blocked_validation_issues"
        write_csv_dicts(preview_path, preview_fields, preview_rows)
        write_json(summary_path, summary)
        print(f"apply_status: {summary['apply_status']}")
        print(f"problem_count: {len(problems)}")
        print(f"summary: {summary_path}")
        print(f"preview_csv: {preview_path}")
        return 1

    for csv_path_text, patch_rows in grouped.items():
        if not csv_path_text:
            raise SystemExit("Review CSV contains a blank csv_path.")
        csv_path = Path(csv_path_text)
        if not csv_path.exists():
            raise SystemExit(f"Target transcript CSV not found: {csv_path}")
        output_fields, output_rows, changed = apply_group(csv_path, patch_rows, code_columns, preview_rows)
        summary.setdefault("targets", []).append(
            {
                "csv_path": str(csv_path),
                "patch_rows": len(patch_rows),
                "changed_rows": changed,
            }
        )
        if args.execute:
            backup_dir.mkdir(parents=True, exist_ok=True)
            backup_path = backup_dir / safe_backup_name(csv_path, len(summary["targets"]))
            shutil.copy2(csv_path, backup_path)
            write_csv_dicts(csv_path, output_fields, output_rows)

    write_csv_dicts(preview_path, preview_fields, preview_rows)
    if args.execute:
        write_csv_dicts(log_path, preview_fields, preview_rows)
        summary["apply_status"] = "applied"
        summary["applied_at"] = datetime.now(ZoneInfo("America/New_York")).isoformat()
    else:
        summary["apply_status"] = "dry_run_ready"

    write_json(summary_path, summary)
    manifest = load_json_object(manifest_path)
    if manifest:
        manifest["apply_status"] = summary["apply_status"]
        manifest["apply_summary"] = str(summary_path)
        manifest.setdefault("artifacts", {})["apply_patch_preview_csv"] = str(preview_path)
        if args.execute:
            manifest["applied_at"] = summary["applied_at"]
            manifest["artifacts"]["applied_patch_log_csv"] = str(log_path)
            manifest["artifacts"]["backup_dir"] = str(backup_dir)
        write_json(manifest_path, manifest)

    print(f"apply_status: {summary['apply_status']}")
    print(f"target_csv_count: {summary['target_csv_count']}")
    print(f"target_row_count: {summary['target_row_count']}")
    print(f"preview_csv: {preview_path}")
    if args.execute:
        print(f"applied_patch_log: {log_path}")
        print(f"backup_dir: {backup_dir}")
    else:
        print("execute: false")
    print(f"summary: {summary_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
