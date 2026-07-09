#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import shutil
import sys
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

SCRIPT_REPO_ROOT = Path(__file__).resolve().parents[3]
if str(SCRIPT_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(SCRIPT_REPO_ROOT))

from py_scripts.lib.qualitative_coding.csv_io import read_csv_dicts
from py_scripts.run.qualitative_coding.build_qualitative_batch import (
    DEFAULT_BATCH_SIZE,
    DEFAULT_ENDPOINT,
    DEFAULT_MODEL,
    METHODOLOGY_MODE,
    build_request,
    chunked,
    prompt_row,
)


def retry_id() -> str:
    now = datetime.now(ZoneInfo("America/New_York"))
    return f"retry_missing_{now.strftime('%Y-%m-%d_%H-%M-%S_%z')}"


def write_json(path: Path, data: object) -> None:
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def write_csv(path: Path, rows: list[dict[str, object]]) -> None:
    fieldnames = [
        "csv_path",
        "source_file",
        "video_id",
        "row_number",
        "row_id",
        "sec",
        "timecode",
        "source",
        "speaker",
        "speaker_type",
        "message_type",
        "paid_amount_text",
        "paid_amount_value",
        "paid_currency",
        "candidate_reason",
        "text",
    ]
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def selected_missing_review_rows(review_csv: Path, max_row_number: int) -> list[dict[str, str]]:
    _, rows = read_csv_dicts(review_csv)
    selected = []
    for row in rows:
        if row.get("response_status") != "missing_response":
            continue
        row_number = int(row.get("row_number") or "0")
        if max_row_number > 0 and row_number > max_row_number:
            continue
        selected.append(row)
    return selected


def rebuild_candidate(review_row: dict[str, str], source_rows_by_csv: dict[str, list[dict[str, str]]], context_rows: int) -> dict[str, object]:
    csv_path = Path(review_row["csv_path"])
    row_number = int(review_row["row_number"])
    row_index = row_number - 1
    rows = source_rows_by_csv[str(csv_path)]
    row = rows[row_index]
    start = max(0, row_index - context_rows)
    return {
        "csv_path": str(csv_path),
        "source_file": review_row.get("source_file", csv_path.name),
        "video_id": review_row.get("video_id", ""),
        "row_number": row_number,
        "row_id": review_row["row_id"],
        "sec": review_row.get("sec", ""),
        "timecode": review_row.get("timecode", ""),
        "source": review_row.get("source", ""),
        "speaker": review_row.get("speaker", ""),
        "speaker_type": review_row.get("speaker_type", ""),
        "message_type": review_row.get("message_type", ""),
        "paid_amount_text": review_row.get("paid_amount_text", ""),
        "paid_amount_value": review_row.get("paid_amount_value", ""),
        "paid_currency": review_row.get("paid_currency", ""),
        "candidate_reason": "retry_missing_response",
        "text": review_row.get("text", ""),
        "current_row": prompt_row(csv_path, row, row_index),
        "context_rows": [prompt_row(csv_path, rows[i], i) for i in range(start, row_index)],
    }


def main() -> int:
    parser = argparse.ArgumentParser(description="Build a retry payload for rows missing from a qualitative coding response.")
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--review-csv", type=Path, default=None)
    parser.add_argument("--max-row-number", type=int, default=0)
    parser.add_argument("--batch-size", type=int, default=DEFAULT_BATCH_SIZE)
    parser.add_argument("--context-rows", type=int, default=4)
    parser.add_argument("--model", default=DEFAULT_MODEL)
    parser.add_argument("--endpoint", default=DEFAULT_ENDPOINT)
    parser.add_argument("--retry-id", default="")
    args = parser.parse_args()

    review_csv = args.review_csv or args.run_dir / "coding_review_half.csv"
    compiled_codebook_path = args.run_dir / "compiled_codebook.json"
    source_manifest_path = args.run_dir / "manifest.json"
    if not review_csv.exists():
        raise SystemExit(f"Review CSV not found: {review_csv}")
    if not compiled_codebook_path.exists():
        raise SystemExit(f"Compiled codebook not found: {compiled_codebook_path}")

    selected_rows = selected_missing_review_rows(review_csv, args.max_row_number)
    retry_dir = args.run_dir / "retries" / (args.retry_id.strip() or retry_id())
    retry_dir.mkdir(parents=True, exist_ok=False)

    source_rows_by_csv: dict[str, list[dict[str, str]]] = {}
    for csv_path in sorted({row["csv_path"] for row in selected_rows}):
        _, rows = read_csv_dicts(Path(csv_path))
        source_rows_by_csv[csv_path] = rows

    candidates = [rebuild_candidate(row, source_rows_by_csv, args.context_rows) for row in selected_rows]
    compact_codebook = json.loads(compiled_codebook_path.read_text(encoding="utf-8"))
    batch_input_path = retry_dir / "batch_input.jsonl"
    candidate_rows_path = retry_dir / "candidate_rows.csv"
    manifest_path = retry_dir / "manifest.json"
    shutil.copy2(compiled_codebook_path, retry_dir / "compiled_codebook.json")

    custom_id_map = {}
    retry_run_id = retry_dir.name
    with batch_input_path.open("w", encoding="utf-8") as handle:
        for part_number, candidate_batch in enumerate(chunked(candidates, args.batch_size), start=1):
            first = candidate_batch[0]
            last = candidate_batch[-1]
            custom_id = (
                f"retry_missing__{first['video_id']}__rows_"
                f"{int(first['row_number']):06d}_{int(last['row_number']):06d}__{retry_run_id}__part_{part_number:04d}"
            )
            request = build_request(custom_id, args.model, args.endpoint, compact_codebook, candidate_batch)
            handle.write(json.dumps(request, ensure_ascii=False) + "\n")
            custom_id_map[custom_id] = {
                "csv_paths": sorted({str(row["csv_path"]) for row in candidate_batch}),
                "row_ids": [row["row_id"] for row in candidate_batch],
                "row_numbers": [row["row_number"] for row in candidate_batch],
            }

    write_csv(candidate_rows_path, candidates)
    manifest = {
        "retry_id": retry_run_id,
        "source_run_dir": str(args.run_dir),
        "source_manifest": str(source_manifest_path),
        "source_review_csv": str(review_csv),
        "created_at": datetime.now(ZoneInfo("America/New_York")).isoformat(),
        "candidate_reason": "retry_missing_response",
        "max_row_number": args.max_row_number,
        "request_count": len(custom_id_map),
        "candidate_row_count": len(candidates),
        "model": args.model,
        "endpoint": args.endpoint,
        "methodology_config": {
            "mode": METHODOLOGY_MODE,
            "interpret_every_missing_target_row": True,
            "chronological_chunking": True,
            "rows_per_request": args.batch_size,
            "cost_optimization_strategy": "retry only missing responses; no duplicate reuse or auto-zero",
        },
        "dedupe_config": {
            "enabled": False,
            "policy": "do not reuse duplicate text decisions; repeated text may have context-specific meaning",
        },
        "auto_zero_config": {
            "enabled": False,
            "policy": "do not auto-zero missing rows",
        },
        "custom_id_map": custom_id_map,
        "artifacts": {
            "manifest": str(manifest_path),
            "compiled_codebook": str(retry_dir / "compiled_codebook.json"),
            "candidate_rows_csv": str(candidate_rows_path),
            "batch_input_jsonl": str(batch_input_path),
        },
    }
    write_json(manifest_path, manifest)
    print(f"retry_dir: {retry_dir}")
    print(f"request_count: {len(custom_id_map)}")
    print(f"candidate_row_count: {len(candidates)}")
    print("submitted: false")
    return 0


if __name__ == "__main__":
    sys.exit(main())
