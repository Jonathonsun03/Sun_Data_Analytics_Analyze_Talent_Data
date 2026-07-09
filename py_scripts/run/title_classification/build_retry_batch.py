#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import shutil
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo


def read_json(path: Path) -> dict[str, object]:
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: Path, data: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def read_jsonl(path: Path) -> list[dict[str, object]]:
    if not path.exists():
        return []
    rows: list[dict[str, object]] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.strip():
            rows.append(json.loads(line))
    return rows


def write_jsonl(path: Path, rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as handle:
        for row in rows:
            handle.write(json.dumps(row, separators=(",", ":"), ensure_ascii=False) + "\n")


def custom_ids_from_error_jsonl(path: Path) -> set[str]:
    ids: set[str] = set()
    for row in read_jsonl(path):
        custom_id = row.get("custom_id")
        if isinstance(custom_id, str) and custom_id:
            ids.add(custom_id)
    return ids


def custom_ids_from_apply_failures(path: Path) -> set[str]:
    if not path.exists():
        return set()
    summary = read_json(path)
    failed = summary.get("failed")
    if not isinstance(failed, list):
        return set()
    ids: set[str] = set()
    for row in failed:
        if isinstance(row, dict) and isinstance(row.get("custom_id"), str):
            ids.add(str(row["custom_id"]))
    return ids


def output_custom_ids(path: Path) -> set[str]:
    return {
        str(row["custom_id"])
        for row in read_jsonl(path)
        if isinstance(row.get("custom_id"), str) and row.get("custom_id")
    }


def request_rows_by_custom_id(path: Path) -> dict[str, dict[str, object]]:
    rows = {}
    for row in read_jsonl(path):
        custom_id = row.get("custom_id")
        if isinstance(custom_id, str) and custom_id:
            rows[custom_id] = row
    return rows


def write_candidate_subset(source_path: Path, target_path: Path, video_ids: set[str]) -> None:
    if not source_path.exists():
        raise SystemExit(f"Missing candidate rows CSV: {source_path}")
    with source_path.open("r", encoding="utf-8", newline="") as src:
        reader = csv.DictReader(src)
        fieldnames = reader.fieldnames or []
        rows = [row for row in reader if row.get("video_id") in video_ids]

    target_path.parent.mkdir(parents=True, exist_ok=True)
    with target_path.open("w", encoding="utf-8", newline="") as dst:
        writer = csv.DictWriter(dst, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def now_stamp() -> str:
    return datetime.now(ZoneInfo("America/New_York")).strftime("%Y%m%d_%H%M%S")


def main() -> int:
    parser = argparse.ArgumentParser(description="Create a retry OpenAI Batch run for failed or missing title-classification requests.")
    parser.add_argument("--source-run-dir", type=Path, required=True)
    parser.add_argument("--run-root", type=Path, required=True)
    parser.add_argument("--print-run-dir", action="store_true")
    args = parser.parse_args()

    source_run_dir = args.source_run_dir
    manifest_path = source_run_dir / "manifest.json"
    if not manifest_path.exists():
        raise SystemExit(f"Missing manifest: {manifest_path}")
    manifest = read_json(manifest_path)
    artifacts = manifest.get("artifacts") if isinstance(manifest.get("artifacts"), dict) else {}

    custom_id_map = manifest.get("custom_id_map")
    if not isinstance(custom_id_map, dict):
        raise SystemExit("Manifest custom_id_map is missing or invalid.")
    all_ids = set(custom_id_map)

    output_path = Path(str(artifacts.get("batch_output_jsonl") or source_run_dir / "batch_output.jsonl"))
    error_path = Path(str(artifacts.get("batch_errors_jsonl") or source_run_dir / "batch_errors.jsonl"))
    input_path = Path(str(artifacts.get("batch_input_jsonl") or source_run_dir / "batch_input.jsonl"))
    candidate_path = Path(str(artifacts.get("candidate_rows_csv") or source_run_dir / "candidate_rows.csv"))

    seen_in_output = output_custom_ids(output_path)
    retry_ids = set()
    retry_ids.update(custom_ids_from_apply_failures(source_run_dir / "apply_summary.json"))
    retry_ids.update(custom_ids_from_error_jsonl(error_path))
    retry_ids.update(all_ids - seen_in_output - custom_ids_from_error_jsonl(error_path))
    retry_ids &= all_ids

    summary_path = source_run_dir / "retry_summary.json"
    retry_summary = {
        "source_run_dir": str(source_run_dir),
        "created_at": datetime.now(ZoneInfo("America/New_York")).isoformat(),
        "total_custom_ids": len(all_ids),
        "output_custom_ids": len(seen_in_output),
        "retry_custom_ids": sorted(retry_ids),
        "retry_count": len(retry_ids),
    }

    if not retry_ids:
        write_json(summary_path, retry_summary)
        if args.print_run_dir:
            print("")
        else:
            print("No retry requests needed.")
        return 0

    input_rows = request_rows_by_custom_id(input_path)
    missing_input_rows = retry_ids - set(input_rows)
    if missing_input_rows:
        raise SystemExit("Missing original input rows for retry custom_ids: " + ", ".join(sorted(missing_input_rows)))

    retry_run_id = f"{manifest.get('run_id', source_run_dir.name)}_retry_{now_stamp()}"
    retry_run_dir = args.run_root / retry_run_id
    if retry_run_dir.exists():
        raise SystemExit(f"Retry run directory already exists: {retry_run_dir}")
    (retry_run_dir / "logs").mkdir(parents=True, exist_ok=False)

    retry_input_path = retry_run_dir / "batch_input.jsonl"
    retry_candidate_path = retry_run_dir / "candidate_rows.csv"
    retry_manifest_path = retry_run_dir / "manifest.json"

    retry_input_rows = [input_rows[custom_id] for custom_id in sorted(retry_ids)]
    write_jsonl(retry_input_path, retry_input_rows)

    retry_custom_id_map = {custom_id: custom_id_map[custom_id] for custom_id in sorted(retry_ids)}
    retry_video_ids: set[str] = set()
    for mapping in retry_custom_id_map.values():
        if isinstance(mapping, dict) and isinstance(mapping.get("video_ids"), list):
            retry_video_ids.update(str(video_id) for video_id in mapping["video_ids"])
    write_candidate_subset(candidate_path, retry_candidate_path, retry_video_ids)

    retry_manifest = dict(manifest)
    retry_manifest.update(
        {
            "run_id": retry_run_id,
            "created_at": datetime.now(ZoneInfo("America/New_York")).isoformat(),
            "retry_of": str(source_run_dir),
            "request_count": len(retry_input_rows),
            "pending_rows": len(retry_video_ids),
            "custom_id_map": retry_custom_id_map,
            "batch_id": None,
            "batch_status": None,
            "input_file_id": None,
            "output_file_id": None,
            "error_file_id": None,
            "submitted_at": None,
            "submission_response": None,
            "last_checked_at": None,
            "last_batch_response": None,
            "artifacts": {
                **artifacts,
                "batch_input_jsonl": str(retry_input_path),
                "candidate_rows_csv": str(retry_candidate_path),
            },
        }
    )
    write_json(retry_manifest_path, retry_manifest)
    retry_summary["retry_run_dir"] = str(retry_run_dir)
    retry_summary["retry_manifest"] = str(retry_manifest_path)
    write_json(summary_path, retry_summary)
    shutil.copy2(summary_path, retry_run_dir / "retry_source_summary.json")

    if args.print_run_dir:
        print(retry_run_dir)
    else:
        print(f"Retry run directory: {retry_run_dir}")
        print(f"Retry requests: {len(retry_input_rows)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
