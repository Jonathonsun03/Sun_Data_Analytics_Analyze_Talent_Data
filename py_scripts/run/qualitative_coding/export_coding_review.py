#!/usr/bin/env python3
from __future__ import annotations

import argparse
import sys
from pathlib import Path

SCRIPT_REPO_ROOT = Path(__file__).resolve().parents[3]
if str(SCRIPT_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(SCRIPT_REPO_ROOT))

from py_scripts.lib.qualitative_coding.csv_io import write_csv_dicts
from py_scripts.lib.qualitative_coding.review import build_review_from_paths


def main() -> int:
    parser = argparse.ArgumentParser(description="Join qualitative coding model output with source text for review.")
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--response-json", type=Path, default=None)
    parser.add_argument("--raw-responses-jsonl", type=Path, default=None)
    parser.add_argument("--manifest", type=Path, default=None)
    parser.add_argument("--output", type=Path, default=None)
    args = parser.parse_args()

    response_path = args.response_json or args.run_dir / "sync_test_response_text.json"
    raw_response_path = args.raw_responses_jsonl
    manifest_path = args.manifest or args.run_dir / "manifest.json"
    output_path = args.output or args.run_dir / "coding_review.csv"
    candidate_rows_path = args.run_dir / "candidate_rows.csv"
    compiled_codebook_path = args.run_dir / "compiled_codebook.json"

    required_paths = [candidate_rows_path, compiled_codebook_path]
    if raw_response_path is None:
        required_paths.append(response_path)
    else:
        required_paths.append(raw_response_path)
    for path in required_paths:
        if not path.exists():
            raise SystemExit(f"Required file not found: {path}")

    fieldnames, rows = build_review_from_paths(
        candidate_rows_path,
        response_path,
        compiled_codebook_path,
        manifest_path=manifest_path,
        raw_response_jsonl_path=raw_response_path,
    )
    write_csv_dicts(output_path, fieldnames, rows)
    coded = sum(1 for row in rows if row.get("response_status") == "coded")
    missing = sum(1 for row in rows if row.get("response_status") == "missing_response")
    unknown = sum(1 for row in rows if row.get("response_status") == "unknown_code")
    positives = sum(1 for row in rows if row.get("positive_code_count") not in {"", "0"})
    duplicates = sum(1 for row in rows if row.get("response_duplicate_status", "").startswith("duplicate"))

    print(f"review_csv: {output_path}")
    print(f"rows: {len(rows)}")
    print(f"coded_rows: {coded}")
    print(f"missing_response_rows: {missing}")
    print(f"unknown_code_rows: {unknown}")
    print(f"positive_coded_rows: {positives}")
    print(f"duplicate_response_rows: {duplicates}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
