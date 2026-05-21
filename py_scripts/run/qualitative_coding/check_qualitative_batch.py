#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import sys
import urllib.error
import urllib.request
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo


API_BASE = "https://api.openai.com"


def load_dotenv(path: Path) -> None:
    if not path.exists():
        return
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        key = key.strip()
        value = value.strip().strip('"').strip("'")
        if key and key not in os.environ:
            os.environ[key] = value


def read_json(path: Path) -> dict[str, object]:
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: Path, data: object) -> None:
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def api_get_json(path: str, api_key: str) -> dict[str, object]:
    request = urllib.request.Request(
        API_BASE + path,
        headers={"Authorization": f"Bearer {api_key}"},
        method="GET",
    )
    try:
        with urllib.request.urlopen(request, timeout=180) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise SystemExit(f"OpenAI API request failed with HTTP {exc.code}:\n{detail}") from exc
    except urllib.error.URLError as exc:
        raise SystemExit(f"OpenAI API request failed: {exc}") from exc


def api_get_bytes(path: str, api_key: str) -> bytes:
    request = urllib.request.Request(
        API_BASE + path,
        headers={"Authorization": f"Bearer {api_key}"},
        method="GET",
    )
    try:
        with urllib.request.urlopen(request, timeout=180) as response:
            return response.read()
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise SystemExit(f"OpenAI file download failed with HTTP {exc.code}:\n{detail}") from exc
    except urllib.error.URLError as exc:
        raise SystemExit(f"OpenAI file download failed: {exc}") from exc


def main() -> int:
    parser = argparse.ArgumentParser(description="Check and optionally retrieve a qualitative OpenAI Batch API run.")
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--retrieve-output", action="store_true", help="Download output/error JSONL files when available.")
    parser.add_argument("--env-file", type=Path, default=Path(".env"))
    args = parser.parse_args()

    load_dotenv(args.env_file)
    api_key = os.environ.get("OPENAI_API_KEY", "").strip()
    if not api_key:
        raise SystemExit("OPENAI_API_KEY is not set.")

    manifest_path = args.run_dir / "manifest.json"
    if not manifest_path.exists():
        raise SystemExit(f"Missing manifest: {manifest_path}")
    manifest = read_json(manifest_path)
    batch_id = str(manifest.get("batch_id") or "")
    if not batch_id:
        raise SystemExit("Manifest has no batch_id. Submit the batch first.")

    batch = api_get_json(f"/v1/batches/{batch_id}", api_key)
    manifest["batch_status"] = batch.get("status")
    manifest["output_file_id"] = batch.get("output_file_id")
    manifest["error_file_id"] = batch.get("error_file_id")
    manifest["last_checked_at"] = datetime.now(ZoneInfo("America/New_York")).isoformat()
    manifest["last_batch_response"] = batch

    if args.retrieve_output:
        output_file_id = batch.get("output_file_id")
        error_file_id = batch.get("error_file_id")
        if output_file_id:
            output_path = args.run_dir / "batch_output.jsonl"
            output_path.write_bytes(api_get_bytes(f"/v1/files/{output_file_id}/content", api_key))
            manifest.setdefault("artifacts", {})["batch_output_jsonl"] = str(output_path)
        if error_file_id:
            error_path = args.run_dir / "batch_errors.jsonl"
            error_path.write_bytes(api_get_bytes(f"/v1/files/{error_file_id}/content", api_key))
            manifest.setdefault("artifacts", {})["batch_errors_jsonl"] = str(error_path)

    write_json(manifest_path, manifest)
    write_json(args.run_dir / "batch_status.json", batch)

    print(f"run_dir: {args.run_dir}")
    print(f"batch_id: {batch_id}")
    print(f"status: {batch.get('status')}")
    print(f"output_file_id: {batch.get('output_file_id')}")
    print(f"error_file_id: {batch.get('error_file_id')}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
