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


def api_request(method: str, path: str, api_key: str, *, body: object | None = None, headers: dict[str, str] | None = None) -> dict[str, object]:
    request_headers = {"Authorization": f"Bearer {api_key}"}
    if headers:
        request_headers.update(headers)
    data = None if body is None else json.dumps(body).encode("utf-8")
    if data is not None and "Content-Type" not in request_headers:
        request_headers["Content-Type"] = "application/json"
    request = urllib.request.Request(API_BASE + path, data=data, headers=request_headers, method=method)
    try:
        with urllib.request.urlopen(request, timeout=180) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise SystemExit(f"OpenAI API request failed with HTTP {exc.code}:\n{detail}") from exc
    except urllib.error.URLError as exc:
        raise SystemExit(f"OpenAI API request failed: {exc}") from exc


def upload_file(path: Path, api_key: str) -> dict[str, object]:
    boundary = "----sun-data-qualitative-batch-boundary"
    file_bytes = path.read_bytes()
    body = b"".join(
        [
            f"--{boundary}\r\n".encode(),
            b'Content-Disposition: form-data; name="purpose"\r\n\r\n',
            b"batch\r\n",
            f"--{boundary}\r\n".encode(),
            f'Content-Disposition: form-data; name="file"; filename="{path.name}"\r\n'.encode(),
            b"Content-Type: application/jsonl\r\n\r\n",
            file_bytes,
            b"\r\n",
            f"--{boundary}--\r\n".encode(),
        ]
    )
    request = urllib.request.Request(
        API_BASE + "/v1/files",
        data=body,
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": f"multipart/form-data; boundary={boundary}",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=180) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise SystemExit(f"OpenAI file upload failed with HTTP {exc.code}:\n{detail}") from exc
    except urllib.error.URLError as exc:
        raise SystemExit(f"OpenAI file upload failed: {exc}") from exc


def main() -> int:
    parser = argparse.ArgumentParser(description="Submit a qualitative coding JSONL run to the OpenAI Batch API.")
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--execute", action="store_true", help="Actually upload and submit the batch.")
    parser.add_argument("--env-file", type=Path, default=Path(".env"))
    parser.add_argument("--completion-window", default="24h")
    args = parser.parse_args()

    load_dotenv(args.env_file)
    manifest_path = args.run_dir / "manifest.json"
    if not manifest_path.exists():
        raise SystemExit(f"Missing manifest: {manifest_path}")
    manifest = read_json(manifest_path)
    batch_input_path = Path(str(manifest["artifacts"]["batch_input_jsonl"]))
    if not batch_input_path.exists():
        raise SystemExit(f"Missing batch input JSONL: {batch_input_path}")

    print(f"run_dir: {args.run_dir}")
    print(f"batch_input_jsonl: {batch_input_path}")
    print(f"request_count: {manifest.get('request_count')}")
    print(f"endpoint: {manifest.get('endpoint')}")
    print(f"completion_window: {args.completion_window}")
    if not args.execute:
        print("execute: false")
        print("No API batch submitted. Re-run with --execute to upload and submit.")
        return 0

    api_key = os.environ.get("OPENAI_API_KEY", "").strip()
    if not api_key:
        raise SystemExit("OPENAI_API_KEY is not set. Export it before running with --execute.")

    upload = upload_file(batch_input_path, api_key)
    input_file_id = str(upload["id"])
    batch = api_request(
        "POST",
        "/v1/batches",
        api_key,
        body={
            "input_file_id": input_file_id,
            "endpoint": manifest.get("endpoint", "/v1/responses"),
            "completion_window": args.completion_window,
            "metadata": {
                "run_id": str(manifest.get("run_id", args.run_dir.name)),
                "talent_query": str(manifest.get("talent_query", "")),
            },
        },
    )

    manifest["input_file_id"] = input_file_id
    manifest["batch_id"] = batch.get("id")
    manifest["batch_status"] = batch.get("status")
    manifest["submitted_at"] = datetime.now(ZoneInfo("America/New_York")).isoformat()
    manifest["submission_response"] = batch
    write_json(manifest_path, manifest)
    write_json(args.run_dir / "submit_response.json", {"file": upload, "batch": batch})

    print("execute: true")
    print(f"input_file_id: {input_file_id}")
    print(f"batch_id: {batch.get('id')}")
    print(f"batch_status: {batch.get('status')}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
