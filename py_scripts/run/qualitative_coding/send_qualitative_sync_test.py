#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import sys
import urllib.error
import urllib.request
from pathlib import Path


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


def iter_requests(batch_input_path: Path) -> list[dict[str, object]]:
    requests: list[dict[str, object]] = []
    with batch_input_path.open("r", encoding="utf-8") as handle:
        for line in handle:
            if not line.strip():
                continue
            requests.append(json.loads(line))
    return requests


def select_requests(batch_input_path: Path, custom_id: str | None, send_all: bool, max_requests: int) -> list[dict[str, object]]:
    requests = iter_requests(batch_input_path)
    if custom_id:
        selected = [request for request in requests if request.get("custom_id") == custom_id]
        if not selected:
            raise SystemExit(f"custom_id not found in {batch_input_path}: {custom_id}")
    elif send_all:
        selected = requests
    else:
        selected = requests[:1]
    if max_requests > 0:
        selected = selected[:max_requests]
    if not selected:
        raise SystemExit(f"No requests found in {batch_input_path}")
    return selected


def validate_request(request: dict[str, object]) -> tuple[str, str, dict[str, object]]:
    custom_id = str(request["custom_id"])
    method = request.get("method")
    endpoint = str(request.get("url", ""))
    body = request.get("body")
    if method != "POST" or endpoint != "/v1/responses" or not isinstance(body, dict):
        raise SystemExit(f"Sync test only supports POST /v1/responses requests. Found {method} {endpoint}.")
    return custom_id, endpoint, body


def extract_response_text(response: dict[str, object]) -> str:
    texts: list[str] = []
    for item in response.get("output", []) or []:
        if not isinstance(item, dict):
            continue
        for content in item.get("content", []) or []:
            if isinstance(content, dict) and content.get("type") in {"output_text", "text"}:
                text = content.get("text")
                if isinstance(text, str):
                    texts.append(text)
    if texts:
        return "\n".join(texts).strip()
    output_text = response.get("output_text")
    return output_text if isinstance(output_text, str) else ""


def post_response(path: str, body: dict[str, object], api_key: str) -> dict[str, object]:
    request = urllib.request.Request(
        API_BASE + path,
        data=json.dumps(body).encode("utf-8"),
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=180) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise SystemExit(f"OpenAI API request failed with HTTP {exc.code}:\n{detail}") from exc
    except urllib.error.URLError as exc:
        raise SystemExit(f"OpenAI API request failed: {exc}") from exc


def main() -> int:
    parser = argparse.ArgumentParser(description="Send one generated qualitative coding request synchronously for review.")
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--custom-id", default="")
    parser.add_argument("--all", action="store_true", help="Send every request in batch_input.jsonl.")
    parser.add_argument("--max-requests", type=int, default=0, help="Optional cap on sent requests for testing.")
    parser.add_argument("--execute", action="store_true", help="Actually call the OpenAI API and write response files.")
    parser.add_argument("--output-prefix", default="sync_test")
    parser.add_argument("--env-file", type=Path, default=Path(".env"), help="Path to a dotenv file containing OPENAI_API_KEY.")
    args = parser.parse_args()

    load_dotenv(args.env_file)

    batch_input_path = args.run_dir / "batch_input.jsonl"
    if not batch_input_path.exists():
        raise SystemExit(f"Missing batch input file: {batch_input_path}")

    selected_requests = select_requests(batch_input_path, args.custom_id.strip() or None, args.all, args.max_requests)
    first_custom_id, first_endpoint, first_body = validate_request(selected_requests[0])

    print(f"run_dir: {args.run_dir}")
    print(f"request_count: {len(selected_requests)}")
    print(f"first_custom_id: {first_custom_id}")
    print(f"endpoint: {first_endpoint}")
    print(f"model: {first_body.get('model')}")
    if not args.execute:
        print("execute: false")
        print("No API request sent. Re-run with --execute to send selected request(s).")
        return 0

    api_key = os.environ.get("OPENAI_API_KEY", "").strip()
    if not api_key:
        raise SystemExit("OPENAI_API_KEY is not set. Export it before running with --execute.")

    raw_path = args.run_dir / f"{args.output_prefix}_responses.jsonl"
    text_path = args.run_dir / f"{args.output_prefix}_response_text.json"
    combined = {
        "coded_rows": [],
        "batch_notes": None,
        "request_results": [],
    }
    with raw_path.open("w", encoding="utf-8") as raw_handle:
        for index, request in enumerate(selected_requests, start=1):
            custom_id, endpoint, body = validate_request(request)
            print(f"sending_request: {index}/{len(selected_requests)} {custom_id}", flush=True)
            response = post_response(endpoint, body, api_key)
            raw_handle.write(json.dumps({"custom_id": custom_id, "response": response}, ensure_ascii=False) + "\n")
            response_text = extract_response_text(response)
            try:
                parsed = json.loads(response_text)
                coded_rows = parsed.get("coded_rows", []) if isinstance(parsed, dict) else []
                if isinstance(coded_rows, list):
                    combined["coded_rows"].extend(coded_rows)
                combined["request_results"].append(
                    {
                        "custom_id": custom_id,
                        "status": "parsed",
                        "coded_row_count": len(coded_rows) if isinstance(coded_rows, list) else 0,
                    }
                )
            except json.JSONDecodeError:
                combined["request_results"].append(
                    {
                        "custom_id": custom_id,
                        "status": "parse_error",
                        "response_text": response_text,
                    }
                )
    text_path.write_text(json.dumps(combined, indent=2, sort_keys=True) + "\n", encoding="utf-8")

    print("execute: true")
    print(f"responses_jsonl: {raw_path}")
    print(f"response_text_json: {text_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
