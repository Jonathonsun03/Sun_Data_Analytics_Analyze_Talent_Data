#!/usr/bin/env python3
from __future__ import annotations

import argparse
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


def now_iso() -> str:
    return datetime.now(ZoneInfo("America/New_York")).isoformat()


def state_from_manifest(run_dir: Path, *, status_override: str | None = None, applied_at: str | None = None) -> dict[str, object]:
    manifest_path = run_dir / "manifest.json"
    if not manifest_path.exists():
        raise SystemExit(f"Missing manifest: {manifest_path}")
    manifest = read_json(manifest_path)
    artifacts = manifest.get("artifacts") if isinstance(manifest.get("artifacts"), dict) else {}

    status = status_override or str(manifest.get("batch_status") or manifest.get("status") or "created")
    state: dict[str, object] = {
        "run_dir": str(run_dir),
        "manifest_path": str(manifest_path),
        "batch_id": manifest.get("batch_id"),
        "input_file_id": manifest.get("input_file_id"),
        "output_file_id": manifest.get("output_file_id"),
        "error_file_id": manifest.get("error_file_id"),
        "status": status,
        "created_at": manifest.get("created_at"),
        "submitted_at": manifest.get("submitted_at"),
        "last_checked_at": manifest.get("last_checked_at"),
        "applied_at": applied_at,
        "request_count": manifest.get("request_count"),
        "pending_rows": manifest.get("pending_rows"),
        "artifacts": artifacts,
        "updated_at": now_iso(),
    }
    return state


def main() -> int:
    parser = argparse.ArgumentParser(description="Create, print, or clear title-classification scheduled state.")
    subparsers = parser.add_subparsers(dest="command", required=True)

    write_parser = subparsers.add_parser("write")
    write_parser.add_argument("--state-path", type=Path, required=True)
    write_parser.add_argument("--run-dir", type=Path, required=True)
    write_parser.add_argument("--status")
    write_parser.add_argument("--applied-at")

    get_parser = subparsers.add_parser("get")
    get_parser.add_argument("--state-path", type=Path, required=True)
    get_parser.add_argument("--field", required=True)

    clear_parser = subparsers.add_parser("clear")
    clear_parser.add_argument("--state-path", type=Path, required=True)
    clear_parser.add_argument("--archive-path", type=Path)

    find_parser = subparsers.add_parser("find-active")
    find_parser.add_argument("--run-root", type=Path, required=True)

    args = parser.parse_args()

    if args.command == "write":
        state = state_from_manifest(args.run_dir, status_override=args.status, applied_at=args.applied_at)
        write_json(args.state_path, state)
        print(args.state_path)
        return 0

    if args.command == "get":
        if not args.state_path.exists():
            return 1
        state = read_json(args.state_path)
        value = state.get(args.field)
        if value is None:
            return 1
        print(value)
        return 0

    if args.command == "clear":
        if not args.state_path.exists():
            return 0
        if args.archive_path:
            args.archive_path.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(args.state_path, args.archive_path)
        args.state_path.unlink()
        return 0

    if args.command == "find-active":
        terminal_statuses = {"failed", "expired", "cancelled", "applied"}
        candidates: list[tuple[float, Path]] = []
        for manifest_path in args.run_root.glob("*/manifest.json"):
            try:
                manifest = read_json(manifest_path)
            except Exception:
                continue
            batch_id = manifest.get("batch_id")
            if not batch_id:
                continue
            status = str(manifest.get("batch_status") or "")
            run_dir = manifest_path.parent
            if (run_dir / "apply_summary.json").exists():
                continue
            if status == "completed" and (run_dir / "completed_state.json").exists():
                continue
            if status in terminal_statuses:
                continue
            candidates.append((manifest_path.stat().st_mtime, run_dir))
        if not candidates:
            return 1
        candidates.sort(reverse=True)
        print(candidates[0][1])
        return 0

    raise AssertionError(args.command)


if __name__ == "__main__":
    raise SystemExit(main())
