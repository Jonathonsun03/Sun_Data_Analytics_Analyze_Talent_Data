"""Atomic, serialized lifecycle edits; never edits data or OAuth credentials."""

from __future__ import annotations

import fcntl
import json
import os
import tempfile
from datetime import datetime, timezone

from .policy import CODE_PATTERN, control_root, validate_record

ACTIONS = {
    "archive": ("retrieval_enabled", False),
    "restore": ("retrieval_enabled", True),
    "pause": ("processing_enabled", False),
    "resume": ("processing_enabled", True),
}


class StaleRevision(ValueError):
    """The caller must refresh before attempting another edit."""


def update_control(code: str, action: str, *, actor: str, channel_ids: list[str],
                   aliases: list[str], expected_revision: int) -> dict:
    """Change exactly one switch, rejecting stale dashboard edits."""
    if not CODE_PATTERN.fullmatch(code) or action not in ACTIONS:
        raise ValueError("Invalid talent code or action")
    if not actor.strip():
        raise ValueError("An actor is required")
    if type(expected_revision) is not int or expected_revision < 0:
        raise ValueError("Expected revision must be a nonnegative integer")
    root = control_root()
    if root is None:
        raise RuntimeError("Configure TALENT_CONTROL_ROOT before changing controls")
    path = root / f"{code}.json"
    with (root / ".write.lock").open("a") as lock:
        if os.fstat(lock.fileno()).st_uid == os.geteuid():
            os.fchmod(lock.fileno(), 0o600)
        fcntl.flock(lock, fcntl.LOCK_EX)
        record = validate_record(json.loads(path.read_text()), code) if path.exists() else {
            "version": 1, "talent_code": code, "revision": 0,
            "retrieval_enabled": True, "processing_enabled": True,
            "channel_ids": [], "aliases": [],
        }
        if record["revision"] != expected_revision:
            raise StaleRevision("Talent control changed; refresh before saving again")
        field, value = ACTIONS[action]
        record[field] = value
        record["revision"] += 1
        record["channel_ids"] = sorted(set(record["channel_ids"] + channel_ids))
        record["aliases"] = sorted(set(record["aliases"] + aliases))
        record["updated_at"] = datetime.now(timezone.utc).isoformat()
        record["updated_by"] = actor.strip()
        record["last_action"] = action
        validate_record(record, code)
        descriptor, temporary = tempfile.mkstemp(prefix=".control-", dir=root)
        try:
            with os.fdopen(descriptor, "w", encoding="utf-8") as output:
                json.dump(record, output, indent=2)
                output.write("\n")
                output.flush()
                os.fsync(output.fileno())
            os.chmod(temporary, 0o640)
            os.replace(temporary, path)
        finally:
            if os.path.exists(temporary):
                os.unlink(temporary)
    return record
