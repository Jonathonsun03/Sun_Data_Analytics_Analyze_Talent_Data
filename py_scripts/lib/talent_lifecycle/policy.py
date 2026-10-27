"""Read the shared, opt-in lifecycle directory without caching decisions."""

from __future__ import annotations

import json
import os
import re
from pathlib import Path

CODE_PATTERN = re.compile(r"^[A-Za-z][A-Za-z0-9_-]{0,79}$")


def control_root() -> Path | None:
    """Return the configured directory; an unavailable configured mount fails closed."""
    value = os.environ.get("TALENT_CONTROL_ROOT", "").strip()
    if not value:
        return None
    path = Path(value).expanduser()
    mount = os.environ.get("TALENT_CONTROL_MOUNT", "").strip()
    if mount and (not os.path.ismount(mount) or not path.resolve().is_relative_to(Path(mount).resolve())):
        raise RuntimeError("Talent control mount is unavailable or does not contain the control directory")
    if not path.is_dir():
        raise RuntimeError(f"Talent control directory is unavailable: {path}")
    return path


def validate_record(record: dict, code: str) -> dict:
    """Reject malformed records rather than silently allowing work."""
    if not CODE_PATTERN.fullmatch(code):
        raise ValueError("Invalid talent code")
    if type(record.get("version")) is not int or record["version"] != 1 or record.get("talent_code") != code:
        raise ValueError(f"Invalid lifecycle identity/version for {code}")
    for field in ("retrieval_enabled", "processing_enabled"):
        if type(record.get(field)) is not bool:
            raise ValueError(f"{code}: {field} must be boolean")
    for field in ("channel_ids", "aliases"):
        values = record.get(field)
        if not isinstance(values, list) or any(not isinstance(x, str) for x in values):
            raise ValueError(f"{code}: {field} must be a list of strings")
    if type(record.get("revision")) is not int or record["revision"] < 1:
        raise ValueError(f"Invalid revision for {code}")
    return record


def records() -> list[dict]:
    """Read one authoritative JSON record per talent; temporary files are ignored."""
    root = control_root()
    if root is None:
        return []
    return [
        validate_record(json.loads(path.read_text(encoding="utf-8")), path.stem)
        for path in sorted(root.iterdir()) if path.suffix == ".json"
    ]


def allowed(*identifiers: str, activity: str = "processing") -> bool:
    """Any matching disabled identity blocks work, including explicit selections."""
    if activity not in {"retrieval", "processing"}:
        raise ValueError("Activity must be retrieval or processing")
    requested = {str(value).strip().casefold() for value in identifiers if value}
    for record in records():
        identities = [record["talent_code"], *record["channel_ids"], *record["aliases"]]
        if requested.intersection(value.strip().casefold() for value in identities):
            if not record[f"{activity}_enabled"]:
                return False
    return True


def require_allowed(*identifiers: str, activity: str = "processing") -> None:
    """Stop before a protected unit of work starts."""
    if not allowed(*identifiers, activity=activity):
        raise RuntimeError(f"Talent {activity} is disabled: {', '.join(identifiers)}")


def require_path_allowed(path: Path) -> None:
    """Protect legacy folder-based entrypoints using registered exact aliases."""
    require_allowed(*Path(path).parts, *Path(path).resolve().parts)
