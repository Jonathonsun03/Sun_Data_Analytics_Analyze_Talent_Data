"""Guard already-built batches before submission or application."""

import csv
import json
from pathlib import Path

from .policy import control_root, require_allowed, require_path_allowed


def require_manifest_allowed(manifest: dict) -> None:
    """Hold a mixed batch intact when any member is paused; do not lose results."""
    if control_root() is None:
        return
    paths = manifest.get("selected_transcripts", [])
    for path in paths:
        require_path_allowed(Path(path))
    candidates = manifest.get("artifacts", {}).get("candidate_rows_csv")
    if candidates:
        with Path(candidates).open(encoding="utf-8-sig", newline="") as source:
            for row in csv.DictReader(source):
                require_allowed(row.get("talent_code", ""), row.get("talent_name", ""))
    elif not paths:
        raise ValueError("Cannot establish batch talent membership; rebuild with the maintained runner")


def require_run_allowed(run_dir: Path) -> None:
    require_manifest_allowed(json.loads((run_dir / "manifest.json").read_text()))
