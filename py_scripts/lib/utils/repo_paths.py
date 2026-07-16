from __future__ import annotations

from pathlib import Path


def find_repo_root(start: Path | str | None = None) -> Path:
    """Find the repository root from a path inside the repository."""
    current = Path(start or Path.cwd()).expanduser().resolve()
    if current.is_file():
        current = current.parent

    for candidate in (current, *current.parents):
        if (candidate / ".git").exists():
            return candidate

    raise RuntimeError(f"Could not locate repository root from {current}")
