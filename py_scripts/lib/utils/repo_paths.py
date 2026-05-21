from __future__ import annotations

from pathlib import Path


def find_repo_root(start: Path) -> Path:
    resolved = start.resolve()
    candidates = [resolved]
    candidates.extend(resolved.parents)
    for candidate in candidates:
        if (candidate / "AGENTS.md").exists() and (candidate / "prompts").is_dir():
            return candidate
    raise RuntimeError(f"Could not locate repository root from {start}")
