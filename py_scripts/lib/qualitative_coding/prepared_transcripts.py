from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

from .csv_io import read_csv_dicts
from .text import is_missing, squish
from py_scripts.lib.utils.paths import TalentPaths, translate_datalake_path


@dataclass(frozen=True)
class TargetFile:
    path: Path
    row_count: int
    pending_rows: int
    missing_code_columns: tuple[str, ...]
    extra_code_columns: tuple[str, ...]


def list_prepared_transcript_files(talents: Iterable[TalentPaths]) -> list[Path]:
    files: list[Path] = []
    for talent in talents:
        if talent.qualitative_prep_dir.exists():
            files.extend(sorted(talent.qualitative_prep_dir.glob("*.csv")))
    return files


def filter_transcripts(paths: list[Path], selector: str | None, talent_root: Path) -> list[Path]:
    if not selector:
        return paths
    selected = translate_datalake_path(selector, talent_root)
    if selected.is_absolute() and selected.exists():
        selected = selected.resolve()
        return [path for path in paths if path.resolve() == selected]
    needle = selector.lower()
    return [path for path in paths if path.name.lower() == needle or needle in path.name.lower()]


def row_id_for(path: Path, row: dict[str, str], index: int) -> str:
    existing = squish(row.get("row_id", ""))
    if existing:
        return existing
    video_id = squish(row.get("video_id", "")) or path.stem
    return f"{video_id}_{index + 1:06d}"


def row_needs_coding(row: dict[str, str], code_columns: list[str], missing_columns: Iterable[str], reprocess: bool) -> bool:
    return reprocess or bool(tuple(missing_columns)) or any(is_missing(row.get(col, "")) for col in code_columns)


def inspect_target(path: Path, code_columns: list[str]) -> TargetFile:
    fieldnames, rows = read_csv_dicts(path)
    missing_columns = tuple(col for col in code_columns if col not in fieldnames)
    pending = 0
    for row in rows:
        if row_needs_coding(row, code_columns, missing_columns, reprocess=False):
            pending += 1
    extra = tuple(col for col in fieldnames if col.startswith("code_") and col not in code_columns)
    return TargetFile(path=path, row_count=len(rows), pending_rows=pending, missing_code_columns=missing_columns, extra_code_columns=extra)
