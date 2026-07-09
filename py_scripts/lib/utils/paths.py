from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path

from py_scripts.lib.utils.env import load_repo_env


REPO_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_TALENT_ROOTS = (
    Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data"),
    Path("/mnt/router_data/DataLake/Sun_Data_Analytics/Talent_data"),
    Path("/mnt/datalake/Datalake/Sun_Data_Analytics/Talent_data"),
)


@dataclass(frozen=True)
class TalentPaths:
    talent_name: str
    talent_slug: str
    talent_path: Path
    text_playback_path: Path
    qualitative_coding_root: Path
    qualitative_prep_dir: Path


def normalize_match(value: str) -> str:
    return "".join(ch.lower() for ch in value if ch.isalnum())


def talent_slugify(value: str) -> str:
    text = normalize_match(value)
    return text or "talent"


def default_talent_root() -> Path:
    load_repo_env()
    for key in ("TALENT_DATALAKE_ROOT", "CHAT_OUTPUT_ROOT"):
        raw = os.environ.get(key, "").strip()
        if raw:
            return Path(raw).expanduser()
    for candidate in DEFAULT_TALENT_ROOTS:
        if candidate.exists():
            return candidate
    raise SystemExit("Datalake root not configured. Set TALENT_DATALAKE_ROOT.")


def processed_root(talent_root: Path | None = None) -> Path:
    load_repo_env()
    env_root = os.environ.get("TALENT_PROCESSED_ROOT", "").strip()
    if env_root:
        return Path(env_root).expanduser()
    talent_root = talent_root or default_talent_root()
    return talent_root.parent / "Processed" / "Talent_Data"


def qualitative_codebook_root(talent_root: Path | None = None) -> Path:
    return (
        processed_root(talent_root)
        / "Qualitative Codebooks"
        / "concept_areas"
        / "definitions"
        / "monetary_personality"
    )


def qualitative_batch_runs_root(talent_root: Path | None = None) -> Path:
    return processed_root(talent_root) / "qualitative_batch_runs"


def qualitative_run_code_sets_root(talent_root: Path | None = None) -> Path:
    return processed_root(talent_root) / "Qualitative Codebooks" / "run_code_sets"


def translate_datalake_path(path_text: str, talent_root: Path | None = None) -> Path:
    talent_root = talent_root or default_talent_root()
    text = path_text.replace("\\", "/").strip()
    if text.startswith("path:"):
        text = text[len("path:") :]
    replacements = {
        "Z:/DataLake/Sun_Data_Analytics/Talent_data": str(talent_root),
        "X:/datalake/DataLake/Sun_Data_Analytics/Talent_data": str(talent_root),
        "/DataLake/Sun_Data_Analytics/Talent_data": str(talent_root),
        "Z:/DataLake/Sun_Data_Analytics/Processed/Talent_Data": str(processed_root(talent_root)),
        "X:/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data": str(processed_root(talent_root)),
        "/DataLake/Sun_Data_Analytics/Processed/Talent_Data": str(processed_root(talent_root)),
    }
    for prefix, replacement in replacements.items():
        if text.startswith(prefix):
            text = replacement + text[len(prefix) :]
            break
    return Path(text).expanduser()


def resolve_talent_paths(
    query: str,
    talent_root: Path | None = None,
    coding_folder: str = "monetary conversation codes",
    must_have_text_playback: bool = False,
) -> list[TalentPaths]:
    talent_root = talent_root or default_talent_root()
    needle = normalize_match(query)
    if needle == "all":
        matches = [path for path in talent_root.iterdir() if path.is_dir()]
    else:
        matches = [path for path in talent_root.iterdir() if path.is_dir() and needle in normalize_match(path.name)]
    if not matches:
        raise SystemExit(f"No talent folders matched {query!r} under {talent_root}")

    talents: list[TalentPaths] = []
    missing_text: list[str] = []
    for talent_path in sorted(matches):
        text_playback_path = talent_path / "text_playback"
        if must_have_text_playback and not text_playback_path.exists():
            missing_text.append(talent_path.name)
        qualitative_coding_root = talent_path / "qualitative coding"
        talents.append(
            TalentPaths(
                talent_name=talent_path.name,
                talent_slug=talent_slugify(talent_path.name),
                talent_path=talent_path,
                text_playback_path=text_playback_path,
                qualitative_coding_root=qualitative_coding_root,
                qualitative_prep_dir=qualitative_coding_root / coding_folder,
            )
        )
    if missing_text:
        raise SystemExit(f"Missing text_playback folder for: {', '.join(missing_text)}")
    return talents
