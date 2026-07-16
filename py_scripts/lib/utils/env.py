from __future__ import annotations

import os
from pathlib import Path

from py_scripts.lib.utils.repo_paths import find_repo_root


_LOADED_ENV_FILES: set[Path] = set()


def _env_name(line: str) -> str:
    stripped = line.strip()
    if not stripped or stripped.startswith("#"):
        return ""
    if stripped.startswith("export "):
        stripped = stripped[len("export ") :].strip()
    name, sep, _ = stripped.partition("=")
    if not sep:
        return ""
    name = name.strip()
    if not name or not (name[0].isalpha() or name[0] == "_"):
        return ""
    if any(not (ch.isalnum() or ch == "_") for ch in name):
        return ""
    return name


def _env_value(line: str) -> str:
    stripped = line.strip()
    if stripped.startswith("export "):
        stripped = stripped[len("export ") :].strip()
    _, _, value = stripped.partition("=")
    value = value.strip()
    if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
        return value[1:-1]
    return value


def _apply_env_aliases(override: bool = False) -> None:
    aliases = {
        "TALENT_DATALAKE_ROOT": "TALENT_DATA_ROOT",
        "TALENT_STAGING_ROOT": "STAGING_ROOT",
    }
    for canonical, alias in aliases.items():
        if override or not os.environ.get(canonical, "").strip():
            alias_value = os.environ.get(alias, "").strip()
            if alias_value:
                os.environ[canonical] = alias_value


def load_repo_env(
    repo_root: Path | str | None = None,
    env_file: str = ".env",
    override: bool = False,
) -> Path:
    if repo_root is not None:
        root = Path(repo_root).expanduser()
    elif os.environ.get("TALENT_REPO_ROOT", "").strip():
        root = Path(os.environ["TALENT_REPO_ROOT"]).expanduser()
    else:
        root = find_repo_root(Path(__file__))
    env_path = (root / env_file).resolve()

    if not override and env_path in _LOADED_ENV_FILES:
        return env_path
    if not env_path.exists():
        _LOADED_ENV_FILES.add(env_path)
        _apply_env_aliases(override=override)
        return env_path

    for raw_line in env_path.read_text(encoding="utf-8").splitlines():
        name = _env_name(raw_line)
        if not name:
            continue
        if not override and os.environ.get(name, ""):
            continue
        os.environ[name] = _env_value(raw_line)

    _apply_env_aliases(override=override)
    _LOADED_ENV_FILES.add(env_path)
    return env_path
