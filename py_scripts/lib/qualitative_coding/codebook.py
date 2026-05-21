from __future__ import annotations

from dataclasses import asdict, dataclass
from pathlib import Path

from .csv_io import read_csv_dicts
from .text import squish
from py_scripts.lib.utils.paths import qualitative_codebook_root, translate_datalake_path


@dataclass(frozen=True)
class CodeDef:
    code_id: str
    code_column: str
    primary_code_id: str
    primary_code: str
    secondary_code_id: str
    secondary_code: str
    parent_code_id: str
    definition: str
    examples_from_text: str

    def compact_dict(self) -> dict[str, str]:
        return asdict(self)


def list_codebook_snapshots(talent_root: Path | None = None) -> list[Path]:
    snapshots_root = qualitative_codebook_root(talent_root) / "snapshots"
    if not snapshots_root.exists():
        return []
    return sorted(snapshots_root.glob("*"), key=lambda p: p.stat().st_mtime, reverse=True)


def resolve_codebook(selector: str, talent_root: Path | None = None) -> Path:
    selector = (selector or "current").strip()
    root = qualitative_codebook_root(talent_root)
    if selector == "current":
        path = root / "current" / "personality_qualitative_code_log.csv"
    elif selector == "latest_snapshot":
        snapshots = list_codebook_snapshots(talent_root)
        if not snapshots:
            raise SystemExit(f"No codebook snapshots found under: {root / 'snapshots'}")
        path = snapshots[0]
    elif selector.startswith("snapshot:"):
        slug = selector[len("snapshot:") :].strip()
        path = translate_datalake_path(slug, talent_root) if any(sep in slug for sep in ("/", "\\")) else root / "snapshots" / slug
    elif selector.startswith("path:"):
        path = translate_datalake_path(selector, talent_root)
    else:
        candidate = translate_datalake_path(selector, talent_root)
        path = candidate if candidate.exists() or any(sep in selector for sep in ("/", "\\")) else root / "snapshots" / selector
    if not path.exists():
        raise SystemExit(f"Codebook not found: {path}")
    return path.resolve()


def load_codebook(path: Path) -> list[CodeDef]:
    required = {
        "Primary Code ID",
        "Primary Code",
        "Secondary Code ID",
        "Secondary Code",
        "Definition",
        "Examples from text",
    }
    _, rows = read_csv_dicts(path)
    if not rows:
        raise SystemExit(f"Codebook is empty: {path}")
    missing = sorted(required - set(rows[0]))
    if missing:
        raise SystemExit(f"Codebook missing required columns: {', '.join(missing)}")

    code_defs: list[CodeDef] = []
    seen: set[str] = set()
    for row in rows:
        primary_id = squish(row["Primary Code ID"])
        secondary_id = squish(row["Secondary Code ID"])
        code_id = secondary_id or primary_id
        if not code_id:
            continue
        code_column = f"code_{code_id}"
        if code_column in seen:
            continue
        seen.add(code_column)
        code_defs.append(
            CodeDef(
                code_id=code_id,
                code_column=code_column,
                primary_code_id=primary_id,
                primary_code=squish(row["Primary Code"]),
                secondary_code_id=secondary_id,
                secondary_code=squish(row["Secondary Code"]),
                parent_code_id=primary_id if secondary_id else "",
                definition=squish(row["Definition"]),
                examples_from_text=squish(row["Examples from text"]),
            )
        )
    if not code_defs:
        raise SystemExit(f"No usable code IDs found in codebook: {path}")
    return code_defs


def hierarchy_pairs(code_defs: list[CodeDef]) -> list[tuple[str, str]]:
    pairs = []
    primary_columns = {f"code_{code.primary_code_id}" for code in code_defs if code.primary_code_id}
    for code in code_defs:
        if code.secondary_code_id:
            parent = f"code_{code.primary_code_id}"
            if parent in primary_columns:
                pairs.append((code.code_column, parent))
    return pairs
