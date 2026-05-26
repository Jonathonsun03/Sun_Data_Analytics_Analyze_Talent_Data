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
    analytic_object: str = ""
    row_source_scope: str = ""
    source_group: str = ""

    def compact_dict(self) -> dict[str, str]:
        item = asdict(self)
        item.pop("analytic_object", None)
        return {key: value for key, value in item.items() if value != ""}


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
    fieldnames, rows = read_csv_dicts(path)
    if "selected_for_processing" in fieldnames:
        return load_selected_code_library(path, fieldnames, rows)

    required = {
        "Primary Code ID",
        "Primary Code",
        "Secondary Code ID",
        "Secondary Code",
        "Definition",
        "Examples from text",
    }
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


def selected_truthy(value: str) -> bool:
    return squish(value).lower() in {"true", "t", "1", "yes", "y", "selected"}


def first_value(row: dict[str, str], *names: str) -> str:
    for name in names:
        value = squish(row.get(name, ""))
        if value:
            return value
    return ""


def load_selected_code_library(path: Path, fieldnames: list[str], rows: list[dict[str, str]]) -> list[CodeDef]:
    required = {
        "selected_for_processing",
        "code_id",
        "code_column",
        "definition",
    }
    missing = sorted(required - set(fieldnames))
    if missing:
        raise SystemExit(f"Selected code library missing required columns: {', '.join(missing)}")

    selected_rows = [row for row in rows if selected_truthy(row.get("selected_for_processing", ""))]
    if not selected_rows:
        raise SystemExit(f"No rows selected for processing in code library: {path}")

    code_defs: list[CodeDef] = []
    seen: set[str] = set()
    for row in selected_rows:
        code_id = first_value(row, "code_id", "secondary_code_id", "primary_code_id")
        code_column = first_value(row, "code_column") or f"code_{code_id}"
        if not code_id or not code_column or code_column in seen:
            continue
        seen.add(code_column)
        primary_id = first_value(row, "primary_code_id", "Primary Code ID")
        secondary_id = first_value(row, "secondary_code_id", "Secondary Code ID")
        code_defs.append(
            CodeDef(
                code_id=code_id,
                code_column=code_column,
                primary_code_id=primary_id,
                primary_code=first_value(row, "primary_code", "Primary Code", "code_name"),
                secondary_code_id=secondary_id,
                secondary_code=first_value(row, "secondary_code", "Secondary Code", "code_name"),
                parent_code_id=primary_id if secondary_id else "",
                definition=first_value(row, "definition", "Definition"),
                examples_from_text=first_value(row, "examples_from_text", "Examples from text", "example_quote"),
                analytic_object=first_value(row, "analytic_object"),
                row_source_scope=first_value(row, "row_source_scope"),
                source_group=first_value(row, "source_group"),
            )
        )
    if not code_defs:
        raise SystemExit(f"No usable selected code IDs found in code library: {path}")
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
