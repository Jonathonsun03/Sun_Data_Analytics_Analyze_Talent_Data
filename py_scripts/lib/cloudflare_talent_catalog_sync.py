from __future__ import annotations

import json
import os
import re
import shutil
import subprocess
import tempfile
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable


TALENT_CODE_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_-]{0,79}$")


@dataclass(frozen=True)
class DuckDbTalent:
    talent_code: str
    display_name: str
    catalog_active: bool


@dataclass(frozen=True)
class D1Talent:
    id: str
    display_name: str
    active: bool
    talent_code: str | None = None
    catalog_active: bool = True


@dataclass(frozen=True)
class PlannedTalent:
    id: str
    talent_code: str
    display_name: str
    catalog_active: bool
    action: str
    matched_existing: bool


@dataclass(frozen=True)
class CatalogSyncPlan:
    talents: tuple[PlannedTalent, ...]
    warnings: tuple[str, ...]
    errors: tuple[str, ...]

    @property
    def inserted_count(self) -> int:
        return sum(talent.action == "insert" for talent in self.talents)

    @property
    def updated_count(self) -> int:
        return sum(talent.action == "update" for talent in self.talents)

    @property
    def unchanged_count(self) -> int:
        return sum(talent.action == "unchanged" for talent in self.talents)


def load_repo_environment(repo_root: Path) -> None:
    env_path = repo_root / ".env"
    if not env_path.is_file() or not os.access(env_path, os.R_OK):
        return

    for raw_line in env_path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        if line.startswith("export "):
            line = line[7:].lstrip()
        if "=" not in line:
            continue
        name, value = line.split("=", maxsplit=1)
        name = name.strip()
        if not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", name):
            continue
        value = value.strip()
        if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
            value = value[1:-1]
        os.environ.setdefault(name, value)


def resolve_duckdb_path(repo_root: Path, configured_path: Path | None = None) -> Path:
    load_repo_environment(repo_root)
    if configured_path is not None:
        path = configured_path
    elif os.getenv("UNIFIED_CATALOG_DB_PATH", "").strip():
        path = Path(os.environ["UNIFIED_CATALOG_DB_PATH"].strip())
    else:
        data_root = next(
            (
                os.getenv(name, "").strip()
                for name in (
                    "TALENT_DATALAKE_ROOT",
                    "TALENT_DATA_ROOT",
                    "DATA_LAKE_ROOT",
                )
                if os.getenv(name, "").strip()
            ),
            "",
        )
        if not data_root:
            raise RuntimeError(
                "DuckDB path is not configured. Set UNIFIED_CATALOG_DB_PATH or "
                "TALENT_DATALAKE_ROOT, or pass --database-path."
            )
        path = Path(data_root) / "Data_lakehouse" / "talent_lakehouse.duckdb"

    path = path.expanduser().resolve()
    if not path.is_file():
        raise RuntimeError(f"Unified DuckDB database not found: {path}")
    return path


def display_name_from_duckdb(value: str) -> str:
    display_name = value.replace("_", " ").strip()
    display_name = re.sub(
        r"\s+variance\s+project$",
        "",
        display_name,
        flags=re.IGNORECASE,
    )
    display_name = re.sub(r"\s+ch$", "", display_name, flags=re.IGNORECASE)
    return re.sub(r"\s+", " ", display_name).strip()


def read_duckdb_catalog(database_path: Path) -> tuple[DuckDbTalent, ...]:
    try:
        import duckdb
    except ImportError as error:
        raise RuntimeError(
            "DuckDB support is not installed. Run "
            "`.venv/bin/python -m pip install -r py_scripts/requirements.txt`."
        ) from error

    connection = duckdb.connect(str(database_path), read_only=True)
    try:
        rows = connection.execute(
            """
            SELECT talent_code, talent_name, active
            FROM catalog.talents
            ORDER BY talent_code
            """
        ).fetchall()
    finally:
        connection.close()

    talents: list[DuckDbTalent] = []
    for talent_code, talent_name, active in rows:
        talent_code = str(talent_code).strip()
        display_name = display_name_from_duckdb(str(talent_name))
        if not TALENT_CODE_PATTERN.fullmatch(talent_code):
            raise RuntimeError(f"DuckDB contains an invalid talent_code: {talent_code!r}")
        if not display_name:
            raise RuntimeError(f"DuckDB talent {talent_code!r} has no display name")
        talents.append(
            DuckDbTalent(
                talent_code=talent_code,
                display_name=display_name,
                catalog_active=bool(active),
            )
        )

    if not talents:
        raise RuntimeError("DuckDB catalog.talents contains no talent rows")
    return tuple(talents)


def normalized_name(value: str) -> str:
    return "".join(re.findall(r"[a-z0-9]+", value.casefold()))


def talent_id_for_name(value: str) -> str:
    identifier = "-".join(re.findall(r"[a-z0-9]+", value.casefold()))[:80].strip("-")
    if not identifier:
        raise ValueError("Talent display name must contain a letter or number")
    return identifier


def build_catalog_sync_plan(
    duckdb_talents: Iterable[DuckDbTalent],
    d1_talents: Iterable[D1Talent],
) -> CatalogSyncPlan:
    duckdb_talents = tuple(duckdb_talents)
    d1_talents = tuple(d1_talents)
    warnings: list[str] = []
    errors: list[str] = []
    planned: list[PlannedTalent] = []

    codes = [talent.talent_code.casefold() for talent in duckdb_talents]
    duplicate_codes = sorted({code for code in codes if codes.count(code) > 1})
    if duplicate_codes:
        errors.append(
            "DuckDB contains duplicate talent codes: " + ", ".join(duplicate_codes)
        )

    d1_by_code = {
        talent.talent_code.casefold(): talent
        for talent in d1_talents
        if talent.talent_code
    }
    d1_by_name: dict[str, list[D1Talent]] = {}
    for talent in d1_talents:
        d1_by_name.setdefault(normalized_name(talent.display_name), []).append(talent)

    claimed_ids: set[str] = set()
    existing_ids = {talent.id for talent in d1_talents}
    duckdb_codes = {talent.talent_code.casefold() for talent in duckdb_talents}

    for talent in duckdb_talents:
        existing = d1_by_code.get(talent.talent_code.casefold())
        matched_existing = existing is not None

        if existing is None:
            name_matches = [
                candidate
                for candidate in d1_by_name.get(normalized_name(talent.display_name), [])
                if candidate.talent_code is None and candidate.id not in claimed_ids
            ]
            if len(name_matches) == 1:
                existing = name_matches[0]
                matched_existing = True
                warnings.append(
                    f"{talent.talent_code} will attach to existing D1 talent "
                    f"'{existing.id}' by exact display name."
                )
            elif len(name_matches) > 1:
                errors.append(
                    f"{talent.talent_code} matches multiple unmapped D1 talents by "
                    f"display name: {', '.join(item.id for item in name_matches)}"
                )
                continue

        if existing is None:
            base_id = talent_id_for_name(talent.display_name)
            talent_id = base_id
            if talent_id in existing_ids or talent_id in claimed_ids:
                talent_id = f"{base_id}-{talent.talent_code.casefold()}"
            suffix = 2
            while talent_id in existing_ids or talent_id in claimed_ids:
                talent_id = f"{base_id}-{talent.talent_code.casefold()}-{suffix}"
                suffix += 1
            action = "insert"
        else:
            talent_id = existing.id
            action = (
                "unchanged"
                if existing.display_name == talent.display_name
                and existing.talent_code == talent.talent_code
                and existing.catalog_active == talent.catalog_active
                else "update"
            )

        claimed_ids.add(talent_id)
        planned.append(
            PlannedTalent(
                id=talent_id,
                talent_code=talent.talent_code,
                display_name=talent.display_name,
                catalog_active=talent.catalog_active,
                action=action,
                matched_existing=matched_existing,
            )
        )

    missing_codes = sorted(
        talent.talent_code
        for talent in d1_talents
        if talent.talent_code and talent.talent_code.casefold() not in duckdb_codes
    )
    if missing_codes:
        warnings.append(
            "D1 catalog talents missing from DuckDB were retained unchanged: "
            + ", ".join(missing_codes)
        )

    return CatalogSyncPlan(
        talents=tuple(sorted(planned, key=lambda talent: talent.talent_code)),
        warnings=tuple(warnings),
        errors=tuple(errors),
    )


def sql_literal(value: str | None) -> str:
    if value is None:
        return "NULL"
    return "'" + value.replace("'", "''") + "'"


def render_catalog_sync_sql(plan: CatalogSyncPlan) -> str:
    if plan.errors:
        raise ValueError("Cannot render a catalog sync plan containing errors")

    synced_at = datetime.now(timezone.utc).replace(microsecond=0).isoformat()
    statements = ["PRAGMA foreign_keys = ON;"]
    for talent in plan.talents:
        if talent.action == "insert":
            statements.append(
                "INSERT INTO talents ("
                "id, display_name, active, talent_code, catalog_active, "
                "catalog_synced_at, updated_at"
                ") VALUES ("
                f"{sql_literal(talent.id)}, {sql_literal(talent.display_name)}, 1, "
                f"{sql_literal(talent.talent_code)}, "
                f"{1 if talent.catalog_active else 0}, {sql_literal(synced_at)}, "
                "CURRENT_TIMESTAMP);"
            )
        elif talent.action == "update":
            statements.append(
                "UPDATE talents SET "
                f"display_name = {sql_literal(talent.display_name)}, "
                f"talent_code = {sql_literal(talent.talent_code)}, "
                f"catalog_active = {1 if talent.catalog_active else 0}, "
                f"catalog_synced_at = {sql_literal(synced_at)}, "
                "updated_at = CURRENT_TIMESTAMP "
                f"WHERE id = {sql_literal(talent.id)};"
            )

    statements.append(
        "INSERT INTO talent_catalog_sync_runs ("
        "source, synced_at, discovered_count, inserted_count, updated_count"
        ") VALUES ("
        f"'duckdb', {sql_literal(synced_at)}, {len(plan.talents)}, "
        f"{plan.inserted_count}, {plan.updated_count});"
    )
    return "\n".join(statements) + "\n"


def configured_node_environment() -> dict[str, str]:
    environment = os.environ.copy()
    if shutil.which("node", path=environment.get("PATH")):
        return environment

    configured_node = environment.get("SUN_NODE_BINARY", "").strip()
    candidates = [Path(configured_node)] if configured_node else []
    candidates.extend(
        sorted(
            Path.home().glob(".nvm/versions/node/*/bin/node"),
            reverse=True,
        )
    )
    node = next((candidate for candidate in candidates if candidate.is_file()), None)
    if node is None:
        raise RuntimeError(
            "Node.js was not found. Set SUN_NODE_BINARY to the Node executable used by Wrangler."
        )
    environment["PATH"] = f"{node.parent}:{environment.get('PATH', '')}"
    return environment


def wrangler_binary(project: Path) -> Path:
    binary = project / "node_modules" / ".bin" / "wrangler"
    if not binary.is_file():
        raise RuntimeError(
            f"Wrangler is not installed at {binary}. Run `npm install` in {project}."
        )
    return binary


def run_wrangler(
    project: Path,
    arguments: list[str],
    *,
    capture_output: bool = False,
) -> subprocess.CompletedProcess[str]:
    project = project.expanduser().resolve()
    if not (project / "wrangler.jsonc").is_file():
        raise RuntimeError(f"Wrangler project is missing {project / 'wrangler.jsonc'}")
    command = [str(wrangler_binary(project)), *arguments]
    environment = configured_node_environment()
    if arguments[:3] == ["d1", "migrations", "apply"]:
        environment["CI"] = "true"
    return subprocess.run(
        command,
        cwd=project,
        env=environment,
        check=True,
        capture_output=capture_output,
        text=True,
    )


def wrangler_query(
    project: Path,
    database_name: str,
    sql: str,
) -> list[dict[str, object]]:
    completed = run_wrangler(
        project,
        [
            "d1",
            "execute",
            database_name,
            "--remote",
            "--json",
            "--command",
            sql,
        ],
        capture_output=True,
    )
    payload = json.loads(completed.stdout)
    if not isinstance(payload, list) or not payload:
        raise RuntimeError("Wrangler returned an unexpected D1 response")
    return payload[0].get("results", [])


def d1_catalog_schema_ready(project: Path, database_name: str) -> bool:
    rows = wrangler_query(project, database_name, "PRAGMA table_info(talents)")
    columns = {str(row.get("name")) for row in rows}
    return {"talent_code", "catalog_active", "catalog_synced_at"}.issubset(columns)


def read_d1_talents(
    project: Path,
    database_name: str,
    *,
    schema_ready: bool,
) -> tuple[D1Talent, ...]:
    if schema_ready:
        sql = (
            "SELECT id, display_name, active, talent_code, catalog_active "
            "FROM talents ORDER BY display_name"
        )
    else:
        sql = "SELECT id, display_name, active FROM talents ORDER BY display_name"
    rows = wrangler_query(project, database_name, sql)
    return tuple(
        D1Talent(
            id=str(row["id"]),
            display_name=str(row["display_name"]),
            active=bool(row["active"]),
            talent_code=(str(row["talent_code"]) if row.get("talent_code") else None),
            catalog_active=bool(row.get("catalog_active", 1)),
        )
        for row in rows
    )


def apply_migrations(project: Path, database_name: str) -> None:
    run_wrangler(
        project,
        ["d1", "migrations", "apply", database_name, "--remote"],
    )


def execute_catalog_sync(
    project: Path,
    database_name: str,
    sql: str,
) -> None:
    sql_path: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            encoding="utf-8",
            prefix="sun-talent-catalog-",
            suffix=".sql",
            dir="/tmp",
            delete=False,
        ) as handle:
            handle.write(sql)
            sql_path = Path(handle.name)
        sql_path.chmod(0o600)
        run_wrangler(
            project,
            [
                "d1",
                "execute",
                database_name,
                "--remote",
                "--yes",
                f"--file={sql_path}",
            ],
        )
    finally:
        if sql_path and sql_path.exists():
            sql_path.unlink()


def plan_summary(plan: CatalogSyncPlan) -> list[str]:
    lines = [
        f"DuckDB talents: {len(plan.talents)}",
        f"New D1 talents: {plan.inserted_count}",
        f"Updated or mapped talents: {plan.updated_count}",
        f"Unchanged talents: {plan.unchanged_count}",
    ]
    for talent in plan.talents:
        lines.append(
            f"  {talent.action.upper():9} {talent.talent_code:8} "
            f"{talent.display_name} -> {talent.id}"
        )
    return lines
