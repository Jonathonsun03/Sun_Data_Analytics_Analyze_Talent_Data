#!/usr/bin/env python3
from __future__ import annotations

import argparse
import fcntl
import os
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT))

from py_scripts.lib.cloudflare_talent_catalog_sync import (  # noqa: E402
    apply_migrations,
    build_catalog_sync_plan,
    d1_catalog_schema_ready,
    execute_catalog_sync,
    plan_summary,
    read_d1_talents,
    read_duckdb_catalog,
    render_catalog_sync_sql,
    resolve_duckdb_path,
)


DEFAULT_APPS_REPO = REPO_ROOT.parent / "sun_Data_analytics_apps"
LOCK_PATH = Path("/tmp/sun-data-talent-catalog-sync.lock")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Preview or synchronize the read-only DuckDB talent catalog to the "
            "Cloudflare D1 permission catalog."
        )
    )
    parser.add_argument(
        "--database-path",
        type=Path,
        help="Unified DuckDB path; otherwise resolve it from the repository environment.",
    )
    parser.add_argument("--database-name", default="sun-data-permissions")
    parser.add_argument(
        "--wrangler-project",
        type=Path,
        default=Path(os.getenv("SUN_APPS_REPO", DEFAULT_APPS_REPO)),
    )
    parser.add_argument(
        "--apply",
        action="store_true",
        help="Apply the validated catalog plan to remote D1. Default is preview only.",
    )
    parser.add_argument(
        "--skip-migrations",
        action="store_true",
        help="Do not apply pending D1 migrations before an --apply run.",
    )
    return parser.parse_args()


def build_plan(args: argparse.Namespace):
    database_path = resolve_duckdb_path(REPO_ROOT, args.database_path)
    duckdb_talents = read_duckdb_catalog(database_path)
    schema_ready = d1_catalog_schema_ready(args.wrangler_project, args.database_name)
    d1_talents = read_d1_talents(
        args.wrangler_project,
        args.database_name,
        schema_ready=schema_ready,
    )
    return build_catalog_sync_plan(duckdb_talents, d1_talents), schema_ready


def main() -> int:
    args = parse_args()
    LOCK_PATH.touch(mode=0o600, exist_ok=True)
    with LOCK_PATH.open("r+", encoding="utf-8") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            print("Another talent catalog sync is already running.", file=sys.stderr)
            return 3

        try:
            plan, schema_ready = build_plan(args)
        except (RuntimeError, OSError, ValueError, subprocess.CalledProcessError) as error:
            print(f"ERROR: {error}", file=sys.stderr)
            return 2

        print("DuckDB -> Cloudflare D1 talent catalog sync preview")
        for line in plan_summary(plan):
            print(line)
        if not schema_ready:
            print("NOTICE: D1 catalog migration is pending.")
        for warning in plan.warnings:
            print(f"WARNING: {warning}", file=sys.stderr)
        for error in plan.errors:
            print(f"ERROR: {error}", file=sys.stderr)

        if plan.errors:
            print("No changes applied. Resolve every conflict first.", file=sys.stderr)
            return 2
        if not args.apply:
            print("Preview only. Rerun with --apply after reviewing this output.")
            return 0

        try:
            if not args.skip_migrations:
                apply_migrations(args.wrangler_project, args.database_name)
            if not d1_catalog_schema_ready(args.wrangler_project, args.database_name):
                raise RuntimeError(
                    "D1 catalog migration is not applied; rerun without --skip-migrations."
                )
            plan, _ = build_plan(args)
            if plan.errors:
                raise RuntimeError("Catalog conflicts appeared after migration")
            execute_catalog_sync(
                args.wrangler_project,
                args.database_name,
                render_catalog_sync_sql(plan),
            )
        except (RuntimeError, OSError, ValueError, subprocess.CalledProcessError) as error:
            print(f"ERROR: {error}", file=sys.stderr)
            return 2

        print("Remote D1 talent catalog synchronized successfully.")
        return 0


if __name__ == "__main__":
    raise SystemExit(main())
