#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import subprocess
import sys
import tempfile
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT))

from py_scripts.lib.cloudflare_permissions_sync import (  # noqa: E402
    build_sync_plan,
    plan_summary,
    read_csv_records,
    read_google_sheet,
    render_sync_sql,
)


DEFAULT_APPS_REPO = REPO_ROOT.parent / "sun_Data_analytics_apps"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Preview or synchronize client/talent permissions from Google Sheets "
            "to the Cloudflare D1 permissions database."
        )
    )
    source = parser.add_mutually_exclusive_group()
    source.add_argument(
        "--spreadsheet-id",
        default=os.getenv("PERMISSIONS_SPREADSHEET_ID", "").strip() or None,
        help="Google spreadsheet ID; defaults to PERMISSIONS_SPREADSHEET_ID.",
    )
    source.add_argument(
        "--permissions-csv",
        type=Path,
        help="CSV export of the permissions tab for offline operation.",
    )
    parser.add_argument(
        "--talents-csv",
        type=Path,
        help="CSV export of the talents tab; required with --permissions-csv.",
    )
    parser.add_argument("--product-id", default="youtube-analytics")
    parser.add_argument("--database-name", default="sun-data-permissions")
    parser.add_argument(
        "--wrangler-project",
        type=Path,
        default=Path(os.getenv("SUN_APPS_REPO", DEFAULT_APPS_REPO)),
    )
    parser.add_argument(
        "--apply",
        action="store_true",
        help="Apply the validated snapshot to remote D1. The default is preview only.",
    )
    parser.add_argument(
        "--skip-migrations",
        action="store_true",
        help="Do not apply pending D1 migrations before synchronizing.",
    )
    return parser.parse_args()


def load_source(
    args: argparse.Namespace,
) -> tuple[list[dict[str, str]], list[dict[str, str]]]:
    if args.permissions_csv:
        if not args.talents_csv:
            raise SystemExit("--talents-csv is required with --permissions-csv")
        return read_csv_records(args.permissions_csv), read_csv_records(args.talents_csv)
    if args.talents_csv:
        raise SystemExit("--talents-csv can only be used with --permissions-csv")
    if not args.spreadsheet_id:
        raise SystemExit(
            "Pass --spreadsheet-id, set PERMISSIONS_SPREADSHEET_ID, or pass both CSV files."
        )
    return read_google_sheet(args.spreadsheet_id)


def wrangler_binary(project: Path) -> Path:
    binary = project / "node_modules" / ".bin" / "wrangler"
    if not binary.is_file():
        raise SystemExit(
            f"Wrangler is not installed at {binary}. Run `npm install` in {project}."
        )
    return binary


def run_wrangler(args: argparse.Namespace, sql: str) -> None:
    project = args.wrangler_project.resolve()
    config = project / "wrangler.jsonc"
    if not config.is_file():
        raise SystemExit(f"Wrangler project is missing {config}")
    wrangler = wrangler_binary(project)

    if not args.skip_migrations:
        subprocess.run(
            [
                str(wrangler),
                "d1",
                "migrations",
                "apply",
                args.database_name,
                "--remote",
            ],
            cwd=project,
            check=True,
        )

    sql_path: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            encoding="utf-8",
            prefix="sun-permissions-",
            suffix=".sql",
            dir="/tmp",
            delete=False,
        ) as handle:
            handle.write(sql)
            sql_path = Path(handle.name)
        sql_path.chmod(0o600)
        subprocess.run(
            [
                str(wrangler),
                "d1",
                "execute",
                args.database_name,
                "--remote",
                f"--file={sql_path}",
            ],
            cwd=project,
            check=True,
        )
    finally:
        if sql_path and sql_path.exists():
            sql_path.unlink()


def main() -> int:
    args = parse_args()
    try:
        permission_records, talent_records = load_source(args)
    except RuntimeError as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 2
    plan = build_sync_plan(
        permission_records,
        talent_records,
        default_product_id=args.product_id,
    )

    print("Cloudflare permissions sync preview")
    for line in plan_summary(plan):
        print(line)
    for warning in plan.warnings:
        print(f"WARNING: {warning}", file=sys.stderr)
    for error in plan.errors:
        print(f"ERROR: {error}", file=sys.stderr)

    if plan.errors:
        print(
            "No changes applied. Correct every active row before synchronizing.",
            file=sys.stderr,
        )
        return 2
    if not plan.grants:
        print("No changes applied because the snapshot has no grants.", file=sys.stderr)
        return 2
    if not args.apply:
        print("Preview only. Rerun with --apply after reviewing this output.")
        return 0

    run_wrangler(args, render_sync_sql(plan, source="google_sheets"))
    print("Remote D1 permissions synchronized successfully.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
