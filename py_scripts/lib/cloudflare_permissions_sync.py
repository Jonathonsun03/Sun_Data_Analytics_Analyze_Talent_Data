from __future__ import annotations

import csv
import hashlib
import json
import re
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable


TRUE_VALUES = {"1", "true", "yes", "y"}
FALSE_VALUES = {"", "0", "false", "no", "n"}
KNOWN_TALENT_ALIASES = {
    "teri": "0004_terberri_solaris_ch",
}


@dataclass(frozen=True)
class Talent:
    id: str
    display_name: str


@dataclass(frozen=True)
class PermissionGrant:
    source_key: str
    source_client_id: str
    email: str
    product_id: str
    talent_id: str
    role: str
    access_start_date: str | None
    access_end_date: str | None


@dataclass(frozen=True)
class SyncPlan:
    talents: tuple[Talent, ...]
    grants: tuple[PermissionGrant, ...]
    warnings: tuple[str, ...]
    errors: tuple[str, ...]

    @property
    def user_count(self) -> int:
        return len({grant.email for grant in self.grants})


def rows_to_records(rows: list[list[object]]) -> list[dict[str, str]]:
    if not rows:
        return []

    headers = [str(value).strip() for value in rows[0]]
    records: list[dict[str, str]] = []
    for row in rows[1:]:
        values = [str(value).strip() if value is not None else "" for value in row]
        values.extend([""] * (len(headers) - len(values)))
        record = dict(zip(headers, values, strict=False))
        if any(record.values()):
            records.append(record)
    return records


def read_csv_records(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return [
            {str(key).strip(): (value or "").strip() for key, value in row.items()}
            for row in csv.DictReader(handle)
        ]


def read_google_sheet(
    spreadsheet_id: str,
) -> tuple[list[dict[str, str]], list[dict[str, str]]]:
    try:
        import google.auth
        from google.auth.exceptions import DefaultCredentialsError
        from googleapiclient.discovery import build
    except ImportError as error:
        raise RuntimeError(
            "Google Sheets support is not installed. Run "
            "`.venv/bin/python -m pip install -r py_scripts/requirements.txt`."
        ) from error

    try:
        credentials, _ = google.auth.default(
            scopes=["https://www.googleapis.com/auth/spreadsheets.readonly"]
        )
    except DefaultCredentialsError as error:
        raise RuntimeError(
            "Google credentials are not configured. Set "
            "GOOGLE_APPLICATION_CREDENTIALS to an existing credential JSON file; "
            "the example /secure/path/google-reader.json path is not a real file."
        ) from error
    service = build("sheets", "v4", credentials=credentials, cache_discovery=False)
    response = (
        service.spreadsheets()
        .values()
        .batchGet(
            spreadsheetId=spreadsheet_id,
            ranges=["permissions!A:AD", "talents!A:O"],
            majorDimension="ROWS",
        )
        .execute()
    )
    value_ranges = response.get("valueRanges", [])
    if len(value_ranges) != 2:
        raise RuntimeError("Google Sheets did not return both required tabs.")

    return (
        rows_to_records(value_ranges[0].get("values", [])),
        rows_to_records(value_ranges[1].get("values", [])),
    )


def normalized_key(value: str) -> str:
    return "_".join(re.findall(r"[a-z0-9]+", value.casefold()))


def parse_bool(value: str, *, field: str, row_number: int) -> bool:
    normalized = value.strip().casefold()
    if normalized in TRUE_VALUES:
        return True
    if normalized in FALSE_VALUES:
        return False
    raise ValueError(f"row {row_number}: {field} has invalid boolean value {value!r}")


def normalize_date(value: str, *, field: str, row_number: int) -> str | None:
    value = value.strip()
    if not value:
        return None

    for date_format in ("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y"):
        try:
            return datetime.strptime(value, date_format).date().isoformat()
        except ValueError:
            pass
    raise ValueError(f"row {row_number}: {field} has invalid date value {value!r}")


def friendly_talent_name(record: dict[str, str]) -> str:
    value = (
        record.get("datalake_folder_name")
        or record.get("canonical_talent_name")
        or record.get("talent_slug")
        or record.get("talent_id")
        or ""
    )
    value = value.replace("_", " ").strip()
    value = re.sub(r"\s*[【\[].*?variance project.*$", "", value, flags=re.IGNORECASE)
    value = re.sub(r"\s+ch$", "", value, flags=re.IGNORECASE)
    return re.sub(r"\s+", " ", value).strip()


def talent_aliases(record: dict[str, str], display_name: str) -> set[str]:
    aliases = {
        record.get("talent_id", ""),
        record.get("talent_slug", ""),
        record.get("canonical_talent_name", ""),
        record.get("datalake_folder_name", ""),
        display_name,
    }
    aliases.update(re.split(r"[,;|]", record.get("aliases", "")))

    display_words = display_name.split()
    if display_words:
        aliases.add(display_words[0])
    return {normalized_key(alias) for alias in aliases if normalized_key(alias)}


def build_talent_catalog(
    talent_records: Iterable[dict[str, str]],
) -> tuple[dict[str, Talent], dict[str, str], list[str]]:
    talents: dict[str, Talent] = {}
    alias_candidates: dict[str, set[str]] = {}
    errors: list[str] = []

    for row_number, record in enumerate(talent_records, start=2):
        try:
            if not parse_bool(record.get("active", ""), field="active", row_number=row_number):
                continue
        except ValueError as error:
            errors.append(str(error))
            continue

        talent_id = record.get("talent_id", "").strip()
        display_name = friendly_talent_name(record)
        if not talent_id or not display_name:
            errors.append(f"talents row {row_number}: talent_id and display name are required")
            continue

        talents[talent_id] = Talent(id=talent_id, display_name=display_name)
        for alias in talent_aliases(record, display_name):
            alias_candidates.setdefault(alias, set()).add(talent_id)

    aliases = {
        alias: next(iter(talent_ids))
        for alias, talent_ids in alias_candidates.items()
        if len(talent_ids) == 1
    }
    for alias, talent_id in KNOWN_TALENT_ALIASES.items():
        if talent_id in talents:
            aliases[alias] = talent_id
    return talents, aliases, errors


def valid_email(value: str) -> bool:
    return bool(re.fullmatch(r"[^@\s]+@[^@\s]+\.[^@\s]+", value))


def grant_source_key(
    source_client_id: str,
    email: str,
    product_id: str,
    talent_id: str,
) -> str:
    canonical = json.dumps(
        [source_client_id.casefold(), email, product_id, talent_id],
        ensure_ascii=True,
        separators=(",", ":"),
    )
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()


def build_sync_plan(
    permission_records: Iterable[dict[str, str]],
    talent_records: Iterable[dict[str, str]],
    *,
    default_product_id: str = "youtube-analytics",
) -> SyncPlan:
    talents, aliases, errors = build_talent_catalog(talent_records)
    warnings: list[str] = []
    grants: dict[str, PermissionGrant] = {}

    required_columns = {"client_id", "client_email", "active"}
    permission_records = list(permission_records)
    if not permission_records:
        errors.append("permissions tab contains no data rows")
    elif missing := required_columns - set(permission_records[0]):
        errors.append(f"permissions tab is missing columns: {', '.join(sorted(missing))}")

    for row_number, record in enumerate(permission_records, start=2):
        try:
            active = parse_bool(
                record.get("active", ""),
                field="active",
                row_number=row_number,
            )
        except ValueError as error:
            errors.append(str(error))
            continue
        if not active:
            continue

        client_id = record.get("client_id", "").strip()
        email = record.get("client_email", "").strip().casefold()
        if not client_id:
            errors.append(f"row {row_number}: active record is missing client_id")
            continue
        if not email:
            errors.append(f"row {row_number}: active client {client_id!r} is missing client_email")
            continue
        if not valid_email(email):
            errors.append(f"row {row_number}: active client {client_id!r} has an invalid email")
            continue

        requested_talent_id = record.get("talent_id", "").strip()
        delivery_group = (
            record.get("delivery_group_id", "").strip()
            or record.get("delivery_group_display_name", "").strip()
        )
        talent_id = requested_talent_id or aliases.get(normalized_key(delivery_group), "")
        if talent_id not in talents:
            label = requested_talent_id or delivery_group or "(blank)"
            errors.append(
                f"row {row_number}: client {client_id!r} has unknown talent {label!r}"
            )
            continue

        product_id = record.get("product_id", "").strip() or default_product_id
        role = record.get("role", "").strip() or "viewer"
        try:
            access_start_date = normalize_date(
                record.get("access_start_date", ""),
                field="access_start_date",
                row_number=row_number,
            )
            access_end_date = normalize_date(
                record.get("access_end_date", ""),
                field="access_end_date",
                row_number=row_number,
            )
        except ValueError as error:
            errors.append(str(error))
            continue
        if access_start_date and access_end_date and access_end_date < access_start_date:
            errors.append(f"row {row_number}: access_end_date is before access_start_date")
            continue

        source_key = grant_source_key(client_id, email, product_id, talent_id)
        grant = PermissionGrant(
            source_key=source_key,
            source_client_id=client_id,
            email=email,
            product_id=product_id,
            talent_id=talent_id,
            role=role,
            access_start_date=access_start_date,
            access_end_date=access_end_date,
        )
        previous = grants.get(source_key)
        if previous and previous != grant:
            errors.append(f"row {row_number}: conflicting duplicate permission grant")
            continue
        grants[source_key] = grant

    if not grants:
        errors.append("the source produced zero active permission grants")

    used_talent_ids = {grant.talent_id for grant in grants.values()}
    unused_count = len(talents) - len(used_talent_ids)
    if unused_count:
        warnings.append(f"{unused_count} active talents currently have no permission grants")

    return SyncPlan(
        talents=tuple(sorted(talents.values(), key=lambda talent: talent.id)),
        grants=tuple(sorted(grants.values(), key=lambda grant: grant.source_key)),
        warnings=tuple(warnings),
        errors=tuple(errors),
    )


def sql_literal(value: str | None) -> str:
    if value is None:
        return "NULL"
    return "'" + value.replace("'", "''") + "'"


def render_sync_sql(plan: SyncPlan, *, source: str) -> str:
    statements = ["PRAGMA foreign_keys = ON;"]
    for talent in plan.talents:
        statements.append(
            "INSERT INTO talents (id, display_name, active, updated_at) "
            f"VALUES ({sql_literal(talent.id)}, {sql_literal(talent.display_name)}, 1, CURRENT_TIMESTAMP) "
            "ON CONFLICT(id) DO UPDATE SET "
            "display_name = excluded.display_name, active = 1, updated_at = CURRENT_TIMESTAMP;"
        )

    for email in sorted({grant.email for grant in plan.grants}):
        statements.append(
            "INSERT INTO users (email, active, updated_at) "
            f"VALUES ({sql_literal(email)}, 1, CURRENT_TIMESTAMP) "
            "ON CONFLICT(email) DO UPDATE SET active = 1, updated_at = CURRENT_TIMESTAMP;"
        )

    statements.append(
        f"DELETE FROM permission_grants WHERE source = {sql_literal(source)};"
    )
    for grant in plan.grants:
        statements.append(
            "INSERT INTO permission_grants ("
            "source, source_key, source_client_id, user_id, product_id, talent_id, role, "
            "access_start_date, access_end_date, active, updated_at"
            ") SELECT "
            f"{sql_literal(source)}, {sql_literal(grant.source_key)}, "
            f"{sql_literal(grant.source_client_id)}, users.id, "
            f"{sql_literal(grant.product_id)}, {sql_literal(grant.talent_id)}, "
            f"{sql_literal(grant.role)}, {sql_literal(grant.access_start_date)}, "
            f"{sql_literal(grant.access_end_date)}, 1, CURRENT_TIMESTAMP "
            f"FROM users WHERE users.email = {sql_literal(grant.email)} COLLATE NOCASE;"
        )

    synced_at = datetime.now(timezone.utc).replace(microsecond=0).isoformat()
    statements.append(
        "INSERT INTO permission_sync_runs "
        "(source, synced_at, user_count, grant_count) VALUES ("
        f"{sql_literal(source)}, {sql_literal(synced_at)}, "
        f"{plan.user_count}, {len(plan.grants)});"
    )
    return "\n".join(statements) + "\n"


def mask_email(email: str) -> str:
    local, domain = email.split("@", maxsplit=1)
    visible = local[:1]
    return f"{visible}{'*' * max(3, len(local) - 1)}@{domain}"


def plan_summary(plan: SyncPlan) -> list[str]:
    talent_names = {talent.id: talent.display_name for talent in plan.talents}
    grants_by_email: dict[str, list[str]] = {}
    for grant in plan.grants:
        grants_by_email.setdefault(grant.email, []).append(talent_names[grant.talent_id])

    lines = [
        f"Users: {plan.user_count}",
        f"Talent grants: {len(plan.grants)}",
        f"Canonical talents: {len(plan.talents)}",
    ]
    for email, names in sorted(grants_by_email.items()):
        lines.append(f"  {mask_email(email)}: {', '.join(sorted(set(names)))}")
    return lines
