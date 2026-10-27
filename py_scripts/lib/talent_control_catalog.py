"""Read canonical identities for lifecycle administration; never writes the catalog."""

import re

from py_scripts.lib.talent_lifecycle.policy import CODE_PATTERN


def read_catalog(database_path):
    """Return codes, channel IDs and exact aliases from the existing DuckDB catalog."""
    import duckdb

    with duckdb.connect(str(database_path), read_only=True) as connection:
        talents = connection.execute(
            "SELECT talent_code, talent_name FROM catalog.talents ORDER BY talent_code"
        ).fetchall()
        channels = connection.execute(
            "SELECT talent_code, channel_id, channel_name FROM catalog.channels"
        ).fetchall()
        aliases = connection.execute(
            "SELECT talent_code, alias FROM catalog.talent_aliases"
        ).fetchall()
    catalog = {}
    for code, name in talents:
        if not isinstance(code, str) or not CODE_PATTERN.fullmatch(code) or code in catalog:
            raise RuntimeError("Invalid or duplicate canonical talent identity")
        catalog[code] = {"talent_code": code, "display_name": name,
                         "channel_ids": [], "aliases": [name]}
    if not catalog:
        raise RuntimeError("Canonical talent catalog is empty")
    for code, channel_id, name in channels:
        if code in catalog:
            catalog[code]["channel_ids"].append(channel_id)
            catalog[code]["aliases"].append(name)
    for code, alias in aliases:
        if code in catalog:
            catalog[code]["aliases"].append(alias)
    for talent in catalog.values():
        names = [name for name in talent["aliases"] if isinstance(name, str) and name]
        talent["aliases"] = sorted(set(names + [re.sub(r'[<>:"/\\|?*]', "_", name) for name in names]))
        talent["channel_ids"] = sorted(set(talent["channel_ids"]))
    return catalog
