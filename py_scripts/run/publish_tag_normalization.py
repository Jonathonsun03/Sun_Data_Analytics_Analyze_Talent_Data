#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import sys
from datetime import datetime, timezone
from pathlib import Path
from uuid import uuid4

import duckdb
import pandas as pd


REPO_ROOT = next(
    parent for parent in Path(__file__).resolve().parents if (parent / ".git").exists()
)
sys.path.insert(0, str(REPO_ROOT))

from py_scripts.lib.tag_normalization import build_tag_mapping
from py_scripts.lib.utils.paths import unified_database_path
from py_scripts.lib.utils.sql_queries import load_sql_query


DICTIONARY_ID = "title_tags"
METHOD = "deterministic_rapidfuzz_v1"
FUZZY_SCORE_CUTOFF = 86.0
FUZZY_HIGH_CONFIDENCE_CUTOFF = 96.0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build and publish the current title-tag normalization dictionary."
    )
    parser.add_argument(
        "--execute",
        action="store_true",
        help="Publish and activate the mapping.",
    )
    parser.add_argument("--db-path", type=Path, default=None)
    return parser.parse_args()


def extract_tags(rows: pd.DataFrame) -> pd.Series:
    tags: list[str] = []
    for value in rows["classification_json"]:
        try:
            payload = json.loads(str(value))
        except (TypeError, ValueError, json.JSONDecodeError):
            continue
        values = payload.get("tags", []) if isinstance(payload, dict) else []
        if not isinstance(values, list):
            continue
        tags.extend(str(tag).strip() for tag in values if str(tag).strip())
    return pd.Series(tags, dtype="string")


def mapping_checksum(mapping: pd.DataFrame, config: dict[str, object]) -> str:
    payload = {
        "config": config,
        "mapping": mapping.fillna("").to_dict(orient="records"),
    }
    encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def main() -> None:
    args = parse_args()
    db_path = args.db_path or unified_database_path()
    con = duckdb.connect(str(db_path), read_only=not args.execute)
    try:
        source = con.execute(
            load_sql_query("unified_db", "normalization", "current_title_tags.sql")
        ).fetchdf()
        title_version_ids = sorted(source["title_version_id"].dropna().unique())
        source_title_version_id = (
            str(title_version_ids[0]) if len(title_version_ids) == 1 else None
        )
        tags = extract_tags(source)
        mapping = build_tag_mapping(
            tags,
            fuzzy_score_cutoff=FUZZY_SCORE_CUTOFF,
            fuzzy_high_confidence_cutoff=FUZZY_HIGH_CONFIDENCE_CUTOFF,
        )
        config = {
            "method": METHOD,
            "fuzzy_score_cutoff": FUZZY_SCORE_CUTOFF,
            "fuzzy_high_confidence_cutoff": FUZZY_HIGH_CONFIDENCE_CUTOFF,
            "source_title_version_id": source_title_version_id,
        }
        checksum = mapping_checksum(mapping, config)
        dictionary_version = f"title-tags-{checksum[:12]}"
        changed = int(mapping["normalization_method"].ne("unchanged").sum())
        canonical_count = int(mapping["canonical_label"].nunique())

        print(f"Database: {db_path}")
        print(f"Current classified titles: {len(source)}")
        print(f"Raw tag assignments: {len(tags)}")
        print(f"Distinct raw labels: {len(mapping)}")
        print(f"Canonical labels: {canonical_count}")
        print(f"Changed mappings: {changed}")
        print(f"Dictionary version: {dictionary_version}")

        if not args.execute:
            print("Preview only; rerun with --execute to publish.")
            return
        if mapping.empty:
            raise RuntimeError("No current title tags were available to publish.")

        pipeline_run_id = (
            "tag_normalization_"
            + datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S")
            + "_"
            + uuid4().hex[:12]
        )
        con.execute("BEGIN")
        try:
            con.execute(
                load_sql_query(
                    "unified_db",
                    "normalization",
                    "ensure_label_dictionary.sql",
                )
            )
            existing = con.execute(
                """
                SELECT COUNT(*)
                FROM normalization.label_dictionaries
                WHERE dictionary_id = ?
                  AND mapping_checksum = ?
                  AND active
                """,
                [DICTIONARY_ID, checksum],
            ).fetchone()[0]
            if existing:
                con.execute("ROLLBACK")
                print("Active mapping is already current; no database changes made.")
                return

            con.execute(
                """
                INSERT INTO ops.pipeline_runs (
                  pipeline_run_id, pipeline_name, started_at, status
                ) VALUES (?, 'tag_normalization', CURRENT_TIMESTAMP, 'running')
                """,
                [pipeline_run_id],
            )
            con.execute(
                """
                UPDATE normalization.label_dictionaries
                SET active = FALSE
                WHERE dictionary_id = ? AND active
                """,
                [DICTIONARY_ID],
            )
            con.execute(
                """
                INSERT INTO normalization.label_dictionaries (
                  dictionary_id, dictionary_version, label_type, method,
                  config_json, source_title_version_id, mapping_checksum,
                  active, pipeline_run_id
                ) VALUES (?, ?, 'title_tag', ?, ?, ?, ?, TRUE, ?)
                """,
                [
                    DICTIONARY_ID,
                    dictionary_version,
                    METHOD,
                    json.dumps(config, sort_keys=True),
                    source_title_version_id,
                    checksum,
                    pipeline_run_id,
                ],
            )
            publish = mapping.copy()
            publish.insert(0, "dictionary_version", dictionary_version)
            publish.insert(0, "dictionary_id", DICTIONARY_ID)
            con.register("incoming_label_mappings", publish)
            con.execute(
                """
                INSERT INTO normalization.label_mappings
                SELECT
                  dictionary_id,
                  dictionary_version,
                  raw_label,
                  normalized_label,
                  canonical_label,
                  broader_group,
                  normalization_method,
                  observed_uses,
                  rapidfuzz_component,
                  component_size
                FROM incoming_label_mappings
                """
            )
            con.unregister("incoming_label_mappings")
            con.execute(
                """
                UPDATE ops.pipeline_runs
                SET completed_at = CURRENT_TIMESTAMP, status = 'completed'
                WHERE pipeline_run_id = ?
                """,
                [pipeline_run_id],
            )
            con.execute("COMMIT")
        except Exception:
            con.execute("ROLLBACK")
            raise
        print(f"Published and activated {dictionary_version}.")
    finally:
        con.close()


if __name__ == "__main__":
    main()
