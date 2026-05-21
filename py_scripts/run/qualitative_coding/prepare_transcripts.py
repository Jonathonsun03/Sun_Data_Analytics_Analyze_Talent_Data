#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import sys
from pathlib import Path

SCRIPT_REPO_ROOT = Path(__file__).resolve().parents[3]
if str(SCRIPT_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(SCRIPT_REPO_ROOT))

from py_scripts.lib.qualitative_coding.codebook import load_codebook, resolve_codebook
from py_scripts.lib.qualitative_coding.csv_io import read_csv_dicts, write_csv_dicts
from py_scripts.lib.utils.paths import default_talent_root, resolve_talent_paths


def list_source_transcripts(text_playback_path: Path) -> list[Path]:
    if not text_playback_path.exists():
        return []
    return sorted(text_playback_path.glob("*.csv"))


def prepare_rows(source_path: Path, code_columns: list[str]) -> tuple[list[str], list[dict[str, str]]]:
    source_fields, source_rows = read_csv_dicts(source_path)
    metadata_fields = ["source_file", "source_path"]
    original_fields = [field for field in source_fields if field not in metadata_fields and field not in code_columns]
    fieldnames = metadata_fields + original_fields + code_columns

    prepared_rows: list[dict[str, str]] = []
    normalized_source_path = str(source_path.resolve())
    for source_row in source_rows:
        out = {
            "source_file": source_path.name,
            "source_path": normalized_source_path,
        }
        for field in original_fields:
            out[field] = source_row.get(field, "")
        for code_column in code_columns:
            out[code_column] = ""
        prepared_rows.append(out)
    return fieldnames, prepared_rows


def prepare_transcript(source_path: Path, output_path: Path, code_columns: list[str]) -> int:
    fieldnames, rows = prepare_rows(source_path, code_columns)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    write_csv_dicts(output_path, fieldnames, rows)
    return len(rows)


def write_prep_index(path: Path, rows: list[dict[str, object]]) -> None:
    fieldnames = [
        "talent_name",
        "source_path",
        "output_path",
        "source_rows",
        "output_exists_before",
        "action",
    ]
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    parser = argparse.ArgumentParser(description="Prepare transcript CSVs for qualitative coding using Python.")
    parser.add_argument("--talent-query", default="Avaritia")
    parser.add_argument("--coding-folder", default="monetary conversation codes")
    parser.add_argument("--codebook", default="current")
    parser.add_argument("--datalake-root", type=Path, default=None)
    parser.add_argument("--reprocess", action="store_true", help="Overwrite existing prepared CSVs.")
    parser.add_argument("--dry-run", action="store_true", help="Report planned writes without creating or editing files.")
    parser.add_argument("--prep-index", type=Path, default=None, help="Optional CSV report of selected source/output files.")
    args = parser.parse_args()

    talent_root = args.datalake_root or default_talent_root()
    codebook_path = resolve_codebook(args.codebook, talent_root)
    code_defs = load_codebook(codebook_path)
    code_columns = [code.code_column for code in code_defs]
    talents = resolve_talent_paths(
        args.talent_query,
        talent_root=talent_root,
        coding_folder=args.coding_folder,
        must_have_text_playback=True,
    )

    report_rows: list[dict[str, object]] = []
    total_sources = 0
    total_written = 0
    total_skipped = 0

    for talent in talents:
        source_paths = list_source_transcripts(talent.text_playback_path)
        total_sources += len(source_paths)
        for source_path in source_paths:
            output_path = talent.qualitative_prep_dir / source_path.name
            exists_before = output_path.exists()
            should_write = args.reprocess or not exists_before
            action = "write" if should_write else "skip_existing"
            source_row_count = 0
            if should_write:
                _, source_rows = read_csv_dicts(source_path)
                source_row_count = len(source_rows)
                if not args.dry_run:
                    source_row_count = prepare_transcript(source_path, output_path, code_columns)
                total_written += 1
            else:
                total_skipped += 1
            report_rows.append(
                {
                    "talent_name": talent.talent_name,
                    "source_path": str(source_path),
                    "output_path": str(output_path),
                    "source_rows": source_row_count,
                    "output_exists_before": str(exists_before).lower(),
                    "action": action if not args.dry_run else f"dry_run_{action}",
                }
            )

    if args.prep_index:
        if not args.dry_run:
            args.prep_index.parent.mkdir(parents=True, exist_ok=True)
        write_prep_index(args.prep_index, report_rows)

    print(f"talent_query: {args.talent_query}")
    print(f"codebook_path: {codebook_path}")
    print(f"coding_folder: {args.coding_folder}")
    print(f"dry_run: {str(args.dry_run).lower()}")
    print(f"selected_talents: {len(talents)}")
    print(f"source_transcript_files: {total_sources}")
    print(f"prepared_files_written: {total_written}")
    print(f"prepared_files_skipped_existing: {total_skipped}")
    for talent in talents:
        print(f"- talent: {talent.talent_name}")
        print(f"  text_playback_path: {talent.text_playback_path}")
        print(f"  qualitative_prep_dir: {talent.qualitative_prep_dir}")

    return 0


if __name__ == "__main__":
    sys.exit(main())

