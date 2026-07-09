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
from py_scripts.lib.utils.paths import TalentPaths, default_talent_root, resolve_talent_paths, talent_slugify, translate_datalake_path


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


def infer_talent_from_source_path(source_path: Path, talent_root: Path, coding_folder: str) -> TalentPaths:
    try:
        relative = source_path.resolve().relative_to(talent_root.resolve())
    except ValueError as exc:
        raise SystemExit(f"Selected transcript source_path is not under talent root: {source_path}") from exc
    parts = relative.parts
    if len(parts) < 3 or parts[1] != "text_playback":
        raise SystemExit(f"Selected transcript source_path must point under <talent>/text_playback: {source_path}")
    talent_path = talent_root / parts[0]
    talent_name = talent_path.name
    return TalentPaths(
        talent_name=talent_name,
        talent_slug=talent_slugify(talent_name),
        talent_path=talent_path,
        text_playback_path=talent_path / "text_playback",
        qualitative_coding_root=talent_path / "qualitative coding",
        qualitative_prep_dir=talent_path / "qualitative coding" / coding_folder,
    )


def selected_manifest_sources(path: Path, talent_root: Path, coding_folder: str) -> list[tuple[TalentPaths, Path]]:
    fields, rows = read_csv_dicts(path)
    if "source_path" not in fields:
        raise SystemExit(f"Selected transcripts CSV must contain source_path: {path}")
    selected: list[tuple[TalentPaths, Path]] = []
    seen: set[Path] = set()
    for row in rows:
        raw_source = row.get("source_path", "")
        if not raw_source.strip():
            continue
        source_path = translate_datalake_path(raw_source, talent_root).resolve()
        if source_path in seen:
            continue
        seen.add(source_path)
        if not source_path.exists():
            raise SystemExit(f"Selected transcript source_path does not exist: {source_path}")
        selected.append((infer_talent_from_source_path(source_path, talent_root, coding_folder), source_path))
    if not selected:
        raise SystemExit(f"No source_path rows found in selected transcripts CSV: {path}")
    return selected


def main() -> int:
    parser = argparse.ArgumentParser(description="Prepare transcript CSVs for qualitative coding using Python.")
    parser.add_argument("--talent-query", default="Avaritia")
    parser.add_argument("--coding-folder", default="monetary conversation codes")
    parser.add_argument("--codebook", default="current")
    parser.add_argument("--selected-transcripts-csv", type=Path, default=None)
    parser.add_argument("--datalake-root", type=Path, default=None)
    parser.add_argument("--reprocess", action="store_true", help="Overwrite existing prepared CSVs.")
    parser.add_argument("--dry-run", action="store_true", help="Report planned writes without creating or editing files.")
    parser.add_argument("--prep-index", type=Path, default=None, help="Optional CSV report of selected source/output files.")
    args = parser.parse_args()

    talent_root = args.datalake_root or default_talent_root()
    codebook_path = resolve_codebook(args.codebook, talent_root)
    code_defs = load_codebook(codebook_path)
    code_columns = [code.code_column for code in code_defs]
    selected_sources = (
        selected_manifest_sources(args.selected_transcripts_csv, talent_root, args.coding_folder)
        if args.selected_transcripts_csv
        else None
    )
    talents = (
        sorted({item[0] for item in selected_sources}, key=lambda talent: talent.talent_name)
        if selected_sources
        else resolve_talent_paths(
            args.talent_query,
            talent_root=talent_root,
            coding_folder=args.coding_folder,
            must_have_text_playback=True,
        )
    )

    report_rows: list[dict[str, object]] = []
    total_sources = 0
    total_written = 0
    total_skipped = 0

    if selected_sources:
        source_pairs = selected_sources
    else:
        source_pairs = [
            (talent, source_path)
            for talent in talents
            for source_path in list_source_transcripts(talent.text_playback_path)
        ]

    for talent, source_path in source_pairs:
        if not selected_sources:
            total_sources += 1
        else:
            total_sources = len(source_pairs)
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
    print(f"selected_transcripts_csv: {args.selected_transcripts_csv or ''}")
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
