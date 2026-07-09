#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import sys
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

SCRIPT_REPO_ROOT = Path(__file__).resolve().parents[3]
if str(SCRIPT_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(SCRIPT_REPO_ROOT))

from py_scripts.lib.qualitative_coding.codebook import CodeDef, load_codebook, resolve_codebook
from py_scripts.lib.qualitative_coding.csv_io import read_csv_dicts
from py_scripts.lib.qualitative_coding.prepared_transcripts import (
    TargetFile,
    filter_transcripts,
    list_prepared_transcript_files,
    row_id_for,
    row_needs_coding,
)
from py_scripts.lib.qualitative_coding.text import squish
from py_scripts.lib.utils.paths import (
    REPO_ROOT,
    default_talent_root,
    qualitative_batch_runs_root,
    qualitative_run_code_sets_root,
    resolve_talent_paths,
)
from py_scripts.lib.utils.paths import TalentPaths, talent_slugify, translate_datalake_path


DEFAULT_MODEL = "gpt-5-mini"
DEFAULT_ENDPOINT = "/v1/responses"
DEFAULT_BATCH_SIZE = 50
METHODOLOGY_MODE = "chronological_every_row_qualitative_coding_v1"


CODING_RESPONSE_SCHEMA = {
    "type": "object",
    "additionalProperties": False,
    "properties": {
        "coded_rows": {
            "type": "array",
            "items": {
                "type": "object",
                "additionalProperties": False,
                "properties": {
                    "row_id": {"type": "string"},
                    "codes": {
                        "type": "array",
                        "items": {"type": "string"},
                    },
                    "confidence": {"type": "string", "enum": ["high", "medium", "low"]},
                    "needs_review": {"type": "boolean"},
                    "review_reason": {"type": ["string", "null"]},
                },
                "required": ["row_id", "codes", "confidence", "needs_review", "review_reason"],
            },
        },
        "batch_notes": {"type": ["string", "null"]},
    },
    "required": ["coded_rows", "batch_notes"],
}


def make_run_id(prefix: str = "qualitative_batch") -> str:
    now = datetime.now(ZoneInfo("America/New_York"))
    return f"{prefix}_{now.strftime('%Y-%m-%d_%H-%M-%S_%z')}"


def write_json(path: Path, data: object) -> None:
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def write_csv(path: Path, fieldnames: list[str], rows: list[dict[str, object]]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def safe_count_chars(value: object) -> int:
    return len(str(value or ""))


def selection_metadata_path(codebook_path: Path) -> Path | None:
    if codebook_path.name.endswith("_codebook.csv"):
        candidate = codebook_path.with_name(codebook_path.name.replace("_codebook.csv", "_selection.csv"))
        if candidate.exists():
            return candidate
    return None


def load_selection_metadata(codebook_path: Path) -> dict[str, dict[str, str]]:
    path = selection_metadata_path(codebook_path)
    if path is None:
        return {}
    _, rows = read_csv_dicts(path)
    metadata: dict[str, dict[str, str]] = {}
    for row in rows:
        code_id = squish(row.get("code_id", ""))
        if not code_id:
            continue
        row_metadata = {
            "row_source_scope": squish(row.get("row_source_scope", "")),
            "source_group": squish(row.get("source_group", "")),
        }
        metadata[code_id] = {key: value for key, value in row_metadata.items() if value}
    return metadata


def prompt_row(path: Path, row: dict[str, str], index: int) -> dict[str, object]:
    fields = (
        "source",
        "speaker",
        "text",
        "timecode",
        "paid_amount_text",
        "paid_amount_value",
        "paid_currency",
    )
    out: dict[str, object] = {
        "row_id": row_id_for(path, row, index),
        "row_number": index + 1,
    }
    for field in fields:
        if field in row:
            out[field] = squish(row.get(field, ""))
    return out


def select_pending_rows(
    target: TargetFile,
    code_columns: list[str],
    row_limit: int,
    context_rows: int,
    reprocess: bool,
) -> list[dict[str, object]]:
    fields, rows = read_csv_dicts(target.path)
    missing_columns = tuple(col for col in code_columns if col not in fields)
    selected: list[dict[str, object]] = []
    for index, row in enumerate(rows):
        if not row_needs_coding(row, code_columns, missing_columns, reprocess=reprocess):
            continue
        start = max(0, index - context_rows)
        selected.append(
            {
                "csv_path": str(target.path),
                "row_index": index,
                "row_number": index + 1,
                "row_id": row_id_for(target.path, row, index),
                "source_file": squish(row.get("source_file", target.path.name)),
                "video_id": squish(row.get("video_id", target.path.stem)),
                "sec": squish(row.get("sec", "")),
                "timecode": squish(row.get("timecode", "")),
                "source": squish(row.get("source", "")),
                "speaker": squish(row.get("speaker", "")),
                "speaker_type": squish(row.get("speaker_type", "")),
                "message_type": squish(row.get("message_type", "")),
                "paid_amount_text": squish(row.get("paid_amount_text", "")),
                "paid_amount_value": squish(row.get("paid_amount_value", "")),
                "paid_currency": squish(row.get("paid_currency", "")),
                "text": squish(row.get("text", "")),
                "candidate_reason": "pending_code_values",
                "current_row": prompt_row(target.path, row, index),
                "context_rows": [prompt_row(target.path, rows[i], i) for i in range(start, index)],
            }
        )
        if row_limit > 0 and len(selected) >= row_limit:
            break
    return selected


def chunked(values: list[dict[str, object]], size: int) -> list[list[dict[str, object]]]:
    return [values[i : i + size] for i in range(0, len(values), size)]


def codebook_payload(code_defs: list[CodeDef], selection_metadata: dict[str, dict[str, str]]) -> list[dict[str, str]]:
    payload: list[dict[str, str]] = []
    for code in code_defs:
        item = code.compact_dict()
        metadata = selection_metadata.get(code.code_id, {})
        if metadata:
            item.update(metadata)
        item = {key: value for key, value in item.items() if value != ""}
        payload.append(item)
    return payload


def custom_id_for(talent_slug: str, candidate_batch: list[dict[str, object]], run_id: str, part_number: int) -> str:
    first = candidate_batch[0]
    last = candidate_batch[-1]
    video_id = str(first["row_id"]).rsplit("_", 1)[0]
    return (
        f"{talent_slug}__{video_id}__rows_"
        f"{int(first['row_number']):06d}_{int(last['row_number']):06d}__{run_id}__part_{part_number:04d}"
    )


def build_request(
    custom_id: str,
    model: str,
    endpoint: str,
    compact_codebook: list[dict[str, str]],
    candidate_batch: list[dict[str, object]],
) -> dict[str, object]:
    target_rows = [
        {
            "row_id": row["row_id"],
            "row_number": row["row_number"],
            "previous_context_rows": row["context_rows"],
            "current_row": row["current_row"],
        }
        for row in candidate_batch
    ]
    return {
        "custom_id": custom_id,
        "method": "POST",
        "url": endpoint,
        "body": {
            "model": model,
            "text": {
                "format": {
                    "type": "json_schema",
                    "name": "qualitative_coding_decisions",
                    "strict": True,
                    "schema": CODING_RESPONSE_SCHEMA,
                }
            },
            "input": [
                {
                    "role": "system",
                    "content": (
                        "You are a qualitative coding assistant. Return structured coding decisions only. "
                        "Work as a qualitative analyst reading transcript rows in chronological order. Code every "
                        "row listed in target_rows, including rows with no positive codes. Context rows are evidence "
                        "only and must never be returned as coded_rows. Use the supplied code definitions as the "
                        "authority for what part of the stream interaction each code analyzes. If a code explicitly "
                        "includes row_source_scope, use it as a hard source restriction; otherwise do not infer a "
                        "streamer-only or chat-only restriction from metadata. For monetary-conversation coding, prior context may establish "
                        "that the current row is interactionally adjacent to a paid event, gift, membership, goal, donor "
                        "action, or streamer monetary acknowledgment. If the current row performs a supplied code's "
                        "interactional function in response to that established event, code it even when the current row "
                        "does not repeat explicit money terms. Keep the current-row rule: context establishes the anchor, "
                        "but the current row must perform the coded function. Return positive code_ids only; absent codes "
                        "are treated as zero."
                    ),
                },
                {
                    "role": "user",
                    "content": json.dumps(
                        {
                            "codebook": compact_codebook,
                            "target_rows": target_rows,
                            "methodology": {
                                "mode": METHODOLOGY_MODE,
                                "interpret_every_target_row": True,
                                "preserve_chronological_context": True,
                                "no_auto_zero": True,
                                "no_duplicate_reuse": True,
                                "repeated_text_may_have_context_specific_meaning": True,
                            },
                            "instructions": {
                                "code_only_target_rows": True,
                                "do_not_code_previous_context_rows": True,
                                "use_code_row_source_scope_only_when_explicit": True,
                                "do_not_infer_source_restrictions_from_metadata": True,
                                "do_not_send_original_full_transcript": True,
                                "request_contains_only_target_rows_and_requested_context_rows": True,
                                "return_each_target_row_id_exactly_once": True,
                                "do_not_omit_zero_code_rows": True,
                                "do_not_return_duplicate_row_ids": True,
                                "row_id_source_of_truth": [row["row_id"] for row in candidate_batch],
                                "return_positive_code_ids_only": True,
                                "treat_absent_codes_as_zero": True,
                                "response_shape": {
                                    "coded_rows": [
                                        {
                                            "row_id": "string",
                                            "codes": ["A1", "A1b"],
                                            "confidence": "high|medium|low",
                                            "needs_review": False,
                                            "review_reason": None,
                                        }
                                    ],
                                    "batch_notes": None,
                                },
                            },
                        },
                        ensure_ascii=False,
                    ),
                },
            ],
        },
    }


def inspect_targets(paths: list[Path], code_columns: list[str]) -> list[TargetFile]:
    from py_scripts.lib.qualitative_coding.prepared_transcripts import inspect_target

    return [inspect_target(path, code_columns) for path in paths]


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


def selected_manifest_targets(path: Path, talent_root: Path, coding_folder: str) -> tuple[list[TalentPaths], list[Path], list[str]]:
    fields, rows = read_csv_dicts(path)
    if "source_path" not in fields:
        raise SystemExit(f"Selected transcripts CSV must contain source_path: {path}")
    talents_by_path: dict[Path, TalentPaths] = {}
    prepared_paths: list[Path] = []
    source_paths: list[str] = []
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
        talent = infer_talent_from_source_path(source_path, talent_root, coding_folder)
        talents_by_path[talent.talent_path] = talent
        prepared_paths.append(talent.qualitative_prep_dir / source_path.name)
        source_paths.append(str(source_path))
    if not prepared_paths:
        raise SystemExit(f"No source_path rows found in selected transcripts CSV: {path}")
    missing_prepared = [str(path) for path in prepared_paths if not path.exists()]
    if missing_prepared:
        raise SystemExit(
            "Prepared transcript CSVs are missing. Run without --skip-prepare first. Missing: "
            + "; ".join(missing_prepared[:10])
        )
    talents = sorted(talents_by_path.values(), key=lambda talent: talent.talent_name)
    return talents, prepared_paths, source_paths


def write_qa_summary(path: Path, manifest: dict[str, object]) -> None:
    lines = [
        "# Qualitative Batch Build Summary",
        "",
        f"- run_id: {manifest['run_id']}",
        f"- created_at: {manifest['created_at']}",
        f"- talent_query: {manifest['talent_query']}",
        f"- codebook: {manifest['resolved_codebook_path']}",
        f"- selected_transcripts: {manifest['selected_transcript_count']}",
        f"- candidate_rows: {manifest['candidate_row_count']}",
        f"- auto_zero_rows: {manifest['auto_zero_row_count']}",
        f"- request_count: {manifest['request_count']}",
        f"- run_code_set: {manifest['artifacts']['run_code_set_csv']}",
        f"- batch_input: {manifest['artifacts']['batch_input_jsonl']}",
        "",
        "No API job was submitted by this build step.",
    ]
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(description="Build dry-run Batch API input artifacts for qualitative coding.")
    parser.add_argument("--talent-query", default="Avaritia")
    parser.add_argument("--coding-folder", default="monetary conversation codes")
    parser.add_argument("--codebook", default="current")
    parser.add_argument("--selected-transcripts-csv", type=Path, default=None)
    parser.add_argument("--transcript", default="")
    parser.add_argument("--limit", type=int, default=0, help="Maximum number of prepared transcript CSVs to include.")
    parser.add_argument("--row-limit", type=int, default=25, help="Maximum pending rows per selected transcript.")
    parser.add_argument("--batch-size", type=int, default=DEFAULT_BATCH_SIZE, help="Rows per JSONL request.")
    parser.add_argument("--context-rows", type=int, default=4)
    parser.add_argument("--model", default=DEFAULT_MODEL)
    parser.add_argument("--endpoint", default=DEFAULT_ENDPOINT)
    parser.add_argument("--run-id", default="")
    parser.add_argument("--run-root", type=Path, default=None)
    parser.add_argument("--allow-existing-run-dir", action="store_true")
    parser.add_argument("--reprocess", action="store_true")
    args = parser.parse_args()

    if args.limit < 0 or args.row_limit < 0 or args.batch_size <= 0 or args.context_rows < 0:
        raise SystemExit("--limit, --row-limit, and --context-rows must be non-negative; --batch-size must be positive.")

    talent_root = default_talent_root()
    run_root = args.run_root or qualitative_batch_runs_root(talent_root)
    run_id = args.run_id.strip() or make_run_id()
    run_dir = run_root / run_id
    run_dir.mkdir(parents=True, exist_ok=args.allow_existing_run_dir)

    codebook_path = resolve_codebook(args.codebook, talent_root)
    code_defs = load_codebook(codebook_path)
    code_columns = [code.code_column for code in code_defs]
    selection_metadata = load_selection_metadata(codebook_path)
    compact_codebook = codebook_payload(code_defs, selection_metadata)

    selected_source_paths: list[str] = []
    if args.selected_transcripts_csv:
        talents, transcript_files, selected_source_paths = selected_manifest_targets(
            args.selected_transcripts_csv,
            talent_root,
            args.coding_folder,
        )
    else:
        talents = resolve_talent_paths(args.talent_query, talent_root, coding_folder=args.coding_folder)
        transcript_files = filter_transcripts(list_prepared_transcript_files(talents), args.transcript or None, talent_root)
    targets = inspect_targets(transcript_files, code_columns)
    pending_targets = [target for target in targets if args.reprocess or target.pending_rows > 0]
    if args.limit > 0:
        pending_targets = pending_targets[: args.limit]

    candidate_rows: list[dict[str, object]] = []
    for target in pending_targets:
        candidate_rows.extend(
            select_pending_rows(
                target=target,
                code_columns=code_columns,
                row_limit=args.row_limit,
                context_rows=args.context_rows,
                reprocess=args.reprocess,
            )
        )

    request_rows = []
    custom_id_map: dict[str, dict[str, object]] = {}
    compact_batches = chunked(candidate_rows, args.batch_size)
    talent_slug = "multi_talent" if args.selected_transcripts_csv else (talents[0].talent_slug if talents else "talent")
    for part_number, candidate_batch in enumerate(compact_batches, start=1):
        custom_id = custom_id_for(talent_slug, candidate_batch, run_id, part_number)
        request = build_request(custom_id, args.model, args.endpoint, compact_codebook, candidate_batch)
        request_rows.append(request)
        custom_id_map[custom_id] = {
            "csv_paths": sorted({str(row["csv_path"]) for row in candidate_batch}),
            "row_ids": [row["row_id"] for row in candidate_batch],
            "row_numbers": [row["row_number"] for row in candidate_batch],
        }

    compiled_codebook_path = run_dir / "compiled_codebook.json"
    run_code_set_path = run_dir / "run_code_set.csv"
    candidate_rows_path = run_dir / "candidate_rows.csv"
    auto_zero_path = run_dir / "auto_zero_candidates.csv"
    batch_input_path = run_dir / "batch_input.jsonl"
    cost_estimate_path = run_dir / "cost_estimate.json"
    manifest_path = run_dir / "manifest.json"
    qa_summary_path = run_dir / "qa_summary.md"

    write_json(compiled_codebook_path, compact_codebook)
    code_set_fields = [
        "source_group",
        "row_source_scope",
        "code_id",
        "code_column",
        "primary_code_id",
        "primary_code",
        "secondary_code_id",
        "secondary_code",
        "parent_code_id",
        "definition",
        "examples_from_text",
    ]
    write_csv(run_code_set_path, code_set_fields, compact_codebook)
    central_code_set_dir = qualitative_run_code_sets_root(talent_root) / run_id
    central_code_set_dir.mkdir(parents=True, exist_ok=args.allow_existing_run_dir)
    central_run_code_set_path = central_code_set_dir / "run_code_set.csv"
    write_csv(central_run_code_set_path, code_set_fields, compact_codebook)
    write_csv(
        candidate_rows_path,
        [
            "csv_path",
            "source_file",
            "video_id",
            "row_number",
            "row_id",
            "sec",
            "timecode",
            "source",
            "speaker",
            "speaker_type",
            "message_type",
            "paid_amount_text",
            "paid_amount_value",
            "paid_currency",
            "candidate_reason",
            "text",
        ],
        candidate_rows,
    )
    write_csv(
        auto_zero_path,
        ["csv_path", "row_number", "row_id", "auto_zero_reason", "text"],
        [],
    )
    with batch_input_path.open("w", encoding="utf-8") as handle:
        for request in request_rows:
            handle.write(json.dumps(request, ensure_ascii=False) + "\n")

    input_chars = sum(safe_count_chars(json.dumps(request, ensure_ascii=False)) for request in request_rows)
    cost_estimate = {
        "request_count": len(request_rows),
        "candidate_row_count": len(candidate_rows),
        "input_characters": input_chars,
        "rough_input_tokens": round(input_chars / 4),
        "model": args.model,
        "note": "Token count is a rough character-based estimate; no API job was submitted.",
    }
    write_json(cost_estimate_path, cost_estimate)

    manifest: dict[str, object] = {
        "run_id": run_id,
        "created_at": datetime.now(ZoneInfo("America/New_York")).isoformat(),
        "repo_root": str(REPO_ROOT),
        "talent_query": args.talent_query,
        "selected_transcripts_csv": str(args.selected_transcripts_csv or ""),
        "resolved_talents": [
            {
                "talent_name": talent.talent_name,
                "talent_slug": talent.talent_slug,
                "talent_path": str(talent.talent_path),
                "qualitative_prep_dir": str(talent.qualitative_prep_dir),
            }
            for talent in talents
        ],
        "coding_folder": args.coding_folder,
        "transcript_selector": args.transcript,
        "selected_transcript_count": len(pending_targets),
        "selected_transcripts": [str(target.path) for target in pending_targets],
        "selected_source_transcripts": selected_source_paths,
        "codebook_selector": args.codebook,
        "resolved_codebook_path": str(codebook_path),
        "selection_metadata_path": str(selection_metadata_path(codebook_path) or ""),
        "model": args.model,
        "endpoint": args.endpoint,
        "candidate_gate_config": {
            "version": METHODOLOGY_MODE,
            "row_limit_per_transcript": args.row_limit,
            "candidate_reason": "pending_code_values",
            "selection_policy": "all pending rows are model candidates; no rule-based exclusion or auto-zero",
        },
        "context_window_config": {"previous_rows": args.context_rows},
        "dedupe_config": {
            "enabled": False,
            "policy": "do not reuse duplicate text decisions; repeated text may have context-specific meaning",
        },
        "auto_zero_config": {
            "enabled": False,
            "policy": "do not auto-zero non-candidates in methodology-preserving mode",
        },
        "methodology_config": {
            "mode": METHODOLOGY_MODE,
            "interpret_every_pending_row": True,
            "chronological_chunking": True,
            "rows_per_request": args.batch_size,
            "cost_optimization_strategy": "batch API plus larger chronological chunks; minimal request rows; no duplicate reuse or auto-zero",
            "code_applicability": "code definitions control applicability; row_source_scope is a hard restriction only when explicitly provided",
        },
        "request_count": len(request_rows),
        "row_count_total": sum(target.row_count for target in pending_targets),
        "candidate_row_count": len(candidate_rows),
        "auto_zero_row_count": 0,
        "duplicate_reuse_count": 0,
        "custom_id_map": custom_id_map,
        "batch_id": None,
        "input_file_id": None,
        "output_file_id": None,
        "error_file_id": None,
        "apply_status": "not_applicable_build_only",
        "artifacts": {
            "manifest": str(manifest_path),
            "compiled_codebook": str(compiled_codebook_path),
            "run_code_set_csv": str(run_code_set_path),
            "central_run_code_set_csv": str(central_run_code_set_path),
            "candidate_rows_csv": str(candidate_rows_path),
            "auto_zero_candidates_csv": str(auto_zero_path),
            "batch_input_jsonl": str(batch_input_path),
            "cost_estimate_json": str(cost_estimate_path),
            "qa_summary_md": str(qa_summary_path),
        },
    }
    write_json(manifest_path, manifest)
    write_qa_summary(qa_summary_path, manifest)

    print(f"run_dir: {run_dir}")
    print(f"request_count: {len(request_rows)}")
    print(f"candidate_row_count: {len(candidate_rows)}")
    print("submitted: false")
    return 0


if __name__ == "__main__":
    sys.exit(main())
