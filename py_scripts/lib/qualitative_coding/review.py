from __future__ import annotations

import json
from collections import Counter, defaultdict
from pathlib import Path
from typing import Iterable

from .csv_io import read_csv_dicts
from .text import squish


REVIEW_BASE_FIELDS = [
    "target_unique_id",
    "request_custom_id",
    "is_required_target",
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
    "response_status",
    "positive_codes",
    "positive_code_count",
    "confidence",
    "needs_review",
    "review_reason",
    "response_decision_count",
    "response_duplicate_status",
    "validation_error",
]


def load_compiled_codebook(path: Path) -> list[dict[str, str]]:
    data = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(data, list):
        raise SystemExit(f"Compiled codebook must be a JSON array: {path}")
    return [item for item in data if isinstance(item, dict)]


def code_columns_from_compiled_codebook(compiled_codebook: Iterable[dict[str, str]]) -> list[str]:
    columns: list[str] = []
    seen: set[str] = set()
    for code in compiled_codebook:
        column = squish(code.get("code_column", ""))
        if column and column not in seen:
            seen.add(column)
            columns.append(column)
    return columns


def code_id_to_column_map(compiled_codebook: Iterable[dict[str, str]]) -> dict[str, str]:
    out: dict[str, str] = {}
    for code in compiled_codebook:
        code_id = squish(code.get("code_id", ""))
        column = squish(code.get("code_column", ""))
        if code_id and column:
            out[code_id] = column
    return out


def load_parsed_response(path: Path) -> dict[str, object]:
    data = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        raise SystemExit(f"Parsed response must be a JSON object: {path}")
    return data


def load_manifest(path: Path | None) -> dict[str, object]:
    if path is None or not path.exists():
        return {}
    data = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        raise SystemExit(f"Manifest must be a JSON object: {path}")
    return data


def manifest_row_request_map(manifest: dict[str, object]) -> dict[str, str]:
    out: dict[str, str] = {}
    custom_id_map = manifest.get("custom_id_map", {})
    if not isinstance(custom_id_map, dict):
        return out
    for custom_id, mapping in custom_id_map.items():
        if not isinstance(mapping, dict):
            continue
        row_ids = mapping.get("row_ids", [])
        if not isinstance(row_ids, list):
            continue
        for row_id in row_ids:
            clean = squish(row_id)
            if clean:
                out[clean] = str(custom_id)
    return out


def response_decisions_by_row_id(parsed_response: dict[str, object]) -> dict[str, list[dict[str, object]]]:
    coded_rows = parsed_response.get("coded_rows", [])
    if not isinstance(coded_rows, list):
        raise SystemExit("Parsed response field `coded_rows` must be an array.")
    decisions: dict[str, list[dict[str, object]]] = defaultdict(list)
    for item in coded_rows:
        if not isinstance(item, dict):
            continue
        row_id = squish(item.get("row_id", ""))
        if row_id:
            decisions[row_id].append(item)
    return decisions


def response_decisions_from_raw_jsonl(path: Path) -> dict[str, list[dict[str, object]]]:
    decisions: dict[str, list[dict[str, object]]] = defaultdict(list)
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            if not line.strip():
                continue
            wrapper = json.loads(line)
            custom_id = squish(wrapper.get("custom_id", ""))
            response = wrapper.get("response", {})
            if not isinstance(response, dict):
                continue
            body = response.get("body")
            response_obj = body if isinstance(body, dict) else response
            text = extract_response_text(response_obj)
            if not text:
                continue
            parsed = json.loads(text)
            coded_rows = parsed.get("coded_rows", []) if isinstance(parsed, dict) else []
            if not isinstance(coded_rows, list):
                continue
            for item in coded_rows:
                if not isinstance(item, dict):
                    continue
                row_id = squish(item.get("row_id", ""))
                if row_id:
                    copied = dict(item)
                    copied["_response_custom_id"] = custom_id
                    decisions[row_id].append(copied)
    return decisions


def extract_response_text(response: dict[str, object]) -> str:
    texts: list[str] = []
    for item in response.get("output", []) or []:
        if not isinstance(item, dict):
            continue
        for content in item.get("content", []) or []:
            if isinstance(content, dict) and content.get("type") in {"output_text", "text"}:
                text = content.get("text")
                if isinstance(text, str):
                    texts.append(text)
    if texts:
        return "\n".join(texts).strip()
    output_text = response.get("output_text")
    return output_text if isinstance(output_text, str) else ""


def normalize_codes(value: object) -> list[str]:
    if not isinstance(value, list):
        return []
    return [squish(item) for item in value if squish(item)]


def decision_signature(decision: dict[str, object]) -> tuple[tuple[str, ...], str, str, str]:
    codes = tuple(sorted(normalize_codes(decision.get("codes", []))))
    confidence = squish(decision.get("confidence", ""))
    needs_review = str(bool(decision.get("needs_review", False))).lower()
    review_reason = squish(decision.get("review_reason", ""))
    return codes, confidence, needs_review, review_reason


def choose_decision(decisions: list[dict[str, object]]) -> tuple[dict[str, object] | None, str]:
    if not decisions:
        return None, ""
    if len(decisions) == 1:
        return decisions[0], "unique"
    counts = Counter(decision_signature(decision) for decision in decisions)
    if len(counts) == 1:
        return decisions[0], "duplicate_same_decision"
    most_common_signature, _ = counts.most_common(1)[0]
    for decision in decisions:
        if decision_signature(decision) == most_common_signature:
            return decision, "duplicate_conflicting_decision"
    return decisions[0], "duplicate_conflicting_decision"


def joined_review_rows(
    candidate_rows: list[dict[str, str]],
    compiled_codebook: list[dict[str, str]],
    parsed_response: dict[str, object] | None = None,
    decisions_by_row_id: dict[str, list[dict[str, object]]] | None = None,
    row_request_map: dict[str, str] | None = None,
) -> tuple[list[str], list[dict[str, str]]]:
    code_columns = code_columns_from_compiled_codebook(compiled_codebook)
    code_id_map = code_id_to_column_map(compiled_codebook)
    decisions = decisions_by_row_id or response_decisions_by_row_id(parsed_response or {})
    row_request_map = row_request_map or {}
    fieldnames = REVIEW_BASE_FIELDS + code_columns
    review_rows: list[dict[str, str]] = []

    for candidate in candidate_rows:
        row_id = squish(candidate.get("row_id", ""))
        target_unique_id = f"{squish(candidate.get('csv_path', ''))}::{row_id}"
        decision_list = decisions.get(row_id, [])
        decision, duplicate_status = choose_decision(decision_list)
        out = {field: squish(candidate.get(field, "")) for field in REVIEW_BASE_FIELDS}
        out["target_unique_id"] = target_unique_id
        out["request_custom_id"] = row_request_map.get(row_id, squish(decision.get("_response_custom_id", "")) if decision else "")
        out["is_required_target"] = "true"
        out["response_status"] = "missing_response"
        out["positive_codes"] = ""
        out["positive_code_count"] = ""
        out["confidence"] = ""
        out["needs_review"] = ""
        out["review_reason"] = ""
        out["response_decision_count"] = str(len(decision_list))
        out["response_duplicate_status"] = duplicate_status
        out["validation_error"] = ""
        for column in code_columns:
            out[column] = ""

        if decision is not None:
            codes = normalize_codes(decision.get("codes", []))
            unknown_codes = [code for code in codes if code not in code_id_map]
            out["response_status"] = "coded" if not unknown_codes else "unknown_code"
            out["positive_codes"] = ";".join(codes)
            out["positive_code_count"] = str(len(codes))
            out["confidence"] = squish(decision.get("confidence", ""))
            out["needs_review"] = str(bool(decision.get("needs_review", False))).lower()
            out["review_reason"] = squish(decision.get("review_reason", ""))
            validation_errors = []
            if unknown_codes:
                validation_errors.append(f"unknown_codes: {';'.join(unknown_codes)}")
            if len(decision_list) > 1:
                validation_errors.append(f"duplicate_returned_row_id_count: {len(decision_list)}")
            if duplicate_status == "duplicate_conflicting_decision":
                validation_errors.append("duplicate_conflicting_decision")
            response_custom_ids = sorted({squish(item.get("_response_custom_id", "")) for item in decision_list if squish(item.get("_response_custom_id", ""))})
            expected_custom_id = row_request_map.get(row_id, "")
            if expected_custom_id and response_custom_ids and response_custom_ids != [expected_custom_id]:
                validation_errors.append(f"unexpected_response_custom_id: {';'.join(response_custom_ids)}")
            out["validation_error"] = " | ".join(validation_errors)
            for column in code_columns:
                out[column] = "0"
            for code in codes:
                column = code_id_map.get(code)
                if column:
                    out[column] = "1"

        review_rows.append(out)

    return fieldnames, review_rows


def build_review_from_paths(
    candidate_rows_path: Path,
    response_path: Path,
    compiled_codebook_path: Path,
    manifest_path: Path | None = None,
    raw_response_jsonl_path: Path | None = None,
) -> tuple[list[str], list[dict[str, str]]]:
    _, candidate_rows = read_csv_dicts(candidate_rows_path)
    compiled_codebook = load_compiled_codebook(compiled_codebook_path)
    manifest = load_manifest(manifest_path)
    row_request_map = manifest_row_request_map(manifest)
    if raw_response_jsonl_path is not None and raw_response_jsonl_path.exists():
        decisions = response_decisions_from_raw_jsonl(raw_response_jsonl_path)
        return joined_review_rows(
            candidate_rows,
            compiled_codebook,
            decisions_by_row_id=decisions,
            row_request_map=row_request_map,
        )
    parsed_response = load_parsed_response(response_path)
    return joined_review_rows(
        candidate_rows,
        compiled_codebook,
        parsed_response=parsed_response,
        row_request_map=row_request_map,
    )
