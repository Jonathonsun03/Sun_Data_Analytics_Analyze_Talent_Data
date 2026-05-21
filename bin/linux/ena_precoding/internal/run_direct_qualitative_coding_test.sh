#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
INTERNAL_DIR="${SCRIPT_DIR}/internal"
PREPARE_RUNNER="${INTERNAL_DIR}/prepare_transcript_python.sh"
DIRECT_RUNNER="${INTERNAL_DIR}/code_qualitative_transcripts.sh"
DEFAULT_RUN_ROOT="/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs"

RUN_ROOT="${QUALITATIVE_BATCH_RUN_ROOT:-$DEFAULT_RUN_ROOT}"
RUN_ID=""
SKIP_PREPARE="false"
declare -a DIRECT_ARGS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh [wrapper options] -- [coding options]

Description:
  Creates one organized run folder under Processed/Talent_Data/qualitative_batch_runs,
  prepares transcript CSVs, compiles the codebook JSON, runs the direct Codex
  qualitative-coding runner, keeps the review CSV at the run-folder top level,
  and organizes logs/JSON metadata into subfolders.

Wrapper options:
  --run-id VALUE       Folder name to create under qualitative_batch_runs.
  --run-root PATH      Override the qualitative_batch_runs root.
  --skip-prepare       Do not run the prepared-transcript build step first.
  -h, --help          Show this help.

Coding options:
  Everything after -- is passed to code_qualitative_transcripts.sh.

Example:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
    --run-id "direct_ava_k84_150row_streamer_scope_test" \
    -- \
    --talent-query "Avaritia" \
    --transcript "k84ImiUcjbE" \
    --limit 1 \
    --row-limit 150

If --run-id is omitted, a timestamped folder is created automatically.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="${2:-}"
      if [[ -z "$RUN_ID" ]]; then
        echo "Error: --run-id requires a value" >&2
        exit 1
      fi
      shift 2
      ;;
    --run-root)
      RUN_ROOT="${2:-}"
      if [[ -z "$RUN_ROOT" ]]; then
        echo "Error: --run-root requires a value" >&2
        exit 1
      fi
      shift 2
      ;;
    --skip-prepare)
      SKIP_PREPARE="true"
      shift
      ;;
    --)
      shift
      DIRECT_ARGS=("$@")
      break
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      DIRECT_ARGS+=("$1")
      shift
      ;;
  esac
done

if [[ ! -x "$DIRECT_RUNNER" ]]; then
  echo "Error: direct runner is not executable: $DIRECT_RUNNER" >&2
  exit 1
fi
if [[ ! -x "$PREPARE_RUNNER" ]]; then
  echo "Error: prepare runner is not executable: $PREPARE_RUNNER" >&2
  exit 1
fi

RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
if [[ -z "$RUN_ID" ]]; then
  RUN_ID="direct_qualitative_coding_${RUN_TS}"
fi

CODEBOOK_SELECTOR="current"
TALENT_QUERY="Avaritia"
CODING_FOLDER="monetary conversation codes"
CODING_REPROCESS="false"
CODING_DRY_RUN="false"
TRANSCRIPT_SELECTOR=""
LIMIT_SCOPE=""
for ((i = 0; i < ${#DIRECT_ARGS[@]}; i++)); do
  case "${DIRECT_ARGS[$i]}" in
    --codebook)
      if [[ $((i + 1)) -lt ${#DIRECT_ARGS[@]} ]]; then
        CODEBOOK_SELECTOR="${DIRECT_ARGS[$((i + 1))]}"
      fi
      ;;
    --talent-query|--talent)
      if [[ $((i + 1)) -lt ${#DIRECT_ARGS[@]} ]]; then
        TALENT_QUERY="${DIRECT_ARGS[$((i + 1))]}"
      fi
      ;;
    --coding-folder)
      if [[ $((i + 1)) -lt ${#DIRECT_ARGS[@]} ]]; then
        CODING_FOLDER="${DIRECT_ARGS[$((i + 1))]}"
      fi
      ;;
    --transcript)
      if [[ $((i + 1)) -lt ${#DIRECT_ARGS[@]} ]]; then
        TRANSCRIPT_SELECTOR="${DIRECT_ARGS[$((i + 1))]}"
      fi
      ;;
    --limit)
      if [[ $((i + 1)) -lt ${#DIRECT_ARGS[@]} ]]; then
        LIMIT_SCOPE="${DIRECT_ARGS[$((i + 1))]}"
      fi
      ;;
    --reprocess)
      CODING_REPROCESS="true"
      ;;
    --dry-run)
      CODING_DRY_RUN="true"
      ;;
  esac
done

RUN_DIR="${RUN_ROOT%/}/${RUN_ID}"
if [[ -e "$RUN_DIR" ]]; then
  echo "Error: run directory already exists: $RUN_DIR" >&2
  exit 1
fi

LOGS_DIR="$RUN_DIR/logs"
METADATA_DIR="$RUN_DIR/metadata"
mkdir -p "$LOGS_DIR" "$METADATA_DIR"

STDOUT_LOG="$LOGS_DIR/runner_stdout.log"
PREPARE_LOG="$LOGS_DIR/prepare_stdout.log"
COMMAND_FILE="$METADATA_DIR/command.sh"
README_FILE="$RUN_DIR/README.md"
MANIFEST_FILE="$METADATA_DIR/manifest.json"
COMPILED_CODEBOOK_FILE="$METADATA_DIR/compiled_codebook.json"
PREP_INDEX_FILE="$METADATA_DIR/prepared_transcripts_index.csv"
STATE_BEFORE_FILE="$METADATA_DIR/coding_state_before.json"
STATE_AFTER_FILE="$METADATA_DIR/coding_state_after.json"
REVIEW_CSV="$RUN_DIR/direct_coding_review.csv"

{
  printf 'cd %q\n' "$REPO_ROOT"
  printf '# Prepare/build prepared transcript CSVs unless skipped\n'
  printf '%q --talent-query %q --coding-folder %q --codebook %q --prep-index %q' "$PREPARE_RUNNER" "$TALENT_QUERY" "$CODING_FOLDER" "$CODEBOOK_SELECTOR" "$PREP_INDEX_FILE"
  if [[ "$CODING_REPROCESS" == "true" ]]; then
    printf ' --reprocess'
  fi
  if [[ "$CODING_DRY_RUN" == "true" ]]; then
    printf ' --dry-run'
  fi
  printf '\n\n'
  printf '# Code prepared CSV rows\n'
  printf '%q' "$DIRECT_RUNNER"
  for arg in "${DIRECT_ARGS[@]}"; do
    printf ' %q' "$arg"
  done
  printf '\n'
} > "$COMMAND_FILE"

cat > "$README_FILE" <<README
# Direct Qualitative Coding Run

This folder is the control folder for one direct Codex qualitative-coding run.

Run id:

\`\`\`text
$RUN_ID
\`\`\`

The direct runner edits prepared transcript CSVs in place under:

\`\`\`text
Z:\\DataLake\\Sun_Data_Analytics\\Talent_data\\<Talent Name>\\qualitative coding\\monetary conversation codes\\
\`\`\`

This run folder collects the surrounding evidence trail:

- \`direct_coding_review.csv\`: exported edited row block, when the final summary includes one touched CSV and a row range.
- \`logs/prepare_stdout.log\`: output from the prepared-transcript build step.
- \`logs/runner_stdout.log\`: terminal output from the direct runner.
- \`logs/codex_run.log\`: copied full Codex run log, when available.
- \`logs/codex_final.md\`: copied final Codex summary, when available.
- \`metadata/command.sh\`: exact command launched by this wrapper.
- \`metadata/compiled_codebook.json\`: compact JSON copy of the codebook used for this run.
- \`metadata/prepared_transcripts_index.csv\`: source/output CSV mapping from the prepare step.
- \`metadata/coding_state_before.json\`: selected transcript row/code state before coding.
- \`metadata/coding_state_after.json\`: selected transcript row/code state after coding.
- \`metadata/manifest.json\`: machine-readable summary of paths and parsed run details.

The CSV edited by the run remains in the per-talent prepared transcript folder.
README

cd "$REPO_ROOT"

export CODEBOOK_SELECTOR COMPILED_CODEBOOK_FILE
python3 - <<'PY'
import json
import os
import sys
from pathlib import Path

repo = Path.cwd()
if str(repo) not in sys.path:
    sys.path.insert(0, str(repo))

from py_scripts.lib.qualitative_coding.codebook import load_codebook, resolve_codebook

selector = os.environ["CODEBOOK_SELECTOR"]
output_path = Path(os.environ["COMPILED_CODEBOOK_FILE"])
codebook_path = resolve_codebook(selector)
code_defs = load_codebook(codebook_path)
payload = [code.compact_dict() for code in code_defs]
output_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
(output_path.parent / "codebook_source.txt").write_text(str(codebook_path) + "\n", encoding="utf-8")
PY

if [[ "$SKIP_PREPARE" == "false" ]]; then
  PREPARE_ARGS=(
    --talent-query "$TALENT_QUERY"
    --coding-folder "$CODING_FOLDER"
    --codebook "$CODEBOOK_SELECTOR"
    --prep-index "$PREP_INDEX_FILE"
  )
  if [[ "$CODING_REPROCESS" == "true" ]]; then
    PREPARE_ARGS+=(--reprocess)
  fi
  if [[ "$CODING_DRY_RUN" == "true" ]]; then
    PREPARE_ARGS+=(--dry-run)
  fi
  "$PREPARE_RUNNER" "${PREPARE_ARGS[@]}" 2>&1 | tee "$PREPARE_LOG"
else
  echo "Prepare step skipped by --skip-prepare" | tee "$PREPARE_LOG"
fi

export CODEBOOK_SELECTOR TALENT_QUERY CODING_FOLDER TRANSCRIPT_SELECTOR LIMIT_SCOPE CODING_REPROCESS STATE_BEFORE_FILE
python3 - <<'PY'
import json
import os
import sys
from pathlib import Path

repo = Path.cwd()
if str(repo) not in sys.path:
    sys.path.insert(0, str(repo))

from py_scripts.lib.qualitative_coding.codebook import load_codebook, resolve_codebook
from py_scripts.lib.qualitative_coding.csv_io import read_csv_dicts
from py_scripts.lib.qualitative_coding.prepared_transcripts import filter_transcripts, list_prepared_transcript_files, row_id_for, row_needs_coding
from py_scripts.lib.qualitative_coding.text import is_missing
from py_scripts.lib.utils.paths import default_talent_root, resolve_talent_paths

talent_root = default_talent_root()
codebook_path = resolve_codebook(os.environ["CODEBOOK_SELECTOR"], talent_root)
code_columns = [code.code_column for code in load_codebook(codebook_path)]
talents = resolve_talent_paths(os.environ["TALENT_QUERY"], talent_root, coding_folder=os.environ["CODING_FOLDER"])
paths = filter_transcripts(list_prepared_transcript_files(talents), os.environ.get("TRANSCRIPT_SELECTOR") or None, talent_root)

selected = []
for path in paths:
    fields, rows = read_csv_dicts(path)
    missing_columns = tuple(col for col in code_columns if col not in fields)
    pending_count = sum(row_needs_coding(row, code_columns, missing_columns, reprocess=False) for row in rows)
    if os.environ.get("CODING_REPROCESS") == "true" or pending_count > 0:
        selected.append((path, fields, rows, missing_columns, pending_count))

limit_text = os.environ.get("LIMIT_SCOPE", "").strip()
if limit_text:
    selected = selected[: int(limit_text)]

payload = {
    "codebook_path": str(codebook_path),
    "code_columns": code_columns,
    "selected_files": [],
}
for path, fields, rows, missing_columns, pending_count in selected:
    payload_rows = []
    for index, row in enumerate(rows):
        code_values = {col: row.get(col, "") for col in code_columns}
        pending = row_needs_coding(row, code_columns, missing_columns, reprocess=False)
        complete = all((col in row and not is_missing(row.get(col, ""))) for col in code_columns)
        payload_rows.append(
            {
                "row_number": index + 1,
                "row_id": row_id_for(path, row, index),
                "pending": pending,
                "complete": complete,
                "code_values": code_values,
            }
        )
    payload["selected_files"].append(
        {
            "csv_path": str(path),
            "row_count": len(rows),
            "pending_rows": pending_count,
            "missing_code_columns": list(missing_columns),
            "rows": payload_rows,
        }
    )

Path(os.environ["STATE_BEFORE_FILE"]).write_text(json.dumps(payload, indent=2), encoding="utf-8")
PY

set +e
"$DIRECT_RUNNER" "${DIRECT_ARGS[@]}" 2>&1 | tee "$STDOUT_LOG"
runner_status=${PIPESTATUS[0]}
set -e

LOG_FILE="$(sed -n 's/^Log file: //p' "$STDOUT_LOG" | tail -1)"
FINAL_MSG_FILE="$(sed -n 's/^Final message file: //p' "$STDOUT_LOG" | tail -1)"

if [[ -n "$LOG_FILE" && -f "$LOG_FILE" ]]; then
  cp "$LOG_FILE" "$LOGS_DIR/codex_run.log"
fi
if [[ -n "$FINAL_MSG_FILE" && -f "$FINAL_MSG_FILE" ]]; then
  cp "$FINAL_MSG_FILE" "$LOGS_DIR/codex_final.md"
fi

export RUN_DIR LOGS_DIR METADATA_DIR RUN_ID RUN_TS RUN_ROOT REPO_ROOT runner_status LOG_FILE FINAL_MSG_FILE REVIEW_CSV COMPILED_CODEBOOK_FILE CODEBOOK_SELECTOR PREP_INDEX_FILE PREPARE_LOG SKIP_PREPARE TALENT_QUERY CODING_FOLDER TRANSCRIPT_SELECTOR LIMIT_SCOPE CODING_REPROCESS STATE_BEFORE_FILE STATE_AFTER_FILE
python3 - <<'PY'
import csv
import json
import os
import re
import sys
from pathlib import Path

repo = Path(os.environ["REPO_ROOT"])
if str(repo) not in sys.path:
    sys.path.insert(0, str(repo))

from py_scripts.lib.qualitative_coding.csv_io import read_csv_dicts
from py_scripts.lib.qualitative_coding.prepared_transcripts import row_id_for, row_needs_coding
from py_scripts.lib.qualitative_coding.text import is_missing

run_dir = Path(os.environ["RUN_DIR"])
logs_dir = Path(os.environ["LOGS_DIR"])
metadata_dir = Path(os.environ["METADATA_DIR"])
final_copy = logs_dir / "codex_final.md"
stdout_log = logs_dir / "runner_stdout.log"
review_csv = Path(os.environ["REVIEW_CSV"])
state_before_path = Path(os.environ["STATE_BEFORE_FILE"])
state_after_path = Path(os.environ["STATE_AFTER_FILE"])

final_text = final_copy.read_text(encoding="utf-8") if final_copy.exists() else ""
stdout_text = stdout_log.read_text(encoding="utf-8") if stdout_log.exists() else ""

def first_match(pattern: str, text: str) -> str:
    match = re.search(pattern, text, re.M)
    return match.group(1).strip() if match else ""

touched_csv = first_match(r"(?:Touched CSV|Touched):\s*\n`([^`]+)`", final_text)
codebook_used = first_match(r"^- Codebook used:\s*`([^`]+)`", final_text)
codebook_selector = first_match(r"^- Codebook selector:\s*`([^`]+)`", final_text)
rows_coded = ""
rows_match = re.search(
    r"^- Rows coded:\s*(?:[0-9,]+\s+pending rows,\s+indices\s+`?([0-9]+-[0-9]+)`?|`?([0-9]+-[0-9]+)`?)",
    final_text,
    re.M,
)
if rows_match:
    rows_coded = next((group for group in rows_match.groups() if group), "")
remaining_pending = first_match(r"^- (?:Remaining pending rows in this CSV|Pending rows remaining):\s*([0-9,]+)", final_text)
validation = first_match(r"^- Validation passed:\s*(.+)$", final_text)

review_written = ""
review_error = ""

try:
    before_state = json.loads(state_before_path.read_text(encoding="utf-8"))
    code_columns = before_state.get("code_columns", [])
    review_fieldnames = None
    review_rows = []
    after_state = {
        "codebook_path": before_state.get("codebook_path", ""),
        "code_columns": code_columns,
        "selected_files": [],
    }
    for file_state in before_state.get("selected_files", []):
        source_path = Path(file_state["csv_path"])
        before_rows = {row["row_id"]: row for row in file_state.get("rows", [])}
        fields, rows = read_csv_dicts(source_path)
        missing_columns = tuple(col for col in code_columns if col not in fields)
        after_rows_for_state = []
        for index, row in enumerate(rows):
            row_id = row_id_for(source_path, row, index)
            code_values = {col: row.get(col, "") for col in code_columns}
            pending = row_needs_coding(row, code_columns, missing_columns, reprocess=False)
            complete = all((col in row and not is_missing(row.get(col, ""))) for col in code_columns)
            after_rows_for_state.append(
                {
                    "row_number": index + 1,
                    "row_id": row_id,
                    "pending": pending,
                    "complete": complete,
                    "code_values": code_values,
                }
            )
            before_row = before_rows.get(row_id)
            if not before_row:
                continue
            became_complete = bool(before_row.get("pending")) and complete and not pending
            code_changed = before_row.get("code_values", {}) != code_values
            if became_complete or (os.environ.get("CODING_REPROCESS") == "true" and code_changed):
                if review_fieldnames is None:
                    review_fieldnames = ["csv_path", "review_status"] + fields
                review_out = {"csv_path": str(source_path), "review_status": "newly_coded" if became_complete else "code_changed"}
                for field in fields:
                    review_out[field] = row.get(field, "")
                review_rows.append(review_out)
        after_state["selected_files"].append(
            {
                "csv_path": str(source_path),
                "row_count": len(rows),
                "pending_rows": sum(1 for row in after_rows_for_state if row["pending"]),
                "missing_code_columns": list(missing_columns),
                "rows": after_rows_for_state,
            }
        )
    state_after_path.write_text(json.dumps(after_state, indent=2), encoding="utf-8")
    if review_rows:
        assert review_fieldnames is not None
        with review_csv.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=review_fieldnames)
            writer.writeheader()
            writer.writerows(review_rows)
        review_written = str(review_csv)
except Exception as exc:
    review_error = str(exc)

if not review_written and touched_csv and rows_coded and "-" in rows_coded:
    start_s, end_s = rows_coded.split("-", 1)
    try:
        start = int(start_s)
        end = int(end_s)
        source_path = Path(touched_csv)
        with source_path.open("r", newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            rows = list(reader)
            fieldnames = reader.fieldnames or []
        selected = rows[start - 1 : end]
        with review_csv.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(selected)
        review_written = str(review_csv)
    except Exception as exc:  # Keep manifest creation resilient.
        review_error = str(exc)

manifest = {
    "run_id": os.environ["RUN_ID"],
    "created_at": os.environ["RUN_TS"],
    "run_dir": str(run_dir),
    "repo_root": os.environ["REPO_ROOT"],
    "runner_status": int(os.environ["runner_status"]),
    "direct_runner_log_source": os.environ.get("LOG_FILE", ""),
    "direct_runner_final_source": os.environ.get("FINAL_MSG_FILE", ""),
    "artifacts": {
        "command": str(metadata_dir / "command.sh"),
        "compiled_codebook": os.environ.get("COMPILED_CODEBOOK_FILE", ""),
        "codebook_source": str(metadata_dir / "codebook_source.txt") if (metadata_dir / "codebook_source.txt").exists() else "",
        "prepared_transcripts_index": os.environ.get("PREP_INDEX_FILE", ""),
        "coding_state_before": os.environ.get("STATE_BEFORE_FILE", ""),
        "coding_state_after": os.environ.get("STATE_AFTER_FILE", ""),
        "prepare_stdout": os.environ.get("PREPARE_LOG", ""),
        "runner_stdout": str(stdout_log),
        "codex_run_log": str(logs_dir / "codex_run.log") if (logs_dir / "codex_run.log").exists() else "",
        "codex_final": str(final_copy) if final_copy.exists() else "",
        "direct_coding_review_csv": review_written,
        "manifest": str(metadata_dir / "manifest.json"),
    },
    "parsed_final_summary": {
        "touched_csv": touched_csv,
        "codebook_selector": codebook_selector,
        "codebook_selector_requested": os.environ.get("CODEBOOK_SELECTOR", ""),
        "codebook_used": codebook_used,
        "rows_coded": rows_coded,
        "remaining_pending_rows": remaining_pending,
        "validation": validation,
    },
    "run_scope": {
        "talent_query": os.environ.get("TALENT_QUERY", ""),
        "coding_folder": os.environ.get("CODING_FOLDER", ""),
        "prepare_skipped": os.environ.get("SKIP_PREPARE", ""),
        "transcript_selector": os.environ.get("TRANSCRIPT_SELECTOR", ""),
        "limit": os.environ.get("LIMIT_SCOPE", ""),
        "reprocess": os.environ.get("CODING_REPROCESS", ""),
    },
    "notes": [
        "The direct runner edits prepared transcript CSVs in place.",
        "This folder collects run metadata, logs, final summary, and a review slice for audit/comparison.",
    ],
}
if review_error:
    manifest["review_export_error"] = review_error
if not final_text:
    manifest["notes"].append("No final Codex summary was copied; inspect runner_stdout.log and source log path.")
if stdout_text and "Final message file:" not in stdout_text:
    manifest["notes"].append("Runner stdout did not include a final-message path.")

(metadata_dir / "manifest.json").write_text(json.dumps(manifest, indent=2), encoding="utf-8")
PY

echo
echo "Batch run folder: $RUN_DIR"
echo "Manifest: $MANIFEST_FILE"
if [[ -f "$REVIEW_CSV" ]]; then
  echo "Review CSV: $REVIEW_CSV"
fi

exit "$runner_status"
