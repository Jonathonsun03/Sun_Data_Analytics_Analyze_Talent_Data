#!/usr/bin/env bash
set -euo pipefail

# Load repo .env defaults without overriding already-exported values.
_ENV_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
while [[ "${_ENV_ROOT}" != "/" ]]; do
  if [[ -e "${_ENV_ROOT}/.git" ]]; then
    break
  fi
  _ENV_ROOT="$(dirname "${_ENV_ROOT}")"
done
if [[ -f "${_ENV_ROOT}/bin/linux/load_repo_env.sh" ]]; then
  # shellcheck source=/dev/null
  source "${_ENV_ROOT}/bin/linux/load_repo_env.sh"
  load_repo_env "${_ENV_ROOT}"
fi
unset _ENV_ROOT

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
INTERNAL_DIR="${SCRIPT_DIR}/internal"
PREPARE_RUNNER="${INTERNAL_DIR}/prepare_transcript_python.sh"
BUILD_RUNNER="${INTERNAL_DIR}/build_qualitative_batch.sh"
SUBMIT_RUNNER="${INTERNAL_DIR}/submit_qualitative_batch.sh"
CHECK_RUNNER="${INTERNAL_DIR}/check_qualitative_batch.sh"
EXPORT_REVIEW_RUNNER="${INTERNAL_DIR}/export_coding_review.sh"
APPLY_RUNNER="${INTERNAL_DIR}/apply_qualitative_batch.sh"
DIRECT_TEST_RUNNER="${INTERNAL_DIR}/run_direct_qualitative_coding_test.sh"
DEFAULT_RUN_ROOT="${TALENT_DATALAKE_ROOT%/Talent_data}/Processed/Talent_Data/qualitative_batch_runs"

MODE="build"
RUN_ID=""
RUN_ROOT="${QUALITATIVE_BATCH_RUN_ROOT:-$DEFAULT_RUN_ROOT}"
RUN_DIR=""
EXECUTE="false"
RETRIEVE_OUTPUT="false"
SKIP_PREPARE="false"
ALLOW_CHAT_CODES="false"
ZERO_CHAT_CODES="false"
POLL_INTERVAL_SECONDS="300"
MAX_WAIT_SECONDS="90000"
declare -a CODING_ARGS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh [wrapper options] -- [batch build options]

Default mode:
  build    Prepare CSVs, compile codebook, and build OpenAI Batch API JSONL artifacts.

Wrapper options:
  --mode build|full|submit|check|export-review|apply|direct-test
  --run-id VALUE       Run folder name under qualitative_batch_runs.
  --run-root PATH      Override qualitative_batch_runs root.
  --run-dir PATH       Existing run directory for submit/check/export-review.
  --execute            For submit/full mode, actually upload/create the OpenAI Batch job.
  --retrieve-output    For check mode, download output/error files when available.
  --poll-interval N    Seconds between full-mode status checks. Default: 300.
  --max-wait N         Maximum seconds full mode waits. Default: 90000.
  --skip-prepare       Skip prepared-transcript build before build/direct-test.
  --allow-chat-codes   Apply mode: allow positive codes on source=chat rows.
  --zero-chat-codes    Apply mode: force source=chat rows to zero before applying.
  -h, --help           Show this help.

Batch build options after --:
  --talent-query VALUE
  --transcript VALUE
  --selected-transcripts-csv PATH
  --limit N
  --row-limit N
  --coding-folder VALUE
  --codebook current|latest_snapshot|snapshot:<file>|path:<path>
  --model VALUE
  --batch-size N
  --context-rows N
  --reprocess

Build a real Batch API run folder for five Nova videos:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
    --run-id "batch_nova_5video_$(date +%Y-%m-%d_%H-%M-%S)" \
    -- \
    --talent-query "Nova" \
    --limit 5

Run the whole Batch API flow and wait for output:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
    --mode full \
    --execute \
    --run-id "batch_nova_5video_$(date +%Y-%m-%d_%H-%M-%S)" \
    -- \
    --talent-query "Nova" \
    --limit 5

Submit after reviewing the run folder:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
    --mode submit \
    --run-dir "${TALENT_DATALAKE_ROOT%/Talent_data}/Processed/Talent_Data/qualitative_batch_runs/<run_id>" \
    --execute

Check and retrieve output later:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
    --mode check \
    --run-dir "${TALENT_DATALAKE_ROOT%/Talent_data}/Processed/Talent_Data/qualitative_batch_runs/<run_id>" \
    --retrieve-output

Export review CSV after retrieval:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
    --mode export-review \
    --run-dir "${TALENT_DATALAKE_ROOT%/Talent_data}/Processed/Talent_Data/qualitative_batch_runs/<run_id>"

Dry-run the save-back step into the talent library:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
    --mode apply \
    --run-dir "${TALENT_DATALAKE_ROOT%/Talent_data}/Processed/Talent_Data/qualitative_batch_runs/<run_id>"

Apply reviewed codes back to the prepared transcript CSVs:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
    --mode apply \
    --run-dir "${TALENT_DATALAKE_ROOT%/Talent_data}/Processed/Talent_Data/qualitative_batch_runs/<run_id>" \
    --execute

Direct Codex test mode, kept only for small immediate test runs:
  bin/linux/ena_precoding/run_qualitative_coding_batch.sh --mode direct-test -- ...
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode) MODE="${2:-}"; shift 2 ;;
    --run-id) RUN_ID="${2:-}"; shift 2 ;;
    --run-root) RUN_ROOT="${2:-}"; shift 2 ;;
    --run-dir) RUN_DIR="${2:-}"; shift 2 ;;
    --execute) EXECUTE="true"; shift ;;
    --retrieve-output) RETRIEVE_OUTPUT="true"; shift ;;
    --allow-chat-codes) ALLOW_CHAT_CODES="true"; shift ;;
    --zero-chat-codes) ZERO_CHAT_CODES="true"; shift ;;
    --poll-interval) POLL_INTERVAL_SECONDS="${2:-}"; shift 2 ;;
    --max-wait) MAX_WAIT_SECONDS="${2:-}"; shift 2 ;;
    --skip-prepare) SKIP_PREPARE="true"; shift ;;
    --) shift; CODING_ARGS=("$@"); break ;;
    -h|--help) usage; exit 0 ;;
    *) CODING_ARGS+=("$1"); shift ;;
  esac
done

require_executable() {
  local path="$1"
  if [[ ! -x "$path" ]]; then
    echo "Error: helper is not executable: $path" >&2
    exit 1
  fi
}

extract_arg_value() {
  local name="$1"
  local default="$2"
  local i
  for ((i = 0; i < ${#CODING_ARGS[@]}; i++)); do
    if [[ "${CODING_ARGS[$i]}" == "$name" && $((i + 1)) -lt ${#CODING_ARGS[@]} ]]; then
      printf '%s\n' "${CODING_ARGS[$((i + 1))]}"
      return 0
    fi
  done
  printf '%s\n' "$default"
}

has_arg() {
  local name="$1"
  local arg
  for arg in "${CODING_ARGS[@]}"; do
    [[ "$arg" == "$name" ]] && return 0
  done
  return 1
}

write_batch_readme() {
  local run_dir="$1"
  cat > "$run_dir/README.md" <<README
# Qualitative Batch API Run

This folder contains one OpenAI Batch API qualitative-coding run.

Top-level files:

- \`candidate_rows.csv\`: all transcript rows included in the batch input.
- \`batch_input.jsonl\`: OpenAI Batch API request file.
- \`run_code_set.csv\`: human-readable frozen copy of the exact codes used.
- \`coding_review.csv\`: created after output retrieval/export.
- \`compiled_codebook.json\`: codebook used for this run.
- \`manifest.json\`: run metadata, selected transcripts, codebook, OpenAI batch ids, and artifact paths.

After submission, \`manifest.json\` records \`batch_id\`, \`input_file_id\`, status, output file ids, and retrieval artifacts.
README
}

build_mode() {
  require_executable "$PREPARE_RUNNER"
  require_executable "$BUILD_RUNNER"

  local run_id="$RUN_ID"
  if [[ -z "$run_id" ]]; then
    run_id="qualitative_batch_$(date +%Y-%m-%d_%H-%M-%S_%z)"
  fi
  local run_dir="${RUN_ROOT%/}/${run_id}"
  if [[ -e "$run_dir" ]]; then
    echo "Error: run directory already exists: $run_dir" >&2
    exit 1
  fi

  local talent_query coding_folder codebook
  talent_query="$(extract_arg_value "--talent-query" "Avaritia")"
  if [[ "$talent_query" == "Avaritia" ]]; then
    talent_query="$(extract_arg_value "--talent" "$talent_query")"
  fi
  coding_folder="$(extract_arg_value "--coding-folder" "monetary conversation codes")"
  codebook="$(extract_arg_value "--codebook" "current")"
  selected_transcripts_csv="$(extract_arg_value "--selected-transcripts-csv" "")"

  mkdir -p "$run_dir/logs" "$run_dir/metadata"

  {
    printf 'cd %q\n' "$REPO_ROOT"
    printf '%q --run-id %q --run-root %q --' "$0" "$run_id" "$RUN_ROOT"
    for arg in "${CODING_ARGS[@]}"; do
      printf ' %q' "$arg"
    done
    printf '\n'
  } > "$run_dir/metadata/command.sh"

  if [[ "$SKIP_PREPARE" == "false" ]]; then
    local prep_args=(
      --talent-query "$talent_query"
      --coding-folder "$coding_folder"
      --codebook "$codebook"
      --prep-index "$run_dir/metadata/prepared_transcripts_index.csv"
    )
    if [[ -n "$selected_transcripts_csv" ]]; then
      prep_args+=(--selected-transcripts-csv "$selected_transcripts_csv")
    fi
    if has_arg "--reprocess"; then
      prep_args+=(--reprocess)
    fi
    "$PREPARE_RUNNER" "${prep_args[@]}" 2>&1 | tee "$run_dir/logs/prepare_stdout.log"
  else
    echo "Prepare step skipped by --skip-prepare" | tee "$run_dir/logs/prepare_stdout.log"
  fi

  "$BUILD_RUNNER" \
    --run-id "$run_id" \
    --run-root "$RUN_ROOT" \
    --allow-existing-run-dir \
    "${CODING_ARGS[@]}" 2>&1 | tee "$run_dir/logs/build_stdout.log"

  write_batch_readme "$run_dir"
  echo
  echo "Batch run folder: $run_dir"
  echo "Review/input rows: $run_dir/candidate_rows.csv"
  echo "Batch API input: $run_dir/batch_input.jsonl"
  echo "Run code set: $run_dir/run_code_set.csv"
  echo "Codebook JSON: $run_dir/compiled_codebook.json"
  echo "Manifest: $run_dir/manifest.json"
  echo "Next: review the folder, then submit with --mode submit --run-dir \"$run_dir\" --execute"
}

run_dir_for_current_args() {
  local run_id="$RUN_ID"
  if [[ -z "$run_id" ]]; then
    echo "Error: --run-id is required for full mode so the run can be found later" >&2
    exit 1
  fi
  printf '%s\n' "${RUN_ROOT%/}/${run_id}"
}

submit_mode() {
  require_executable "$SUBMIT_RUNNER"
  if [[ -z "$RUN_DIR" ]]; then
    echo "Error: --run-dir is required for submit mode" >&2
    exit 1
  fi
  local args=(--run-dir "$RUN_DIR")
  [[ "$EXECUTE" == "true" ]] && args+=(--execute)
  "$SUBMIT_RUNNER" "${args[@]}"
}

check_mode() {
  require_executable "$CHECK_RUNNER"
  if [[ -z "$RUN_DIR" ]]; then
    echo "Error: --run-dir is required for check mode" >&2
    exit 1
  fi
  local args=(--run-dir "$RUN_DIR")
  [[ "$RETRIEVE_OUTPUT" == "true" ]] && args+=(--retrieve-output)
  "$CHECK_RUNNER" "${args[@]}"
}

export_review_mode() {
  require_executable "$EXPORT_REVIEW_RUNNER"
  if [[ -z "$RUN_DIR" ]]; then
    echo "Error: --run-dir is required for export-review mode" >&2
    exit 1
  fi
  local raw="$RUN_DIR/batch_output.jsonl"
  if [[ -f "$raw" ]]; then
    "$EXPORT_REVIEW_RUNNER" --run-dir "$RUN_DIR" --raw-responses-jsonl "$raw" --output "$RUN_DIR/coding_review.csv"
  else
    "$EXPORT_REVIEW_RUNNER" --run-dir "$RUN_DIR" --output "$RUN_DIR/coding_review.csv"
  fi
}

apply_mode() {
  require_executable "$APPLY_RUNNER"
  if [[ -z "$RUN_DIR" ]]; then
    echo "Error: --run-dir is required for apply mode" >&2
    exit 1
  fi
  local args=(--run-dir "$RUN_DIR")
  [[ "$EXECUTE" == "true" ]] && args+=(--execute)
  [[ "$ALLOW_CHAT_CODES" == "true" ]] && args+=(--allow-chat-codes)
  [[ "$ZERO_CHAT_CODES" == "true" ]] && args+=(--zero-chat-codes)
  "$APPLY_RUNNER" "${args[@]}"
}

manifest_value() {
  local run_dir="$1"
  local key="$2"
  python3 - "$run_dir/manifest.json" "$key" <<'PY'
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
key = sys.argv[2]
if not path.exists():
    print("")
    raise SystemExit(0)
data = json.loads(path.read_text(encoding="utf-8"))
value = data.get(key, "")
print("" if value is None else value)
PY
}

full_mode() {
  if [[ "$EXECUTE" != "true" ]]; then
    echo "Error: full mode submits to the OpenAI Batch API; pass --execute to confirm." >&2
    exit 1
  fi
  if [[ ! "$POLL_INTERVAL_SECONDS" =~ ^[0-9]+$ || "$POLL_INTERVAL_SECONDS" -lt 1 ]]; then
    echo "Error: --poll-interval must be a positive integer number of seconds" >&2
    exit 1
  fi
  if [[ ! "$MAX_WAIT_SECONDS" =~ ^[0-9]+$ || "$MAX_WAIT_SECONDS" -lt 1 ]]; then
    echo "Error: --max-wait must be a positive integer number of seconds" >&2
    exit 1
  fi

  local run_dir
  run_dir="$(run_dir_for_current_args)"

  echo "[full] Step 1/5: build batch input"
  build_mode

  echo "[full] Step 2/5: submit OpenAI Batch job"
  RUN_DIR="$run_dir" EXECUTE="true" submit_mode

  echo "[full] Step 3/5: wait for completion"
  local waited=0
  local status=""
  while true; do
    RUN_DIR="$run_dir" check_mode
    status="$(manifest_value "$run_dir" "batch_status")"
    echo "[full] status=${status:-unknown}; waited=${waited}s"
    case "$status" in
      completed)
        break
        ;;
      failed|expired|cancelled|cancelling)
        echo "Error: batch ended with status: $status" >&2
        exit 1
        ;;
    esac
    if (( waited >= MAX_WAIT_SECONDS )); then
      echo "Error: timed out waiting for batch completion after ${waited}s" >&2
      echo "Run can be resumed later with: $0 --mode check --run-dir \"$run_dir\" --retrieve-output" >&2
      exit 1
    fi
    sleep "$POLL_INTERVAL_SECONDS"
    waited=$((waited + POLL_INTERVAL_SECONDS))
  done

  echo "[full] Step 4/5: retrieve output"
  RUN_DIR="$run_dir" RETRIEVE_OUTPUT="true" check_mode

  echo "[full] Step 5/5: export coding review CSV"
  RUN_DIR="$run_dir" export_review_mode

  echo
  echo "Full batch run complete."
  echo "Run folder: $run_dir"
  echo "Review CSV: $run_dir/coding_review.csv"
  echo "Manifest: $run_dir/manifest.json"
  echo "Next: review coding_review.csv, then dry-run save-back with: $0 --mode apply --run-dir \"$run_dir\""
}

direct_test_mode() {
  require_executable "$DIRECT_TEST_RUNNER"
  local args=(--run-root "$RUN_ROOT")
  [[ -n "$RUN_ID" ]] && args+=(--run-id "$RUN_ID")
  [[ "$SKIP_PREPARE" == "true" ]] && args+=(--skip-prepare)
  "$DIRECT_TEST_RUNNER" "${args[@]}" -- "${CODING_ARGS[@]}"
}

case "$MODE" in
  build) build_mode ;;
  full) full_mode ;;
  submit) submit_mode ;;
  check) check_mode ;;
  export-review) export_review_mode ;;
  apply) apply_mode ;;
  direct-test) direct_test_mode ;;
  *) echo "Error: unknown mode: $MODE" >&2; usage >&2; exit 1 ;;
esac
