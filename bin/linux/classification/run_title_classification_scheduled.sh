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

BATCH_RUNNER="bin/linux/classification/run_title_classification_batch.sh"
STATE_TOOL="r_scripts/run/title_classification/update_scheduled_state.R"
RETRY_TOOL="py_scripts/run/title_classification/build_retry_batch.py"

TALENT_DATA_ROOT="${TALENT_DATALAKE_ROOT:?Set TALENT_DATA_ROOT or TALENT_DATALAKE_ROOT in .env}"
TALENT_DATA_ROOT="${TALENT_DATA_ROOT%/}"
ANALYTICS_ROOT="${TALENT_DATA_ROOT%/Talent_data}"
EXPORT_ROOT="${TITLE_CLASSIFICATIONS_DIR:-${ANALYTICS_ROOT}/Processed/Title_classification}"
CLASSIFICATION_LOG_ROOT="${TITLE_CLASSIFICATION_LOG_ROOT:-${ANALYTICS_ROOT}/Processed/Logs/classification}"
BATCH_RUN_ROOT="${TITLE_CLASSIFICATION_BATCH_RUN_ROOT:-${CLASSIFICATION_LOG_ROOT}/title_classification/batch_runs}"
STATE_DB_PATH="${TITLE_CLASSIFICATION_STATE_DB_PATH:-${TALENT_DATA_ROOT}/classifications.duckdb}"
LOG_ROOT="${CLASSIFICATION_LOG_ROOT}/title_classification_scheduled"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/classification/run_title_classification_scheduled.sh [build options...]

Description:
  Idempotent scheduled OpenAI Batch API lifecycle for title classification.

  If classifications.duckdb has an active scheduled title-classification state, this
  checks status, retrieves completed output, applies completed results, creates
  a retry run for failed/missing custom_ids, and clears state when done.

  If there is no pending state, this refreshes notes/titles.csv, builds a new
  one-video-per-request Batch API JSONL input for pending videos, submits it,
  and records the pending run state.

Common build options:
  --model NAME
  --talent NAME
  --force-reclassify

Environment overrides:
  TALENT_DATALAKE_ROOT
  TITLE_CLASSIFICATION_STATE_DB_PATH
  TITLE_CLASSIFICATION_BATCH_RUN_ROOT
  TITLE_CLASSIFICATION_LOG_ROOT
  TITLE_CLASSIFICATIONS_DIR
  OPENAI_API_KEY
USAGE
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

cd "${REPO_ROOT}"

mkdir -p "${LOG_ROOT}" "${BATCH_RUN_ROOT}"
export TITLE_CLASSIFICATION_BATCH_RUN_ROOT="${BATCH_RUN_ROOT}"
GLOBAL_LOG="${LOG_ROOT}/scheduled_$(date +%Y%m%d_%H%M%S).log"

log() {
  printf '[title-classification-scheduled] %s\n' "$*" | tee -a "${GLOBAL_LOG}"
}

run_logged() {
  local run_dir="$1"
  shift
  mkdir -p "${run_dir}/logs"
  log "Running: $*"
  set +e
  "$@" 2>&1 | tee -a "${GLOBAL_LOG}" "${run_dir}/logs/scheduled.log"
  local status="${PIPESTATUS[0]}"
  set -e
  return "${status}"
}

run_build_logged() {
  local run_dir="$1"
  shift
  log "Running: $*"
  set +e
  "$@" 2>&1 | tee -a "${GLOBAL_LOG}"
  local status="${PIPESTATUS[0]}"
  set -e
  if [[ -d "${run_dir}/logs" ]]; then
    cp "${GLOBAL_LOG}" "${run_dir}/logs/scheduled.log"
  fi
  return "${status}"
}

state_field() {
  Rscript "${STATE_TOOL}" get --db-path "${STATE_DB_PATH}" --field "$1"
}

state_exists() {
  Rscript "${STATE_TOOL}" exists --db-path "${STATE_DB_PATH}" >/dev/null
}

write_state() {
  local run_dir="$1"
  shift
  Rscript "${STATE_TOOL}" write --db-path "${STATE_DB_PATH}" --run-dir "${run_dir}" "$@" >/dev/null
}

archive_state() {
  local archive_path="$1"
  Rscript "${STATE_TOOL}" archive --db-path "${STATE_DB_PATH}" --archive-path "${archive_path}" >/dev/null
}

clear_state() {
  local archive_path="$1"
  Rscript "${STATE_TOOL}" clear --db-path "${STATE_DB_PATH}" --archive-path "${archive_path}"
}

manifest_field() {
  local run_dir="$1"
  local field="$2"
  python3 -c 'import json, sys; data = json.load(open(sys.argv[1])); value = data.get(sys.argv[2]); sys.exit(1) if value is None else print(value, end="")' "${run_dir}/manifest.json" "${field}"
}

LOCK_DIR="${LOG_ROOT}/title_classification_scheduled.lock"
if ! mkdir "${LOCK_DIR}" 2>/dev/null; then
  log "Another scheduled title classification run appears to be active: ${LOCK_DIR}"
  exit 0
fi
trap 'rmdir "${LOCK_DIR}" 2>/dev/null || true' EXIT

log "Started at $(date -Is)"
log "State DB path: ${STATE_DB_PATH}"
log "Batch run root: ${BATCH_RUN_ROOT}"
log "Log root: ${LOG_ROOT}"

if ! state_exists; then
  RECOVERED_RUN_DIR="$(
    Rscript "${STATE_TOOL}" find-active --run-root "${BATCH_RUN_ROOT}" 2>/dev/null || true
  )"
  if [[ -n "${RECOVERED_RUN_DIR}" ]]; then
    log "Recovered active submitted run without scheduled state: ${RECOVERED_RUN_DIR}"
    write_state "${RECOVERED_RUN_DIR}"
  fi
fi

if state_exists; then
  RUN_DIR="$(state_field run_dir)"
  if [[ ! -f "${RUN_DIR}/manifest.json" ]]; then
    log "State points at a missing manifest: ${RUN_DIR}/manifest.json"
    exit 1
  fi

  log "Pending state found. Checking existing run: ${RUN_DIR}"
  run_logged "${RUN_DIR}" "${BATCH_RUNNER}" --mode check --run-dir "${RUN_DIR}" --retrieve-output
  write_state "${RUN_DIR}"

  STATUS="$(state_field status || true)"
  log "Batch status: ${STATUS}"

  case "${STATUS}" in
    completed)
      if [[ ! -f "${RUN_DIR}/batch_output.jsonl" ]]; then
        log "Batch is completed but output JSONL was not retrieved. Leaving state in place."
        exit 1
      fi

      run_logged "${RUN_DIR}" "${BATCH_RUNNER}" --mode apply --run-dir "${RUN_DIR}" --allow-failures
      APPLIED_AT="$(date -Is)"
      write_state "${RUN_DIR}" --status applied --applied-at "${APPLIED_AT}"
      archive_state "${RUN_DIR}/completed_state.json"

      RETRY_RUN_DIR="$(
        python3 "${RETRY_TOOL}" \
          --source-run-dir "${RUN_DIR}" \
          --run-root "${BATCH_RUN_ROOT}" \
          --print-run-dir
      )"
      if [[ -n "${RETRY_RUN_DIR}" ]]; then
        log "Submitting retry run for failed/missing requests: ${RETRY_RUN_DIR}"
        run_logged "${RETRY_RUN_DIR}" "${BATCH_RUNNER}" --mode submit --run-dir "${RETRY_RUN_DIR}" --execute
        write_state "${RETRY_RUN_DIR}"
        archive_state "${RETRY_RUN_DIR}/pending_state.json"
        log "Retry state saved. Original state retained in ${RUN_DIR}/completed_state.json"
      else
        log "No retry needed. Clearing pending state."
        clear_state "${RUN_DIR}/completed_state.json"
      fi
      ;;
    failed|expired|cancelled)
      log "Batch reached terminal status '${STATUS}'. Leaving state in place for manual inspection."
      exit 1
      ;;
    *)
      log "Batch is not complete yet. State remains pending."
      ;;
  esac

  log "Finished at $(date -Is)"
  exit 0
fi

RUN_ID="title_classification_scheduled_$(date -u +%Y%m%d_%H%M%S)"
RUN_DIR="${BATCH_RUN_ROOT%/}/${RUN_ID}"

log "No pending state found. Building new one-video-per-request Batch run: ${RUN_ID}"
run_build_logged "${RUN_DIR}" "${BATCH_RUNNER}" --run-id "${RUN_ID}" -- --batch-size 1 "$@"

REQUEST_COUNT="$(manifest_field "${RUN_DIR}" request_count || echo 0)"
PENDING_ROWS="$(manifest_field "${RUN_DIR}" pending_rows || echo 0)"
log "Pending rows: ${PENDING_ROWS}; batch requests: ${REQUEST_COUNT}"

if [[ "${REQUEST_COUNT}" == "0" ]]; then
  log "No videos need classification. No batch submitted."
  log "Finished at $(date -Is)"
  exit 0
fi

run_logged "${RUN_DIR}" "${BATCH_RUNNER}" --mode submit --run-dir "${RUN_DIR}" --execute
write_state "${RUN_DIR}"
archive_state "${RUN_DIR}/pending_state.json"

log "Submitted batch and saved pending state."
log "Finished at $(date -Is)"
