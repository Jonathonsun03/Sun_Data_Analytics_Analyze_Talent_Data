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
SCHEDULED_RUNNER="bin/linux/classification/run_title_classification_scheduled.sh"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/classification/run_title_classification_weekly.sh [classification options...]

Description:
  General backfill wrapper for title classification. Without --execute, it
  builds a reviewable OpenAI Batch API run and does not submit it. With
  --execute, it starts or advances the durable scheduled lifecycle, including
  retrieval, validation, apply, export, and retry handling.

Examples:
  Build weekly pending batch:
    bin/linux/classification/run_title_classification_weekly.sh --batch-size 25

  Start the incremental weekly lifecycle:
    bin/linux/classification/run_title_classification_weekly.sh --model gpt-5.6-terra --batch-size 25 --execute

  Advance an existing lifecycle without starting new work:
    bin/linux/classification/run_title_classification_scheduled.sh --check-only

EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

cd "${REPO_ROOT}"

if [[ ! -x "${BATCH_RUNNER}" ]]; then
  echo "Error: missing/executable runner: ${BATCH_RUNNER}" >&2
  exit 1
fi

if [[ ! -x "${SCHEDULED_RUNNER}" ]]; then
  echo "Error: missing/executable runner: ${SCHEDULED_RUNNER}" >&2
  exit 1
fi

EXECUTE="false"
declare -a CLASSIFICATION_ARGS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --execute) EXECUTE="true"; shift ;;
    *) CLASSIFICATION_ARGS+=("$1"); shift ;;
  esac
done

if [[ "${EXECUTE}" == "true" ]]; then
  "${SCHEDULED_RUNNER}" "${CLASSIFICATION_ARGS[@]}"
else
  "${BATCH_RUNNER}" -- "${CLASSIFICATION_ARGS[@]}"
fi
