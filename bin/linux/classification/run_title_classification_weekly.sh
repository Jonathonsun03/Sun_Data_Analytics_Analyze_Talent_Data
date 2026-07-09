#!/usr/bin/env bash
set -euo pipefail

# Load repo .env defaults without overriding already-exported values.
_ENV_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
while [[ "${_ENV_ROOT}" != "/" ]]; do
  if [[ -f "${_ENV_ROOT}/AGENTS.md" && -d "${_ENV_ROOT}/r_scripts" ]]; then
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

usage() {
  cat <<'EOF'
Usage:
  bin/linux/classification/run_title_classification_weekly.sh [classification options...]

Description:
  Weekly wrapper for title classification. This now builds an OpenAI Batch API
  run using the current compiled prompt definitions. Submit with --execute
  after reviewing the run folder, or use run_title_classification_batch.sh for
  submit/check/apply modes.

Examples:
  Build weekly pending batch:
    bin/linux/classification/run_title_classification_weekly.sh --batch-size 25

  Build full reclassification batch with current definitions:
    bin/linux/classification/run_title_classification_weekly.sh --batch-size 25 --force-reclassify

  Build and submit:
    bin/linux/classification/run_title_classification_weekly.sh --batch-size 25 --force-reclassify --execute

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

"${BATCH_RUNNER}" "$@"
