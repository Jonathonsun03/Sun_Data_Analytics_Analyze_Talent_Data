#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="r_scripts/run/run_scheduled_reports.R"
RSCRIPT_BIN="${RSCRIPT_BIN:-Rscript}"

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

cd "${REPO_ROOT}"

if [[ ! -f "${R_SCRIPT}" ]]; then
  echo "Error: missing R script at ${R_SCRIPT}" >&2
  exit 1
fi

"${RSCRIPT_BIN}" "${R_SCRIPT}" "$@"
