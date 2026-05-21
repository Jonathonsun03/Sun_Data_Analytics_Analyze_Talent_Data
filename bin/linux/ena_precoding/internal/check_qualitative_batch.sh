#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"
PY_SCRIPT="${REPO_ROOT}/py_scripts/run/qualitative_coding/check_qualitative_batch.py"

cd "${REPO_ROOT}"
exec python3 "${PY_SCRIPT}" "$@"
