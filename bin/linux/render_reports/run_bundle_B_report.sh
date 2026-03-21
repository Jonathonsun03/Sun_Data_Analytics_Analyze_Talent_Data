#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET="${SCRIPT_DIR}/bundle_B/run_bundle_B_full_pipeline.sh"

if [[ ! -x "${TARGET}" ]]; then
  echo "Error: missing executable: ${TARGET}" >&2
  exit 1
fi

exec "${TARGET}" "$@"
