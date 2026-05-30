#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET="${SCRIPT_DIR}/bundle_E/run_bundle_E_full_pipeline.sh"

exec "${TARGET}" "$@"
