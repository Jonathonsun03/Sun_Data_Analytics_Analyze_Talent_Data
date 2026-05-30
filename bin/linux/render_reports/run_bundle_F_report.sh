#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET="${SCRIPT_DIR}/bundle_F/run_bundle_F_full_pipeline.sh"

exec "${TARGET}" "$@"
