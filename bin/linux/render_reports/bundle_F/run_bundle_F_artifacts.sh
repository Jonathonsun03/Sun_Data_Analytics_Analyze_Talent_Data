#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_F/run_bundle_F_artifacts.sh [options]

Description:
  Bundle F currently builds its tables and plots during render, so this stage is
  intentionally a compatibility no-op for the standard bundle pipeline shape.

Options are accepted for wrapper compatibility and ignored here.
USAGE
}

for arg in "$@"; do
  case "$arg" in
    -h|--help)
      usage
      exit 0
      ;;
  esac
done

echo "[bundle-f-artifacts] Bundle F has no separate artifact build step yet."
echo "[bundle-f-artifacts] Tables and plots are generated during render."
