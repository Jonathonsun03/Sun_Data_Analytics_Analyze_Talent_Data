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

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_G/run_bundle_G_artifacts.sh [options]

Description:
  Bundle G currently builds its tables and plots during render, so this stage is
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

echo "[bundle-g-artifacts] Bundle G has no separate artifact build step yet."
echo "[bundle-g-artifacts] Tables and plots are generated during render."
