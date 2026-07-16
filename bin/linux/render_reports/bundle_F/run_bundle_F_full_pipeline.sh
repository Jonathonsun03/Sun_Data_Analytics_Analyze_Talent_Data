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
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

ARTIFACTS_RUNNER="bin/linux/render_reports/bundle_F/run_bundle_F_artifacts.sh"
REPORT_RUNNER="bin/linux/render_reports/bundle_F/run_bundle_F_render_only.sh"

declare -a PASS_ARGS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_F/run_bundle_F_full_pipeline.sh [options]

Description:
  Runs the current Bundle F pipeline:
    1) compatibility artifact stage
    2) render and publish the final Bundle F day-of-week report

Compatibility flags accepted and ignored until Bundle F interpretation wrappers exist:
  --skip-interpretation
  --skip-editorial-rewrite
  --no-interpretations

All other args are passed through, including:
  --talent / --talents / --talents-file / --all
  --window-days / --start-date / --end-date
  --content-type / --content-types / --reference-day
  --input-source / --input-root / --datalake-root / --staging-root
  --allow-partial-match / --quiet / --dry-run
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --skip-interpretation|--skip-editorial-rewrite|--no-interpretations)
      shift
      ;;
    --window-days)
      [[ $# -ge 2 ]] || { echo "Error: --window-days requires a value" >&2; exit 1; }
      window_key="$(printf '%s' "$2" | tr '[:upper:]' '[:lower:]' | tr '_-' '  ' | sed 's/[[:space:]][[:space:]]*/ /g; s/^ //; s/ $//')"
      if [[ "${window_key}" == "lifetime" || "${window_key}" == "life time" || "${window_key}" == "all" || "${window_key}" == "all data" || "${window_key}" == "all available data" || "${window_key}" == "all available" || "${window_key}" == "full history" || "${window_key}" == "history" ]]; then
        shift 2
      else
        PASS_ARGS+=("$1" "$2")
        shift 2
      fi
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      PASS_ARGS+=("$1")
      shift
      ;;
  esac
done

cd "${REPO_ROOT}"

if [[ ! -x "${ARTIFACTS_RUNNER}" ]]; then
  echo "Error: missing executable: ${ARTIFACTS_RUNNER}" >&2
  exit 1
fi
if [[ ! -x "${REPORT_RUNNER}" ]]; then
  echo "Error: missing executable: ${REPORT_RUNNER}" >&2
  exit 1
fi

run_stage() {
  local label="$1"
  shift
  echo
  echo "[bundle-f-full] ${label}"
  echo "[bundle-f-full] Command: $*"
  "$@"
}

echo "[bundle-f-full] Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "[bundle-f-full] Repo root: ${REPO_ROOT}"

status=0

set +e
run_stage "Stage 1/2: Build artifacts" "${ARTIFACTS_RUNNER}" "${PASS_ARGS[@]}"
rc_artifacts=$?
if [[ ${rc_artifacts} -ne 0 ]]; then
  status=1
  echo "[bundle-f-full] Artifact stage failed with status ${rc_artifacts}" >&2
fi

if [[ ${status} -eq 0 ]]; then
  run_stage "Stage 2/2: Render report" "${REPORT_RUNNER}" "${PASS_ARGS[@]}"
  rc_render=$?
  if [[ ${rc_render} -ne 0 ]]; then
    status=1
    echo "[bundle-f-full] Render stage failed with status ${rc_render}" >&2
  fi
fi
set -e

echo
if [[ ${status} -eq 0 ]]; then
  echo "[bundle-f-full] Complete: success"
else
  echo "[bundle-f-full] Complete: one or more stages failed" >&2
fi

exit "${status}"
