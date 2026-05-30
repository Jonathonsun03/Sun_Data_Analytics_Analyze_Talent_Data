#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

ARTIFACTS_RUNNER="bin/linux/render_reports/bundle_E/run_bundle_E_artifacts.sh"
REPORT_RUNNER="bin/linux/render_reports/bundle_E/run_bundle_E_render_only.sh"

declare -a PASS_ARGS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_E/run_bundle_E_full_pipeline.sh [options]

Description:
  Runs the current Bundle E pipeline:
    1) build figures/tables/artifacts
    2) render and publish the final Bundle E report

Compatibility flags accepted and ignored until Bundle E interpretation wrappers exist:
  --skip-interpretation
  --skip-editorial-rewrite
  --no-interpretations

All other args are passed through, including:
  --talent / --talents / --talents-file / --all
  --window-days / --start-date / --end-date
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
  echo "[bundle-e-full] ${label}"
  echo "[bundle-e-full] Command: $*"
  "$@"
}

echo "[bundle-e-full] Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "[bundle-e-full] Repo root: ${REPO_ROOT}"

status=0

set +e
run_stage "Stage 1/2: Build artifacts" "${ARTIFACTS_RUNNER}" "${PASS_ARGS[@]}"
rc_artifacts=$?
if [[ ${rc_artifacts} -ne 0 ]]; then
  status=1
  echo "[bundle-e-full] Artifact stage failed with status ${rc_artifacts}" >&2
fi

if [[ ${status} -eq 0 ]]; then
  run_stage "Stage 2/2: Render report" "${REPORT_RUNNER}" "${PASS_ARGS[@]}"
  rc_render=$?
  if [[ ${rc_render} -ne 0 ]]; then
    status=1
    echo "[bundle-e-full] Render stage failed with status ${rc_render}" >&2
  fi
fi
set -e

echo
if [[ ${status} -eq 0 ]]; then
  echo "[bundle-e-full] Complete: success"
else
  echo "[bundle-e-full] Complete: one or more stages failed" >&2
fi

exit "${status}"
