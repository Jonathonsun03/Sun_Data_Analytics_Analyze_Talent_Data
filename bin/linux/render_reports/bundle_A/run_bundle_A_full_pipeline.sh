#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

ARTIFACTS_RUNNER="bin/linux/render_reports/bundle_A/run_bundle_A_artifacts.sh"
INTERPRET_RUNNER="bin/linux/render_reports/bundle_A/run_bundle_A_interpretations.sh"
EDITORIAL_RUNNER="bin/linux/render_reports/bundle_A/run_bundle_A_editorial_rewrite.sh"
REPORT_RUNNER="bin/linux/render_reports/bundle_A/run_bundle_A_render_only.sh"

SKIP_INTERPRETATION="false"
SKIP_EDITORIAL_REWRITE="false"

declare -a PASS_ARGS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh [options]

Description:
  Runs the current Bundle A pipeline in four stages:
    1) build figures/tables/artifacts
    2) run interpretation generation
    3) run editorial rewrite over the generated report text
    4) render the final Bundle A report

Notes:
  - The interpretation stage uses `codex exec`.
  - For safer testing, pass `--prompt-filter` and/or `--max-prompts`.

Extra option handled here:
  --skip-interpretation   Skip stage 2 and go straight from artifacts to render
  --skip-editorial-rewrite Skip stage 3 and render the generated text as-is

All other args are passed through to the stage wrappers, including:
  --talent / --talents / --talents-file / --all
  --window-days / --start-date / --end-date
  --input-source / --input-root / --datalake-root / --staging-root
  --quiet / --dry-run
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --skip-interpretation)
      SKIP_INTERPRETATION="true"
      shift
      ;;
    --skip-editorial-rewrite)
      SKIP_EDITORIAL_REWRITE="true"
      shift
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
if [[ ! -x "${INTERPRET_RUNNER}" ]]; then
  echo "Error: missing executable: ${INTERPRET_RUNNER}" >&2
  exit 1
fi
if [[ ! -x "${EDITORIAL_RUNNER}" ]]; then
  echo "Error: missing executable: ${EDITORIAL_RUNNER}" >&2
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
  echo "[bundle-a-full] ${label}"
  echo "[bundle-a-full] Command: $*"
  "$@"
}

echo "[bundle-a-full] Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "[bundle-a-full] Repo root: ${REPO_ROOT}"

status=0

set +e
run_stage "Stage 1/3: Build artifacts" "${ARTIFACTS_RUNNER}" "${PASS_ARGS[@]}"
rc_artifacts=$?
if [[ ${rc_artifacts} -ne 0 ]]; then
  status=1
  echo "[bundle-a-full] Artifact stage failed with status ${rc_artifacts}" >&2
fi

if [[ ${status} -eq 0 && "${SKIP_INTERPRETATION}" != "true" ]]; then
  run_stage "Stage 2/4: Interpretation generation" "${INTERPRET_RUNNER}" "${PASS_ARGS[@]}"
  rc_interp=$?
  if [[ ${rc_interp} -ne 0 ]]; then
    status=1
    echo "[bundle-a-full] Interpretation stage failed with status ${rc_interp}" >&2
  fi
elif [[ "${SKIP_INTERPRETATION}" == "true" ]]; then
  echo
  echo "[bundle-a-full] Stage 2/4 skipped: interpretation"
fi

if [[ ${status} -eq 0 && "${SKIP_EDITORIAL_REWRITE}" != "true" ]]; then
  run_stage "Stage 3/4: Editorial rewrite" "${EDITORIAL_RUNNER}" "${PASS_ARGS[@]}"
  rc_edit=$?
  if [[ ${rc_edit} -ne 0 ]]; then
    status=1
    echo "[bundle-a-full] Editorial rewrite stage failed with status ${rc_edit}" >&2
  fi
elif [[ "${SKIP_EDITORIAL_REWRITE}" == "true" ]]; then
  echo
  echo "[bundle-a-full] Stage 3/4 skipped: editorial rewrite"
fi

if [[ ${status} -eq 0 ]]; then
  run_stage "Stage 4/4: Render report" "${REPORT_RUNNER}" "${PASS_ARGS[@]}"
  rc_render=$?
  if [[ ${rc_render} -ne 0 ]]; then
    status=1
    echo "[bundle-a-full] Render stage failed with status ${rc_render}" >&2
  fi
fi
set -e

echo
if [[ ${status} -eq 0 ]]; then
  echo "[bundle-a-full] Complete: success"
else
  echo "[bundle-a-full] Complete: one or more stages failed" >&2
fi

exit "${status}"
