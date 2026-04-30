#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

RUNNER_A="bin/linux/render_reports/bundle_A/run_bundle_A_render_only.sh"
WINDOWS_CSV="30,60,90"
INPUT_SOURCE="datalake"
INPUT_ROOT=""
DATALAKE_ROOT=""
STAGING_ROOT=""
QUIET="false"
DRY_RUN="false"
INCLUDE_INTERPRETATIONS="false"

declare -a EXTRA_WRAPPER_ARGS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/run_monthly_bundle_A_render_only.sh [options] [-- extra wrapper args]

Description:
  Runs simplified Bundle A render-only reports for all talents across multiple
  rolling windows. By default, this renders 30, 60, and 90 day reports without
  reading existing generated interpretation text.

Options:
  --windows CSV        Rolling lookback windows, comma-separated (default: 30,60,90)
  --input-source NAME  staging|datalake (default: datalake)
  --input-root PATH    Optional input root for talent data
  --datalake-root PATH Optional output datalake root override
  --staging-root PATH  Optional staging root override
  --with-interpretations
                       Include existing generated interpretation markdown
  --quiet              Pass --quiet to the render wrapper
  --dry-run            Pass --dry-run to the render wrapper
  -h, --help           Show help

Pass-through:
  --                   Everything after -- is appended to the render wrapper

Examples:
  Standard monthly simplified Bundle A batch:
    bin/linux/render_reports/run_monthly_bundle_A_render_only.sh

  Container datalake paths:
    bin/linux/render_reports/run_monthly_bundle_A_render_only.sh \
      --input-source datalake \
      --input-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data \
      --datalake-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data
USAGE
}

trim() {
  local s="$1"
  s="${s#"${s%%[![:space:]]*}"}"
  s="${s%"${s##*[![:space:]]}"}"
  printf '%s' "$s"
}

split_windows() {
  local csv="$1"
  local -a vals=()
  local item

  IFS=',' read -r -a vals <<< "${csv}"
  for item in "${vals[@]}"; do
    item="$(trim "${item}")"
    [[ -z "${item}" ]] && continue
    if ! [[ "${item}" =~ ^[0-9]+$ ]] || [[ "${item}" -le 0 ]]; then
      echo "Error: --windows values must be positive integers. Got: ${item}" >&2
      exit 1
    fi
    printf '%s\n' "${item}"
  done
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --windows)
      [[ $# -ge 2 ]] || { echo "Error: --windows requires a value" >&2; exit 1; }
      WINDOWS_CSV="$2"
      shift 2
      ;;
    --input-source)
      [[ $# -ge 2 ]] || { echo "Error: --input-source requires a value" >&2; exit 1; }
      INPUT_SOURCE="$(printf '%s' "$2" | tr '[:upper:]' '[:lower:]')"
      shift 2
      ;;
    --input-root)
      [[ $# -ge 2 ]] || { echo "Error: --input-root requires a value" >&2; exit 1; }
      INPUT_ROOT="$2"
      shift 2
      ;;
    --datalake-root)
      [[ $# -ge 2 ]] || { echo "Error: --datalake-root requires a value" >&2; exit 1; }
      DATALAKE_ROOT="$2"
      shift 2
      ;;
    --staging-root)
      [[ $# -ge 2 ]] || { echo "Error: --staging-root requires a value" >&2; exit 1; }
      STAGING_ROOT="$2"
      shift 2
      ;;
    --with-interpretations)
      INCLUDE_INTERPRETATIONS="true"
      shift
      ;;
    --quiet)
      QUIET="true"
      shift
      ;;
    --dry-run)
      DRY_RUN="true"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      while [[ $# -gt 0 ]]; do
        EXTRA_WRAPPER_ARGS+=("$1")
        shift
      done
      ;;
    *)
      echo "Error: unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ "${INPUT_SOURCE}" != "staging" && "${INPUT_SOURCE}" != "datalake" ]]; then
  echo "Error: --input-source must be either 'staging' or 'datalake'." >&2
  exit 1
fi

mapfile -t WINDOWS < <(split_windows "${WINDOWS_CSV}")
if [[ ${#WINDOWS[@]} -eq 0 ]]; then
  echo "Error: --windows must contain at least one positive integer." >&2
  exit 1
fi

cd "${REPO_ROOT}"

# Cron often runs with a minimal C locale; enforce UTF-8 so talent names with
# full-width brackets resolve consistently.
if [[ -z "${LANG:-}" ]]; then
  export LANG="C.UTF-8"
fi
if [[ -z "${LC_ALL:-}" ]]; then
  export LC_ALL="${LANG}"
fi

if [[ ! -x "${RUNNER_A}" ]]; then
  echo "Error: missing executable: ${RUNNER_A}" >&2
  exit 1
fi

declare -a COMMON_ARGS=(
  --all
  --input-source "${INPUT_SOURCE}"
)

if [[ -n "${INPUT_ROOT}" ]]; then
  COMMON_ARGS+=(--input-root "${INPUT_ROOT}")
fi
if [[ -n "${DATALAKE_ROOT}" ]]; then
  COMMON_ARGS+=(--datalake-root "${DATALAKE_ROOT}")
fi
if [[ -n "${STAGING_ROOT}" ]]; then
  COMMON_ARGS+=(--staging-root "${STAGING_ROOT}")
fi
if [[ "${QUIET}" == "true" ]]; then
  COMMON_ARGS+=(--quiet)
fi
if [[ "${DRY_RUN}" == "true" ]]; then
  COMMON_ARGS+=(--dry-run)
fi
if [[ "${INCLUDE_INTERPRETATIONS}" != "true" ]]; then
  COMMON_ARGS+=(--no-interpretations)
fi

run_window() {
  local window_days="$1"
  shift

  echo
  echo "[monthly-bundle-a-render-only] Window: ${window_days} days"
  echo "[monthly-bundle-a-render-only] Command: $* --window-days ${window_days}"
  "$@" --window-days "${window_days}"
}

echo "[monthly-bundle-a-render-only] Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "[monthly-bundle-a-render-only] Repo root: ${REPO_ROOT}"
echo "[monthly-bundle-a-render-only] Windows: ${WINDOWS[*]}"
echo "[monthly-bundle-a-render-only] Input source: ${INPUT_SOURCE}"
echo "[monthly-bundle-a-render-only] Include interpretations: ${INCLUDE_INTERPRETATIONS}"
echo "[monthly-bundle-a-render-only] LANG: ${LANG:-}"
echo "[monthly-bundle-a-render-only] LC_ALL: ${LC_ALL:-}"

status=0

set +e
for window_days in "${WINDOWS[@]}"; do
  run_window "${window_days}" "${RUNNER_A}" "${COMMON_ARGS[@]}" "${EXTRA_WRAPPER_ARGS[@]}"
  rc=$?
  if [[ ${rc} -ne 0 ]]; then
    status=1
    echo "[monthly-bundle-a-render-only] Window ${window_days} failed with status ${rc}" >&2
  fi
done
set -e

echo
if [[ ${status} -eq 0 ]]; then
  echo "[monthly-bundle-a-render-only] Complete: success"
else
  echo "[monthly-bundle-a-render-only] Complete: one or more windows failed" >&2
fi

exit "${status}"
