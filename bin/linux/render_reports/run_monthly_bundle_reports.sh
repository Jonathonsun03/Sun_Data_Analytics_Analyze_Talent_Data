#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

RUNNER_A="bin/linux/render_reports/run_bundle_A_report.sh"
RUNNER_B="bin/linux/render_reports/run_bundle_B_report.sh"

WINDOW_DAYS="90"
INPUT_SOURCE="datalake"
INPUT_ROOT=""
DATALAKE_ROOT=""
STAGING_ROOT=""
QUIET="false"
DRY_RUN="false"

declare -a EXTRA_WRAPPER_ARGS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/run_monthly_bundle_reports.sh [options] [-- extra wrapper args]

Description:
  Runs Bundle A and Bundle B report wrappers for all talents using a standardized
  rolling window (default: 90 days).

  Stage order:
    1) run_bundle_A_report.sh --all
    2) run_bundle_B_report.sh --all

Options:
  --window-days N      Rolling lookback days for both bundles (default: 90)
  --input-source NAME  staging|datalake (default: datalake)
  --input-root PATH    Optional input root for talent data
  --datalake-root PATH Optional output datalake root override
  --staging-root PATH  Optional staging root override
  --quiet              Pass --quiet to both wrappers
  --dry-run            Pass --dry-run to both wrappers
  -h, --help           Show help

Pass-through:
  --                   Everything after -- is appended to both wrappers

Examples:
  Standard monthly run:
    bin/linux/render_reports/run_monthly_bundle_reports.sh

  Container datalake paths:
    bin/linux/render_reports/run_monthly_bundle_reports.sh \
      --input-source datalake \
      --input-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data \
      --datalake-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --window-days)
      [[ $# -ge 2 ]] || { echo "Error: --window-days requires a value" >&2; exit 1; }
      WINDOW_DAYS="$2"
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

if ! [[ "${WINDOW_DAYS}" =~ ^[0-9]+$ ]] || [[ "${WINDOW_DAYS}" -le 0 ]]; then
  echo "Error: --window-days must be a positive integer." >&2
  exit 1
fi

if [[ "${INPUT_SOURCE}" != "staging" && "${INPUT_SOURCE}" != "datalake" ]]; then
  echo "Error: --input-source must be either 'staging' or 'datalake'." >&2
  exit 1
fi

cd "${REPO_ROOT}"

# Cron often runs with a minimal C locale; enforce UTF-8 so talent names with
# full-width brackets resolve consistently across all wrapper stages.
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
if [[ ! -x "${RUNNER_B}" ]]; then
  echo "Error: missing executable: ${RUNNER_B}" >&2
  exit 1
fi

declare -a COMMON_ARGS=(
  --all
  --window-days "${WINDOW_DAYS}"
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

run_stage() {
  local label="$1"
  shift

  echo
  echo "[monthly-reports] ${label}"
  echo "[monthly-reports] Command: $*"
  "$@"
}

echo "[monthly-reports] Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "[monthly-reports] Repo root: ${REPO_ROOT}"
echo "[monthly-reports] Window days: ${WINDOW_DAYS}"
echo "[monthly-reports] Input source: ${INPUT_SOURCE}"
echo "[monthly-reports] LANG: ${LANG:-}"
echo "[monthly-reports] LC_ALL: ${LC_ALL:-}"

status=0

set +e
run_stage "Stage 1/2: Bundle A" "${RUNNER_A}" "${COMMON_ARGS[@]}" "${EXTRA_WRAPPER_ARGS[@]}"
rc_a=$?
if [[ ${rc_a} -ne 0 ]]; then
  status=1
  echo "[monthly-reports] Bundle A failed with status ${rc_a}" >&2
fi

run_stage "Stage 2/2: Bundle B" "${RUNNER_B}" "${COMMON_ARGS[@]}" "${EXTRA_WRAPPER_ARGS[@]}"
rc_b=$?
if [[ ${rc_b} -ne 0 ]]; then
  status=1
  echo "[monthly-reports] Bundle B failed with status ${rc_b}" >&2
fi
set -e

echo
if [[ ${status} -eq 0 ]]; then
  echo "[monthly-reports] Complete: success"
else
  echo "[monthly-reports] Complete: one or more stages failed" >&2
fi

exit "${status}"
