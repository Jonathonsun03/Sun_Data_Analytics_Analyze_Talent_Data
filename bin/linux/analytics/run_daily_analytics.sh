#!/usr/bin/env bash
set -euo pipefail

# Run durable derived-data refreshes after the nightly Talent Repo collection.
# Add future refreshes as named functions and register them in run_pipeline().

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

# shellcheck source=../load_repo_env.sh
source "${REPO_ROOT}/bin/linux/load_repo_env.sh"
load_repo_env "${REPO_ROOT}"

DRY_RUN="true"

usage() {
  cat <<'USAGE'
Usage: bin/linux/analytics/run_daily_analytics.sh [--execute|--dry-run]

Runs the maintained derived-data refreshes that follow a completed Talent Repo
collection and lakehouse reconciliation.

Options:
  --execute   Publish refreshed derived relations.
  --dry-run   Validate inputs and report source counts without writes (default).
  -h, --help  Show this help.

To add a future refresh, create a named run_<analysis>() function and add one
run_step call in run_pipeline(). Keep each analysis independently runnable.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --execute)
      DRY_RUN="false"
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
    *)
      echo "Error: unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ -z "${TALENT_DATALAKE_ROOT:-}" ]]; then
  echo "Error: TALENT_DATALAKE_ROOT or TALENT_DATA_ROOT is required." >&2
  exit 2
fi

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Error: Rscript is required for daily analytics refreshes." >&2
  exit 127
fi

LOG_ROOT="${TALENT_DATALAKE_ROOT%/}/Logs/daily_analytics"
mkdir -p "${LOG_ROOT}"
exec 9>"${LOG_ROOT}/daily_analytics.lock"
if ! flock -n 9; then
  echo "Daily analytics is already running; skipping concurrent run."
  exit 0
fi

LOG_FILE="${LOG_ROOT}/daily_analytics_$(date -u +%Y%m%dT%H%M%SZ).log"

log() {
  printf '[daily-analytics] %s\n' "$*" | tee -a "${LOG_FILE}"
}

run_step() {
  local name="$1"
  shift
  log "Starting: ${name}"
  "$@" 2>&1 | tee -a "${LOG_FILE}"
  log "Completed: ${name}"
}

run_viewer_activity_refresh() {
  env \
    VIEWER_ACTIVITY_PUBLISH_DRY_RUN="${DRY_RUN}" \
    Rscript --vanilla "${REPO_ROOT}/r_scripts/run/publish_viewer_activity.R"
}

run_pipeline() {
  # Register new daily derived-data refreshes here, in dependency order.
  run_step "viewer activity and engagement profiles" run_viewer_activity_refresh
}

log "Started (dry_run=${DRY_RUN}; lakehouse=${TALENT_DATALAKE_ROOT})"
run_pipeline
log "Completed daily analytics"
