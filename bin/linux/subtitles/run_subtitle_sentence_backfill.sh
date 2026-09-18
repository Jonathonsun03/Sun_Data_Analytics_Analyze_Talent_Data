#!/usr/bin/env bash
set -euo pipefail

ENV_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
while [[ "${ENV_ROOT}" != "/" ]]; do
  if [[ -e "${ENV_ROOT}/.git" ]]; then
    break
  fi
  ENV_ROOT="$(dirname "${ENV_ROOT}")"
done
if [[ -f "${ENV_ROOT}/bin/linux/load_repo_env.sh" ]]; then
  # shellcheck source=/dev/null
  source "${ENV_ROOT}/bin/linux/load_repo_env.sh"
  load_repo_env "${ENV_ROOT}"
fi

REPO_ROOT="${ENV_ROOT}"
R_SCRIPT="r_scripts/run/Subtitle_clean/backfill_subtitle_sentences.R"
DRY_RUN="true"
TALENT_CODE=""
VIDEO_ID=""
MAX_VIDEOS="0"
MAX_NEW_VIDEOS="0"
MAX_ATTEMPTS="3"
REQUEST_PAUSE_SEC="0.2"
TIMEOUT_SEC="120"
PIPELINE_VERSION="subtitle_sentence_v2"
RETRY_FAILED="false"
FORCE="false"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/subtitles/run_subtitle_sentence_backfill.sh [options]

The default is a read-only inventory. Add --execute to call FullStop and write
validated full-track sentence rows to the canonical talent DuckDB.

Options:
  --execute                    Run model inference and publish derived rows
  --dry-run                    Inventory only; no model calls or writes (default)
  --talent-code CODE           Limit to one exact talent code
  --video-id ID                Limit to one exact video ID
  --max-videos N               Process at most N tracks; 0 means all (default)
  --max-new-videos N           Process at most N tracks needing work; current
                               tracks do not count; 0 means all (default)
  --max-attempts N             Maximum FullStop attempts per block (default: 3)
  --request-pause-sec N        Pause after successful model requests (default: 0.2)
  --timeout-sec N              FullStop request timeout in seconds (default: 120)
  --pipeline-version VALUE     Derived pipeline version
  --retry-failed               Reset exhausted failed blocks and try them again
  --force                      Rebuild current tracks and ignore complete checkpoints
  -h, --help                   Show this help
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
    --talent-code)
      [[ $# -ge 2 ]] || { echo "Error: --talent-code requires a value" >&2; exit 1; }
      TALENT_CODE="$2"
      shift 2
      ;;
    --video-id)
      [[ $# -ge 2 ]] || { echo "Error: --video-id requires a value" >&2; exit 1; }
      VIDEO_ID="$2"
      shift 2
      ;;
    --max-videos)
      [[ $# -ge 2 ]] || { echo "Error: --max-videos requires a value" >&2; exit 1; }
      MAX_VIDEOS="$2"
      shift 2
      ;;
    --max-new-videos)
      [[ $# -ge 2 ]] || { echo "Error: --max-new-videos requires a value" >&2; exit 1; }
      MAX_NEW_VIDEOS="$2"
      shift 2
      ;;
    --max-attempts)
      [[ $# -ge 2 ]] || { echo "Error: --max-attempts requires a value" >&2; exit 1; }
      MAX_ATTEMPTS="$2"
      shift 2
      ;;
    --request-pause-sec)
      [[ $# -ge 2 ]] || { echo "Error: --request-pause-sec requires a value" >&2; exit 1; }
      REQUEST_PAUSE_SEC="$2"
      shift 2
      ;;
    --timeout-sec)
      [[ $# -ge 2 ]] || { echo "Error: --timeout-sec requires a value" >&2; exit 1; }
      TIMEOUT_SEC="$2"
      shift 2
      ;;
    --pipeline-version)
      [[ $# -ge 2 ]] || { echo "Error: --pipeline-version requires a value" >&2; exit 1; }
      PIPELINE_VERSION="$2"
      shift 2
      ;;
    --retry-failed)
      RETRY_FAILED="true"
      shift
      ;;
    --force)
      FORCE="true"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Error: unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

cd "${REPO_ROOT}"
if [[ ! -f "${R_SCRIPT}" ]]; then
  echo "Error: missing R script at ${R_SCRIPT}" >&2
  exit 1
fi

if [[ -z "${TALENT_DATALAKE_ROOT:-}" ]]; then
  echo "Error: TALENT_DATALAKE_ROOT is not configured." >&2
  exit 1
fi

LOCK_DIR="${TALENT_DATALAKE_ROOT}/Logs/subtitle_sentence_backfill"
mkdir -p "${LOCK_DIR}"
exec 9>"${LOCK_DIR}/backfill.lock"
if ! flock -n 9; then
  echo "Error: another subtitle sentence backfill holds ${LOCK_DIR}/backfill.lock" >&2
  exit 1
fi

export SUBTITLE_BACKFILL_DRY_RUN="${DRY_RUN}"
export SUBTITLE_BACKFILL_TALENT_CODE="${TALENT_CODE}"
export SUBTITLE_BACKFILL_VIDEO_ID="${VIDEO_ID}"
export SUBTITLE_BACKFILL_MAX_VIDEOS="${MAX_VIDEOS}"
export SUBTITLE_BACKFILL_MAX_NEW_VIDEOS="${MAX_NEW_VIDEOS}"
export SUBTITLE_BACKFILL_MAX_ATTEMPTS="${MAX_ATTEMPTS}"
export SUBTITLE_BACKFILL_REQUEST_PAUSE_SEC="${REQUEST_PAUSE_SEC}"
export SUBTITLE_PUNCTUATION_TIMEOUT_SEC="${TIMEOUT_SEC}"
export SUBTITLE_BACKFILL_PIPELINE_VERSION="${PIPELINE_VERSION}"
export SUBTITLE_BACKFILL_RETRY_FAILED="${RETRY_FAILED}"
export SUBTITLE_BACKFILL_FORCE="${FORCE}"

Rscript --vanilla "${R_SCRIPT}"
