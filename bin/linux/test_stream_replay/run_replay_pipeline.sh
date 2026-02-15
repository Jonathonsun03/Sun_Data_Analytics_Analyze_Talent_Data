#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

SUBTITLE_RUNNER="bin/linux/subtitles/run_subtitle_clean.sh"
REPLAY_RUNNER="bin/linux/test_stream_replay/run_text_replay.sh"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/test_stream_replay/run_replay_pipeline.sh [options]

Description:
  Replay-only pipeline wrapper. Runs:
    1) subtitle clean
    2) text replay render

Options:
  --talent-query VALUE           Talent selector passed to select_talent() (default: all)
  --datalake-root PATH           Override TALENT_DATALAKE_ROOT
  --n-cores N                    Cores used by both stages (default: 1)
  --reclean                      Reprocess subtitle outputs before replay stage
  -h, --help                     Show this help

Examples:
  All talents:
    bin/linux/test_stream_replay/run_replay_pipeline.sh

  Single talent:
    bin/linux/test_stream_replay/run_replay_pipeline.sh --talent-query "Avaritia"

  Single talent, 8 cores, force reclean:
    bin/linux/test_stream_replay/run_replay_pipeline.sh --talent-query "Avaritia" --n-cores 8 --reclean
USAGE
}

TALENT_QUERY_VAL="all"
DATALAKE_ROOT_VAL=""
N_CORES_VAL="1"
RECLEAN_VAL="false"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --talent-query)
      [[ $# -ge 2 ]] || { echo "Error: --talent-query requires a value" >&2; exit 1; }
      TALENT_QUERY_VAL="$2"
      shift 2
      ;;
    --datalake-root)
      [[ $# -ge 2 ]] || { echo "Error: --datalake-root requires a value" >&2; exit 1; }
      DATALAKE_ROOT_VAL="$2"
      shift 2
      ;;
    --n-cores)
      [[ $# -ge 2 ]] || { echo "Error: --n-cores requires a value" >&2; exit 1; }
      N_CORES_VAL="$2"
      shift 2
      ;;
    --reclean)
      RECLEAN_VAL="true"
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

if [[ ! -x "${SUBTITLE_RUNNER}" ]]; then
  echo "Error: subtitle runner not found/executable: ${SUBTITLE_RUNNER}" >&2
  exit 1
fi

if [[ ! -x "${REPLAY_RUNNER}" ]]; then
  echo "Error: replay runner not found/executable: ${REPLAY_RUNNER}" >&2
  exit 1
fi

subtitle_cmd=( "${SUBTITLE_RUNNER}" --talent-query "${TALENT_QUERY_VAL}" --n-cores "${N_CORES_VAL}" )
replay_cmd=( "${REPLAY_RUNNER}" --talent-query "${TALENT_QUERY_VAL}" --n-cores "${N_CORES_VAL}" )

if [[ -n "${DATALAKE_ROOT_VAL}" ]]; then
  subtitle_cmd+=( --datalake-root "${DATALAKE_ROOT_VAL}" )
  replay_cmd+=( --datalake-root "${DATALAKE_ROOT_VAL}" )
fi

if [[ "${RECLEAN_VAL}" == "true" ]]; then
  subtitle_cmd+=( --reclean )
fi

echo "[replay-pipeline] Stage 1/2: subtitle clean"
"${subtitle_cmd[@]}"

echo "[replay-pipeline] Stage 2/2: text replay"
"${replay_cmd[@]}"

echo "[replay-pipeline] Complete"
