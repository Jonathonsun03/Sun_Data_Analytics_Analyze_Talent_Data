#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="scripts/run/text_replay/render_text_replay.R"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/test_stream_replay/run_text_replay.sh [options]

Description:
  Wrapper for text replay generation:
  scripts/run/text_replay/render_text_replay.R

Options:
  --talent-query VALUE           Talent selector passed to select_talent() (default: all)
  --datalake-root PATH           Override TALENT_DATALAKE_ROOT
  --n-cores N                    TEXT_REPLAY_N_CORES for parallel processing (default: 1)
  -h, --help                     Show this help

Examples:
  All talents:
    bin/linux/test_stream_replay/run_text_replay.sh

  Single talent:
    bin/linux/test_stream_replay/run_text_replay.sh --talent-query "Avaritia"

  Single talent with 8 cores:
    bin/linux/test_stream_replay/run_text_replay.sh --talent-query "Avaritia" --n-cores 8
USAGE
}

TALENT_QUERY_VAL="all"
DATALAKE_ROOT_VAL=""
N_CORES_VAL="1"

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

export TALENT_QUERY="${TALENT_QUERY_VAL}"
export TEXT_REPLAY_N_CORES="${N_CORES_VAL}"

if [[ -n "${DATALAKE_ROOT_VAL}" ]]; then
  export TALENT_DATALAKE_ROOT="${DATALAKE_ROOT_VAL}"
fi

Rscript "${R_SCRIPT}"
