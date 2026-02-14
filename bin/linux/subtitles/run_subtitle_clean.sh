#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="scripts/run/Subtitle_clean/Subtitle_clean.R"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/subtitles/run_subtitle_clean.sh [options]

Description:
  Wrapper for subtitle cleaning + summary generation:
  scripts/run/Subtitle_clean/Subtitle_clean.R

Options:
  --talent-query VALUE           Talent selector passed to select_talent() (default: all)
  --datalake-root PATH           Override TALENT_DATALAKE_ROOT
  --quotes-per-talent N          SUBTITLE_QUOTES_PER_TALENT (default: 3)
  --context-rows N               SUBTITLE_CONTEXT_ROWS (default: 10)
  --top-k-sheets N               SUBTITLE_TOP_K_SHEETS (default: 1)
  --pause-gap-sec N              SUBTITLE_PAUSE_GAP_SEC (default: 2.0)
  --n-cores N                    SUBTITLE_N_CORES for parallel work (default: 1)
  --write-ena-txt                Also write subtitle_ena_units.txt
  --reclean                      Reprocess existing subtitle CSVs (skip_existing = FALSE)
  --ena-as-final                 Write ENA output back to each Subtitles/Processed video CSV
  -h, --help                     Show this help

Examples:
  All talents:
    bin/linux/subtitles/run_subtitle_clean.sh

  Single talent query:
    bin/linux/subtitles/run_subtitle_clean.sh --talent-query "Avaritia"

  Custom root and quote sampling:
    bin/linux/subtitles/run_subtitle_clean.sh \
      --datalake-root /mnt/datalake/Datalake/Sun_Data_Analytics/Talent_data \
      --quotes-per-talent 5 \
      --context-rows 12 \
      --top-k-sheets 2 \
      --pause-gap-sec 2.5 \
      --n-cores 4 \
      --write-ena-txt \
      --reclean \
      --ena-as-final
USAGE
}

TALENT_QUERY_VAL="all"
DATALAKE_ROOT_VAL=""
QUOTES_PER_TALENT_VAL="3"
CONTEXT_ROWS_VAL="10"
TOP_K_SHEETS_VAL="1"
PAUSE_GAP_SEC_VAL="2.0"
N_CORES_VAL="1"
WRITE_ENA_TXT_VAL="false"
RECLEAN_VAL="false"
ENA_AS_FINAL_VAL="false"

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
    --quotes-per-talent)
      [[ $# -ge 2 ]] || { echo "Error: --quotes-per-talent requires a value" >&2; exit 1; }
      QUOTES_PER_TALENT_VAL="$2"
      shift 2
      ;;
    --context-rows)
      [[ $# -ge 2 ]] || { echo "Error: --context-rows requires a value" >&2; exit 1; }
      CONTEXT_ROWS_VAL="$2"
      shift 2
      ;;
    --top-k-sheets)
      [[ $# -ge 2 ]] || { echo "Error: --top-k-sheets requires a value" >&2; exit 1; }
      TOP_K_SHEETS_VAL="$2"
      shift 2
      ;;
    --pause-gap-sec)
      [[ $# -ge 2 ]] || { echo "Error: --pause-gap-sec requires a value" >&2; exit 1; }
      PAUSE_GAP_SEC_VAL="$2"
      shift 2
      ;;
    --n-cores)
      [[ $# -ge 2 ]] || { echo "Error: --n-cores requires a value" >&2; exit 1; }
      N_CORES_VAL="$2"
      shift 2
      ;;
    --write-ena-txt)
      WRITE_ENA_TXT_VAL="true"
      shift
      ;;
    --reclean)
      RECLEAN_VAL="true"
      shift
      ;;
    --ena-as-final)
      ENA_AS_FINAL_VAL="true"
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

export TALENT_QUERY="${TALENT_QUERY_VAL}"
export SUBTITLE_QUOTES_PER_TALENT="${QUOTES_PER_TALENT_VAL}"
export SUBTITLE_CONTEXT_ROWS="${CONTEXT_ROWS_VAL}"
export SUBTITLE_TOP_K_SHEETS="${TOP_K_SHEETS_VAL}"
export SUBTITLE_PAUSE_GAP_SEC="${PAUSE_GAP_SEC_VAL}"
export SUBTITLE_N_CORES="${N_CORES_VAL}"
export SUBTITLE_WRITE_ENA_TXT="${WRITE_ENA_TXT_VAL}"
export SUBTITLE_RECLEAN="${RECLEAN_VAL}"
export SUBTITLE_ENA_AS_FINAL="${ENA_AS_FINAL_VAL}"

if [[ -n "${DATALAKE_ROOT_VAL}" ]]; then
  export TALENT_DATALAKE_ROOT="${DATALAKE_ROOT_VAL}"
fi

Rscript "${R_SCRIPT}"
