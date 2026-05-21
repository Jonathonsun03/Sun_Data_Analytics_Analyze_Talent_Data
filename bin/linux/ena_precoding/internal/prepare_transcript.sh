#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"
R_SCRIPT="r_scripts/run/transcript_analysis/prepare_transcript.r"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/ena_precoding/prepare_transcript.sh [options]

Description:
  Prepare transcript CSVs for qualitative coding. By default this only writes
  missing output files, so reruns process newly added transcripts only.

Options:
  --talent-query VALUE           Talent selector passed to select_talent() (default: Avaritia)
  --coding-folder VALUE          Folder under "qualitative coding" (default: monetary conversation codes)
  --datalake-root PATH           Override TALENT_DATALAKE_ROOT
  --reprocess                    Overwrite existing prepared transcript files
  -h, --help                     Show this help

Examples:
  Single talent by partial name:
    bin/linux/ena_precoding/prepare_transcript.sh --talent-query "Avaritia"

  Process all talents, only missing outputs:
    bin/linux/ena_precoding/prepare_transcript.sh --talent-query all

  Rebuild outputs for one talent:
    bin/linux/ena_precoding/prepare_transcript.sh --talent-query "Avaritia" --reprocess
USAGE
}

TALENT_QUERY_VAL="Avaritia"
CODING_FOLDER_VAL="monetary conversation codes"
DATALAKE_ROOT_VAL=""
OVERWRITE_VAL="false"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --talent-query)
      [[ $# -ge 2 ]] || { echo "Error: --talent-query requires a value" >&2; exit 1; }
      TALENT_QUERY_VAL="$2"
      shift 2
      ;;
    --coding-folder)
      [[ $# -ge 2 ]] || { echo "Error: --coding-folder requires a value" >&2; exit 1; }
      CODING_FOLDER_VAL="$2"
      shift 2
      ;;
    --datalake-root)
      [[ $# -ge 2 ]] || { echo "Error: --datalake-root requires a value" >&2; exit 1; }
      DATALAKE_ROOT_VAL="$2"
      shift 2
      ;;
    --reprocess)
      OVERWRITE_VAL="true"
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
export QUALITATIVE_CODING_FOLDER="${CODING_FOLDER_VAL}"
export QUALITATIVE_PREP_OVERWRITE="${OVERWRITE_VAL}"

if [[ -n "${DATALAKE_ROOT_VAL}" ]]; then
  export TALENT_DATALAKE_ROOT="${DATALAKE_ROOT_VAL}"
fi

Rscript "${R_SCRIPT}"
