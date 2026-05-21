#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

BUILD_SCRIPT="r_scripts/run/Title_classification/title_classification/08_build_batch_classification.R"
APPLY_SCRIPT="r_scripts/run/Title_classification/title_classification/09_apply_batch_classification.R"
TITLE_EXPORT_SCRIPT="r_scripts/run/Title_analysis/Export_titles.r"
SUBMIT_SCRIPT="py_scripts/run/qualitative_coding/submit_qualitative_batch.py"
CHECK_SCRIPT="py_scripts/run/qualitative_coding/check_qualitative_batch.py"
EXPORT_SCRIPT="r_scripts/run/Title_classification/title_classification/05_export_results_csv.R"
CONFIG_JSON="classification/config/talent_profiles.json"

EXPORT_ROOT="${TITLE_CLASSIFICATIONS_DIR:-/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Title_classification}"
BATCH_RUN_ROOT="${TITLE_CLASSIFICATION_BATCH_RUN_ROOT:-${EXPORT_ROOT}/batch_runs}"

MODE="build"
RUN_ID=""
RUN_DIR=""
EXECUTE="false"
RETRIEVE_OUTPUT="false"
declare -a CLASSIFY_ARGS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/classification/run_title_classification_batch.sh [wrapper options] -- [build options]

Wrapper options:
  --mode build|submit|check|apply
  --run-id VALUE       Folder name under the title-classification batch_runs folder.
  --run-dir PATH       Existing run directory for submit/check/apply.
  --execute            Submit mode: upload and create the OpenAI Batch API job.
  --retrieve-output    Check mode: download output/error JSONL when available.
  -h, --help           Show this help.

Build options after --:
  --csv PATH
  --talent NAME
  --batch-size N       Rows per Batch API request. Default: 25.
  --model NAME
  --force-reclassify

Build mode refreshes notes/titles.csv from the DataLake before creating the
Batch API request file.

Examples:
  Build pending v7 requests:
    bin/linux/classification/run_title_classification_batch.sh --run-id "title_v7_$(date +%Y-%m-%d_%H-%M-%S)" -- --batch-size 25

  Submit after reviewing the run folder:
    bin/linux/classification/run_title_classification_batch.sh --mode submit --run-dir "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Title_classification/batch_runs/<run_id>" --execute

  Check later and retrieve output:
    bin/linux/classification/run_title_classification_batch.sh --mode check --run-dir "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Title_classification/batch_runs/<run_id>" --retrieve-output

  Apply retrieved output into DuckDB and refresh DataLake current/archive exports:
    bin/linux/classification/run_title_classification_batch.sh --mode apply --run-dir "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Title_classification/batch_runs/<run_id>"
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode) MODE="${2:-}"; shift 2 ;;
    --run-id) RUN_ID="${2:-}"; shift 2 ;;
    --run-dir) RUN_DIR="${2:-}"; shift 2 ;;
    --execute) EXECUTE="true"; shift ;;
    --retrieve-output) RETRIEVE_OUTPUT="true"; shift ;;
    --) shift; CLASSIFY_ARGS=("$@"); break ;;
    -h|--help) usage; exit 0 ;;
    *) CLASSIFY_ARGS+=("$1"); shift ;;
  esac
done

cd "${REPO_ROOT}"

if [[ -z "$RUN_DIR" && -n "$RUN_ID" ]]; then
  RUN_DIR="${BATCH_RUN_ROOT%/}/${RUN_ID}"
fi

refresh_exports() {
  local model prompt_version taxonomy_version stamp archive_csv current_csv
  model="${OPENAI_MODEL:-gpt-5-mini}"
  prompt_version="$(Rscript --vanilla -e "x <- jsonlite::fromJSON('${CONFIG_JSON}'); cat(x[['prompt_version']])" | tail -n 1 | tr -d '\r')"
  taxonomy_version="$(Rscript --vanilla -e "x <- jsonlite::fromJSON('${CONFIG_JSON}'); cat(x[['taxonomy_version']])" | tail -n 1 | tr -d '\r')"
  stamp="$(date -u +"%Y%m%d_%H%M%S")"
  mkdir -p "${EXPORT_ROOT}/current" "${EXPORT_ROOT}/archived"
  archive_csv="${EXPORT_ROOT}/archived/classification_export_${model}_${prompt_version}_${taxonomy_version}_${stamp}.csv"
  current_csv="${EXPORT_ROOT}/current/classification_export_current.csv"
  Rscript "${EXPORT_SCRIPT}" \
    --model "${model}" \
    --prompt-version "${prompt_version}" \
    --taxonomy-version "${taxonomy_version}" \
    --out "${archive_csv}"
  Rscript "${EXPORT_SCRIPT}" \
    --model "${model}" \
    --prompt-version "${prompt_version}" \
    --taxonomy-version "${taxonomy_version}" \
    --out "${current_csv}"
  echo "Current export: ${current_csv}"
  echo "Archived export: ${archive_csv}"
}

case "$MODE" in
  build)
    if [[ -z "$RUN_ID" ]]; then
      RUN_ID="title_classification_batch_$(date +%Y-%m-%d_%H-%M-%S_%z)"
      RUN_DIR="${BATCH_RUN_ROOT%/}/${RUN_ID}"
    fi
    echo "Refreshing notes/titles.csv from DataLake video analytics..."
    Rscript "${TITLE_EXPORT_SCRIPT}" --out "notes/titles.csv"
    Rscript "${BUILD_SCRIPT}" \
      --run-id "$RUN_ID" \
      --run-root "$BATCH_RUN_ROOT" \
      "${CLASSIFY_ARGS[@]}"
    echo "Run directory: ${RUN_DIR}"
    if [[ "$EXECUTE" == "true" ]]; then
      python3 "$SUBMIT_SCRIPT" --run-dir "$RUN_DIR" --execute
    else
      echo "Review the run folder, then submit with:"
      echo "  $0 --mode submit --run-dir \"$RUN_DIR\" --execute"
    fi
    ;;
  submit)
    if [[ -z "$RUN_DIR" ]]; then
      echo "Error: --run-dir or --run-id is required for submit mode." >&2
      exit 1
    fi
    submit_args=(--run-dir "$RUN_DIR")
    [[ "$EXECUTE" == "true" ]] && submit_args+=(--execute)
    python3 "$SUBMIT_SCRIPT" "${submit_args[@]}"
    ;;
  check)
    if [[ -z "$RUN_DIR" ]]; then
      echo "Error: --run-dir or --run-id is required for check mode." >&2
      exit 1
    fi
    check_args=(--run-dir "$RUN_DIR")
    [[ "$RETRIEVE_OUTPUT" == "true" ]] && check_args+=(--retrieve-output)
    python3 "$CHECK_SCRIPT" "${check_args[@]}"
    ;;
  apply)
    if [[ -z "$RUN_DIR" ]]; then
      echo "Error: --run-dir or --run-id is required for apply mode." >&2
      exit 1
    fi
    Rscript "${APPLY_SCRIPT}" --run-dir "$RUN_DIR"
    refresh_exports
    ;;
  *)
    echo "Error: unknown mode: $MODE" >&2
    usage >&2
    exit 1
    ;;
esac
