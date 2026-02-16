#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

CLASSIFY_RUNNER="bin/linux/classification/run_title_classification.sh"
EXPORT_SCRIPT="scripts/run/Title_classification/title_classification/05_export_results_csv.R"
CONFIG_JSON="classification/config/talent_profiles.json"
EXPORT_DIR="classification/output/title_classifications"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/classification/run_title_classification_weekly.sh [classification options...]

Description:
  Weekly wrapper that:
    1) runs title classification
    2) exports a CSV snapshot for the current model/version

  Export location:
    classification/output/title_classifications

Examples:
  Full weekly run:
    bin/linux/classification/run_title_classification_weekly.sh

  Smoke test:
    bin/linux/classification/run_title_classification_weekly.sh --limit-per-talent 5

  Force prompt-test rerun:
    bin/linux/classification/run_title_classification_weekly.sh --limit-per-talent 5 --force-reclassify
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

cd "${REPO_ROOT}"

if [[ ! -x "${CLASSIFY_RUNNER}" ]]; then
  echo "Error: missing/executable runner: ${CLASSIFY_RUNNER}" >&2
  exit 1
fi

if [[ ! -f "${EXPORT_SCRIPT}" ]]; then
  echo "Error: missing export script: ${EXPORT_SCRIPT}" >&2
  exit 1
fi

if [[ ! -f "${CONFIG_JSON}" ]]; then
  echo "Error: missing config: ${CONFIG_JSON}" >&2
  exit 1
fi

mkdir -p "${EXPORT_DIR}"

echo "[weekly-title] Stage 1/2: classification"
"${CLASSIFY_RUNNER}" "$@"

MODEL="${OPENAI_MODEL:-gpt-5-mini}"
PROMPT_VERSION="$(Rscript -e "x <- jsonlite::fromJSON('${CONFIG_JSON}'); cat(x[['prompt_version']])")"
TAXONOMY_VERSION="$(Rscript -e "x <- jsonlite::fromJSON('${CONFIG_JSON}'); cat(x[['taxonomy_version']])")"
STAMP="$(date -u +"%Y%m%d_%H%M%S")"
OUT_CSV="${EXPORT_DIR}/classification_export_${MODEL}_${PROMPT_VERSION}_${TAXONOMY_VERSION}_${STAMP}.csv"

echo "[weekly-title] Stage 2/2: export"
Rscript "${EXPORT_SCRIPT}" \
  --model "${MODEL}" \
  --prompt-version "${PROMPT_VERSION}" \
  --taxonomy-version "${TAXONOMY_VERSION}" \
  --out "${OUT_CSV}"

echo "[weekly-title] Complete"
echo "[weekly-title] Export: ${OUT_CSV}"
