#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="scripts/run/Title_classification/title_classification/07_run_weekly_classification.R"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/classification/run_title_classification.sh [options]

Description:
  Weekly title-classification runner.
  - Ingests titles from CSV into DuckDB videos table
  - Classifies only pending rows by default (idempotent)
  - Supports force reclassify for prompt testing

Options:
  --csv PATH                     Input CSV (default: notes/titles.csv)
  --talent-col NAME              Talent column (default: talent)
  --video-id-col NAME            Video ID column (default: Video ID)
  --title-col NAME               Title column (default: Title)
  --content-type-col NAME        Content type column (default: Content Type)
  --published-at-col NAME        Optional published datetime column
  --talent NAME                  Optional: run a single talent only
  --limit-per-talent N           Optional smoke test limit per talent (default: 0 = all rows)
  --model NAME                   Model override (default: gpt-5-mini)
  --batch-size N                 Batch size (default: 25)
  --max-retries N                Retries per batch (default: 2)
  --force-reclassify             Reclassify even if rows already classified
  -h, --help                     Show this help

Examples:
  Weekly run (all pending):
    bin/linux/classification/run_title_classification.sh

  Weekly run with explicit model:
    bin/linux/classification/run_title_classification.sh --model gpt-5-mini

  Smoke test (5 per talent):
    bin/linux/classification/run_title_classification.sh --limit-per-talent 5

  Prompt test rerun (force):
    bin/linux/classification/run_title_classification.sh --limit-per-talent 5 --force-reclassify
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

cd "${REPO_ROOT}"

if [[ ! -f "${R_SCRIPT}" ]]; then
  echo "Error: missing R script at ${R_SCRIPT}" >&2
  exit 1
fi

Rscript "${R_SCRIPT}" "$@"
