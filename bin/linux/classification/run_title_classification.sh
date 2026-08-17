#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
BATCH_RUNNER="bin/linux/classification/run_title_classification_batch.sh"

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  cat <<'EOF'
Usage:
  bin/linux/classification/run_title_classification.sh [build options...]

Compatibility entrypoint for the Batch API title-classification runner. It
queries canonical catalog.videos and skips unchanged titles already classified
for the active title version.

Common options:
  --talent NAME_OR_CODE
  --limit-per-talent N
  --model NAME
  --batch-size N
  --force-reclassify
EOF
  exit 0
fi

cd "${REPO_ROOT}"
"${BATCH_RUNNER}" -- "$@"
