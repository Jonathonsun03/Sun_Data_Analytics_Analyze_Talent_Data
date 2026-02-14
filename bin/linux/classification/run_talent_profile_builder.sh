#!/usr/bin/env bash
set -euo pipefail

# Runs scripts/run/Title_classification/talent_profile/build_talent_profile.R from repo root.
# Passes all CLI arguments through to the R script.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="scripts/run/Title_classification/talent_profile/build_talent_profile.R"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/classification/run_talent_profile_builder.sh [builder args...]

Description:
  Wrapper for the talent profile builder R script:
  scripts/run/Title_classification/talent_profile/build_talent_profile.R

Examples:
  Single talent:
    bin/linux/classification/run_talent_profile_builder.sh \
      --csv notes/titles.csv \
      --talent "Terberri_Solaris_Ch" \
      --talent-col talent \
      --title-col "Title" \
      --content-type-col "Content Type" \
      --write-overlay \
      --update-master-config

  All talents:
    bin/linux/classification/run_talent_profile_builder.sh \
      --csv notes/titles.csv \
      --all-talents \
      --talent-col talent \
      --title-col "Title" \
      --content-type-col "Content Type" \
      --write-overlay \
      --update-master-config

  GPT-assisted:
    OPENAI_MODEL=gpt-5-mini \
    bin/linux/classification/run_talent_profile_builder.sh \
      --csv notes/titles.csv \
      --all-talents \
      --talent-col talent \
      --title-col "Title" \
      --content-type-col "Content Type" \
      --write-overlay \
      --update-master-config \
      --use-gpt \
      --sample-size 250
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
