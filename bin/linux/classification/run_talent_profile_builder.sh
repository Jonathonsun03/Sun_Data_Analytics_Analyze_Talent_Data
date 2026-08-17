#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="r_scripts/run/title_classification/talent_profile/build_talent_profile.R"

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  cat <<'EOF'
Usage:
  bin/linux/classification/run_talent_profile_builder.sh [builder options...]

Builds reusable profiles from canonical catalog.videos. The default is a dry
run. Pass --execute to publish into catalog.talent_profiles.

Examples:
  bin/linux/classification/run_talent_profile_builder.sh --talent TER4
  bin/linux/classification/run_talent_profile_builder.sh --talent TER4 --profile-version v7 --execute
  bin/linux/classification/run_talent_profile_builder.sh --all-talents --use-gpt --sample-size 250 --execute
EOF
  exit 0
fi

cd "${REPO_ROOT}"
Rscript "${R_SCRIPT}" "$@"
