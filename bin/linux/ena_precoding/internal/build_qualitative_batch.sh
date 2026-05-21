#!/usr/bin/env bash
set -euo pipefail

REPO="$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data"
PY_SCRIPT="$REPO/py_scripts/run/qualitative_coding/build_qualitative_batch.py"

cd "$REPO"
exec python3 "$PY_SCRIPT" "$@"
