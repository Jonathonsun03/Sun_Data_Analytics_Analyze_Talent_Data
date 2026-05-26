#!/usr/bin/env bash
set -euo pipefail

REPO="$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data"
R_SCRIPT="$REPO/r_scripts/run/transcript_selection/select_money_chat_transcripts.R"

cd "$REPO"
exec Rscript --vanilla "$R_SCRIPT" "$@"
