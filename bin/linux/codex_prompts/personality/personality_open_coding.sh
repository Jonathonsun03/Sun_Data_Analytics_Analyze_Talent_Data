#!/usr/bin/env bash
set -euo pipefail

REPO="$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data"
PROMPT_FILE="$REPO/prompts/personality/profile_v3_incremental_open_coding.md"
RUNNER_SCRIPT="$REPO/scripts/run/stream_summaries/personality/personality_profile_v3_incremental_open_coding.py"
LOG_DIR="$REPO/logs/codex"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
LOG_FILE="$LOG_DIR/personality_open_coding_v3_${RUN_TS}.log"

mkdir -p "$LOG_DIR"

cd "$REPO"

echo "Checking repo updates..."
git pull --ff-only || true

echo "Starting personality open-coding v3 runner..."
echo "Runner script: $RUNNER_SCRIPT"
echo "Prompt spec: $PROMPT_FILE"
echo "Log file: $LOG_FILE"

{
  echo "=== Codex run started: $(date) ==="
  echo "Repo: $REPO"
  echo "Runner: $RUNNER_SCRIPT"
  echo "Prompt: $PROMPT_FILE"
  echo

  python3 "$RUNNER_SCRIPT"

  echo
  echo "=== Personality open-coding runner finished: $(date) ==="
} >> "$LOG_FILE" 2>&1
