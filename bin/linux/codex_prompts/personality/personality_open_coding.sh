#!/usr/bin/env bash
set -euo pipefail

REPO="$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data"
DEFAULT_PROMPT_FILE="$REPO/prompts/personality/personality_open_coding.md"
PROMPT_FILE="${1:-${PROMPT_FILE:-$DEFAULT_PROMPT_FILE}}"
RUNNER_SCRIPT="$REPO/py_scripts/run/stream_summaries/personality/personality_profile_v3_incremental_open_coding.py"
LOG_ROOT="/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts"
LOG_DIR="$LOG_ROOT/personality/personality_open_coding"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
RUN_SLUG="personality_open_coding_v3"
LOG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}.log"

mkdir -p "$LOG_DIR"

if [[ ! -f "$PROMPT_FILE" ]]; then
  echo "Error: prompt file not found: $PROMPT_FILE" >&2
  exit 1
fi

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
