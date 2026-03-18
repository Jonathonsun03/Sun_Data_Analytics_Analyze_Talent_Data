#!/usr/bin/env bash
set -euo pipefail

REPO="$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data"
DEFAULT_PROMPT_FILE="$REPO/prompts/summaries/summarizing_stream_v2.md"
PROMPT_FILE="${1:-${PROMPT_FILE:-$DEFAULT_PROMPT_FILE}}"
LOG_ROOT="/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts"
LOG_DIR="$LOG_ROOT/summaries/summarizing_stream_v2"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
RUN_SLUG="summarizing_stream_v2"
LOG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}.log"
FINAL_MSG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}_final.md"

mkdir -p "$LOG_DIR"

if [[ ! -f "$PROMPT_FILE" ]]; then
  echo "Error: prompt file not found: $PROMPT_FILE" >&2
  exit 1
fi

cd "$REPO"

echo "Checking repo updates..."
git pull --ff-only || true

echo "Starting Codex prompt test..."
echo "Prompt file: $PROMPT_FILE"
echo "Log file: $LOG_FILE"
echo "Final message file: $FINAL_MSG_FILE"

{
  echo "=== Codex run started: $(date) ==="
  echo "Repo: $REPO"
  echo "Prompt: $PROMPT_FILE"
  echo

  cat "$PROMPT_FILE" | codex exec \
    --cd "$REPO" \
    --dangerously-bypass-approvals-and-sandbox \
    --output-last-message "$FINAL_MSG_FILE" \
    -

  echo
  echo "=== Codex run finished: $(date) ==="
  echo "Final message saved to: $FINAL_MSG_FILE"
} >> "$LOG_FILE" 2>&1
