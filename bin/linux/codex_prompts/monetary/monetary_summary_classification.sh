#!/usr/bin/env bash
set -euo pipefail

REPO="$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data"
DEFAULT_PROMPT_FILE="$REPO/prompts/monetary/monetary_summary_classification.md"
PROMPT_FILE="${PROMPT_FILE:-$DEFAULT_PROMPT_FILE}"
TALENT_SCOPE=""
LOG_ROOT="/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts"
LOG_DIR="$LOG_ROOT/monetary/monetary_summary_classification"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
RUN_SLUG="monetary_summary_classification"
LOG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}.log"
FINAL_MSG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}_final.md"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/codex_prompts/monetary/monetary_summary_classification.sh [options] [prompt_file]

Options:
  --talent NAME      Process only the talent folder exactly named NAME.
  -h, --help         Show this help.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --talent)
      TALENT_SCOPE="${2:-}"
      if [[ -z "$TALENT_SCOPE" ]]; then
        echo "Error: --talent requires a value" >&2
        exit 1
      fi
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      PROMPT_FILE="$1"
      shift
      ;;
  esac
done

mkdir -p "$LOG_DIR"

if [[ ! -f "$PROMPT_FILE" ]]; then
  echo "Error: prompt file not found: $PROMPT_FILE" >&2
  exit 1
fi

RUN_PROMPT_FILE="$PROMPT_FILE"
if [[ -n "$TALENT_SCOPE" ]]; then
  RUN_PROMPT_FILE="$(mktemp)"
  cp "$PROMPT_FILE" "$RUN_PROMPT_FILE"
  {
    echo
    echo "Additional talent scope requested by the user:"
    echo "- TALENT_SLUG: $TALENT_SCOPE"
    echo "- Process only the talent folder exactly named: $TALENT_SCOPE"
    echo "- Replace any talent placeholder or slug in this prompt with the exact talent folder name above."
    echo "- Keep the final response concise and include output path(s) touched."
  } >> "$RUN_PROMPT_FILE"
fi

cd "$REPO"

echo "Checking repo updates..."
git pull --ff-only || true

echo "Starting Codex prompt run..."
echo "Prompt file: $PROMPT_FILE"
if [[ "$RUN_PROMPT_FILE" != "$PROMPT_FILE" ]]; then
  echo "Scoped prompt file: $RUN_PROMPT_FILE"
fi
echo "Log file: $LOG_FILE"
echo "Final message file: $FINAL_MSG_FILE"

{
  echo "=== Codex run started: $(date) ==="
  echo "Repo: $REPO"
  echo "Prompt: $PROMPT_FILE"
  if [[ "$RUN_PROMPT_FILE" != "$PROMPT_FILE" ]]; then
    echo "Scoped prompt: $RUN_PROMPT_FILE"
  fi
  echo

  cat "$RUN_PROMPT_FILE" | codex exec \
    --cd "$REPO" \
    --dangerously-bypass-approvals-and-sandbox \
    --output-last-message "$FINAL_MSG_FILE" \
    -

  echo
  echo "=== Codex run finished: $(date) ==="
  echo "Final message saved to: $FINAL_MSG_FILE"
} >> "$LOG_FILE" 2>&1

if [[ "$RUN_PROMPT_FILE" != "$PROMPT_FILE" ]]; then
  rm -f "$RUN_PROMPT_FILE"
fi
