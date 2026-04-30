#!/usr/bin/env bash
set -euo pipefail

REPO="$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data"
DEFAULT_PROMPT_FILE="$REPO/prompts/personality/personality_qualitative_codebook.md"
PROMPT_FILE="${PROMPT_FILE:-$DEFAULT_PROMPT_FILE}"
CODEX_MODEL="${CODEX_MODEL:-gpt-5.4}"
TALENT_SCOPE=""
RECENT_MONTHS=""
SINCE_DATE=""
UNTIL_DATE=""
LOG_ROOT="/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts"
LOG_DIR="$LOG_ROOT/personality/personality_qualitative_codebook"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
RUN_SLUG="personality_qualitative_codebook"
LOG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}.log"
FINAL_MSG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}_final.md"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/codex_prompts/personality/personality_qualitative_codebook.sh [options] [prompt_file]

Options:
  --talent NAME      Process only the talent folder exactly named NAME.
  --recent-months N  Focus this run on the last N months.
  --since DATE       Focus this run on data on or after DATE (YYYY-MM-DD).
  --until DATE       Focus this run on data on or before DATE (YYYY-MM-DD).
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
    --recent-months)
      RECENT_MONTHS="${2:-}"
      if [[ ! "$RECENT_MONTHS" =~ ^[0-9]+$ ]] || [[ "$RECENT_MONTHS" -lt 1 ]]; then
        echo "Error: --recent-months requires a positive integer" >&2
        exit 1
      fi
      shift 2
      ;;
    --since)
      SINCE_DATE="${2:-}"
      if [[ ! "$SINCE_DATE" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
        echo "Error: --since requires YYYY-MM-DD" >&2
        exit 1
      fi
      shift 2
      ;;
    --until)
      UNTIL_DATE="${2:-}"
      if [[ ! "$UNTIL_DATE" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
        echo "Error: --until requires YYYY-MM-DD" >&2
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

if [[ -n "$RECENT_MONTHS" && -n "$SINCE_DATE" ]]; then
  echo "Error: use either --recent-months or --since, not both" >&2
  exit 1
fi

mkdir -p "$LOG_DIR"

if [[ ! -f "$PROMPT_FILE" ]]; then
  echo "Error: prompt file not found: $PROMPT_FILE" >&2
  exit 1
fi

WINDOW_START_DATE="$SINCE_DATE"
WINDOW_END_DATE="${UNTIL_DATE:-$(date +%F)}"
if [[ -n "$RECENT_MONTHS" ]]; then
  WINDOW_START_DATE="$(date -d "$RECENT_MONTHS months ago" +%F)"
fi
HAS_WINDOW_SCOPE=0
if [[ -n "$WINDOW_START_DATE" || -n "$UNTIL_DATE" || -n "$RECENT_MONTHS" ]]; then
  HAS_WINDOW_SCOPE=1
fi

RUN_PROMPT_FILE="$PROMPT_FILE"
if [[ -n "$TALENT_SCOPE" || "$HAS_WINDOW_SCOPE" -eq 1 ]]; then
  RUN_PROMPT_FILE="$(mktemp)"
  cp "$PROMPT_FILE" "$RUN_PROMPT_FILE"
  if [[ -n "$TALENT_SCOPE" ]]; then
    {
      echo
      echo "Additional talent scope requested by the user:"
      echo "- TALENT_SLUG: $TALENT_SCOPE"
      echo "- Process only the talent folder exactly named: $TALENT_SCOPE"
      echo "- Replace any talent placeholder or slug in this prompt with the exact talent folder name above."
      echo "- Keep the final response concise and include output path(s) touched."
    } >> "$RUN_PROMPT_FILE"
  fi
  if [[ "$HAS_WINDOW_SCOPE" -eq 1 ]]; then
    {
      echo
      echo "Additional date-window scope requested by the user:"
      if [[ -n "$RECENT_MONTHS" ]]; then
        echo "- RECENT_MONTHS: $RECENT_MONTHS"
      fi
      echo "- WINDOW_START_DATE: ${WINDOW_START_DATE:-unbounded}"
      echo "- WINDOW_END_DATE: ${WINDOW_END_DATE:-unbounded}"
      echo "- Focus new analysis on streams, summaries, rows, and evidence whose stream date, publish date, report month, or source metadata falls inside this window."
      echo "- Use older existing outputs only as continuity, cumulative state, and comparison context; do not recode or resynthesize older material unless required to preserve the current output schema."
      echo "- If a source item has no resolvable date or month, skip it for window-limited new analysis and mention the skip count if available."
    } >> "$RUN_PROMPT_FILE"
  fi
fi

cd "$REPO"

echo "Checking repo updates..."
git pull --ff-only || true

echo "Starting Codex prompt run..."
echo "Prompt file: $PROMPT_FILE"
echo "Codex model: $CODEX_MODEL"
if [[ "$HAS_WINDOW_SCOPE" -eq 1 ]]; then
  echo "Window scope: ${WINDOW_START_DATE:-unbounded} to ${WINDOW_END_DATE:-unbounded}"
fi
if [[ "$RUN_PROMPT_FILE" != "$PROMPT_FILE" ]]; then
  echo "Scoped prompt file: $RUN_PROMPT_FILE"
fi
echo "Log file: $LOG_FILE"
echo "Final message file: $FINAL_MSG_FILE"

{
  echo "=== Codex run started: $(date) ==="
  echo "Repo: $REPO"
  echo "Prompt: $PROMPT_FILE"
  echo "Model: $CODEX_MODEL"
  if [[ "$HAS_WINDOW_SCOPE" -eq 1 ]]; then
    echo "Window: ${WINDOW_START_DATE:-unbounded} to ${WINDOW_END_DATE:-unbounded}"
  fi
  if [[ "$RUN_PROMPT_FILE" != "$PROMPT_FILE" ]]; then
    echo "Scoped prompt: $RUN_PROMPT_FILE"
  fi
  echo

  cat "$RUN_PROMPT_FILE" | codex exec \
    --cd "$REPO" \
    --model "$CODEX_MODEL" \
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
