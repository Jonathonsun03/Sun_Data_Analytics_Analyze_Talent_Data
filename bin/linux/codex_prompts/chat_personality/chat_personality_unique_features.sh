#!/usr/bin/env bash
set -euo pipefail

# Load repo .env defaults without overriding already-exported values.
_ENV_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
while [[ "${_ENV_ROOT}" != "/" ]]; do
  if [[ -f "${_ENV_ROOT}/AGENTS.md" && -d "${_ENV_ROOT}/r_scripts" ]]; then
    break
  fi
  _ENV_ROOT="$(dirname "${_ENV_ROOT}")"
done
if [[ -f "${_ENV_ROOT}/bin/linux/load_repo_env.sh" ]]; then
  # shellcheck source=/dev/null
  source "${_ENV_ROOT}/bin/linux/load_repo_env.sh"
  load_repo_env "${_ENV_ROOT}"
fi
unset _ENV_ROOT

REPO="${REPO:-${TALENT_REPO_ROOT:-$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data}}"
DEFAULT_PROMPT_FILE="$REPO/prompts/chat_personality/chat_personality_unique_features.md"
PROMPT_FILE="${PROMPT_FILE:-$DEFAULT_PROMPT_FILE}"
CODEX_MODEL="${CODEX_MODEL:-gpt-5.4}"
TALENT_SCOPE=""
RECENT_MONTHS=""
SINCE_DATE=""
UNTIL_DATE=""
TALENT_DATA_ROOT_FOR_LOGS="${TALENT_DATALAKE_ROOT:-/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data}"
ANALYTICS_ROOT_FOR_LOGS="${TALENT_DATA_ROOT_FOR_LOGS%/Talent_data}"
LOG_ROOT="${CODEX_PROMPTS_LOG_ROOT:-${ANALYTICS_ROOT_FOR_LOGS}/Processed/Logs/codex_prompts}"
LOG_DIR="$LOG_ROOT/chat_personality/chat_personality_unique_features"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
RUN_SLUG="chat_personality_unique_features"
LOG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}.log"
FINAL_MSG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}_final.md"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/codex_prompts/chat_personality/chat_personality_unique_features.sh [options] [prompt_file]

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
    --talent) TALENT_SCOPE="${2:-}"; shift 2 ;;
    --recent-months) RECENT_MONTHS="${2:-}"; shift 2 ;;
    --since) SINCE_DATE="${2:-}"; shift 2 ;;
    --until) UNTIL_DATE="${2:-}"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) PROMPT_FILE="$1"; shift ;;
  esac
done

if [[ -n "$RECENT_MONTHS" && ! "$RECENT_MONTHS" =~ ^[0-9]+$ ]]; then
  echo "Error: --recent-months requires a positive integer" >&2
  exit 1
fi
if [[ -n "$SINCE_DATE" && ! "$SINCE_DATE" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
  echo "Error: --since requires YYYY-MM-DD" >&2
  exit 1
fi
if [[ -n "$UNTIL_DATE" && ! "$UNTIL_DATE" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
  echo "Error: --until requires YYYY-MM-DD" >&2
  exit 1
fi
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
  {
    echo
    echo "Additional run scope requested by the user:"
    if [[ -n "$TALENT_SCOPE" ]]; then
      echo "- TALENT_SLUG: $TALENT_SCOPE"
      echo "- Process only the talent folder exactly named: $TALENT_SCOPE"
    fi
    if [[ "$HAS_WINDOW_SCOPE" -eq 1 ]]; then
      if [[ -n "$RECENT_MONTHS" ]]; then
        echo "- RECENT_MONTHS: $RECENT_MONTHS"
      fi
      echo "- WINDOW_START_DATE: ${WINDOW_START_DATE:-unbounded}"
      echo "- WINDOW_END_DATE: ${WINDOW_END_DATE:-unbounded}"
    fi
  } >> "$RUN_PROMPT_FILE"
fi

cd "$REPO"
echo "Checking repo updates..."
git pull --ff-only || true
echo "Starting Codex prompt run..."
echo "Prompt file: $PROMPT_FILE"
echo "Codex model: $CODEX_MODEL"
echo "Log file: $LOG_FILE"
echo "Final message file: $FINAL_MSG_FILE"

{
  echo "=== Codex run started: $(date) ==="
  echo "Repo: $REPO"
  echo "Prompt: $PROMPT_FILE"
  echo "Model: $CODEX_MODEL"
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

