#!/usr/bin/env bash
set -euo pipefail

# Load repo .env defaults without overriding already-exported values.
_ENV_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
while [[ "${_ENV_ROOT}" != "/" ]]; do
  if [[ -e "${_ENV_ROOT}/.git" ]]; then
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
DEFAULT_PROMPT_FILE="$REPO/prompts/shared_qualities/shared_behavior_baseline.md"
PROMPT_FILE="${PROMPT_FILE:-$DEFAULT_PROMPT_FILE}"
CODEX_MODEL="${CODEX_MODEL:-gpt-5.4}"
TALENT_SCOPE=""
RECENT_MONTHS=""
SINCE_DATE=""
UNTIL_DATE=""
PROFILE_DAYS="31"
INCLUDE_STALE_PROFILES=0
TALENT_DATA_ROOT_FOR_LOGS="${TALENT_DATALAKE_ROOT:?Set TALENT_DATA_ROOT or TALENT_DATALAKE_ROOT in .env}"
ANALYTICS_ROOT_FOR_LOGS="${TALENT_DATA_ROOT_FOR_LOGS%/Talent_data}"
LOG_ROOT="${CODEX_PROMPTS_LOG_ROOT:-${ANALYTICS_ROOT_FOR_LOGS}/Processed/Logs/codex_prompts}"
LOG_DIR="$LOG_ROOT/shared_qualities/shared_behavior_baseline"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
RUN_SLUG="shared_behavior_baseline"
LOG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}.log"
FINAL_MSG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}_final.md"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/codex_prompts/shared_qualities/shared_behavior_baseline.sh [options] [prompt_file]

Options:
  --talent NAME      Focus this run on the talent folder exactly named NAME.
  --recent-months N  Focus this run on the last N months.
  --since DATE       Focus this run on data on or after DATE (YYYY-MM-DD).
  --until DATE       Focus this run on data on or before DATE (YYYY-MM-DD).
  --profile-days N   Eligible talents need a personality profile modified in the last N days. Default: 31.
  --include-stale-profiles
                     Include older personality profiles if open-coding outputs are usable.
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
    --profile-days)
      PROFILE_DAYS="${2:-}"
      if [[ ! "$PROFILE_DAYS" =~ ^[0-9]+$ ]] || [[ "$PROFILE_DAYS" -lt 1 ]]; then
        echo "Error: --profile-days requires a positive integer" >&2
        exit 1
      fi
      shift 2
      ;;
    --include-stale-profiles)
      INCLUDE_STALE_PROFILES=1
      shift
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
if [[ -n "$TALENT_SCOPE" || "$HAS_WINDOW_SCOPE" -eq 1 || -n "$PROFILE_DAYS" || "$INCLUDE_STALE_PROFILES" -eq 1 ]]; then
  RUN_PROMPT_FILE="$(mktemp)"
  cp "$PROMPT_FILE" "$RUN_PROMPT_FILE"
  if [[ -n "$TALENT_SCOPE" ]]; then
    {
      echo
      echo "Additional talent scope requested by the user:"
      echo "- TALENT_SLUG: $TALENT_SCOPE"
      echo "- Focus this run on the talent folder exactly named: $TALENT_SCOPE"
      echo "- Replace any talent placeholder or slug in this prompt with the exact talent folder name above."
      echo "- If the workflow is cross-talent, use other eligible talents only as comparison context needed for the focused output."
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
  {
    echo
    echo "Additional shared-interactions eligibility scope:"
    echo "- PROFILE_DAYS: $PROFILE_DAYS"
    if [[ "$INCLUDE_STALE_PROFILES" -eq 1 ]]; then
      echo "- INCLUDE_STALE_PROFILES: yes"
      echo "- Include talents with older personality_profile outputs when their personality_open_coding outputs are usable."
    else
      echo "- INCLUDE_STALE_PROFILES: no"
      echo "- Include only talents with a non-empty personality_profile overall output modified within PROFILE_DAYS."
    fi
    echo "- Use py_scripts/lib/shared_interactions_eligibility.py as the source of truth for eligible talent discovery."
    echo "- If using the maintained Python builder, pass --profile-days $PROFILE_DAYS$(if [[ "$INCLUDE_STALE_PROFILES" -eq 1 ]]; then printf ' --include-stale-profiles'; fi)."
  } >> "$RUN_PROMPT_FILE"
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
