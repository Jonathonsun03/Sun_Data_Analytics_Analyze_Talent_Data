#!/usr/bin/env bash
set -euo pipefail

REPO="$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data"
DEFAULT_PROMPT_FILE="$REPO/prompts/summaries/stream_summary_codex.md"
PROMPT_FILE="${PROMPT_FILE:-$DEFAULT_PROMPT_FILE}"
TALENT_SCOPE=""
LIMIT_SCOPE=""
LOG_ROOT="/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts"
LOG_DIR="$LOG_ROOT/summaries/stream_summary_codex"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
RUN_SLUG="stream_summary_codex"
LOG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}.log"
FINAL_MSG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}_final.md"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/codex_prompts/summaries/stream_summary_codex.sh [options] [prompt_file]

Options:
  --talent NAME      Narrow this Codex run to one talent folder name.
  --limit N          Process at most N missing/empty stream summaries for the selected scope.
  -h, --help         Show this help.

Examples:
  Full maintained run:
    bin/linux/codex_prompts/summaries/stream_summary_codex.sh

  Smoke test on Leia only:
    bin/linux/codex_prompts/summaries/stream_summary_codex.sh --talent "Leia Memoria【Variance Project】" --limit 1
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
    --limit)
      LIMIT_SCOPE="${2:-}"
      if [[ -z "$LIMIT_SCOPE" ]]; then
        echo "Error: --limit requires a value" >&2
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
if [[ -n "$TALENT_SCOPE" || -n "$LIMIT_SCOPE" ]]; then
  RUN_PROMPT_FILE="$(mktemp)"
  cp "$PROMPT_FILE" "$RUN_PROMPT_FILE"
  {
    echo
    echo "Additional run scope requested by the user:"
    if [[ -n "$TALENT_SCOPE" ]]; then
      echo "- Process only the talent folder exactly named: $TALENT_SCOPE"
    fi
    if [[ -n "$LIMIT_SCOPE" ]]; then
      echo "- Process at most $LIMIT_SCOPE missing or empty stream summary file(s) in this run."
    fi
    echo "- This is a smoke test of the maintained shell runner; keep the final response concise and include the output path(s) touched."
  } >> "$RUN_PROMPT_FILE"
fi

cd "$REPO"

echo "Checking repo updates..."
git pull --ff-only || true

echo "Starting Codex stream-summary run..."
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
