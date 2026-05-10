#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
CODEX_PROMPTS_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"

TALENT_SCOPE=""
SUMMARY_LIMIT=""
RECENT_MONTHS=""
SINCE_DATE=""
UNTIL_DATE=""
DRY_RUN=0
SKIP_SUMMARIES=0
SKIP_OVERALL=0
SKIP_MONETARY=0
SKIP_PERSONALITY=0

usage() {
  cat <<'EOF'
Usage:
  bin/linux/codex_prompts/all_analysis/run_stream_analysis_pipeline.sh [options]

Options:
  --talent NAME       Run for the exact talent folder NAME.
  --summary-limit N   Process at most N missing/empty per-stream summaries.
  --recent-months N   Pass a rolling date focus to the personality pipeline.
  --since DATE        Pass a start date focus to the personality pipeline (YYYY-MM-DD).
  --until DATE        Pass an end date focus to the personality pipeline (YYYY-MM-DD).
  --skip-summaries    Skip per-stream summary generation.
  --skip-overall      Skip overall channel summary / summary classification.
  --skip-monetary     Skip money timestamps and monetary analysis.
  --skip-personality  Skip personality open coding/profile/unique-feature pipeline.
  --dry-run           Print commands without executing them.
  -h, --help          Show this help.

Pipeline order:
  1. summaries/stream_summary_codex.sh
  2. overall_themes/overall_channel_summary.sh
  3. monetary/money_timestamps_incremental.sh
  4. monetary/monetary_summary_classification.sh
  5. personality_pipeline/run_personality_pipeline.sh

Notes:
  - The old "summary classification" layer is now represented by
    overall_channel_summary outputs, with legacy fallbacks still supported
    by downstream scripts.
  - Omit --talent to let each maintained step discover all eligible talents.
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
    --summary-limit)
      SUMMARY_LIMIT="${2:-}"
      if [[ ! "$SUMMARY_LIMIT" =~ ^[0-9]+$ ]] || [[ "$SUMMARY_LIMIT" -lt 1 ]]; then
        echo "Error: --summary-limit requires a positive integer" >&2
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
    --skip-summaries)
      SKIP_SUMMARIES=1
      shift
      ;;
    --skip-overall)
      SKIP_OVERALL=1
      shift
      ;;
    --skip-monetary)
      SKIP_MONETARY=1
      shift
      ;;
    --skip-personality)
      SKIP_PERSONALITY=1
      shift
      ;;
    --dry-run)
      DRY_RUN=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Error: unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ -n "$RECENT_MONTHS" && -n "$SINCE_DATE" ]]; then
  echo "Error: use either --recent-months or --since, not both" >&2
  exit 1
fi

run_step() {
  local label="$1"
  shift
  local cmd=("$@")

  echo
  echo "=== Running analysis step: $label ==="
  if [[ "$DRY_RUN" -eq 1 ]]; then
    printf '%q' "${cmd[0]}"
    printf ' %q' "${cmd[@]:1}"
    printf '\n'
  else
    if ! "${cmd[@]}"; then
      echo "Error: analysis step failed: $label" >&2
      exit 1
    fi
  fi
}

add_talent_arg() {
  local -n target_args="$1"
  if [[ -n "$TALENT_SCOPE" ]]; then
    target_args+=(--talent "$TALENT_SCOPE")
  fi
}

echo "Stream analysis pipeline runner"
if [[ -n "$TALENT_SCOPE" ]]; then
  echo "Talent scope: $TALENT_SCOPE"
else
  echo "Talent scope: all eligible talents"
fi

if [[ "$SKIP_SUMMARIES" -eq 0 ]]; then
  summary_args=()
  add_talent_arg summary_args
  if [[ -n "$SUMMARY_LIMIT" ]]; then
    summary_args+=(--limit "$SUMMARY_LIMIT")
  fi
  run_step "per-stream summaries" "$CODEX_PROMPTS_DIR/summaries/stream_summary_codex.sh" "${summary_args[@]}"
else
  echo "Skipping per-stream summaries."
fi

if [[ "$SKIP_OVERALL" -eq 0 ]]; then
  overall_args=()
  add_talent_arg overall_args
  run_step "overall channel summary / summary classification" "$CODEX_PROMPTS_DIR/overall_themes/overall_channel_summary.sh" "${overall_args[@]}"
else
  echo "Skipping overall channel summary / summary classification."
fi

if [[ "$SKIP_MONETARY" -eq 0 ]]; then
  money_timestamp_args=()
  monetary_args=()
  add_talent_arg money_timestamp_args
  add_talent_arg monetary_args
  run_step "money timestamps" "$CODEX_PROMPTS_DIR/monetary/money_timestamps_incremental.sh" "${money_timestamp_args[@]}"
  run_step "monetary summary classification" "$CODEX_PROMPTS_DIR/monetary/monetary_summary_classification.sh" "${monetary_args[@]}"
else
  echo "Skipping monetary analysis."
fi

if [[ "$SKIP_PERSONALITY" -eq 0 ]]; then
  personality_args=()
  add_talent_arg personality_args
  if [[ -n "$RECENT_MONTHS" ]]; then
    personality_args+=(--recent-months "$RECENT_MONTHS")
  fi
  if [[ -n "$SINCE_DATE" ]]; then
    personality_args+=(--since "$SINCE_DATE")
  fi
  if [[ -n "$UNTIL_DATE" ]]; then
    personality_args+=(--until "$UNTIL_DATE")
  fi
  if [[ "$DRY_RUN" -eq 1 ]]; then
    personality_args+=(--dry-run)
  fi
  run_step "personality pipeline" "$CODEX_PROMPTS_DIR/personality_pipeline/run_personality_pipeline.sh" "${personality_args[@]}"
else
  echo "Skipping personality pipeline."
fi

echo
if [[ "$DRY_RUN" -eq 1 ]]; then
  echo "Dry run complete. No analysis steps were executed."
else
  echo "Stream analysis pipeline complete."
fi
