#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
CODEX_PROMPTS_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"

TALENT_SCOPE=""
RECENT_MONTHS=""
SINCE_DATE=""
UNTIL_DATE=""
DRY_RUN=0

usage() {
  cat <<'EOF'
Usage:
  bin/linux/codex_prompts/personality_pipeline/run_personality_pipeline.sh [options]

Options:
  --talent NAME      Run the full personality pipeline for the exact talent folder NAME.
  --recent-months N  Limit the run focus to the last N months.
  --since DATE       Limit the run focus to data on or after DATE (YYYY-MM-DD).
  --until DATE       Limit the run focus to data on or before DATE (YYYY-MM-DD).
  --dry-run          Print the commands that would run without executing them.
  -h, --help         Show this help.

Pipeline order:
  1. personality_open_coding.sh
  2. shared_behavior_baseline.sh
  3. personality_unique_features.sh
  4. personality_qualitative_codebook.sh
  5. personality_profile.sh
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

STEPS=(
  "$CODEX_PROMPTS_DIR/personality/personality_open_coding.sh"
  "$CODEX_PROMPTS_DIR/shared_qualities/shared_behavior_baseline.sh"
  "$CODEX_PROMPTS_DIR/personality/personality_unique_features.sh"
  "$CODEX_PROMPTS_DIR/personality/personality_qualitative_codebook.sh"
  "$CODEX_PROMPTS_DIR/personality/personality_profile.sh"
)

run_step() {
  local step_path="$1"
  local step_name
  local step_args=()
  step_name="$(basename "$step_path")"

  if [[ ! -x "$step_path" ]]; then
    echo "Error: step is missing or not executable: $step_path" >&2
    exit 1
  fi

  if [[ -n "$TALENT_SCOPE" ]]; then
    step_args+=(--talent "$TALENT_SCOPE")
  fi
  if [[ -n "$RECENT_MONTHS" ]]; then
    step_args+=(--recent-months "$RECENT_MONTHS")
  fi
  if [[ -n "$SINCE_DATE" ]]; then
    step_args+=(--since "$SINCE_DATE")
  fi
  if [[ -n "$UNTIL_DATE" ]]; then
    step_args+=(--until "$UNTIL_DATE")
  fi

  echo
  echo "=== Running personality pipeline step: $step_name ==="
  if [[ "$DRY_RUN" -eq 1 ]]; then
    printf '%q' "$step_path"
    printf ' %q' "${step_args[@]}"
    printf '\n'
  else
    if ! "$step_path" "${step_args[@]}"; then
      echo "Error: personality pipeline step failed: $step_name" >&2
      exit 1
    fi
  fi
}

echo "Personality pipeline runner"
if [[ -n "$TALENT_SCOPE" ]]; then
  echo "Talent scope: $TALENT_SCOPE"
else
  echo "Talent scope: all eligible talents"
fi
if [[ -n "$RECENT_MONTHS" ]]; then
  echo "Window scope: last $RECENT_MONTHS month(s)"
elif [[ -n "$SINCE_DATE" || -n "$UNTIL_DATE" ]]; then
  echo "Window scope: ${SINCE_DATE:-beginning} to ${UNTIL_DATE:-today}"
else
  echo "Window scope: no date limit"
fi

for step in "${STEPS[@]}"; do
  run_step "$step"
done

echo
if [[ "$DRY_RUN" -eq 1 ]]; then
  echo "Dry run complete. No pipeline steps were executed."
else
  echo "Personality pipeline complete."
fi
