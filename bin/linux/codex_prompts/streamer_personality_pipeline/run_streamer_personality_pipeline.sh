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
  bin/linux/codex_prompts/streamer_personality_pipeline/run_streamer_personality_pipeline.sh [options]

Options:
  --talent NAME      Run the full streamer personality pipeline for the exact talent folder NAME.
  --recent-months N  Limit the run focus to the last N months.
  --since DATE       Limit the run focus to data on or after DATE (YYYY-MM-DD).
  --until DATE       Limit the run focus to data on or before DATE (YYYY-MM-DD).
  --dry-run          Print the commands that would run without executing them.
  -h, --help         Show this help.

Pipeline order:
  1. streamer_personality_open_coding.sh
  2. shared_behavior_baseline.sh
  3. streamer_personality_unique_features.sh
  4. streamer_personality_qualitative_codebook.sh
  5. streamer_personality_profile.sh
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
  "scoped:$CODEX_PROMPTS_DIR/streamer_personality/streamer_personality_open_coding.sh"
  "global:$CODEX_PROMPTS_DIR/shared_qualities/shared_behavior_baseline.sh"
  "scoped:$CODEX_PROMPTS_DIR/streamer_personality/streamer_personality_unique_features.sh"
  "global:$CODEX_PROMPTS_DIR/streamer_personality/streamer_personality_qualitative_codebook.sh"
  "scoped:$CODEX_PROMPTS_DIR/streamer_personality/streamer_personality_profile.sh"
)

run_step() {
  local step_spec="$1"
  local step_scope="${step_spec%%:*}"
  local step_path="${step_spec#*:}"
  local step_name
  local step_args=()
  step_name="$(basename "$step_path")"

  if [[ ! -x "$step_path" ]]; then
    echo "Error: step is missing or not executable: $step_path" >&2
    exit 1
  fi

  if [[ -n "$TALENT_SCOPE" && "$step_scope" == "scoped" ]]; then
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
  echo "=== Running streamer personality pipeline step: $step_name ==="
  if [[ "$DRY_RUN" -eq 1 ]]; then
    printf '%q' "$step_path"
    if [[ "${#step_args[@]}" -gt 0 ]]; then
      printf ' %q' "${step_args[@]}"
    fi
    printf '\n'
  else
    if ! "$step_path" "${step_args[@]}"; then
      echo "Error: streamer personality pipeline step failed: $step_name" >&2
      exit 1
    fi
  fi
}

echo "Streamer personality pipeline runner"
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
  echo "Streamer personality pipeline complete."
fi
