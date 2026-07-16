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
DEFAULT_PROMPT_FILE="$REPO/prompts/qualitative_coding/monetary_conversation/prompt_wrapper.md"
PROMPT_FILE="${PROMPT_FILE:-$DEFAULT_PROMPT_FILE}"
CODEX_BIN="${CODEX_BIN:-codex}"
CODEX_MODEL="${CODEX_MODEL:-}"

TALENT_SCOPE="Avaritia"
TRANSCRIPT_SCOPE=""
LIMIT_SCOPE=""
ROW_LIMIT_SCOPE=""
CODING_FOLDER_SCOPE="monetary conversation codes"
CODEBOOK_SCOPE="current"
DRY_RUN_SCOPE="false"
REPROCESS_SCOPE="false"

TALENT_DATA_ROOT_FOR_LOGS="${TALENT_DATALAKE_ROOT:?Set TALENT_DATA_ROOT or TALENT_DATALAKE_ROOT in .env}"
ANALYTICS_ROOT_FOR_LOGS="${TALENT_DATA_ROOT_FOR_LOGS%/Talent_data}"
LOG_ROOT="${CODEX_PROMPTS_LOG_ROOT:-${ANALYTICS_ROOT_FOR_LOGS}/Processed/Logs/codex_prompts}"
LOG_DIR="$LOG_ROOT/qualitative_coding/monetary_conversation"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
RUN_SLUG="code_qualitative_transcripts"
LOG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}.log"
FINAL_MSG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}_final.md"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/ena_precoding/code_qualitative_transcripts.sh [options] [prompt_file]

Description:
  Cron-friendly Codex runner for the qualitative coding stage. It feeds
  prompts/qualitative_coding/monetary_conversation/prompt_wrapper.md into
  `codex exec`, with optional run scope appended for talent, transcript, and
  transcript count.

Options:
  --talent-query VALUE           Talent selector/folder query (default: Avaritia)
  --transcript VALUE             Specific prepared transcript full path, exact basename, or partial basename
  --limit N                      Code at most N pending transcript CSVs
  --row-limit N                  Code at most N pending rows per selected transcript CSV
  --coding-folder VALUE          Folder under "qualitative coding" (default: monetary conversation codes)
  --codebook VALUE               Codebook selector: current, latest_snapshot, snapshot:<file>, path:<path>
                                 (default: current)
  --model VALUE                  Codex model override
  --dry-run                      Ask Codex to resolve and report targets only, without editing CSVs
  --reprocess                    Allow recoding rows/files that already have code values
  -h, --help                     Show this help

Examples:
  Code 5 pending transcripts for one talent:
    bin/linux/ena_precoding/code_qualitative_transcripts.sh --talent-query "Avaritia" --limit 5

  Code only the first 25 pending rows in one selected transcript:
    bin/linux/ena_precoding/code_qualitative_transcripts.sh --talent-query "Avaritia" --transcript "k84ImiUcjbE" --limit 1 --row-limit 25

  Code one specific transcript by video id or partial basename:
    bin/linux/ena_precoding/code_qualitative_transcripts.sh --talent-query "Avaritia" --transcript "k84ImiUcjbE"

  Use a codebook snapshot:
    bin/linux/ena_precoding/code_qualitative_transcripts.sh \
      --talent-query "Avaritia" \
      --limit 3 \
      --codebook "snapshot:personality_qualitative_code_log_2026-05-08_20-25-01_-0400.md"

  Cron example, code 2 pending transcripts:
    15 * * * * cd /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data && bin/linux/ena_precoding/code_qualitative_transcripts.sh --talent-query "Avaritia" --limit 2
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --talent-query|--talent)
      TALENT_SCOPE="${2:-}"
      if [[ -z "$TALENT_SCOPE" ]]; then
        echo "Error: $1 requires a value" >&2
        exit 1
      fi
      shift 2
      ;;
    --transcript)
      TRANSCRIPT_SCOPE="${2:-}"
      if [[ -z "$TRANSCRIPT_SCOPE" ]]; then
        echo "Error: --transcript requires a value" >&2
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
    --row-limit)
      ROW_LIMIT_SCOPE="${2:-}"
      if [[ -z "$ROW_LIMIT_SCOPE" ]]; then
        echo "Error: --row-limit requires a value" >&2
        exit 1
      fi
      shift 2
      ;;
    --coding-folder)
      CODING_FOLDER_SCOPE="${2:-}"
      if [[ -z "$CODING_FOLDER_SCOPE" ]]; then
        echo "Error: --coding-folder requires a value" >&2
        exit 1
      fi
      shift 2
      ;;
    --codebook)
      CODEBOOK_SCOPE="${2:-}"
      if [[ -z "$CODEBOOK_SCOPE" ]]; then
        echo "Error: --codebook requires a value" >&2
        exit 1
      fi
      shift 2
      ;;
    --model)
      CODEX_MODEL="${2:-}"
      if [[ -z "$CODEX_MODEL" ]]; then
        echo "Error: --model requires a value" >&2
        exit 1
      fi
      shift 2
      ;;
    --dry-run)
      DRY_RUN_SCOPE="true"
      shift
      ;;
    --reprocess)
      REPROCESS_SCOPE="true"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      if [[ "$1" == --* ]]; then
        echo "Error: unknown option: $1" >&2
        usage >&2
        exit 1
      fi
      PROMPT_FILE="$1"
      shift
      ;;
  esac
done

if [[ -n "$LIMIT_SCOPE" && ! "$LIMIT_SCOPE" =~ ^[1-9][0-9]*$ ]]; then
  echo "Error: --limit must be a positive integer" >&2
  exit 1
fi
if [[ -n "$ROW_LIMIT_SCOPE" && ! "$ROW_LIMIT_SCOPE" =~ ^[1-9][0-9]*$ ]]; then
  echo "Error: --row-limit must be a positive integer" >&2
  exit 1
fi

if ! timeout 10 mkdir -p "$LOG_DIR"; then
  LOG_DIR="/tmp/sun_data_codex_logs/qualitative_coding/monetary_conversation"
  mkdir -p "$LOG_DIR"
  LOG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}.log"
  FINAL_MSG_FILE="$LOG_DIR/${RUN_SLUG}_${RUN_TS}_final.md"
  echo "Warning: DataLake log directory was unavailable or slow; using $LOG_DIR" >&2
fi

if [[ ! -f "$PROMPT_FILE" ]]; then
  echo "Error: prompt file not found: $PROMPT_FILE" >&2
  exit 1
fi

if ! command -v "$CODEX_BIN" >/dev/null 2>&1; then
  echo "Error: codex CLI not found: $CODEX_BIN" >&2
  exit 1
fi

RUN_PROMPT_FILE="$(mktemp)"
cp "$PROMPT_FILE" "$RUN_PROMPT_FILE"
{
  echo
  echo "Additional run scope requested by the user:"
  echo "- This is the second-stage qualitative coding run. Use Codex to code prepared CSV files directly."
  echo "- Do not create or modify scripts, wrappers, helpers, or pipeline infrastructure during this run."
  echo "- Repository: $REPO"
  echo "- Prompt wrapper: $PROMPT_FILE"
  echo "- Talent query: $TALENT_SCOPE"
  echo "- Do not search outside the configured data lake root."
  echo "- Resolve talent folders only under: ${TALENT_DATALAKE_ROOT}"
  echo "- Prepared transcript folder under each talent: qualitative coding/$CODING_FOLDER_SCOPE"
  echo "- For dry-run inspection, do not call python or pandas. Use shell tools or Rscript with existing project helpers."
  echo "- Codebook selector: $CODEBOOK_SCOPE"
  if [[ -n "$TRANSCRIPT_SCOPE" ]]; then
    echo "- Process only transcript files matching this full path, exact basename, or partial basename: $TRANSCRIPT_SCOPE"
    echo "- Search for the transcript only inside the resolved prepared transcript folder for the selected talent."
  fi
  if [[ -n "$LIMIT_SCOPE" ]]; then
    echo "- Process at most $LIMIT_SCOPE pending prepared transcript CSV file(s) in this run."
  fi
  if [[ -n "$ROW_LIMIT_SCOPE" ]]; then
    echo "- Within each selected transcript CSV, code at most $ROW_LIMIT_SCOPE pending row(s) in this run."
  fi
  if [[ "$DRY_RUN_SCOPE" == "true" ]]; then
    echo "- Dry run: resolve targets and report what would be coded. Do not edit CSV files. Do not edit repository files."
  fi
  if [[ "$REPROCESS_SCOPE" == "true" ]]; then
    echo "- Reprocess mode: rows/files that already have code values may be recoded."
  else
    echo "- Default mode: only code rows/files with missing NA code values; do not overwrite completed coding."
  fi
  echo "- Keep the final response concise and include the CSV path(s) touched or selected."
} >> "$RUN_PROMPT_FILE"

cd "$REPO"

echo "Checking repo updates..."
git pull --ff-only || true

echo "Starting Codex qualitative-coding run..."
echo "Prompt file: $PROMPT_FILE"
echo "Scoped prompt file: $RUN_PROMPT_FILE"
echo "Log file: $LOG_FILE"
echo "Final message file: $FINAL_MSG_FILE"
if [[ -n "$CODEX_MODEL" ]]; then
  echo "Codex model: $CODEX_MODEL"
fi

CODEX_ARGS=(
  exec
  --cd "$REPO"
  --dangerously-bypass-approvals-and-sandbox
  --output-last-message "$FINAL_MSG_FILE"
)

if [[ -n "$CODEX_MODEL" ]]; then
  CODEX_ARGS+=(--model "$CODEX_MODEL")
fi

{
  echo "=== Codex qualitative-coding run started: $(date) ==="
  echo "Repo: $REPO"
  echo "Prompt: $PROMPT_FILE"
  echo "Scoped prompt: $RUN_PROMPT_FILE"
  echo "Talent query: $TALENT_SCOPE"
  echo "Transcript scope: ${TRANSCRIPT_SCOPE:-<none>}"
  echo "Limit: ${LIMIT_SCOPE:-<none>}"
  echo "Row limit: ${ROW_LIMIT_SCOPE:-<none>}"
  echo "Coding folder: $CODING_FOLDER_SCOPE"
  echo "Codebook selector: $CODEBOOK_SCOPE"
  echo "Dry run: $DRY_RUN_SCOPE"
  echo "Reprocess: $REPROCESS_SCOPE"
  if [[ -n "$CODEX_MODEL" ]]; then
    echo "Model: $CODEX_MODEL"
  fi
  echo

  cat "$RUN_PROMPT_FILE" | "$CODEX_BIN" "${CODEX_ARGS[@]}" -

  echo
  echo "=== Codex qualitative-coding run finished: $(date) ==="
  echo "Final message saved to: $FINAL_MSG_FILE"
} >> "$LOG_FILE" 2>&1

rm -f "$RUN_PROMPT_FILE"
