#!/usr/bin/env bash
set -euo pipefail

REPO="${REPO:-$HOME/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data}"
DATA_ROOT="${DATA_ROOT:-/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data}"
RUNNER="${RUNNER:-$REPO/bin/linux/codex_prompts/summaries/stream_summary_codex.sh}"
LOG_DIR="${LOG_DIR:-/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/summaries/stream_summary_codex_scheduled}"
LOCK_DIR="${LOCK_DIR:-/tmp/stream_summary_codex_scheduled.lock}"

LIMIT="${STREAM_SUMMARY_LIMIT:-20}"
TALENT="${STREAM_SUMMARY_TALENT:-}"
WINDOW_START="${STREAM_SUMMARY_WINDOW_START:-00:04}"
WINDOW_END="${STREAM_SUMMARY_WINDOW_END:-05:05}"
ENFORCE_WINDOW=1
DRY_RUN=0

usage() {
  cat <<'EOF'
Usage:
  bin/linux/codex_prompts/summaries/stream_summary_codex_scheduled.sh [options]

Options:
  --talent NAME       Limit the scheduled run to one talent folder.
  --all-talents       Scan all talent folders. This is the default.
  --limit N           Max missing/empty summaries to create this run. Default: 20.
  --window HH:MM-HH:MM
                      Only run inside this local time window. Default: 00:04-05:05.
  --force             Ignore the time-window guard.
  --dry-run           Print pending counts and the runner command without launching Codex.
  -h, --help          Show this help.

Environment overrides:
  STREAM_SUMMARY_TALENT
  STREAM_SUMMARY_LIMIT
  STREAM_SUMMARY_WINDOW_START
  STREAM_SUMMARY_WINDOW_END

Examples:
  Catch up Nova after a reset:
    STREAM_SUMMARY_TALENT="Nova Aokami Ch" STREAM_SUMMARY_LIMIT=20 \
      bin/linux/codex_prompts/summaries/stream_summary_codex_scheduled.sh

  Regular all-talent scan:
    bin/linux/codex_prompts/summaries/stream_summary_codex_scheduled.sh
EOF
}

time_to_minutes() {
  local value="$1"
  local hour minute
  IFS=: read -r hour minute <<< "$value"
  if [[ ! "$hour" =~ ^[0-9]{1,2}$ || ! "$minute" =~ ^[0-9]{2}$ ]]; then
    echo "Invalid time: $value" >&2
    exit 2
  fi
  echo $((10#$hour * 60 + 10#$minute))
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --talent)
      TALENT="${2:-}"
      if [[ -z "$TALENT" ]]; then
        echo "Error: --talent requires a value" >&2
        exit 2
      fi
      shift 2
      ;;
    --all-talents)
      TALENT=""
      shift
      ;;
    --limit)
      LIMIT="${2:-}"
      if [[ -z "$LIMIT" || ! "$LIMIT" =~ ^[0-9]+$ || "$LIMIT" -lt 1 ]]; then
        echo "Error: --limit requires a positive integer" >&2
        exit 2
      fi
      shift 2
      ;;
    --window)
      value="${2:-}"
      if [[ "$value" != *-* ]]; then
        echo "Error: --window must look like HH:MM-HH:MM" >&2
        exit 2
      fi
      WINDOW_START="${value%-*}"
      WINDOW_END="${value#*-}"
      shift 2
      ;;
    --force)
      ENFORCE_WINDOW=0
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
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ ! "$LIMIT" =~ ^[0-9]+$ || "$LIMIT" -lt 1 ]]; then
  echo "Error: limit must be a positive integer: $LIMIT" >&2
  exit 2
fi

mkdir -p "$LOG_DIR"
RUN_TS="$(date +%Y-%m-%d_%H-%M-%S)"
LOG_FILE="$LOG_DIR/stream_summary_codex_scheduled_${RUN_TS}.log"

{
  echo "=== scheduled stream-summary check started: $(date) ==="
  echo "Repo: $REPO"
  echo "Data root: $DATA_ROOT"
  echo "Runner: $RUNNER"
  echo "Talent scope: ${TALENT:-ALL}"
  echo "Limit: $LIMIT"
  echo "Window: $WINDOW_START-$WINDOW_END"
  echo
} | tee -a "$LOG_FILE"

if [[ "$ENFORCE_WINDOW" -eq 1 ]]; then
  now_minutes="$(date +%H:%M | xargs -I{} bash -c 'IFS=: read -r h m <<< "$1"; echo $((10#$h * 60 + 10#$m))' _ {})"
  start_minutes="$(time_to_minutes "$WINDOW_START")"
  end_minutes="$(time_to_minutes "$WINDOW_END")"
  in_window=0
  if [[ "$start_minutes" -le "$end_minutes" ]]; then
    if [[ "$now_minutes" -ge "$start_minutes" && "$now_minutes" -le "$end_minutes" ]]; then
      in_window=1
    fi
  else
    if [[ "$now_minutes" -ge "$start_minutes" || "$now_minutes" -le "$end_minutes" ]]; then
      in_window=1
    fi
  fi
  if [[ "$in_window" -ne 1 ]]; then
    echo "Outside scheduled window; exiting without running Codex." | tee -a "$LOG_FILE"
    exit 0
  fi
fi

if ! mkdir "$LOCK_DIR" 2>/dev/null; then
  echo "Another scheduled stream-summary run appears to be active: $LOCK_DIR" | tee -a "$LOG_FILE"
  exit 0
fi
trap 'rmdir "$LOCK_DIR" 2>/dev/null || true' EXIT

if [[ ! -x "$RUNNER" ]]; then
  echo "Runner is missing or not executable: $RUNNER" | tee -a "$LOG_FILE"
  exit 1
fi

PENDING_REPORT="$(python3 - "$DATA_ROOT" "$TALENT" <<'PY'
import sys
from pathlib import Path

data_root = Path(sys.argv[1])
talent_scope = sys.argv[2]
skip_names = {"VarianceProject"}
total_pending = 0
total_eligible = 0
lines = []

if not data_root.exists():
    print("ERROR\t0\t0\tdata root does not exist")
    raise SystemExit(0)

talent_dirs = sorted(p for p in data_root.iterdir() if p.is_dir())
if talent_scope:
    talent_dirs = [p for p in talent_dirs if p.name == talent_scope]

for talent_dir in talent_dirs:
    if talent_dir.name in skip_names:
        continue
    text_dir = talent_dir / "text_playback"
    if not text_dir.is_dir():
        continue
    csvs = sorted(text_dir.glob("*.csv"))
    if not csvs:
        continue
    out_dir = talent_dir / "stream_summaries" / "stream_summary_codex"
    pending = 0
    for csv_path in csvs:
        out_path = out_dir / f"{csv_path.stem}_summary.md"
        if not out_path.exists() or out_path.stat().st_size == 0:
            pending += 1
    total_eligible += len(csvs)
    total_pending += pending
    if pending:
        lines.append(f"{talent_dir.name}\t{pending}\t{len(csvs)}")

print(f"TOTAL\t{total_pending}\t{total_eligible}")
for line in lines:
    print(line)
PY
)"

echo "$PENDING_REPORT" | tee -a "$LOG_FILE"
TOTAL_PENDING="$(printf '%s\n' "$PENDING_REPORT" | awk -F'\t' '$1=="TOTAL"{print $2}')"
if [[ -z "$TOTAL_PENDING" || "$TOTAL_PENDING" == "0" ]]; then
  echo "No missing or empty stream summaries found; exiting." | tee -a "$LOG_FILE"
  exit 0
fi

RUN_LIMIT="$LIMIT"
if [[ "$TOTAL_PENDING" -lt "$RUN_LIMIT" ]]; then
  RUN_LIMIT="$TOTAL_PENDING"
fi

CMD=("$RUNNER")
if [[ -n "$TALENT" ]]; then
  CMD+=(--talent "$TALENT")
fi
CMD+=(--limit "$RUN_LIMIT")

printf 'Runner command:' | tee -a "$LOG_FILE"
printf ' %q' "${CMD[@]}" | tee -a "$LOG_FILE"
printf '\n' | tee -a "$LOG_FILE"

if [[ "$DRY_RUN" -eq 1 ]]; then
  echo "Dry run requested; not launching Codex." | tee -a "$LOG_FILE"
  exit 0
fi

cd "$REPO"
"${CMD[@]}" 2>&1 | tee -a "$LOG_FILE"
echo "=== scheduled stream-summary check finished: $(date) ===" | tee -a "$LOG_FILE"
