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

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="r_scripts/run/Text_Replay_Analysis/Text_replay_analysis_openAI"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/stream_summary/run_stream_summary.sh [options]

Description:
  Legacy R-based stream text summarizer using OpenAI.
  - Processes pending files by default (idempotent by prompt+model+file version)
  - Tracks runs in DuckDB table: stream_summaries
  - Writes markdown outputs into talent folder stream_summary/
  - Requires an explicit --prompt-path

Options:
  --talent-project NAME         Talent project folder (default: Leia Memoria【Variance Project】)
  --model NAME                  Model (default: gpt-4.1-mini)
  --prompt-path PATH            Prompt markdown path (required)
  --input-subdir NAME           Input subdir under talent project (default: text_playback)
  --output-subdir NAME          Output subdir under talent project (default: stream_summary)
  --one-stream VALUE            One stream only (full path, basename, or basename without .csv)
  --limit N                     Process first N pending rows (smoke test)
  --force-resummarize           Rerun selected streams regardless of prior success
  -h, --help                    Show this help

Examples:
  Weekly pending run:
    bin/linux/stream_summary/run_stream_summary.sh --talent-project "Leia Memoria【Variance Project】"

  Smoke test 5:
    bin/linux/stream_summary/run_stream_summary.sh --talent-project "Leia Memoria【Variance Project】" --limit 5

  Single stream test:
    bin/linux/stream_summary/run_stream_summary.sh --talent-project "Leia Memoria【Variance Project】" --one-stream "my_stream.csv"
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

cd "${REPO_ROOT}"

if [[ ! -f "${R_SCRIPT}" ]]; then
  echo "Error: missing R script at ${R_SCRIPT}" >&2
  exit 1
fi

Rscript "${R_SCRIPT}" "$@"
