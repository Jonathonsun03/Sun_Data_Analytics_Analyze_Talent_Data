#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="scripts/run/Text_Replay_Analysis/Text_replay_analysis_openAI"

usage() {
  cat <<'EOF'
Usage:
  bin/linux/stream_summary/run_stream_summary.sh [options]

Description:
  Summarize stream text logs to markdown using OpenAI.
  - Processes pending files by default (idempotent by prompt+model+file version)
  - Tracks runs in DuckDB table: stream_summaries
  - Writes markdown outputs into talent folder stream_summary/

Options:
  --talent-project NAME         Talent project folder (default: Leia Memoria【Variance Project】)
  --model NAME                  Model (default: gpt-4.1-mini)
  --prompt-path PATH            Prompt markdown path
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
