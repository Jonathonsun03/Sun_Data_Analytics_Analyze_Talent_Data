#!/usr/bin/env bash
set -euo pipefail

ENV_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
while [[ "${ENV_ROOT}" != "/" ]]; do
  if [[ -e "${ENV_ROOT}/.git" ]]; then
    break
  fi
  ENV_ROOT="$(dirname "${ENV_ROOT}")"
done
if [[ -f "${ENV_ROOT}/bin/linux/load_repo_env.sh" ]]; then
  # shellcheck source=/dev/null
  source "${ENV_ROOT}/bin/linux/load_repo_env.sh"
  load_repo_env "${ENV_ROOT}"
fi

REPO_ROOT="${ENV_ROOT}"
SESSION_NAME="${SUBTITLE_BACKFILL_TMUX_SESSION:-subtitle-sentence-backfill}"
RUNNER="${REPO_ROOT}/bin/linux/subtitles/run_subtitle_sentence_backfill.sh"

if ! command -v tmux >/dev/null 2>&1; then
  echo "Error: tmux is not installed." >&2
  exit 1
fi
if [[ -z "${TALENT_DATALAKE_ROOT:-}" ]]; then
  echo "Error: TALENT_DATALAKE_ROOT is not configured." >&2
  exit 1
fi
if tmux has-session -t "${SESSION_NAME}" 2>/dev/null; then
  echo "Error: tmux session already exists: ${SESSION_NAME}" >&2
  echo "Attach with: tmux attach -t ${SESSION_NAME}" >&2
  exit 1
fi

LOG_DIR="${TALENT_DATALAKE_ROOT}/Logs/subtitle_sentence_backfill"
mkdir -p "${LOG_DIR}"
LOG_FILE="${LOG_DIR}/backfill_$(date -u +%Y%m%dT%H%M%SZ).log"

runner_args=("${RUNNER}" --execute "$@")
printf -v runner_command '%q ' "${runner_args[@]}"
printf -v repo_quoted '%q' "${REPO_ROOT}"
printf -v log_quoted '%q' "${LOG_FILE}"
tmux_command="set -o pipefail; cd ${repo_quoted} && ${runner_command}2>&1 | tee -a ${log_quoted}"

tmux new-session -d -s "${SESSION_NAME}" "${tmux_command}"

echo "Started subtitle sentence backfill."
echo "  tmux session: ${SESSION_NAME}"
echo "  log: ${LOG_FILE}"
echo "  attach: tmux attach -t ${SESSION_NAME}"
echo "  follow log: tail -f ${LOG_FILE}"
