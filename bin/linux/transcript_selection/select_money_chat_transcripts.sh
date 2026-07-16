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
R_SCRIPT="$REPO/r_scripts/run/transcript_selection/select_money_chat_transcripts.R"

cd "$REPO"
exec Rscript --vanilla "$R_SCRIPT" "$@"
