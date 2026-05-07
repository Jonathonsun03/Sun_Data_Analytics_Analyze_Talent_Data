#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="r_scripts/run/gdrive_push/sync_client_permissions_sheet.R"

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/gdrive_push/run_client_permissions_sheet_sync.sh [options]

Description:
  Updates the Google Sheets control-plane tabs in the client permissions
  spreadsheet:
    1. talents
    2. reports
    3. talent_report_schedule

  This is intentionally separate from run_gdrive_push.sh, which creates,
  uploads, and shares client-facing Google Drive folders/files.

Options:
  --config PATH                 Config YAML path
                                (default: r_scripts/run/gdrive_push/config/config.yml)
  --sheet SHEET_ID_OR_URL        Override permissions_sheet
  --datalake-root PATH           Override datalake_root
  --google-email EMAIL           Override google_email
  --google-auth-cache PATH       Override google_auth_cache
  --talents-tab TAB_NAME         Talents worksheet name (default: talents)
  --reports-tab TAB_NAME         Reports worksheet name (default: reports)
  --schedule-tab TAB_NAME        Schedule worksheet name (default: talent_report_schedule)
  --skip-talents                 Do not update the talents tab
  --skip-reports                 Do not update the reports tab
  --skip-schedule                Do not update the talent_report_schedule tab
  -h, --help                     Show this help

Example:
  bin/linux/gdrive_push/run_client_permissions_sheet_sync.sh
USAGE
}

ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --config|--sheet|--datalake-root|--google-email|--google-auth-cache|--talents-tab|--reports-tab|--schedule-tab)
      [[ $# -ge 2 ]] || { echo "Error: $1 requires a value" >&2; exit 1; }
      ARGS+=("$1" "$2")
      shift 2
      ;;
    --skip-talents|--skip-reports|--skip-schedule)
      ARGS+=("$1")
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Error: unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

cd "${REPO_ROOT}"

if [[ ! -f "${R_SCRIPT}" ]]; then
  echo "Error: missing R script at ${R_SCRIPT}" >&2
  exit 1
fi

Rscript "${R_SCRIPT}" "${ARGS[@]}"
