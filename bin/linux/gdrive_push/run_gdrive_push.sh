#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
R_SCRIPT="r_scripts/run/gdrive_push/prepare_client_drive_structure.R"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/gdrive_push/run_gdrive_push.sh [options]

Description:
  Wrapper for preparing client Google Drive delivery folders and optionally
  uploading selected delivery files from the DataLake, plus sharing delivery
  group folders with client recipients.

  Underlying R script:
    r_scripts/run/gdrive_push/prepare_client_drive_structure.R

Options:
  --config PATH                  Config YAML path
                                 (default: r_scripts/run/gdrive_push/config/config.yml)
  --sheet SHEET_ID_OR_URL         Override permissions_sheet
  --sheet-tab TAB_NAME            Override permissions_sheet_tab
  --client-drive-root FOLDER_ID   Override client_drive_root
  --datalake-root PATH            Override datalake_root
  --google-email EMAIL            Override google_email
  --google-auth-cache PATH        Override google_auth_cache
  --raw-days N                    Number of recent raw-data days to deliver (default from config)
  --share-role ROLE               Share role for delivery group folders (default from config)
  --execute                       Actually create folders; otherwise dry-run
  --upload-files                  Include selected file uploads; requires --execute to upload
  --share-folders                 Share delivery group folders with client_email recipients
  -h, --help                      Show this help

Examples:
  Dry run:
    bin/linux/gdrive_push/run_gdrive_push.sh

  Create folders only:
    bin/linux/gdrive_push/run_gdrive_push.sh --execute

  Create folders and upload selected files:
    bin/linux/gdrive_push/run_gdrive_push.sh --execute --upload-files

  Create folders, upload files, and share delivery groups:
    bin/linux/gdrive_push/run_gdrive_push.sh --execute --upload-files --share-folders
USAGE
}

ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --config|--sheet|--sheet-tab|--client-drive-root|--datalake-root|--google-email|--google-auth-cache|--raw-days|--share-role)
      [[ $# -ge 2 ]] || { echo "Error: $1 requires a value" >&2; exit 1; }
      ARGS+=("$1" "$2")
      shift 2
      ;;
    --execute|--upload-files|--active-only|--share-folders)
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
