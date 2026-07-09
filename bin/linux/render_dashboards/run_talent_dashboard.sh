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

QUARTO_BIN="${QUARTO_BIN:-quarto}"
TEMPLATE="r_scripts/notebooks/dashboards/talent_dashboard/index.qmd"
TALENT=""
DATA_SOURCE="datalake"
DATA_ROOT=""
START_DATE=""
END_DATE=""
CONTENT_TYPES="live,video,short"
REFERENCE_DAY="Monday"
OUTPUT_DIR=""
OUTPUT_DIR_SET="false"
OUTPUT_FILE=""
DRY_RUN="false"

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_dashboards/run_talent_dashboard.sh --talent NAME [options]

Options:
  --talent NAME                 Talent name/folder/query passed to the dashboard.
  --data-source datalake|staging
  --data-root PATH              Explicit data root override.
  --start-date YYYY-MM-DD
  --end-date YYYY-MM-DD
  --content-types live,video,short
  --reference-day DAY
  --output-dir PATH              Explicit output directory override.
  --output FILE
  --dry-run
  -h, --help
USAGE
}

trim() {
  local s="$1"
  s="${s#"${s%%[![:space:]]*}"}"
  s="${s%"${s##*[![:space:]]}"}"
  printf '%s' "$s"
}

slugify() {
  local slug
  slug="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/_/g; s/^_+//; s/_+$//')"
  [[ -n "${slug}" ]] || slug="talent"
  printf '%s' "$slug"
}

resolve_path() {
  local p="$1"
  if [[ "$p" = /* || "$p" =~ ^[A-Za-z]:[\\/].* || "$p" =~ ^\\\\.* ]]; then
    printf '%s' "$p"
  else
    printf '%s' "${REPO_ROOT}/${p}"
  fi
}

yaml_quote() {
  local s="$1"
  s="${s//\'/\'\'}"
  printf "'%s'" "$s"
}

yaml_nullable_scalar() {
  local s="$1"
  if [[ -z "$s" ]]; then
    printf 'null'
  else
    yaml_quote "$s"
  fi
}

yaml_content_types() {
  local raw="$1"
  local item
  local first="true"
  printf '['
  IFS=',' read -r -a parts <<< "$raw"
  for item in "${parts[@]}"; do
    item="$(trim "$item")"
    [[ -z "$item" ]] && continue
    if [[ "$first" == "false" ]]; then
      printf ', '
    fi
    yaml_quote "$item"
    first="false"
  done
  printf ']'
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --talent)
      TALENT="$2"
      shift 2
      ;;
    --data-source)
      DATA_SOURCE="$2"
      shift 2
      ;;
    --data-root)
      DATA_ROOT="$2"
      shift 2
      ;;
    --start-date)
      START_DATE="$2"
      shift 2
      ;;
    --end-date)
      END_DATE="$2"
      shift 2
      ;;
    --content-types|--content-type)
      CONTENT_TYPES="$2"
      shift 2
      ;;
    --reference-day)
      REFERENCE_DAY="$2"
      shift 2
      ;;
    --output-dir)
      OUTPUT_DIR="$2"
      OUTPUT_DIR_SET="true"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --dry-run)
      DRY_RUN="true"
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

if [[ -z "${TALENT}" ]]; then
  echo "Error: --talent is required." >&2
  usage >&2
  exit 1
fi

case "${DATA_SOURCE}" in
  datalake|staging) ;;
  *)
    echo "Error: --data-source must be 'datalake' or 'staging'." >&2
    exit 1
    ;;
esac

RESOLVED_TALENT_PATH="$(
  cd "${REPO_ROOT}"
  Rscript --vanilla -e 'args <- commandArgs(trailingOnly = TRUE); talent <- args[[1]]; data_source <- tolower(args[[2]]); data_root <- args[[3]]; suppressPackageStartupMessages({ source("r_scripts/lib/utils/staging_root.R"); source("r_scripts/lib/utils/datalake_root.r"); source("r_scripts/lib/utils/talent_select.R") }); root <- if (nzchar(data_root)) normalizePath(data_root, winslash = "/", mustWork = FALSE) else if (identical(data_source, "datalake")) normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE) else normalizePath(get_staging_root(), winslash = "/", mustWork = FALSE); cat(normalizePath(select_talent(talent, root = root), winslash = "/", mustWork = FALSE))' "${TALENT}" "${DATA_SOURCE}" "${DATA_ROOT}"
)"
RESOLVED_TALENT_NAME="$(basename "${RESOLVED_TALENT_PATH}")"

if [[ -z "${OUTPUT_FILE}" ]]; then
  OUTPUT_FILE="creator_analytics_dashboard_$(slugify "${TALENT}").html"
fi

if [[ "${OUTPUT_DIR_SET}" != "true" ]]; then
  OUTPUT_DIR="${RESOLVED_TALENT_PATH}/reports/dashboard_overall"
fi

OUTPUT_DIR_ABS="$(resolve_path "${OUTPUT_DIR}")"
OUTPUT_PATH_ABS="${OUTPUT_DIR_ABS}/${OUTPUT_FILE}"
OUTPUT_PARENT_ABS="$(dirname "${OUTPUT_PATH_ABS}")"
TEMPLATE_DIR_ABS="${REPO_ROOT}/$(dirname "${TEMPLATE}")"
TEMPLATE_FILE="$(basename "${TEMPLATE}")"
RENDER_OUTPUT_FILE="$(basename "${OUTPUT_FILE}")"
RENDERED_PATH="${TEMPLATE_DIR_ABS}/${RENDER_OUTPUT_FILE}"

PARAM_FILE="$(mktemp "${TMPDIR:-/tmp}/talent-dashboard-params.XXXXXX.yml")"
trap 'rm -f "${PARAM_FILE}"' EXIT

{
  printf 'talent: %s\n' "$(yaml_quote "${TALENT}")"
  printf 'data_source: %s\n' "$(yaml_quote "${DATA_SOURCE}")"
  printf 'data_root: %s\n' "$(yaml_nullable_scalar "${DATA_ROOT}")"
  printf 'start_date: %s\n' "$(yaml_nullable_scalar "${START_DATE}")"
  printf 'end_date: %s\n' "$(yaml_nullable_scalar "${END_DATE}")"
  printf 'content_types: %s\n' "$(yaml_content_types "${CONTENT_TYPES}")"
  printf 'reference_day: %s\n' "$(yaml_quote "${REFERENCE_DAY}")"
} > "${PARAM_FILE}"

echo "Dashboard template: ${TEMPLATE}"
echo "Talent: ${TALENT}"
echo "Resolved talent folder: ${RESOLVED_TALENT_NAME}"
echo "Data source: ${DATA_SOURCE}"
echo "Output: ${OUTPUT_PATH_ABS}"
echo "Params: ${PARAM_FILE}"

CMD=(
  "${QUARTO_BIN}" render "${TEMPLATE_FILE}"
  --execute-params "${PARAM_FILE}"
  --output "${RENDER_OUTPUT_FILE}"
)

if [[ "${DRY_RUN}" == "true" ]]; then
  printf 'Dry run cd: %q\n' "${TEMPLATE_DIR_ABS}"
  printf 'Dry run command:'
  printf ' %q' "${CMD[@]}"
  printf '\n'
  if [[ "${RENDERED_PATH}" != "${OUTPUT_PATH_ABS}" ]]; then
    printf 'Dry run move: mv %q %q\n' "${RENDERED_PATH}" "${OUTPUT_PATH_ABS}"
  fi
  exit 0
fi

mkdir -p "${OUTPUT_PARENT_ABS}"

cd "${TEMPLATE_DIR_ABS}"
"${CMD[@]}"

if [[ "${RENDERED_PATH}" != "${OUTPUT_PATH_ABS}" ]]; then
  mv "${RENDERED_PATH}" "${OUTPUT_PATH_ABS}"
fi

echo "Rendered dashboard: ${OUTPUT_PATH_ABS}"
