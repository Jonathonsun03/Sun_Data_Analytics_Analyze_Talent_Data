#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

QUARTO_BIN="${QUARTO_BIN:-quarto}"
TEMPLATE="templates/dashboards/talent_dashboard/index.qmd"
TALENT=""
DATA_SOURCE="datalake"
DATA_ROOT=""
START_DATE=""
END_DATE=""
CONTENT_TYPES="live,video,short"
REFERENCE_DAY="Monday"
OUTPUT_DIR="outputs/dashboards/talent_dashboard"
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
  --output-dir PATH
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

if [[ -z "${OUTPUT_FILE}" ]]; then
  OUTPUT_FILE="creator_analytics_dashboard_$(slugify "${TALENT}").html"
fi

OUTPUT_DIR_ABS="$(resolve_path "${OUTPUT_DIR}")"
mkdir -p "${OUTPUT_DIR_ABS}"

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
echo "Data source: ${DATA_SOURCE}"
echo "Output: ${OUTPUT_DIR_ABS}/${OUTPUT_FILE}"
echo "Params: ${PARAM_FILE}"

CMD=(
  "${QUARTO_BIN}" render "${TEMPLATE}"
  --execute-params "${PARAM_FILE}"
  --output-dir "${OUTPUT_DIR_ABS}"
  --output "${OUTPUT_FILE}"
)

if [[ "${DRY_RUN}" == "true" ]]; then
  printf 'Dry run command:'
  printf ' %q' "${CMD[@]}"
  printf '\n'
  exit 0
fi

cd "${REPO_ROOT}"
"${CMD[@]}"

echo "Rendered dashboard: ${OUTPUT_DIR_ABS}/${OUTPUT_FILE}"

