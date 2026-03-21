#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

DEFAULT_IMPORT_SCRIPT="r_scripts/run/bundle_A/import_data.r"
RSCRIPT_BIN="${RSCRIPT_BIN:-Rscript}"
IMPORT_SCRIPT="${DEFAULT_IMPORT_SCRIPT}"
INPUT_SOURCE="datalake"
INPUT_ROOT=""
DATALAKE_ROOT_ARG=""
STAGING_ROOT_ARG=""
WINDOW_DAYS=""
START_DATE=""
END_DATE=""
QUIET="false"
DRY_RUN="false"
TALENTS_FILE=""
TALENTS_CSV=""
ALL_TALENTS="false"

declare -a TALENTS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_A/run_bundle_A_artifacts.sh [options]

Description:
  Builds Bundle A evidence artifacts for one or more talents and writes them to:
    <datalake_root>/<talent>/reports/bundle_A/artifacts/

  It calls:
    r_scripts/run/bundle_A/import_data.r

Talent selection:
  --talent NAME
  --talents "A,B,C"
  --talents-file PATH
  --all

Window alignment:
  --window-days N
  --start-date YYYY-MM-DD
  --end-date YYYY-MM-DD

Roots:
  --input-source NAME     staging|datalake (default: datalake)
  --input-root PATH       Optional talent data root override
  --datalake-root PATH    Optional datalake root override
  --staging-root PATH     Optional staging root override

Behavior:
  --import-script PATH    Override import script path
  --quiet                 Accepted for wrapper parity
  --dry-run               Print commands without running
  -h, --help              Show help
USAGE
}

trim() {
  local s="$1"
  s="${s#"${s%%[![:space:]]*}"}"
  s="${s%"${s##*[![:space:]]}"}"
  printf '%s' "$s"
}

resolve_path() {
  local p="$1"
  if [[ "$p" = /* || "$p" =~ ^[A-Za-z]:[\\/].* || "$p" =~ ^\\\\.* ]]; then
    printf '%s' "$p"
  else
    printf '%s' "${REPO_ROOT}/${p}"
  fi
}

load_talents_from_file() {
  local path="$1"
  local full_path
  full_path="$(resolve_path "$path")"
  if [[ ! -f "${full_path}" ]]; then
    echo "Error: talents file not found: ${full_path}" >&2
    exit 1
  fi
  while IFS= read -r line || [[ -n "$line" ]]; do
    line="$(trim "$line")"
    [[ -z "$line" ]] && continue
    [[ "$line" =~ ^# ]] && continue
    TALENTS+=("$line")
  done < "${full_path}"
}

load_talents_all() {
  local out
  out="$(
    cd "${REPO_ROOT}"
    TALENT_DATA_SOURCE_INPUT="${INPUT_SOURCE}" TALENT_DATA_ROOT_INPUT="${INPUT_ROOT}" "${RSCRIPT_BIN}" --vanilla -e "src <- tolower(Sys.getenv('TALENT_DATA_SOURCE_INPUT', 'staging')); root_in <- Sys.getenv('TALENT_DATA_ROOT_INPUT', ''); source('r_scripts/lib/utils/staging_root.R'); source('r_scripts/lib/utils/datalake_root.r'); source('r_scripts/lib/utils/talent_select.R'); root <- if (nzchar(root_in)) root_in else if (identical(src, 'datalake')) get_datalake_root() else get_staging_root(); cat(list_talents(root = root)\$name, sep='\n')"
  )"
  while IFS= read -r line; do
    line="$(trim "$line")"
    [[ -z "$line" ]] && continue
    TALENTS+=("$line")
  done <<< "$out"
}

dedupe_talents_in_place() {
  local -A seen=()
  local t=""
  local -a unique_list=()
  for t in "${TALENTS[@]}"; do
    t="$(trim "$t")"
    [[ -z "$t" ]] && continue
    if [[ -z "${seen[$t]+x}" ]]; then
      seen["$t"]=1
      unique_list+=("$t")
    fi
  done
  TALENTS=("${unique_list[@]}")
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --talent)
      [[ $# -ge 2 ]] || { echo "Error: --talent requires a value" >&2; exit 1; }
      TALENTS+=("$2")
      shift 2
      ;;
    --talents)
      [[ $# -ge 2 ]] || { echo "Error: --talents requires a value" >&2; exit 1; }
      TALENTS_CSV="$2"
      shift 2
      ;;
    --talents-file)
      [[ $# -ge 2 ]] || { echo "Error: --talents-file requires a value" >&2; exit 1; }
      TALENTS_FILE="$2"
      shift 2
      ;;
    --all)
      ALL_TALENTS="true"
      shift
      ;;
    --window-days)
      [[ $# -ge 2 ]] || { echo "Error: --window-days requires a value" >&2; exit 1; }
      WINDOW_DAYS="$2"
      shift 2
      ;;
    --start-date)
      [[ $# -ge 2 ]] || { echo "Error: --start-date requires a value" >&2; exit 1; }
      START_DATE="$2"
      shift 2
      ;;
    --end-date)
      [[ $# -ge 2 ]] || { echo "Error: --end-date requires a value" >&2; exit 1; }
      END_DATE="$2"
      shift 2
      ;;
    --input-source)
      [[ $# -ge 2 ]] || { echo "Error: --input-source requires a value" >&2; exit 1; }
      INPUT_SOURCE="$(printf '%s' "$2" | tr '[:upper:]' '[:lower:]')"
      shift 2
      ;;
    --input-root)
      [[ $# -ge 2 ]] || { echo "Error: --input-root requires a value" >&2; exit 1; }
      INPUT_ROOT="$2"
      shift 2
      ;;
    --datalake-root)
      [[ $# -ge 2 ]] || { echo "Error: --datalake-root requires a value" >&2; exit 1; }
      DATALAKE_ROOT_ARG="$2"
      shift 2
      ;;
    --staging-root)
      [[ $# -ge 2 ]] || { echo "Error: --staging-root requires a value" >&2; exit 1; }
      STAGING_ROOT_ARG="$2"
      shift 2
      ;;
    --import-script)
      [[ $# -ge 2 ]] || { echo "Error: --import-script requires a value" >&2; exit 1; }
      IMPORT_SCRIPT="$2"
      shift 2
      ;;
    --quiet)
      QUIET="true"
      shift
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

if [[ -n "${INPUT_ROOT}" ]]; then
  INPUT_ROOT="$(resolve_path "${INPUT_ROOT}")"
fi
if [[ -n "${DATALAKE_ROOT_ARG}" ]]; then
  DATALAKE_ROOT_ARG="$(resolve_path "${DATALAKE_ROOT_ARG}")"
fi
if [[ -n "${STAGING_ROOT_ARG}" ]]; then
  STAGING_ROOT_ARG="$(resolve_path "${STAGING_ROOT_ARG}")"
fi

if [[ "${INPUT_SOURCE}" != "staging" && "${INPUT_SOURCE}" != "datalake" ]]; then
  echo "Error: --input-source must be either 'staging' or 'datalake'." >&2
  exit 1
fi
if [[ -n "${WINDOW_DAYS}" ]] && { ! [[ "${WINDOW_DAYS}" =~ ^[0-9]+$ ]] || [[ "${WINDOW_DAYS}" -le 0 ]]; }; then
  echo "Error: --window-days must be a positive integer." >&2
  exit 1
fi
if [[ -n "${START_DATE}" ]] && ! [[ "${START_DATE}" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
  echo "Error: --start-date must be YYYY-MM-DD." >&2
  exit 1
fi
if [[ -n "${END_DATE}" ]] && ! [[ "${END_DATE}" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
  echo "Error: --end-date must be YYYY-MM-DD." >&2
  exit 1
fi

if [[ -n "${TALENTS_CSV}" ]]; then
  IFS=',' read -r -a csv_arr <<< "${TALENTS_CSV}"
  for t in "${csv_arr[@]}"; do
    t="$(trim "$t")"
    [[ -n "$t" ]] && TALENTS+=("$t")
  done
fi
if [[ -n "${TALENTS_FILE}" ]]; then
  load_talents_from_file "${TALENTS_FILE}"
fi
if [[ "${ALL_TALENTS}" == "true" ]]; then
  load_talents_all
fi

dedupe_talents_in_place

if [[ ${#TALENTS[@]} -eq 0 ]]; then
  TALENTS=("Ava")
fi

cd "${REPO_ROOT}"

if [[ -z "${LANG:-}" ]]; then
  export LANG="C.UTF-8"
fi
if [[ -z "${LC_ALL:-}" ]]; then
  export LC_ALL="${LANG}"
fi

IMPORT_SCRIPT_PATH="$(resolve_path "${IMPORT_SCRIPT}")"
if [[ ! -f "${IMPORT_SCRIPT_PATH}" ]]; then
  echo "Error: import script not found: ${IMPORT_SCRIPT_PATH}" >&2
  exit 1
fi

echo "[bundle-a-artifacts] Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "[bundle-a-artifacts] Repo root: ${REPO_ROOT}"
echo "[bundle-a-artifacts] Import script: ${IMPORT_SCRIPT_PATH}"
echo "[bundle-a-artifacts] Input source: ${INPUT_SOURCE}"
echo "[bundle-a-artifacts] Talents: ${TALENTS[*]}"

status=0
for talent_query in "${TALENTS[@]}"; do
  cmd=(env "TALENT_QUERY=${talent_query}" "TALENT_DATA_SOURCE=${INPUT_SOURCE}")
  if [[ -n "${INPUT_ROOT}" ]]; then
    cmd+=("TALENT_DATA_ROOT=${INPUT_ROOT}")
  fi
  if [[ -n "${DATALAKE_ROOT_ARG}" ]]; then
    cmd+=("TALENT_DATALAKE_ROOT=${DATALAKE_ROOT_ARG}")
  fi
  if [[ -n "${STAGING_ROOT_ARG}" ]]; then
    cmd+=("TALENT_STAGING_ROOT=${STAGING_ROOT_ARG}")
  fi
  if [[ -n "${WINDOW_DAYS}" ]]; then
    cmd+=("TALENT_WINDOW_DAYS=${WINDOW_DAYS}")
  fi
  if [[ -n "${START_DATE}" ]]; then
    cmd+=("TALENT_START_DATE=${START_DATE}")
  fi
  if [[ -n "${END_DATE}" ]]; then
    cmd+=("TALENT_END_DATE=${END_DATE}")
  fi
  cmd+=("${RSCRIPT_BIN}" "${IMPORT_SCRIPT_PATH}")

  echo
  echo "[bundle-a-artifacts] Talent: ${talent_query}"
  echo "[bundle-a-artifacts] Command: ${cmd[*]}"
  if [[ "${DRY_RUN}" == "true" ]]; then
    continue
  fi

  set +e
  "${cmd[@]}"
  rc=$?
  set -e
  if [[ ${rc} -ne 0 ]]; then
    status=1
    echo "[bundle-a-artifacts] Failed for talent: ${talent_query}" >&2
  fi
done

exit "${status}"
