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

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

IMPORT_SCRIPT="r_scripts/run/bundles/bundle_e/import_data.R"
RSCRIPT_BIN="${RSCRIPT_BIN:-Rscript}"
INPUT_SOURCE="datalake"
INPUT_ROOT=""
DATALAKE_ROOT_ARG=""
STAGING_ROOT_ARG=""
TALENTS_FILE=""
ALL_TALENTS="false"
ALLOW_PARTIAL_MATCH="false"
WINDOW_DAYS=""
START_DATE=""
END_DATE=""
DRY_RUN="false"

declare -a TALENTS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_E/run_bundle_E_artifacts.sh [options]

Description:
  Builds Bundle E artifacts for one or more talents under:
    <datalake_root>/<talent>/reports/bundle_e/artifacts/

Talent selection:
  --talent NAME
  --talents "A,B,C"
  --talents-file PATH
  --all
  --allow-partial-match

Window alignment:
  --window-days N
  --start-date YYYY-MM-DD
  --end-date YYYY-MM-DD

Roots:
  --input-source staging|datalake
  --input-root PATH
  --datalake-root PATH
  --staging-root PATH

Behavior:
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

resolve_path() {
  local p="$1"
  if [[ "$p" = /* || "$p" =~ ^[A-Za-z]:[\\/].* || "$p" =~ ^\\\\.* ]]; then
    printf '%s' "$p"
  else
    printf '%s' "${REPO_ROOT}/${p}"
  fi
}

resolve_data_root() {
  if [[ -n "${INPUT_ROOT}" ]]; then
    resolve_path "${INPUT_ROOT}"
  elif [[ "${INPUT_SOURCE}" == "datalake" ]]; then
    if [[ -n "${DATALAKE_ROOT_ARG}" ]]; then
      resolve_path "${DATALAKE_ROOT_ARG}"
    else
      (cd "${REPO_ROOT}" && "${RSCRIPT_BIN}" --vanilla -e "source('r_scripts/lib/utils/datalake_root.r'); cat(get_datalake_root())")
    fi
  else
    if [[ -n "${STAGING_ROOT_ARG}" ]]; then
      resolve_path "${STAGING_ROOT_ARG}"
    else
      (cd "${REPO_ROOT}" && "${RSCRIPT_BIN}" --vanilla -e "source('r_scripts/lib/utils/staging_root.R'); cat(get_staging_root())")
    fi
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

resolve_talent_folder_name() {
  local talent_query="$1"
  local root="$2"
  (
    cd "${REPO_ROOT}"
    TALENT_QUERY_INPUT="${talent_query}" TALENT_DATALAKE_ROOT_RESOLVE="${root}" ALLOW_PARTIAL_MATCH_INPUT="${ALLOW_PARTIAL_MATCH}" \
      "${RSCRIPT_BIN}" --vanilla -e "q <- Sys.getenv('TALENT_QUERY_INPUT'); root <- Sys.getenv('TALENT_DATALAKE_ROOT_RESOLVE'); allow_partial <- tolower(Sys.getenv('ALLOW_PARTIAL_MATCH_INPUT', 'false')) == 'true'; key <- function(x) gsub('^_+|_+$', '', gsub('_+', '_', gsub('[^a-z0-9]+', '_', tolower(x)))); if (!nzchar(root) || !dir.exists(root)) { cat('ERR: data root missing: ', root, sep=''); quit(status = 2) }; dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE); names <- basename(dirs); ql <- tolower(q); nl <- tolower(names); qk <- key(q); nk <- key(names); ix_exact <- which(nl == ql | nk == qk); if (length(ix_exact) >= 1) { cat(names[ix_exact[[1]]]); quit(status = 0) }; if (!allow_partial) { cat('ERR: no exact folder match for talent query: ', q, '. Use full folder name or pass --allow-partial-match.', sep=''); quit(status = 3) }; ix_part <- which(grepl(ql, nl, fixed = TRUE) | grepl(qk, nk, fixed = TRUE)); if (length(ix_part) == 1) { cat(names[ix_part[[1]]]); quit(status = 0) }; if (length(ix_part) > 1) { cat('ERR: multiple partial matches for talent query: ', q, '. Matches: ', paste(names[ix_part], collapse=', '), sep=''); quit(status = 3) }; cat('ERR: no folder match for talent query: ', q, sep=''); quit(status = 3)"
  )
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
      IFS=',' read -r -a csv_arr <<< "$2"
      for t in "${csv_arr[@]}"; do
        t="$(trim "$t")"
        [[ -n "$t" ]] && TALENTS+=("$t")
      done
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
    --allow-partial-match)
      ALLOW_PARTIAL_MATCH="true"
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
    --dry-run)
      DRY_RUN="true"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --quiet|--skip-interpretation|--skip-editorial-rewrite|--no-interpretations)
      shift
      ;;
    *)
      echo "Error: unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ "${INPUT_SOURCE}" != "staging" && "${INPUT_SOURCE}" != "datalake" ]]; then
  echo "Error: --input-source must be either staging or datalake." >&2
  exit 1
fi
if [[ -n "${WINDOW_DAYS}" ]] && { ! [[ "${WINDOW_DAYS}" =~ ^[0-9]+$ ]] || [[ "${WINDOW_DAYS}" -le 0 ]]; }; then
  echo "Error: --window-days must be a positive integer." >&2
  exit 1
fi

cd "${REPO_ROOT}"

IMPORT_SCRIPT_PATH="$(resolve_path "${IMPORT_SCRIPT}")"
if [[ ! -f "${IMPORT_SCRIPT_PATH}" ]]; then
  echo "Error: import script not found: ${IMPORT_SCRIPT_PATH}" >&2
  exit 1
fi

if [[ -n "${DATALAKE_ROOT_ARG}" ]]; then
  export TALENT_DATALAKE_ROOT="${DATALAKE_ROOT_ARG}"
fi
if [[ -n "${STAGING_ROOT_ARG}" ]]; then
  export TALENT_STAGING_ROOT="${STAGING_ROOT_ARG}"
fi
if [[ -z "${INPUT_ROOT}" && "${INPUT_SOURCE}" == "datalake" && -n "${DATALAKE_ROOT_ARG}" ]]; then
  INPUT_ROOT="${DATALAKE_ROOT_ARG}"
fi
if [[ -z "${INPUT_ROOT}" && "${INPUT_SOURCE}" == "staging" && -n "${STAGING_ROOT_ARG}" ]]; then
  INPUT_ROOT="${STAGING_ROOT_ARG}"
fi
if [[ -n "${INPUT_ROOT}" ]]; then
  INPUT_ROOT="$(resolve_path "${INPUT_ROOT}")"
fi
if [[ -z "${INPUT_ROOT}" ]]; then
  INPUT_ROOT="$(resolve_data_root)"
fi

if [[ -n "${TALENTS_FILE}" ]]; then
  load_talents_from_file "${TALENTS_FILE}"
fi
if [[ "${ALL_TALENTS}" == "true" ]]; then
  load_talents_all
fi
dedupe_talents_in_place

if [[ ${#TALENTS[@]} -eq 0 ]]; then
  echo "Error: pass --talent, --talents, --talents-file, or --all." >&2
  exit 1
fi

echo "[bundle-e-artifacts] Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "[bundle-e-artifacts] Repo root: ${REPO_ROOT}"
echo "[bundle-e-artifacts] Import script: ${IMPORT_SCRIPT_PATH}"
echo "[bundle-e-artifacts] Input source: ${INPUT_SOURCE}"
echo "[bundle-e-artifacts] Talents: ${TALENTS[*]}"

status=0
for talent_query in "${TALENTS[@]}"; do
  set +e
  talent="$(resolve_talent_folder_name "${talent_query}" "${INPUT_ROOT}")"
  rc_resolve=$?
  set -e
  if [[ ${rc_resolve} -ne 0 ]]; then
    status=1
    echo "Error: ${talent}" >&2
    continue
  fi
  talent="$(trim "${talent}")"
  cmd=("${RSCRIPT_BIN}" "${IMPORT_SCRIPT_PATH}")
  echo
  echo "[bundle-e-artifacts] Talent query: ${talent_query}"
  echo "[bundle-e-artifacts] Resolved talent: ${talent}"
  echo "[bundle-e-artifacts] Command: env TALENT_QUERY=${talent} TALENT_DATA_SOURCE=${INPUT_SOURCE} TALENT_DATA_ROOT=${INPUT_ROOT} TALENT_WINDOW_DAYS=${WINDOW_DAYS} ${cmd[*]}"

  if [[ "${DRY_RUN}" == "true" ]]; then
    continue
  fi

  set +e
  TALENT_QUERY="${talent}" \
    TALENT_DATA_SOURCE="${INPUT_SOURCE}" \
    TALENT_DATA_ROOT="${INPUT_ROOT}" \
    TALENT_WINDOW_DAYS="${WINDOW_DAYS}" \
    TALENT_START_DATE="${START_DATE}" \
    TALENT_END_DATE="${END_DATE}" \
    "${cmd[@]}"
  rc=$?
  set -e
  if [[ ${rc} -ne 0 ]]; then
    status=1
    echo "[bundle-e-artifacts] Artifact build failed for ${talent} with status ${rc}" >&2
  fi
done

exit "${status}"
