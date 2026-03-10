#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

DEFAULT_RENDER_SCRIPT="scripts/run/bundle_B/render_bundle_B.R"
DEFAULT_BUNDLE_NAME="bundle_B"
DEFAULT_OUTPUT_PREFIX="Bundle_B"
DEFAULT_REPORT_SUBDIR="reports"

RSCRIPT_BIN="${RSCRIPT_BIN:-Rscript}"
RENDER_SCRIPT="${DEFAULT_RENDER_SCRIPT}"
BUNDLE_NAME="${DEFAULT_BUNDLE_NAME}"
OUTPUT_PREFIX="${DEFAULT_OUTPUT_PREFIX}"
REPORT_SUBDIR="${DEFAULT_REPORT_SUBDIR}"
WINDOW_DAYS=""
START_DATE=""
END_DATE=""
WINDOW_LABEL=""
DATALAKE_ROOT_ARG=""
STAGING_ROOT_ARG=""
INPUT_RMD=""
INPUT_SOURCE="datalake"
INPUT_ROOT=""
TALENTS_FILE=""
TALENTS_CSV=""
ALL_TALENTS="false"
QUIET="false"
DRY_RUN="false"
ALLOW_PARTIAL_MATCH="false"

declare -a TALENTS=()
declare -a EXTRA_RENDER_ARGS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/run_bundle_B_report.sh [options] [-- extra render args]

Description:
  Linux wrapper for Bundle B rendering that writes each report to a datalake path:
    <datalake_root>/<talent>/<report_subdir>/<bundle_name>/
  and names files with window segment in prefix (for example: Bundle_B_window_90d_ava.html).

  It calls:
    scripts/run/bundle_B/render_bundle_B.R

Required:
  At least one talent selector OR --all (defaults to Ava if none supplied).

Talent selection:
  --talent NAME                Single talent (repeatable). Full folder name is safest.
  --talents "A,B,C"            Comma-separated talents
  --talents-file PATH          File with one talent per line (# comments allowed)
  --all                        Resolve all talents from selected input source root
  --allow-partial-match        Allow partial folder matching if exact match is not found

Analysis window:
  --window-days N              Days back from today (e.g., 90, 60, 30, 14, 7)
  --start-date YYYY-MM-DD      Optional explicit range start date
  --end-date YYYY-MM-DD        Optional explicit range end date
  --window-label LABEL         Optional folder label override (default: window_<N>d or all_data)

Output routing:
  --datalake-root PATH         Override TALENT_DATALAKE_ROOT
  --report-subdir NAME         Default: reports
  --bundle-name NAME           Default: bundle_B
  --output-prefix NAME         Default: Bundle_B

Render behavior:
  --input-source NAME          Data source for report input: staging|datalake (default: datalake)
  --input-root PATH            Explicit report input root override (talent folders root)
  --render-script PATH         Override render script path (default: scripts/run/bundle_B/render_bundle_B.R)
  --input PATH                 Optional input Rmd override passed to render script
  --rscript-bin PATH           Rscript binary (default: Rscript)
  --staging-root PATH          Override TALENT_STAGING_ROOT (used for --all and render)
  --quiet                      Pass --quiet to render script
  --dry-run                    Print commands without running

Pass-through:
  --                           Everything after -- is passed to render_bundle_B.R as-is

Examples:
  Single talent, 90d window:
    bin/linux/render_reports/run_bundle_B_report.sh --talent Ava --window-days 90

  Batch from file:
    bin/linux/render_reports/run_bundle_B_report.sh --talents-file notes/talent_list.txt --window-days 30

  All talents, custom roots:
    bin/linux/render_reports/run_bundle_B_report.sh \
      --all \
      --staging-root /mnt/staging/staging/Talent_data \
      --datalake-root /mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data \
      --window-days 60
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

resolve_datalake_root() {
  if [[ -n "${DATALAKE_ROOT_ARG}" ]]; then
    resolve_path "${DATALAKE_ROOT_ARG}"
    return 0
  fi
  if [[ -n "${TALENT_DATALAKE_ROOT:-}" ]]; then
    if [[ "${TALENT_DATALAKE_ROOT}" = /* ]]; then
      printf '%s' "${TALENT_DATALAKE_ROOT}"
    else
      resolve_path "${TALENT_DATALAKE_ROOT}"
    fi
    return 0
  fi

  (
    cd "${REPO_ROOT}"
    "${RSCRIPT_BIN}" --vanilla -e "source('scripts/lib/utils/datalake_root.r'); cat(get_datalake_root())"
  )
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
    TALENT_DATA_SOURCE_INPUT="${INPUT_SOURCE}" TALENT_DATA_ROOT_INPUT="${INPUT_ROOT}" "${RSCRIPT_BIN}" --vanilla -e "src <- tolower(Sys.getenv('TALENT_DATA_SOURCE_INPUT', 'staging')); root_in <- Sys.getenv('TALENT_DATA_ROOT_INPUT', ''); source('scripts/lib/utils/staging_root.R'); source('scripts/lib/utils/datalake_root.r'); source('scripts/lib/utils/talent_select.R'); root <- if (nzchar(root_in)) root_in else if (identical(src, 'datalake')) get_datalake_root() else get_staging_root(); cat(list_talents(root = root)\$name, sep='\n')"
  )"
  while IFS= read -r line; do
    line="$(trim "$line")"
    [[ -z "$line" ]] && continue
    TALENTS+=("$line")
  done <<< "$out"
}

resolve_talent_folder_name() {
  local talent_query="$1"
  (
    cd "${REPO_ROOT}"
    TALENT_QUERY_INPUT="${talent_query}" TALENT_DATALAKE_ROOT_RESOLVE="${DATALAKE_ROOT}" ALLOW_PARTIAL_MATCH_INPUT="${ALLOW_PARTIAL_MATCH}" \
      "${RSCRIPT_BIN}" --vanilla -e "q <- Sys.getenv('TALENT_QUERY_INPUT'); root <- Sys.getenv('TALENT_DATALAKE_ROOT_RESOLVE'); allow_partial <- tolower(Sys.getenv('ALLOW_PARTIAL_MATCH_INPUT', 'false')) == 'true'; if (!nzchar(root) || !dir.exists(root)) { cat('ERR: datalake root missing: ', root, sep=''); quit(status = 2) }; dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE); names <- basename(dirs); if (length(names) == 0) { cat('ERR: no talent folders under datalake root'); quit(status = 2) }; ql <- tolower(q); nl <- tolower(names); ix_exact <- which(nl == ql); if (length(ix_exact) >= 1) { cat(names[ix_exact[[1]]]); quit(status = 0) }; if (!allow_partial) { cat('ERR: no exact folder match for talent query: ', q, '. Use full folder name or pass --allow-partial-match.', sep=''); quit(status = 3) }; ix_part <- which(grepl(ql, nl, fixed = TRUE)); if (length(ix_part) == 1) { cat(names[ix_part[[1]]]); quit(status = 0) }; if (length(ix_part) > 1) { cat('ERR: multiple partial matches for talent query: ', q, '. Matches: ', paste(names[ix_part], collapse=', '), sep=''); quit(status = 3) }; cat('ERR: no folder match for talent query: ', q, sep=''); quit(status = 3)"
  )
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
    --window-label)
      [[ $# -ge 2 ]] || { echo "Error: --window-label requires a value" >&2; exit 1; }
      WINDOW_LABEL="$2"
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
    --report-subdir)
      [[ $# -ge 2 ]] || { echo "Error: --report-subdir requires a value" >&2; exit 1; }
      REPORT_SUBDIR="$2"
      shift 2
      ;;
    --bundle-name)
      [[ $# -ge 2 ]] || { echo "Error: --bundle-name requires a value" >&2; exit 1; }
      BUNDLE_NAME="$2"
      shift 2
      ;;
    --output-prefix)
      [[ $# -ge 2 ]] || { echo "Error: --output-prefix requires a value" >&2; exit 1; }
      OUTPUT_PREFIX="$2"
      shift 2
      ;;
    --render-script)
      [[ $# -ge 2 ]] || { echo "Error: --render-script requires a value" >&2; exit 1; }
      RENDER_SCRIPT="$2"
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
    --input)
      [[ $# -ge 2 ]] || { echo "Error: --input requires a value" >&2; exit 1; }
      INPUT_RMD="$2"
      shift 2
      ;;
    --rscript-bin)
      [[ $# -ge 2 ]] || { echo "Error: --rscript-bin requires a value" >&2; exit 1; }
      RSCRIPT_BIN="$2"
      shift 2
      ;;
    --allow-partial-match)
      ALLOW_PARTIAL_MATCH="true"
      shift
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
    --)
      shift
      while [[ $# -gt 0 ]]; do
        EXTRA_RENDER_ARGS+=("$1")
        shift
      done
      ;;
    *)
      echo "Error: unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ -n "${STAGING_ROOT_ARG}" ]]; then
  export TALENT_STAGING_ROOT="${STAGING_ROOT_ARG}"
fi
if [[ -n "${DATALAKE_ROOT_ARG}" ]]; then
  export TALENT_DATALAKE_ROOT="${DATALAKE_ROOT_ARG}"
fi

if [[ -n "${INPUT_ROOT}" ]]; then
  INPUT_ROOT="$(resolve_path "${INPUT_ROOT}")"
fi

if [[ "${INPUT_SOURCE}" != "staging" && "${INPUT_SOURCE}" != "datalake" ]]; then
  echo "Error: --input-source must be either 'staging' or 'datalake'." >&2
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

if [[ -n "${WINDOW_DAYS}" ]]; then
  if ! [[ "${WINDOW_DAYS}" =~ ^[0-9]+$ ]] || [[ "${WINDOW_DAYS}" -le 0 ]]; then
    echo "Error: --window-days must be a positive integer." >&2
    exit 1
  fi
fi

if [[ -n "${START_DATE}" ]] && ! [[ "${START_DATE}" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
  echo "Error: --start-date must be YYYY-MM-DD." >&2
  exit 1
fi
if [[ -n "${END_DATE}" ]] && ! [[ "${END_DATE}" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
  echo "Error: --end-date must be YYYY-MM-DD." >&2
  exit 1
fi

RENDER_SCRIPT_PATH="$(resolve_path "${RENDER_SCRIPT}")"
if [[ ! -f "${RENDER_SCRIPT_PATH}" ]]; then
  echo "Error: render script not found: ${RENDER_SCRIPT_PATH}" >&2
  exit 1
fi

if [[ -n "${INPUT_RMD}" ]]; then
  INPUT_RMD="$(resolve_path "${INPUT_RMD}")"
fi

DATALAKE_ROOT="$(resolve_datalake_root)"
if [[ -z "${DATALAKE_ROOT}" ]]; then
  echo "Error: could not resolve datalake root." >&2
  exit 1
fi
# If input is explicitly rooted to datalake and output root was not explicitly set,
# default output routing to the same root for consistency.
if [[ -z "${DATALAKE_ROOT_ARG}" && "${INPUT_SOURCE}" == "datalake" && -n "${INPUT_ROOT}" ]]; then
  DATALAKE_ROOT="${INPUT_ROOT}"
fi
mkdir -p "${DATALAKE_ROOT}"

WINDOW_SEGMENT="${WINDOW_LABEL}"
if [[ -z "${WINDOW_SEGMENT}" ]]; then
  if [[ -n "${START_DATE}" || -n "${END_DATE}" ]]; then
    if [[ -n "${START_DATE}" && -n "${END_DATE}" ]]; then
      WINDOW_SEGMENT="range_${START_DATE}_to_${END_DATE}"
    elif [[ -n "${START_DATE}" ]]; then
      WINDOW_SEGMENT="from_${START_DATE}"
    else
      WINDOW_SEGMENT="until_${END_DATE}"
    fi
  elif [[ -n "${WINDOW_DAYS}" ]]; then
    WINDOW_SEGMENT="window_${WINDOW_DAYS}d"
  else
    WINDOW_SEGMENT="all_data"
  fi
fi

echo "Repo root: ${REPO_ROOT}"
echo "Render script: ${RENDER_SCRIPT_PATH}"
echo "Rscript bin: ${RSCRIPT_BIN}"
echo "Datalake root: ${DATALAKE_ROOT}"
echo "Input source: ${INPUT_SOURCE}"
if [[ -n "${INPUT_ROOT}" ]]; then
  echo "Input root override: ${INPUT_ROOT}"
fi
echo "Bundle: ${BUNDLE_NAME}"
echo "Report subdir: ${REPORT_SUBDIR}"
echo "Window segment: ${WINDOW_SEGMENT}"
echo "Talents: ${TALENTS[*]}"

status=0
for talent_query in "${TALENTS[@]}"; do
  set +e
  talent_folder_name="$(resolve_talent_folder_name "${talent_query}" 2>/dev/null)"
  rc=$?
  set -e
  if [[ ${rc} -ne 0 ]]; then
    status=1
    echo "Error: ${talent_folder_name}" >&2
    continue
  fi
  talent_folder_name="$(trim "${talent_folder_name}")"
  if [[ -z "${talent_folder_name}" ]]; then
    echo "Error: could not resolve talent folder for query: ${talent_query}" >&2
    status=1
    continue
  fi

  output_dir="${DATALAKE_ROOT}/${talent_folder_name}/${REPORT_SUBDIR}/${BUNDLE_NAME}"
  mkdir -p "${output_dir}"
  output_prefix_for_run="${OUTPUT_PREFIX}_${WINDOW_SEGMENT}"

  cmd=(
    "${RSCRIPT_BIN}"
    "${RENDER_SCRIPT_PATH}"
    --talent "${talent_query}"
    --output-dir "${output_dir}"
    --output-prefix "${output_prefix_for_run}"
    --data-source "${INPUT_SOURCE}"
  )

  if [[ -n "${INPUT_ROOT}" ]]; then
    cmd+=(--data-root "${INPUT_ROOT}")
  fi

  if [[ -n "${WINDOW_DAYS}" ]]; then
    cmd+=(--window-days "${WINDOW_DAYS}")
  fi
  if [[ -n "${START_DATE}" ]]; then
    cmd+=(--start-date "${START_DATE}")
  fi
  if [[ -n "${END_DATE}" ]]; then
    cmd+=(--end-date "${END_DATE}")
  fi
  if [[ -n "${INPUT_RMD}" ]]; then
    cmd+=(--input "${INPUT_RMD}")
  fi
  if [[ "${QUIET}" == "true" ]]; then
    cmd+=(--quiet)
  fi
  if [[ ${#EXTRA_RENDER_ARGS[@]} -gt 0 ]]; then
    cmd+=("${EXTRA_RENDER_ARGS[@]}")
  fi

  echo
  echo "==> Talent query: ${talent_query}"
  echo "    Resolved output folder: ${talent_folder_name}"
  echo "    Output dir: ${output_dir}"
  echo "    Output prefix: ${output_prefix_for_run}"
  echo "    Command: ${cmd[*]}"

  if [[ "${DRY_RUN}" == "true" ]]; then
    continue
  fi

  if ! "${cmd[@]}"; then
    status=1
    echo "Error: render failed for talent: ${talent_folder_name}" >&2
  fi
done

if [[ "${DRY_RUN}" == "true" ]]; then
  echo
  echo "Dry run complete."
fi

exit "${status}"
