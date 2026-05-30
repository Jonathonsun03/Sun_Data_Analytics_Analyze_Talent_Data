#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

RENDER_SCRIPT="r_scripts/run/bundle_e/render_bundle_e.R"
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
QUIET="false"
DRY_RUN="false"

declare -a TALENTS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_E/run_bundle_E_render_only.sh [options]

Description:
  Renders Bundle E reports and publishes final HTML to:
    <datalake_root>/<talent>/reports/bundle_e/report/current/

Options:
  --talent NAME
  --talents "A,B,C"
  --talents-file PATH
  --all
  --allow-partial-match
  --window-days N
  --start-date YYYY-MM-DD
  --end-date YYYY-MM-DD
  --input-source staging|datalake
  --input-root PATH
  --datalake-root PATH
  --staging-root PATH
  --quiet
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

slugify_talent() {
  local slug
  slug="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/_/g; s/^_+//; s/_+$//')"
  [[ -n "${slug}" ]] || slug="talent"
  printf '%s' "${slug}"
}

resolve_datalake_root() {
  if [[ -n "${DATALAKE_ROOT_ARG}" ]]; then
    resolve_path "${DATALAKE_ROOT_ARG}"
  elif [[ -n "${TALENT_DATALAKE_ROOT:-}" ]]; then
    resolve_path "${TALENT_DATALAKE_ROOT}"
  else
    (cd "${REPO_ROOT}" && "${RSCRIPT_BIN}" --vanilla -e "source('r_scripts/lib/utils/datalake_root.r'); cat(get_datalake_root())")
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

archive_current_reports() {
  local current_dir="$1"
  local archive_dir="$2"
  local window_segment="$3"
  local current_file base target stamp

  mkdir -p "${current_dir}" "${archive_dir}"
  for current_file in "${current_dir}"/*.html; do
    [[ -e "${current_file}" ]] || continue
    base="$(basename "${current_file}")"
    if [[ -n "${window_segment}" && "${base}" != *"${window_segment}"* ]]; then
      continue
    fi
    target="${archive_dir}/${base}"
    if [[ -e "${target}" ]]; then
      stamp="$(date -u +"%Y%m%dT%H%M%SZ")"
      target="${archive_dir}/${base%.html}_archived_${stamp}.html"
    fi
    mv "${current_file}" "${target}"
  done
}

publish_rendered_report() {
  local output_dir="$1"
  local current_dir="$2"
  local archive_dir="$3"
  local output_prefix_for_run="$4"
  local talent="$5"
  local window_segment="$6"
  local talent_slug rendered_file
  local -a matches=()

  talent_slug="$(slugify_talent "${talent}")"
  rendered_file="${output_dir}/${output_prefix_for_run}_${talent_slug}.html"
  if [[ ! -f "${rendered_file}" ]]; then
    matches=("${output_dir}/${output_prefix_for_run}"_*.html)
    if [[ ${#matches[@]} -eq 1 && -f "${matches[0]}" ]]; then
      rendered_file="${matches[0]}"
    else
      echo "Error: could not identify rendered Bundle E HTML under ${output_dir}" >&2
      return 1
    fi
  fi

  archive_current_reports "${current_dir}" "${archive_dir}" "${window_segment}"
  mv "${rendered_file}" "${current_dir}/$(basename "${rendered_file}")"
  echo "    Published current report: ${current_dir}/$(basename "${rendered_file}")"
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
    --skip-interpretation|--skip-editorial-rewrite|--no-interpretations)
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

DATALAKE_ROOT="$(resolve_datalake_root)"
if [[ -z "${INPUT_ROOT}" ]]; then
  if [[ "${INPUT_SOURCE}" == "datalake" ]]; then
    INPUT_ROOT="${DATALAKE_ROOT}"
  elif [[ -n "${STAGING_ROOT_ARG}" ]]; then
    INPUT_ROOT="$(resolve_path "${STAGING_ROOT_ARG}")"
  else
    INPUT_ROOT="$(cd "${REPO_ROOT}" && "${RSCRIPT_BIN}" --vanilla -e "source('r_scripts/lib/utils/staging_root.R'); cat(get_staging_root())")"
  fi
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
RENDER_SCRIPT_PATH="$(resolve_path "${RENDER_SCRIPT}")"
RUN_DATE="$(date +"%Y-%m-%d")"
WINDOW_SEGMENT="all_data"
if [[ -n "${WINDOW_DAYS}" ]]; then
  WINDOW_SEGMENT="window_${WINDOW_DAYS}d"
elif [[ -n "${START_DATE}" || -n "${END_DATE}" ]]; then
  WINDOW_SEGMENT="range_${START_DATE:-open}_to_${END_DATE:-open}"
fi

echo "Repo root: ${REPO_ROOT}"
echo "Render script: ${RENDER_SCRIPT_PATH}"
echo "Datalake root: ${DATALAKE_ROOT}"
echo "Input source: ${INPUT_SOURCE}"
echo "Render date: ${RUN_DATE}"
echo "Window segment: ${WINDOW_SEGMENT}"
echo "Talents: ${TALENTS[*]}"

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
  output_dir="${DATALAKE_ROOT}/${talent}/reports/bundle_e"
  current_dir="${output_dir}/report/current"
  archive_dir="${output_dir}/report/archive"
  output_prefix_for_run="Bundle_E_${RUN_DATE}_${WINDOW_SEGMENT}"
  cmd=(
    "${RSCRIPT_BIN}"
    "${RENDER_SCRIPT_PATH}"
    --talent "${talent}"
    --output-dir "${output_dir}"
    --output-prefix "${output_prefix_for_run}"
    --data-source "${INPUT_SOURCE}"
  )
  [[ -n "${INPUT_ROOT}" ]] && cmd+=(--data-root "${INPUT_ROOT}")
  [[ -n "${WINDOW_DAYS}" ]] && cmd+=(--window-days "${WINDOW_DAYS}")
  [[ -n "${START_DATE}" ]] && cmd+=(--start-date "${START_DATE}")
  [[ -n "${END_DATE}" ]] && cmd+=(--end-date "${END_DATE}")
  [[ "${QUIET}" == "true" ]] && cmd+=(--quiet)

  echo
  echo "==> Talent query: ${talent_query}"
  echo "    Resolved output folder: ${talent}"
  echo "    Output dir: ${output_dir}"
  echo "    Current dir: ${current_dir}"
  echo "    Command: ${cmd[*]}"

  if [[ "${DRY_RUN}" == "true" ]]; then
    continue
  fi

  mkdir -p "${output_dir}" "${current_dir}" "${archive_dir}"
  if ! "${cmd[@]}"; then
    status=1
    echo "Error: Bundle E render failed for talent: ${talent}" >&2
  elif ! publish_rendered_report "${output_dir}" "${current_dir}" "${archive_dir}" "${output_prefix_for_run}" "${talent}" "${WINDOW_SEGMENT}"; then
    status=1
    echo "Error: Bundle E publish failed for talent: ${talent}" >&2
  fi
done

if [[ "${DRY_RUN}" == "true" ]]; then
  echo
  echo "Dry run complete."
fi

exit "${status}"
