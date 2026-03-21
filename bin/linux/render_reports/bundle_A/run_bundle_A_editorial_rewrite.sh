#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

BUNDLE_NAME="bundle_A"
REPORT_SUBDIR="reports"
INTERPRET_DIR="interpretations"
PROMPT_SPEC="prompts/reports/bundle_a/06_editorial_review/02_full_report_editorial_rewrite/prompt.md"
INPUT_SOURCE="datalake"
INPUT_ROOT=""
DATALAKE_ROOT_ARG=""
STAGING_ROOT_ARG=""
WINDOW_DAYS=""
START_DATE=""
END_DATE=""
TALENTS_FILE=""
TALENTS_CSV=""
ALL_TALENTS="false"
ALLOW_PARTIAL_MATCH="false"
DRY_RUN="false"
PROMPT_FILTER=""
CODEX_BIN="${CODEX_BIN:-codex}"
RSCRIPT_BIN="${RSCRIPT_BIN:-Rscript}"

declare -a TALENTS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_A/run_bundle_A_editorial_rewrite.sh [options]

Description:
  Rewrites all client-facing Bundle A interpretation paragraphs so the final
  report reads as one cohesive narrative before HTML render.

Talent selection:
  --talent NAME
  --talents "A,B,C"
  --talents-file PATH
  --all
  --allow-partial-match

Rewrite selection:
  --prompt-filter TEXT   Rewrite only report paragraphs whose relative path contains TEXT

Compatibility args:
  --window-days N
  --start-date YYYY-MM-DD
  --end-date YYYY-MM-DD
  --input-source NAME
  --input-root PATH
  --datalake-root PATH
  --staging-root PATH
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

safe_slug() {
  local x
  x="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')"
  x="$(printf '%s' "$x" | sed -E 's/[^a-z0-9]+/_/g; s/^_+//; s/_+$//; s/_+/_/g')"
  if [[ -z "$x" ]]; then
    x="item"
  fi
  printf '%s' "$x"
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
    "${RSCRIPT_BIN}" --vanilla -e "source('r_scripts/lib/utils/datalake_root.r'); cat(get_datalake_root())"
  )
}

resolve_logs_root() {
  local datalake_root="$1"
  local analytics_root
  analytics_root="$(dirname "${datalake_root}")"
  printf '%s' "${analytics_root}/Processed/Logs/codex_prompts/reports/bundle_a_editorial_rewrite"
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
    --allow-partial-match)
      ALLOW_PARTIAL_MATCH="true"
      shift
      ;;
    --prompt-filter)
      [[ $# -ge 2 ]] || { echo "Error: --prompt-filter requires a value" >&2; exit 1; }
      PROMPT_FILTER="$2"
      shift 2
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
  echo "Error: no talent selector provided." >&2
  echo "Use one of: --talent NAME, --talents \"A,B\", --talents-file PATH, or --all." >&2
  exit 1
fi

cd "${REPO_ROOT}"

PROMPT_SPEC_PATH="$(resolve_path "${PROMPT_SPEC}")"
if [[ ! -f "${PROMPT_SPEC_PATH}" ]]; then
  echo "Error: prompt spec not found: ${PROMPT_SPEC_PATH}" >&2
  exit 1
fi
if ! command -v "${CODEX_BIN}" >/dev/null 2>&1; then
  echo "Error: codex CLI not found: ${CODEX_BIN}" >&2
  exit 1
fi

DATALAKE_ROOT="$(resolve_datalake_root)"
if [[ -z "${DATALAKE_ROOT}" || ! -d "${DATALAKE_ROOT}" ]]; then
  echo "Error: datalake root not found: ${DATALAKE_ROOT}" >&2
  exit 1
fi
LOG_ROOT="$(resolve_logs_root "${DATALAKE_ROOT}")"
mkdir -p "${LOG_ROOT}"

echo "[bundle-a-editorial] Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "[bundle-a-editorial] Prompt spec: ${PROMPT_SPEC_PATH}"
echo "[bundle-a-editorial] Datalake root: ${DATALAKE_ROOT}"
echo "[bundle-a-editorial] Log root: ${LOG_ROOT}"
echo "[bundle-a-editorial] Talents: ${TALENTS[*]}"

status=0
for talent_query in "${TALENTS[@]}"; do
  set +e
  talent_folder_name="$(resolve_talent_folder_name "${talent_query}" 2>/dev/null)"
  rc=$?
  set -e
  if [[ ${rc} -ne 0 ]]; then
    status=1
    echo "[bundle-a-editorial] Error: ${talent_folder_name}" >&2
    continue
  fi

  interpret_root="${DATALAKE_ROOT}/${talent_folder_name}/${REPORT_SUBDIR}/${BUNDLE_NAME}/${INTERPRET_DIR}"
  work_dir="${interpret_root}/06_editorial_review/02_full_report_editorial_rewrite"
  input_file="${work_dir}/input.md"
  current_json="${work_dir}/current_paragraphs.json"
  rewrite_json="${work_dir}/rewrite_output.json"
  review_note_file="${interpret_root}/06_editorial_review/01_full_report_editorial_review/output.md"
  talent_log_dir="${LOG_ROOT}/$(safe_slug "${talent_folder_name}")"
  mkdir -p "${work_dir}" "${talent_log_dir}"

  if [[ ! -d "${interpret_root}" ]]; then
    status=1
    echo "[bundle-a-editorial] Missing interpretation root: ${interpret_root}" >&2
    continue
  fi

  report_files=()
  exec_summary="${interpret_root}/05_report_bookends/01_executive_summary/output.md"
  conclusion="${interpret_root}/05_report_bookends/02_conclusion/output.md"
  [[ -f "${exec_summary}" ]] && report_files+=("${exec_summary}")
  while IFS= read -r f; do
    [[ -z "${f}" ]] && continue
    report_files+=("${f}")
  done < <(find "${interpret_root}/01_overall_performance_snapshot" "${interpret_root}/02_trends_over_time" "${interpret_root}/03_audience_composition" "${interpret_root}/04_content_strategy_deep_dive" -name 'output.md' 2>/dev/null | sort)
  [[ -f "${conclusion}" ]] && report_files+=("${conclusion}")

  filtered_files=()
  for f in "${report_files[@]}"; do
    rel="${f#${interpret_root}/}"
    if [[ -n "${PROMPT_FILTER}" && "${rel}" != *"${PROMPT_FILTER}"* ]]; then
      continue
    fi
    filtered_files+=("${f}")
  done
  report_files=("${filtered_files[@]}")

  if [[ ${#report_files[@]} -eq 0 ]]; then
    status=1
    echo "[bundle-a-editorial] No report-facing interpretation outputs matched for talent: ${talent_query}" >&2
    continue
  fi

  python3 - "${interpret_root}" "${current_json}" "${report_files[@]}" <<'PY'
import json
import sys
from pathlib import Path

interpret_root = Path(sys.argv[1])
out_path = Path(sys.argv[2])
files = [Path(x) for x in sys.argv[3:]]
payload = {}
for f in files:
    rel = f.relative_to(interpret_root).as_posix().replace("/output.md", "")
    payload[rel] = f.read_text(encoding="utf-8").strip()
out_path.write_text(json.dumps(payload, indent=2, ensure_ascii=False), encoding="utf-8")
PY

    {
      echo "# Bundle A Editorial Rewrite Task"
      echo
      echo "Rewrite the full set of client-facing Bundle A paragraphs so the report reads like one cohesive, professionally edited narrative."
      echo
      echo "## Output rules"
      echo "- Return JSON only."
      echo "- Do not wrap the JSON in code fences."
      echo "- Use exactly the same keys that appear in the current paragraph JSON."
      echo "- Rewrite every value as one short paragraph."
      echo "- Preserve the factual meaning, important numbers, and analytical direction unless the current wording is awkward or overstated."
      echo "- Make the report feel edited as a whole, not like independent prompt outputs."
      echo "- Reduce repeated transitions, repeated structure, and repeated setup language."
      echo "- The executive summary may mention the date range once if useful; later paragraphs should not keep restating it."
      echo
      echo "## Context"
      echo "- Talent: ${talent_folder_name}"
      echo "- Prompt spec path: ${PROMPT_SPEC_PATH}"
      echo "- Current paragraph JSON: ${current_json}"
      if [[ -f "${review_note_file}" ]]; then
        echo "- Editorial review notes available: ${review_note_file}"
      fi
      echo
      echo "## Prompt spec"
      echo
    } > "${input_file}"
    cat "${PROMPT_SPEC_PATH}" >> "${input_file}"
    {
      echo
      echo
      if [[ -f "${review_note_file}" ]]; then
        echo "## Existing editorial review notes"
        echo
        cat "${review_note_file}"
        echo
        echo
      fi
      echo "## Current paragraph JSON"
      echo
      cat "${current_json}"
      echo
      echo
      echo "## Required action"
      echo
      echo "Return a single JSON object that rewrites every provided paragraph and preserves the same keys."
    } >> "${input_file}"

  run_ts="$(date +%Y-%m-%d_%H-%M-%S)"
  log_file="${talent_log_dir}/editorial_rewrite_${run_ts}.log"

  echo
  echo "[bundle-a-editorial] Talent: ${talent_query}"
  echo "[bundle-a-editorial] Interpretation root: ${interpret_root}"
  echo "[bundle-a-editorial] Rewrite input: ${input_file}"
  echo "[bundle-a-editorial] Rewrite output JSON: ${rewrite_json}"
  echo "[bundle-a-editorial] Log file: ${log_file}"

  if [[ "${DRY_RUN}" == "true" ]]; then
    continue
  fi

  rm -f "${rewrite_json}"
  set +e
  (
    echo "=== Codex editorial rewrite started: $(date) ==="
    echo "Repo: ${REPO_ROOT}"
    echo "Talent: ${talent_folder_name}"
    echo "Input file: ${input_file}"
    echo "Rewrite output JSON: ${rewrite_json}"
    echo
    cat "${input_file}" | "${CODEX_BIN}" exec \
      --cd "${REPO_ROOT}" \
      --dangerously-bypass-approvals-and-sandbox \
      --output-last-message "${rewrite_json}" \
      -
    rc_codex=$?
    echo
    echo "=== Codex editorial rewrite finished: $(date) ==="
    echo "Exit code: ${rc_codex}"
    exit ${rc_codex}
  ) >> "${log_file}" 2>&1
  rc_rewrite=$?
  set -e

  if [[ ${rc_rewrite} -ne 0 || ! -s "${rewrite_json}" ]]; then
    status=1
    echo "[bundle-a-editorial] Editorial rewrite failed for talent: ${talent_folder_name}" >&2
    echo "[bundle-a-editorial] See log: ${log_file}" >&2
    continue
  fi

  if ! python3 - "${interpret_root}" "${rewrite_json}" "${report_files[@]}" <<'PY'
import json
import sys
from pathlib import Path

interpret_root = Path(sys.argv[1])
rewrite_json = Path(sys.argv[2])
files = [Path(x) for x in sys.argv[3:]]
data = json.loads(rewrite_json.read_text(encoding="utf-8"))
if not isinstance(data, dict):
    raise SystemExit("Rewrite output is not a JSON object.")

expected = {}
for f in files:
    rel = f.relative_to(interpret_root).as_posix().replace("/output.md", "")
    expected[rel] = f

missing = [k for k in expected if k not in data]
if missing:
    raise SystemExit("Missing rewritten keys: " + ", ".join(missing))

for key, path in expected.items():
    value = data[key]
    if not isinstance(value, str) or not value.strip():
        raise SystemExit(f"Key {key} is missing rewritten paragraph text.")
    path.write_text(value.strip() + "\n", encoding="utf-8")
PY
  then
    status=1
    echo "[bundle-a-editorial] Rewrite JSON validation failed for talent: ${talent_folder_name}" >&2
    echo "[bundle-a-editorial] See log: ${log_file}" >&2
    continue
  fi

  echo "[bundle-a-editorial] Overwrote ${#report_files[@]} report paragraph(s) for ${talent_folder_name}."
done

exit "${status}"
