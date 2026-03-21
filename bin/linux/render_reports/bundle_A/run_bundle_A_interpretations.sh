#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

BUNDLE_NAME="bundle_A"
REPORT_SUBDIR="reports"
INTERPRET_DIR="interpretations"
PROMPTS_ROOT="prompts/reports/bundle_a"
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
MAX_PROMPTS=""
CODEX_BIN="${CODEX_BIN:-codex}"
RSCRIPT_BIN="${RSCRIPT_BIN:-Rscript}"

declare -a TALENTS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/bundle_A/run_bundle_A_interpretations.sh [options]

Description:
  Generates Bundle A interpretation markdown files by pairing prompt specs with
  Bundle A artifact tables/figures and calling `codex exec`.

  Output structure:
    <datalake_root>/<talent>/reports/bundle_A/interpretations/<section>/<prompt>/input.md
    <datalake_root>/<talent>/reports/bundle_A/interpretations/<section>/<prompt>/output.md

Talent selection:
  --talent NAME
  --talents "A,B,C"
  --talents-file PATH
  --all
  --allow-partial-match

Prompt selection:
  --prompt-filter TEXT   Run only prompts whose relative path contains TEXT
  --max-prompts N        Limit prompt count for testing

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
  printf '%s' "${analytics_root}/Processed/Logs/codex_prompts/reports/bundle_a_interpretations"
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

prompt_keys_for_rel_dir() {
  local rel_dir="$1"
  local parent="${rel_dir%%/*}"
  local child="${rel_dir##*/}"

  case "${child}" in
    00_section_synthesis)
      case "${parent}" in
        01_overall_performance_snapshot)
          printf '%s\n' "views_by_content_type combined_performance_trends total_revenue_by_content_type engagement_distribution_successful_videos"
          ;;
        02_trends_over_time)
          printf '%s\n' "revenue_over_time_by_content_type total_views_over_time_by_content_type"
          ;;
        03_audience_composition)
          printf '%s\n' "audience_age_gender_trends"
          ;;
        04_content_strategy_deep_dive)
          printf '%s\n' "weekend_vs_weekday day_of_week_distribution collaboration_effectiveness topic_performance tag_performance average_views_per_tag"
          ;;
        *)
          printf '%s\n' ""
          ;;
      esac
      ;;
    01_views_by_content_type) printf '%s\n' "views_by_content_type" ;;
    02_combined_performance_trends) printf '%s\n' "combined_performance_trends" ;;
    03_total_revenue_by_content_type) printf '%s\n' "total_revenue_by_content_type" ;;
    04_engagement_distribution_successful_videos) printf '%s\n' "engagement_distribution_successful_videos" ;;
    01_revenue_over_time_by_content_type) printf '%s\n' "revenue_over_time_by_content_type" ;;
    02_total_views_over_time_by_content_type) printf '%s\n' "total_views_over_time_by_content_type" ;;
    01_audience_age_gender_trends) printf '%s\n' "audience_age_gender_trends" ;;
    01_weekend_vs_weekday) printf '%s\n' "weekend_vs_weekday" ;;
    02_day_of_week_distribution) printf '%s\n' "day_of_week_distribution" ;;
    03_collaboration_effectiveness) printf '%s\n' "collaboration_effectiveness" ;;
    04_topic_performance) printf '%s\n' "topic_performance topic_view_distribution" ;;
    05_tag_performance) printf '%s\n' "tag_performance tag_view_distribution" ;;
    06_average_views_per_tag) printf '%s\n' "average_views_per_tag tag_performance" ;;
    01_executive_summary) printf '%s\n' "views_by_content_type combined_performance_trends total_revenue_by_content_type engagement_distribution_successful_videos revenue_over_time_by_content_type total_views_over_time_by_content_type audience_age_gender_trends weekend_vs_weekday day_of_week_distribution collaboration_effectiveness topic_performance tag_performance average_views_per_tag" ;;
    02_conclusion) printf '%s\n' "views_by_content_type combined_performance_trends total_revenue_by_content_type engagement_distribution_successful_videos revenue_over_time_by_content_type total_views_over_time_by_content_type audience_age_gender_trends weekend_vs_weekday day_of_week_distribution collaboration_effectiveness topic_performance tag_performance average_views_per_tag" ;;
    01_full_report_editorial_review) printf '%s\n' "views_by_content_type combined_performance_trends total_revenue_by_content_type engagement_distribution_successful_videos revenue_over_time_by_content_type total_views_over_time_by_content_type audience_age_gender_trends weekend_vs_weekday day_of_week_distribution collaboration_effectiveness topic_performance tag_performance average_views_per_tag" ;;
    *)
      printf '%s\n' ""
      ;;
  esac
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
    --max-prompts)
      [[ $# -ge 2 ]] || { echo "Error: --max-prompts requires a value" >&2; exit 1; }
      MAX_PROMPTS="$2"
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

if [[ -n "${MAX_PROMPTS}" ]] && { ! [[ "${MAX_PROMPTS}" =~ ^[0-9]+$ ]] || [[ "${MAX_PROMPTS}" -le 0 ]]; }; then
  echo "Error: --max-prompts must be a positive integer." >&2
  exit 1
fi

cd "${REPO_ROOT}"

if [[ -z "${LANG:-}" ]]; then
  export LANG="C.UTF-8"
fi
if [[ -z "${LC_ALL:-}" ]]; then
  export LC_ALL="${LANG}"
fi

PROMPTS_ROOT_PATH="$(resolve_path "${PROMPTS_ROOT}")"
if [[ ! -d "${PROMPTS_ROOT_PATH}" ]]; then
  echo "Error: prompts root not found: ${PROMPTS_ROOT_PATH}" >&2
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

mapfile -t ALL_PROMPT_FILES < <(find "${PROMPTS_ROOT_PATH}" -mindepth 2 -maxdepth 3 -name 'prompt.md' | sort)
if [[ ${#ALL_PROMPT_FILES[@]} -eq 0 ]]; then
  echo "Error: no prompt.md files found under ${PROMPTS_ROOT_PATH}" >&2
  exit 1
fi

echo "[bundle-a-interpretations] Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "[bundle-a-interpretations] Prompts root: ${PROMPTS_ROOT_PATH}"
echo "[bundle-a-interpretations] Datalake root: ${DATALAKE_ROOT}"
echo "[bundle-a-interpretations] Log root: ${LOG_ROOT}"
echo "[bundle-a-interpretations] Talents: ${TALENTS[*]}"
if [[ -n "${PROMPT_FILTER}" ]]; then
  echo "[bundle-a-interpretations] Prompt filter: ${PROMPT_FILTER}"
fi
if [[ -n "${MAX_PROMPTS}" ]]; then
  echo "[bundle-a-interpretations] Max prompts: ${MAX_PROMPTS}"
fi

status=0
for talent_query in "${TALENTS[@]}"; do
  set +e
  talent_folder_name="$(resolve_talent_folder_name "${talent_query}" 2>/dev/null)"
  rc=$?
  set -e
  if [[ ${rc} -ne 0 ]]; then
    status=1
    echo "[bundle-a-interpretations] Error: ${talent_folder_name}" >&2
    continue
  fi

  artifact_root="${DATALAKE_ROOT}/${talent_folder_name}/${REPORT_SUBDIR}/${BUNDLE_NAME}/artifacts"
  interpret_root="${DATALAKE_ROOT}/${talent_folder_name}/${REPORT_SUBDIR}/${BUNDLE_NAME}/${INTERPRET_DIR}"
  ai_inputs_json="${artifact_root}/bundle_a_ai_inputs.json"
  manifest_json="${artifact_root}/bundle_a_artifact_manifest.json"

  echo
  echo "[bundle-a-interpretations] Talent: ${talent_query}"
  echo "[bundle-a-interpretations] Artifact root: ${artifact_root}"
  echo "[bundle-a-interpretations] Interpretation root: ${interpret_root}"

  if [[ ! -f "${ai_inputs_json}" ]]; then
    status=1
    echo "[bundle-a-interpretations] Missing AI inputs JSON: ${ai_inputs_json}" >&2
    continue
  fi
  if [[ ! -f "${manifest_json}" ]]; then
    status=1
    echo "[bundle-a-interpretations] Missing artifact manifest: ${manifest_json}" >&2
    continue
  fi

  mkdir -p "${interpret_root}"

  prompts_run=0
  prompt_failed=0
  last_output_file=""
  last_output_rel_dir=""
  talent_log_dir="${LOG_ROOT}/$(safe_slug "${talent_folder_name}")"
  mkdir -p "${talent_log_dir}"

  for prompt_file in "${ALL_PROMPT_FILES[@]}"; do
    rel_prompt_file="${prompt_file#${PROMPTS_ROOT_PATH}/}"
    rel_prompt_dir="$(dirname "${rel_prompt_file}")"
    parent_prompt_dir="${rel_prompt_dir%%/*}"
    prompt_dir_name="$(basename "${rel_prompt_dir}")"
    is_report_bookend="false"
    is_editorial_review="false"
    if [[ "${parent_prompt_dir}" == "05_report_bookends" ]]; then
      is_report_bookend="true"
    fi
    if [[ "${parent_prompt_dir}" == "06_editorial_review" ]]; then
      is_editorial_review="true"
    fi
    if [[ -n "${PROMPT_FILTER}" && "${rel_prompt_dir}" != *"${PROMPT_FILTER}"* ]]; then
      continue
    fi
    if [[ -z "${PROMPT_FILTER}" && "${parent_prompt_dir}" == "06_editorial_review" ]]; then
      continue
    fi
    if [[ -n "${MAX_PROMPTS}" && ${prompts_run} -ge ${MAX_PROMPTS} ]]; then
      break
    fi

    prompt_keys_str="$(prompt_keys_for_rel_dir "${rel_prompt_dir}")"
    read -r -a prompt_keys <<< "${prompt_keys_str}"

    output_dir="${interpret_root}/${rel_prompt_dir}"
    mkdir -p "${output_dir}"
    input_file="${output_dir}/input.md"
    output_file="${output_dir}/output.md"
    section_synthesis_file="${interpret_root}/${parent_prompt_dir}/00_section_synthesis/output.md"
    executive_summary_file="${interpret_root}/05_report_bookends/01_executive_summary/output.md"
    conclusion_file="${interpret_root}/05_report_bookends/02_conclusion/output.md"

    date_policy="Assume the report-level date range has already been established. Do not restate the full date range unless it is genuinely necessary for interpretation."
    if [[ "${rel_prompt_dir}" == "01_overall_performance_snapshot/00_section_synthesis" ]]; then
      date_policy="This is the opening synthesis paragraph, so it may mention the explicit date range once if helpful."
    elif [[ "${prompt_dir_name}" == "01_executive_summary" ]]; then
      date_policy="This is the executive summary, so it may mention the explicit date range once if helpful."
    fi

    relevant_tables=()
    relevant_figures=()
    for key in "${prompt_keys[@]}"; do
      [[ -z "${key}" ]] && continue
      if [[ -f "${artifact_root}/tables/${key}.csv" ]]; then
        relevant_tables+=("${artifact_root}/tables/${key}.csv")
      fi
      if [[ -f "${artifact_root}/figures/${key}.png" ]]; then
        relevant_figures+=("${artifact_root}/figures/${key}.png")
      fi
    done

    {
      echo "# Bundle A Interpretation Generation Task"
      echo
      echo "Write the final interpretation markdown for one Bundle A prompt."
      echo
      echo "## Output rules"
      echo "- Return markdown only."
      if [[ "${is_editorial_review}" == "true" ]]; then
        echo "- Return flat bullets for internal editorial QA."
        echo "- Do not wrap the answer in code fences."
        echo "- Follow the prompt spec exactly."
        echo "- Focus on editorial quality rather than analysis."
        echo "- Do not use nested bullets or headings."
      else
        echo "- Return exactly one brief paragraph."
        echo "- Do not wrap the answer in code fences."
        echo "- Follow the prompt spec exactly."
        echo "- Keep the response narrative, not list-based."
        echo "- Do not use bullets, numbered lists, headings, labels, or bold section starters."
        echo "- Naturally cover the interpretation, why it matters, and the recommended follow-up within the paragraph."
        echo "- Make the paragraph read like part of a single cohesive report, not a standalone memo."
        echo "- Avoid repeating setup facts, framing, or takeaways that were already established earlier in the report."
        echo "- Vary sentence openings; do not repeatedly begin with the date range."
        echo "- Use transition phrases sparingly; do not force openers like 'within that context' or 'against that backdrop' unless they read naturally."
        echo "- Blend the interpretation, significance, and follow-up naturally instead of making every paragraph follow the exact same sentence-by-sentence template."
        echo "- ${date_policy}"
      fi
      echo "- Use only the provided evidence files."
      echo "- If evidence is sparse or mixed, say so plainly."
      echo "- Do not add prefaces, notes, or explanations outside the requested output format."
      echo
      echo "## Context"
      echo "- Talent: ${talent_folder_name}"
      echo "- Input source: ${INPUT_SOURCE}"
      if [[ -n "${WINDOW_DAYS}" ]]; then
        echo "- Window days: ${WINDOW_DAYS}"
      fi
      if [[ -n "${START_DATE}" ]]; then
        echo "- Start date: ${START_DATE}"
      fi
      if [[ -n "${END_DATE}" ]]; then
        echo "- End date: ${END_DATE}"
      fi
      echo "- Prompt folder: ${rel_prompt_dir}"
      echo "- Prompt spec path: ${prompt_file}"
      echo "- AI inputs JSON: ${ai_inputs_json}"
      echo "- Artifact manifest: ${manifest_json}"
      echo "- Output path for this run: ${output_file}"
      echo
      echo "## Relevant artifact keys"
      for key in "${prompt_keys[@]}"; do
        [[ -z "${key}" ]] && continue
        echo "- ${key}"
      done
      echo
      echo "## Relevant tables"
      if [[ ${#relevant_tables[@]} -eq 0 ]]; then
        echo "- None explicitly mapped. Use the AI inputs JSON and artifact manifest if needed."
      else
        for tbl in "${relevant_tables[@]}"; do
          echo "- ${tbl}"
        done
      fi
      echo
      echo "## Relevant figures"
      if [[ ${#relevant_figures[@]} -eq 0 ]]; then
        echo "- None explicitly mapped."
      else
        for fig in "${relevant_figures[@]}"; do
          echo "- ${fig}"
        done
      fi
      echo
      echo "## Narrative continuity context"
      echo "- Treat this paragraph as part of a continuous report narrative."
      echo "- Pull forward the bigger picture when useful, but do not restate the same point verbatim."
      echo "- Prefer direct, natural openings when possible; only use bridging phrases when they genuinely help continuity."
      echo
      if [[ -n "${last_output_file}" && -f "${last_output_file}" ]]; then
        echo "## Most recent prior report paragraph"
        echo
        cat "${last_output_file}"
        echo
        echo
      fi
      if [[ "${prompt_dir_name}" != "00_section_synthesis" && -f "${section_synthesis_file}" ]]; then
        echo "## Current section synthesis already written"
        echo
        cat "${section_synthesis_file}"
        echo
        echo
      fi
      if [[ "${is_report_bookend}" == "true" || "${is_editorial_review}" == "true" ]]; then
        echo "## Existing report paragraphs"
        echo
        while IFS= read -r existing_output; do
          [[ -z "${existing_output}" ]] && continue
          if [[ "${existing_output}" == "${output_file}" ]]; then
            continue
          fi
          rel_existing="${existing_output#${interpret_root}/}"
          echo "### ${rel_existing}"
          cat "${existing_output}"
          echo
          echo
        done < <(find "${interpret_root}" -name 'output.md' | sort)
      fi
      if [[ -f "${executive_summary_file}" && "${prompt_dir_name}" != "01_executive_summary" ]]; then
        echo "## Executive summary already written"
        echo
        cat "${executive_summary_file}"
        echo
        echo
      fi
      if [[ -f "${conclusion_file}" && "${prompt_dir_name}" != "02_conclusion" ]]; then
        echo "## Conclusion already written"
        echo
        cat "${conclusion_file}"
        echo
        echo
      fi
      echo
      echo "## Prompt spec"
      echo
    } > "${input_file}"
    cat "${prompt_file}" >> "${input_file}"
    {
      echo
      echo
      echo "## Required action"
      echo
      echo "Read the prompt spec and the listed evidence files, then produce the final markdown only."
    } >> "${input_file}"

    run_ts="$(date +%Y-%m-%d_%H-%M-%S)"
    log_file="${talent_log_dir}/$(safe_slug "${rel_prompt_dir}")_${run_ts}.log"

    echo
    echo "[bundle-a-interpretations] Prompt: ${rel_prompt_dir}"
    echo "[bundle-a-interpretations] Input file: ${input_file}"
    echo "[bundle-a-interpretations] Output file: ${output_file}"
    echo "[bundle-a-interpretations] Log file: ${log_file}"

    prompts_run=$((prompts_run + 1))
    if [[ "${DRY_RUN}" == "true" ]]; then
      continue
    fi

    rm -f "${output_file}"
    set +e
    (
      echo "=== Codex run started: $(date) ==="
      echo "Repo: ${REPO_ROOT}"
      echo "Talent: ${talent_folder_name}"
      echo "Prompt folder: ${rel_prompt_dir}"
      echo "Input file: ${input_file}"
      echo "Output file: ${output_file}"
      echo
      cat "${input_file}" | "${CODEX_BIN}" exec \
        --cd "${REPO_ROOT}" \
        --dangerously-bypass-approvals-and-sandbox \
        --output-last-message "${output_file}" \
        -
      rc_codex=$?
      echo
      echo "=== Codex run finished: $(date) ==="
      echo "Exit code: ${rc_codex}"
      exit ${rc_codex}
    ) >> "${log_file}" 2>&1
    rc_prompt=$?
    set -e

    if [[ ${rc_prompt} -ne 0 || ! -s "${output_file}" ]]; then
      status=1
      prompt_failed=$((prompt_failed + 1))
      echo "[bundle-a-interpretations] Failed prompt: ${rel_prompt_dir}" >&2
      echo "[bundle-a-interpretations] See log: ${log_file}" >&2
      continue
    fi

    echo "[bundle-a-interpretations] Wrote: ${output_file}"
    last_output_file="${output_file}"
    last_output_rel_dir="${rel_prompt_dir}"
  done

  if [[ ${prompts_run} -eq 0 ]]; then
    status=1
    echo "[bundle-a-interpretations] No prompts matched for talent: ${talent_query}" >&2
    continue
  fi

  if [[ ${prompt_failed} -eq 0 ]]; then
    echo "[bundle-a-interpretations] Completed ${prompts_run} prompt(s) for ${talent_folder_name}."
  else
    echo "[bundle-a-interpretations] Completed ${prompts_run} prompt(s) with ${prompt_failed} failure(s) for ${talent_folder_name}." >&2
  fi
done

exit "${status}"
