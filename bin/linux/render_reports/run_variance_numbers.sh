#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

if [[ -f "${REPO_ROOT}/bin/linux/load_repo_env.sh" ]]; then
  # shellcheck source=/dev/null
  source "${REPO_ROOT}/bin/linux/load_repo_env.sh"
  load_repo_env "${REPO_ROOT}"
fi

QUARTO_BIN="${QUARTO_BIN:-quarto}"
RSCRIPT_BIN="${RSCRIPT_BIN:-Rscript}"
NOTEBOOK="${REPO_ROOT}/r_scripts/notebooks/variance_numbers/variance_numbers.qmd"
DATA_ROOT="${TALENT_DATALAKE_ROOT:-${TALENT_DATA_ROOT:-}}"
DRY_RUN="false"

declare -a TALENTS=()

usage() {
  cat <<'USAGE'
Usage:
  bin/linux/render_reports/run_variance_numbers.sh [options]

Description:
  Renders the Variance Numbers notebook and writes each self-contained HTML to:
    <data-root>/<talent>/reports/variance_numbers/

  With no --talent options, renders Katya, Leia, and Avaritia.

Options:
  --talent NAME      Render one talent. May be repeated.
  --data-root PATH   Override TALENT_DATALAKE_ROOT / TALENT_DATA_ROOT.
  --dry-run          Validate inputs and print planned outputs without rendering.
  -h, --help         Show this help.
USAGE
}

slugify() {
  local value="$1"
  local slug
  slug="$(printf '%s' "${value}" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/_/g; s/^_+//; s/_+$//')"
  [[ -n "${slug}" ]] || slug="talent"
  printf '%s' "${slug}"
}

check_r_dependencies() {
  local required_csv
  required_csv="rmarkdown,knitr,dplyr,kableExtra,lubridate,purrr,readr,stringr"

  if ! (
    cd "${REPO_ROOT}"
    VARIANCE_REQUIRED_PACKAGES="${required_csv}" "${RSCRIPT_BIN}" -e '
      required <- strsplit(
        Sys.getenv("VARIANCE_REQUIRED_PACKAGES"),
        ",",
        fixed = TRUE
      )[[1]]
      missing <- required[
        !vapply(required, requireNamespace, logical(1), quietly = TRUE)
      ]
      if (length(missing) > 0) {
        cat(
          "Missing required R packages: ",
          paste(missing, collapse = ", "),
          "\n",
          sep = ""
        )
        quit(status = 2)
      }
    '
  ); then
    cat >&2 <<'INSTALL_HELP'

Install the report dependencies from the repository root with:

Rscript -e 'renv::install(
  c(
    "rmarkdown", "knitr", "dplyr", "kableExtra",
    "lubridate", "purrr", "readr", "stringr"
  ),
  project = getwd()
)'
INSTALL_HELP
    return 1
  fi
}

render_talent() {
  local talent="$1"
  local talent_dir output_dir output_file stage_dir staged_notebook slug

  talent_dir="${DATA_ROOT}/${talent}"
  if [[ ! -d "${talent_dir}" ]]; then
    echo "Error: talent DataLake folder not found: ${talent_dir}" >&2
    return 1
  fi

  slug="$(slugify "${talent}")"
  output_dir="${talent_dir}/reports/variance_numbers"
  output_file="variance_numbers_${slug}.html"

  if [[ "${DRY_RUN}" == "true" ]]; then
    echo "Would render: ${talent}"
    echo "  Output: ${output_dir}/${output_file}"
    return 0
  fi

  stage_dir="$(mktemp -d "${TMPDIR:-/tmp}/variance-numbers.XXXXXX")"
  staged_notebook="${stage_dir}/variance_numbers.qmd"
  cp "${NOTEBOOK}" "${staged_notebook}"
  cp "${REPO_ROOT}/.Rprofile" "${stage_dir}/.Rprofile"
  ln -s "${REPO_ROOT}/renv" "${stage_dir}/renv"
  mkdir -p "${output_dir}"

  echo "Rendering Variance Numbers for: ${talent}"
  if (
    cd "${stage_dir}"
    RENV_PROJECT="${REPO_ROOT}" "${QUARTO_BIN}" render "variance_numbers.qmd" \
      --to html \
      -P "talent:${talent}" \
      -P "data_source:datalake" \
      -P "data_root:${DATA_ROOT}" \
      --output "${output_file}"
  ); then
    mv "${stage_dir}/${output_file}" "${output_dir}/${output_file}"
    rm -rf "${stage_dir}"
    echo "Saved: ${output_dir}/${output_file}"
    return 0
  fi

  echo "Error: render failed for ${talent}." >&2
  echo "Temporary render files retained at: ${stage_dir}" >&2
  return 1
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --talent)
      [[ $# -ge 2 ]] || { echo "Error: --talent requires a value." >&2; exit 1; }
      TALENTS+=("$2")
      shift 2
      ;;
    --data-root)
      [[ $# -ge 2 ]] || { echo "Error: --data-root requires a value." >&2; exit 1; }
      DATA_ROOT="$2"
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

if [[ ${#TALENTS[@]} -eq 0 ]]; then
  TALENTS=(
    "Katya Sable 【Variance Project】"
    "Leia Memoria【Variance Project】"
    "Avaritia Hawthorne 【Variance Project】"
  )
fi

if [[ ! -f "${NOTEBOOK}" ]]; then
  echo "Error: notebook not found: ${NOTEBOOK}" >&2
  exit 1
fi
if [[ -z "${DATA_ROOT}" || ! -d "${DATA_ROOT}" ]]; then
  echo "Error: DataLake root is missing or invalid: ${DATA_ROOT:-<unset>}" >&2
  exit 1
fi
if ! command -v "${QUARTO_BIN}" >/dev/null 2>&1; then
  echo "Error: Quarto executable not found: ${QUARTO_BIN}" >&2
  exit 1
fi
if ! command -v "${RSCRIPT_BIN}" >/dev/null 2>&1; then
  echo "Error: Rscript executable not found: ${RSCRIPT_BIN}" >&2
  exit 1
fi

export TALENT_REPO_ROOT="${REPO_ROOT}"
export TALENT_DATALAKE_ROOT="${DATA_ROOT}"

if [[ "${DRY_RUN}" != "true" ]]; then
  check_r_dependencies
fi

failures=0
for talent in "${TALENTS[@]}"; do
  if ! render_talent "${talent}"; then
    failures=$((failures + 1))
  fi
done

if [[ ${failures} -gt 0 ]]; then
  echo "Variance Numbers completed with ${failures} failed render(s)." >&2
  exit 1
fi

echo "Variance Numbers rendering complete."
