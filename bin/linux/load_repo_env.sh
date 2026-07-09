#!/usr/bin/env bash

load_repo_env() {
  local repo_root="${1:-}"
  local start_dir env_file raw_line line key value

  if [[ -z "${repo_root}" ]]; then
    start_dir="$(pwd)"
    repo_root="${start_dir}"
    while [[ "${repo_root}" != "/" ]]; do
      if [[ -f "${repo_root}/AGENTS.md" && -d "${repo_root}/r_scripts" ]]; then
        break
      fi
      repo_root="$(dirname "${repo_root}")"
    done
  fi

  env_file="${repo_root}/.env"
  [[ -f "${env_file}" ]] || return 0

  while IFS= read -r raw_line || [[ -n "${raw_line}" ]]; do
    line="${raw_line#"${raw_line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    [[ -z "${line}" || "${line}" == \#* || "${line}" != *=* ]] && continue

    key="${line%%=*}"
    value="${line#*=}"
    key="${key#"${key%%[![:space:]]*}"}"
    key="${key%"${key##*[![:space:]]}"}"
    key="${key#export }"
    value="${value#"${value%%[![:space:]]*}"}"
    value="${value%"${value##*[![:space:]]}"}"
    value="${value%\"}"
    value="${value#\"}"
    value="${value%\'}"
    value="${value#\'}"

    [[ "${key}" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] || continue
    [[ -z "${!key+x}" ]] && export "${key}=${value}"
  done < "${env_file}"

  return 0
}
