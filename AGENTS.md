# AGENTS.md

## Repository role
- This repository is the main code-writing workspace.
- Keep project-specific prompts in `prompts/` within this repository.

## Data locations
- Read raw and intermediate data from `/mnt/datalake/`.
- Write non-code outputs, reports, and rendered artifacts to `/mnt/datalake/`.

## Raw data protection
- Raw data in this repository and in mounted data directories is read-only unless explicitly told otherwise.
- Do not modify, overwrite, clean, reformat, or delete raw data files unless the user specifically requests it.
- You may read and inspect raw data to understand structure and support analysis.
- If a transformation is needed, write derived files to an output, staging, or processed-data location instead of changing the raw source.

## File creation rules
- Do not create new scripts in the DataLake.
- Do not create files in the repository root.
- Do not create new ad hoc script directories such as additional `r_scripts/`, `scripts/`, `Scripts/`, or task-specific script folders.
- Repository R scripts live under `r_scripts/`.
- New Python scripts belong under `py_scripts/`.
- Put reusable Python helper functions and shared modules in `py_scripts/lib/`.
- Put runnable Python entrypoint scripts in `py_scripts/run/`.
- Put reusable, language-neutral SQL query files in `sql_queries/`.
- Keep Python and R query loaders in their respective `lib/` directories; do not store SQL queries under `prompts/`.
- Do not create new Python scripts in legacy locations such as `r_scripts/`, `Scripts/tasks/`, `Scripts/lib/`, or other non-`py_scripts/` folders.
- Before creating a new Python script, check whether an existing script or helper can be extended or reused.
- Keep prompt experiments in `prompts/experimental/` unless told otherwise.

## Safety
- Before making changes, explain the plan briefly.
- Prefer small, high-confidence edits.
- Do not move or rename major files unless explicitly asked.

## Required context before editing
- Before changing files, read the repository-level `AGENTS.md` and any more specific `AGENTS.md` files in the target path.
- Before editing a file, look for relevant READMEs from the repository root down to the file's directory. Read the nearest README first, then any parent READMEs that explain the subsystem.
- For work under `r_scripts/`, read `r_scripts/README.md` before making changes. Also read any README in the target subdirectory or its nearest documented parent, such as `r_scripts/lib/ENA_prep/README.md`.
- For work under `py_scripts/`, read `py_scripts/README.md` before making changes. Also read the nearest README under the target path when one exists.
- For work under `sql_queries/`, read `sql_queries/README.md` before adding or editing SQL queries.
- For work under `prompts/`, read `prompts/README.md` and the nearest prompt-family README before editing prompt text.
- If no README exists for the target area, inspect neighboring files to infer the local pattern before editing.
- In the work summary, mention which README or local context files were consulted when the task changes code, prompts, or analysis workflow.

## R style guide
- Use the tidyverse style guide and Google R style guide as the default style references, with local consistency taking precedence when a file already has a clear pattern.
- Prefer readable one-line function calls when they fit comfortably, especially for simple calls such as `source(here::here("r_scripts", "lib", "utils", "source_dir.r"))`.
- Break function calls across lines when a one-line call would be hard to scan, when arguments have meaningful names, or when the line would become long.
- When breaking calls across lines, put one argument per line, align by indentation rather than manual spacing, and keep the closing parenthesis on its own line.
- Use two-space indentation for R code.
- Put spaces around infix operators such as `<-`, `=`, `%>%`, `==`, `+`, and `-`.
- Prefer `<-` for assignment.
- Use explicit namespaces for non-core helper calls in shared utilities when that makes dependencies clearer, for example `purrr::walk()` in a utility that may be sourced before `library(purrr)`.
- Keep library imports and source statements near the top of runnable scripts, ordered from general setup to project helpers.
- Prefer small, named helper functions over long ad hoc blocks in notebooks or runnable scripts when logic is reused.
- Avoid broad restyling in unrelated files. When touching messy code, improve the formatting only around the lines needed for the current task unless the user asks for a larger cleanup.

## R formatting and checks
- Before committing substantial R edits, parse the touched R files with `Rscript` or run the smallest relevant script-level check.
- Use `styler` only when it is already available in the project environment or the user asks to install/configure it.
- Do not run a whole-project formatter unless the user explicitly requests it; prefer scoped formatting of touched files or touched regions.
- When adding shared R helpers, place them in the relevant `r_scripts/lib/` subdirectory and source them from runnable scripts instead of duplicating helper definitions.

## Generated analysis file policy
- Treat previously generated analysis outputs as read-only unless the current task explicitly targets them.
- Do not use generated outputs as primary evidence for new analysis unless the user explicitly requests a comparison or audit.
- When a task defines completion by output files, check for those files first and skip completed units of work unless asked to rerun them.
