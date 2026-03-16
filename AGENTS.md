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
- Put reusable helper functions in `Scripts/lib/`.
- Put task-specific scripts in `Scripts/tasks/`.
- Keep prompt experiments in `prompts/experimental/` unless told otherwise.

## Safety
- Before making changes, explain the plan briefly.
- Prefer small, high-confidence edits.
- Do not move or rename major files unless explicitly asked.

## Generated analysis file policy
- Treat previously generated analysis outputs as read-only unless the current task explicitly targets them.
- Do not use generated outputs as primary evidence for new analysis unless the user explicitly requests a comparison or audit.
- When a task defines completion by output files, check for those files first and skip completed units of work unless asked to rerun them.
