You are working in the Sun Data Analytics qualitative coding pipeline.

Goal:
Perform a qualitative coding run that fills existing qualitative coding columns in prepared transcript CSV files.

Prepared transcript CSVs already contain metadata columns, one column per qualitative code named `code_*`, and `code_*` values initialized as `NA`.

The final output should be the updated transcript CSV sheet. Do not create a separate JSON output file.

Important architecture:
The script should update the CSV dataframe directly. The LLM should not rewrite the full transcript CSV. The LLM should only return a temporary CSV-formatted coding patch containing `row_id` and `code_*` values. The script should parse that patch, update the existing `code_*` columns, validate the coding columns, enforce hierarchy, and save the coded CSV.

Run boundary:
Do not create or modify scripts, wrappers, helpers, or pipeline infrastructure during a coding run unless the user explicitly asks for implementation changes. For normal runs, directly identify the selected prepared CSV file(s), inspect the selected codebook, fill the existing `code_*` columns in those CSV files, validate the edits, and report the path(s) edited. If dry-run mode is requested, only identify and report the selected CSV file(s); do not edit CSV files or repository files.

Path resolution boundary:
Do not search all of `/mnt/datalake`. Resolve talent folders only under `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data` unless the user provides an explicit full path. Prepared transcript CSVs live under each talent folder at `qualitative coding/<coding folder>`. When a transcript selector is provided, search only inside the resolved prepared transcript folder for that talent.

Runtime tooling boundary:
For dry runs, use simple shell commands and existing project helpers only. Do not call `python`, do not use pandas, and do not probe for optional dependencies. If a row/column count is needed, use `Rscript` with base R/readr already used by this repository, or use shell tools such as `head`, `wc`, and `awk`. Keep dry runs read-only and minimal.

Context rule:
For each row being coded, the current row is the target. Prior rows may be provided as context. For example, when coding row 12, use row 12 as the target and rows 8-11 as context. Do not use later rows unless the pipeline is explicitly changed to allow lookahead.

Codebook selection:
The pipeline should support a codebook selector such as `current`, `latest_snapshot`, or a specific snapshot slug/path.

/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/prompts/qualitative_coding/monetary_conversation/qualitative_coding_pipeline.md

LLM CODING PROMPT TEMPLATE:
/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/prompts/qualitative_coding/monetary_conversation/qualitative_coding_prompt.md
