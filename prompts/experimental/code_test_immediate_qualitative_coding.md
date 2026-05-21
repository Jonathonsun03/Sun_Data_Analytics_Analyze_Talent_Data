You are working in the Sun Data Analytics qualitative coding pipeline.

Goal:
Run a small immediate qualitative coding test named `code_test`.

Use the selected `code_test` codebook:

/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_code_library/selections/code_test/code_test_codebook.csv

The final output should be the updated prepared transcript CSV. Do not create a separate JSON output file as the final product.

Important architecture:
The script should update the CSV dataframe directly. The LLM should not rewrite the full transcript CSV. The LLM should only return or use a temporary CSV-formatted coding patch containing `row_id` and the supplied `code_*` values. The script should parse that patch, update the existing `code_*` columns, validate the coding columns, enforce hierarchy, and save the coded CSV.

Run boundary:
Do not create or modify scripts, wrappers, helpers, or pipeline infrastructure during this coding run. Directly identify the selected prepared CSV file, inspect the selected codebook, code the requested row limit, validate the edits, and report the path edited.

Path resolution boundary:
Do not search all of `/mnt/datalake`. Resolve talent folders only under `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data` unless the user provides an explicit full path. Prepared transcript CSVs live under each talent folder at `qualitative coding/<coding folder>`. When a transcript selector is provided, search only inside the resolved prepared transcript folder for that talent.

Context rule:
For each row being coded, the current row is the target. Prior rows may be provided as context. Do not use later rows unless the pipeline is explicitly changed to allow lookahead.

Code applicability rule for this combined test:
- Streamer-side codes apply only to streamer-visible rows, normally `source = subtitle`.
- Chat-side codes apply only to chat rows, normally `source = chat`.
- Use previous context rows only to interpret the target row.
- Do not assign streamer-side codes to chat rows.
- Do not assign chat-side codes to subtitle rows.

Streamer-side codes in this test:
- `code_A1`
- `code_A1a`
- `code_A1b`
- `code_E1`
- `code_E1d`

Chat-side codes in this test:
- `code_CP03`
- `code_CP02`
- `code_CP10`
- `code_CP13`
- `code_CP06`
- `code_CP14`

Hierarchy rule:
- If `code_A1a` or `code_A1b` is 1, `code_A1` must also be 1.
- If `code_E1d` is 1, `code_E1` must also be 1.

Conservative coding rule:
Assign 1 only when the current row clearly fits the code definition. If the row only vaguely or indirectly relates to a definition, assign 0.

Completion:
Report the CSV path edited, the row count coded, and any code columns added.
