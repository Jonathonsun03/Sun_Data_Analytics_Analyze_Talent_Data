You are working in the Sun Data Analytics qualitative coding pipeline.

Goal:
Update prepared transcript CSV files directly by filling their existing `code_*` columns with qualitative coding values.

Do not create a separate JSON output file as the final product. The final product should be the updated prepared transcript CSV.

Input paths:

Codebook root:
Z:\DataLake\Sun_Data_Analytics\Processed\Talent_Data\Qualitative Codebooks\concept_areas\definitions\monetary_personality

Default current codebook:
Z:\DataLake\Sun_Data_Analytics\Processed\Talent_Data\Qualitative Codebooks\concept_areas\definitions\monetary_personality\current\personality_qualitative_code_log.csv

Snapshot folder:
Z:\DataLake\Sun_Data_Analytics\Processed\Talent_Data\Qualitative Codebooks\concept_areas\definitions\monetary_personality\snapshots

Transcript folder example:
Z:\DataLake\Sun_Data_Analytics\Talent_data\Avaritia Hawthorne 【Variance Project】\qualitative coding\monetary conversation codes

The codebook CSV contains:
- Primary Code ID
- Primary Code
- Secondary Code ID
- Secondary Code
- Definition
- Date added
- Examples from text

The transcript CSV contains metadata columns such as:
- source_file
- source_path
- video_id
- sec
- source
- speaker
- text
- message_type
- paid_amount_text
- paid_amount_value
- paid_currency
- timecode
- replay_line

The transcript CSV also contains coding columns beginning with `code_`. These are the target columns and are initialized as NA before coding.

Codebook selection:
The pipeline should accept a codebook selector slug.

Supported selectors:
- `current`: use `current/personality_qualitative_code_log.csv`
- `latest_snapshot`: use the most recently modified file in `snapshots`
- `snapshot:<filename>`: use a specific snapshot filename under `snapshots`
- `path:<path>`: use an explicit codebook path

The log must record the resolved codebook path used for the run.

Codebook-to-column mapping:
- If `Secondary Code ID` is empty or missing, map the row to `code_<Primary Code ID>`.
  Example: `A1` becomes `code_A1`.
- If `Secondary Code ID` is present, map the row to `code_<Secondary Code ID>`.
  Example: `A1a` becomes `code_A1a`.

Processing requirements:

1. Load the selected qualitative codebook using the codebook selector.

2. Load the selected transcript CSV.

3. Determine the expected coding columns from the codebook.

4. Check whether each expected `code_*` column exists in the transcript CSV.
   - If a code column exists, update it.
   - If a code column is missing, create it and initialize it as NA before coding.
   - Preserve all original metadata columns.
   - Preserve the original column order when possible, appending newly created code columns at the end.

5. Identify rows that need coding.
   - By default, code only rows where one or more expected `code_*` columns are NA.
   - Allow a force/reprocess option that recodes all rows.
   - Do not overwrite already-coded values unless force/reprocess is enabled.

6. For each transcript row needing coding, code the current row’s `speaker` and `text`.
   - Use up to 4 prior rows as context.
   - Example: when coding row 12, provide rows 8-11 as prior context and row 12 as the target row.
   - Use context only to clarify the current row.
   - Do not assign a code to the current row only because prior rows match the definition.
   - Do not use future rows by default.

7. For each expected `code_*` column, write:
   - `1` if the current row clearly aligns with the code definition.
   - `0` if the current row does not clearly align with the code definition.

8. Enforce hierarchy after receiving the LLM result:
   - If a secondary code is 1, set the corresponding primary code to 1 if that primary column exists.
   - Example: if `code_A1a == 1`, then `code_A1 = 1`.

9. Validate the result before writing:
   - Every expected `code_*` column must contain only 0 or 1.
   - No expected `code_*` column should contain NA after coding.
   - Non-code columns must remain unchanged.
   - Row count must remain unchanged.
   - Column names must remain unchanged except for any newly added missing `code_*` columns.

10. Save behavior:
   - The intended production behavior is to save direct edits to the prepared transcript CSV.
   - For smoke tests, allow a no-overwrite mode that writes a copy with `_coded` added before the extension.
   - Provide a command-line flag or option for in-place saving versus smoke-test copy saving.

11. Logging:
   Save or print a clear log showing:
   - codebook selector requested
   - codebook file used
   - transcript file processed
   - output file written
   - number of rows coded
   - number of code columns updated
   - missing code columns created
   - transcript code columns not found in the codebook
   - failed LLM calls
   - invalid responses and retries

Important:
The existing prepared CSV is the working sheet. The `code_*` columns are the target cells. The LLM should return only a temporary `row_id`/`code_*` patch, and the script should apply that patch to the sheet.
