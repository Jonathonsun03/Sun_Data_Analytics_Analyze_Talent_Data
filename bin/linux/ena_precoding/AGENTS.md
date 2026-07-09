# AGENTS.md

## Scope
- This guide applies to the qualitative ENA precoding and Batch API workflow in `bin/linux/ena_precoding/`.
- Use `bin/linux/ena_precoding/run_qualitative_coding_batch.sh` as the main entrypoint.
- Treat scripts in `bin/linux/ena_precoding/internal/` as implementation details unless debugging a failed stage.

## Data Safety
- Read selected transcript manifests and codebooks from `/mnt/datalake/`.
- Write run outputs under `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/`.
- Do not edit raw `text_playback` CSVs.
- Prepared qualitative coding CSVs live under `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<Talent>/qualitative coding/<coding folder>/`.
- Apply mode writes back to prepared qualitative coding CSVs only after an explicit `--mode apply --execute`.

## Normal Batch Flow
- Prefer `--mode full --execute` in tmux for long runs.
- Use a stable `--run-id` prefix that identifies the batch and retry number.
- Put wrapper options before `--`; put build options after `--`.
- For selected multi-talent stream sets, always use `--selected-transcripts-csv` with a CSV containing `source_path`.
- Use `--row-limit 0` only when the user really wants every pending row in each selected transcript.

## Codebook Rules
- Use explicit `path:<absolute path>` codebook selectors for reproducible runs.
- Do not rely on `analytic_object` metadata to steer coding.
- Code applicability should come from each code definition. `row_source_scope` should be treated as a hard source restriction only if explicitly populated.
- For the chat monetary growth run, the current recommended codebook is:
  `/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/library/selections/chat_monetary_growth/batch_002/batch_002_codebook.csv`

## Token Queue Limits
- A selected transcript batch can expand from 10 transcript files into tens of thousands of candidate rows.
- `--batch-size 50` means 50 target rows per OpenAI request, not 50 rows total.
- The codebook and context rows are repeated in every Batch API request, so token use grows quickly.
- OpenAI may reject jobs with `token_limit_exceeded` before any row is processed.
- A failed Batch API admission has `request_counts.total = 0`, no `output_file_id`, no `batch_output.jsonl`, and no `coding_review.csv`.
- If one batch is already `in_progress`, wait for it to complete before submitting another large batch.

## Status Interpretation
- `batch_status.json` with `status=completed` and an `output_file_id` means the OpenAI job ran.
- `batch_output.jsonl` means output was retrieved.
- `coding_review.csv` means retrieval was exported into a review sheet.
- Missing `coding_review.csv` can mean either not retrieved yet or never ran; inspect `batch_status.json`.
- Tmux sessions disappearing is normal after completion or failure; inspect the run folder afterward.

## Apply Step
- Review `coding_review.csv` before applying.
- Run apply once without `--execute` to produce a preview.
- Use `--allow-chat-codes` when the selected codebook intentionally codes chat rows.
- Use `--zero-chat-codes` only when intentionally stripping chat-row positives from older runs.
