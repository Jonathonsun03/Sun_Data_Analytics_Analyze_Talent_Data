# ENA Precoding

Use one command from this folder:

```bash
bin/linux/ena_precoding/run_qualitative_coding_batch.sh
```

Everything else lives in `internal/` and is an implementation detail.

## Main Runner

The runner does the Batch API workflow. For normal production use, run it in
`--mode full` so it builds, submits, waits, retrieves, and exports the review
CSV in one command.

1. Builds or refreshes prepared transcript CSVs under the talent folder.
2. Compiles the selected qualitative codebook into JSON.
3. Builds `batch_input.jsonl` for the OpenAI Batch API.
4. Creates one organized run folder under `qualitative_batch_runs`.
5. Submits/checks/retrieves/exports through runner modes.
6. Applies reviewed codes back into the talent-library prepared CSVs when you explicitly run `--mode apply --execute`.

Example:

```bash
cd /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data

bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
  --mode full \
  --execute \
  --run-id "batch_ava_k84_150row_test" \
  -- \
  --talent-query "Avaritia" \
  --transcript "k84ImiUcjbE" \
  --limit 1 \
  --row-limit 150
```

If `--run-id` is omitted, the runner creates a timestamped folder.

## Output Layout

Each run gets one folder here:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id>/
```

Folder layout:

```text
<run_id>/
  README.md
  candidate_rows.csv
  run_code_set.csv
  batch_input.jsonl
  compiled_codebook.json
  manifest.json
  cost_estimate.json
  qa_summary.md
  coding_review.csv             # after retrieval/export
  apply_patch_preview.csv        # after apply dry-run/execute
  applied_patch_log.csv          # after apply execute
  logs/
    build_stdout.log
    prepare_stdout.log
  metadata/
    command.sh
    prepared_transcripts_index.csv
  backups/
    *.before_apply.csv           # after apply execute
```

Each build also writes a central copy of the exact selected code set here:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/run_code_sets/<run_id>/run_code_set.csv
```

The actual prepared transcript CSV is still edited in place under:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<Talent Name>/qualitative coding/monetary conversation codes/
```

## Common Options

Options before `--` belong to the wrapper:

- `--mode build|full|submit|check|export-review|direct-test`: workflow stage. Default is `build`.
- `--mode apply`: dry-run or execute the save-back step into the talent library.
- `--run-id VALUE`: name the run folder.
- `--run-root PATH`: override the `qualitative_batch_runs` root.
- `--run-dir PATH`: existing run folder for `submit`, `check`, and `export-review`.
- `--execute`: actually submit in `submit` or `full` mode.
- `--retrieve-output`: download output/error files in `check` mode.
- `--poll-interval N`: seconds between `full` mode status checks.
- `--max-wait N`: maximum seconds `full` mode waits for completion.
- `--skip-prepare`: skip the prepared-transcript build step.
- `--allow-chat-codes`: apply mode only; allow positive codes on `source=chat` rows.
- `--zero-chat-codes`: apply mode only; force `source=chat` rows to zero before writing.

Options after `--` are passed to the coding stage:

- `--talent-query VALUE`: flexible talent selector such as `Avaritia`.
- `--transcript VALUE`: full path, exact basename, partial basename, or video id.
- `--selected-transcripts-csv PATH`: manifest CSV with a `source_path` column; prepares and builds exactly those streams, including multiple talents.
- `--limit N`: maximum number of prepared transcript CSVs to code.
- `--row-limit N`: maximum number of pending rows per selected CSV.
- `--coding-folder VALUE`: folder under `qualitative coding`; default is `monetary conversation codes`.
- `--codebook VALUE`: `current`, `latest_snapshot`, `snapshot:<filename>`, or `path:<path>`.
- `--dry-run`: resolve targets without editing CSV files.
- `--reprocess`: allow already coded rows/files to be recoded.
- `--model VALUE`: pass a Codex model override.

## Internal Tools

## Manifest Runs

For selected stream sets, use a manifest CSV with `source_path` pointing at the
raw `text_playback` CSVs:

```bash
bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
  --mode build \
  --run-id "money_chat_40stream_$(date +%Y%m%d_%H%M%S)" \
  -- \
  --selected-transcripts-csv "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/qualitative_coding/selected_transcripts/20260522_223950_money_chat_quantile_selection/selected_transcripts.csv" \
  --codebook "path:/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/library/selections/<selection>/<selected_codebook>.csv" \
  --coding-folder "monetary conversation codes" \
  --row-limit 0 \
  --batch-size 50 \
  --context-rows 4 \
  --model gpt-5-mini
```

The build step prepares only the manifest rows, then writes one Batch API run
folder. It does not submit unless you use `--mode full --execute` or submit the
run folder later.

### Chat Monetary Growth 40-Stream Run

The 40 selected streams used for the chat monetary growth ENA workflow live here:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/qualitative_coding/selected_transcripts/20260522_223950_money_chat_quantile_selection/
```

For Batch API work, that selection was split into four 10-transcript manifests:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/qualitative_coding/selected_transcripts/20260522_223950_money_chat_quantile_selection/chat_monetary_growth_batches/batch_001_selected_transcripts.csv
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/qualitative_coding/selected_transcripts/20260522_223950_money_chat_quantile_selection/chat_monetary_growth_batches/batch_002_selected_transcripts.csv
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/qualitative_coding/selected_transcripts/20260522_223950_money_chat_quantile_selection/chat_monetary_growth_batches/batch_003_selected_transcripts.csv
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/qualitative_coding/selected_transcripts/20260522_223950_money_chat_quantile_selection/chat_monetary_growth_batches/batch_004_selected_transcripts.csv
```

The current recommended codebook is:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/library/selections/chat_monetary_growth/batch_002/batch_002_codebook.csv
```

Use one tmux session per batch and run only one or two large batches at a time.
These transcript batches can be very large: 10 transcripts expanded into roughly
30k-55k candidate rows during the first run. `--batch-size 50` chunks the rows
into 50-row OpenAI requests; it does not limit the total rows coded.

Example for batch 003:

```bash
tmux new-session -d -s chat_monetary_growth_batch_003_retry \
'cd /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data && \
bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
  --mode full \
  --execute \
  --run-id "chat_monetary_growth_batch_003_retry_$(date +%Y%m%d_%H%M%S)" \
  --poll-interval 300 \
  --max-wait 90000 \
  -- \
  --selected-transcripts-csv "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/qualitative_coding/selected_transcripts/20260522_223950_money_chat_quantile_selection/chat_monetary_growth_batches/batch_003_selected_transcripts.csv" \
  --codebook "path:/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebooks/library/selections/chat_monetary_growth/batch_002/batch_002_codebook.csv" \
  --coding-folder "chat monetary growth ena codes" \
  --row-limit 0 \
  --batch-size 50 \
  --context-rows 4 \
  --model gpt-5-mini'
```

If OpenAI rejects a run with `token_limit_exceeded`, that batch did not process
any rows. Confirm this in `batch_status.json`: rejected jobs have
`request_counts.total = 0`, no `output_file_id`, no `batch_output.jsonl`, and no
`coding_review.csv`. Retry later with a fresh run id after other in-progress
batches complete.

## Saving Coded Datasets

After `full` mode completes, review the top-level `coding_review.csv` in the
run folder. Then run apply mode once without `--execute`; this writes
`apply_patch_preview.csv` and `apply_summary.json` in the run folder without
touching the talent-library CSVs:

```bash
bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
  --mode apply \
  --run-dir "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id>"
```

When the preview looks right, apply it:

```bash
bin/linux/ena_precoding/run_qualitative_coding_batch.sh \
  --mode apply \
  --run-dir "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id>" \
  --execute
```

Apply mode writes the code columns back to the prepared transcript CSVs under
the talent's `qualitative coding/monetary conversation codes/` folder and saves
backups inside the run folder. By default it blocks rows with missing responses,
unknown codes, validation errors, or positive codes on `source=chat` rows. If an
older run coded chat rows but you want to keep only streamer rows, add
`--zero-chat-codes` to the dry-run and execute commands.

The scripts in `internal/` are lower-level helpers retained for debugging,
legacy batch experiments, or retry workflows. For normal work, do not call them
directly.

`--mode direct-test` is retained only for small immediate Codex test runs. It is
not the normal Batch API production path.
