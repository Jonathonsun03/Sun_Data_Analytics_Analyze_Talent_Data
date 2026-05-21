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
- `--limit N`: maximum number of prepared transcript CSVs to code.
- `--row-limit N`: maximum number of pending rows per selected CSV.
- `--coding-folder VALUE`: folder under `qualitative coding`; default is `monetary conversation codes`.
- `--codebook VALUE`: `current`, `latest_snapshot`, `snapshot:<filename>`, or `path:<path>`.
- `--dry-run`: resolve targets without editing CSV files.
- `--reprocess`: allow already coded rows/files to be recoded.
- `--model VALUE`: pass a Codex model override.

## Internal Tools

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
