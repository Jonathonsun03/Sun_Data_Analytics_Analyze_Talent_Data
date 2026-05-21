# Batch API Qualitative Coding Pipeline Plan

Date: 2026-05-10

Status: planning only. Do not implement from this note without a separate implementation task.

## Non-Goals For This Planning Pass

- Do not modify the current transcript prep pipeline.
- Do not modify the current Codex smoke-test coding wrapper.
- Do not submit OpenAI API or Batch API jobs.
- Do not edit prepared transcript CSVs.
- Do not change raw transcript or DataLake source files.

## Current Pipeline Summary

### Transcript Prep Stage

Current prep entrypoint:

```text
r_scripts/run/transcript_analysis/prepare_transcript.r
bin/linux/ena_precoding/prepare_transcript.sh
```

The prep stage:

- Resolves a talent by flexible query using the repository talent helpers.
- Resolves the talent DataLake root with `get_datalake_root()`.
- Resolves the talent's source transcript folder:

```text
<talent folder>/text_playback
```

- Resolves or creates the prepared qualitative coding folder:

```text
<talent folder>/qualitative coding/monetary conversation codes
```

- Reads the current qualitative codebook from:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current/personality_qualitative_code_log.csv
```

- Derives one `code_*` column per code ID.
- Copies each source transcript CSV into the prepared qualitative coding folder.
- Adds `source_file` and `source_path` metadata columns.
- Adds all `code_*` columns initialized as `NA`.
- Skips prepared files that already exist unless reprocessing is requested.

The prepared CSV structure should be preserved. It is the working sheet for downstream coding.

### Current Path And Codebook Helpers

Current helper file:

```text
r_scripts/lib/clean_data/qualitative_data_prep/qualitative_data_prep.r
```

Relevant helper behavior:

- `resolve_talent_text_playback()` resolves talent folders and their `text_playback` directories.
- `resolve_talent_qualitative_prep_dir()` resolves:

```text
<talent folder>/qualitative coding/<coding folder>
```

- `resolve_qualitative_codebook()` supports current and snapshot codebook resolution.
- `list_qualitative_codebook_snapshots()` lists codebook snapshots.

These helpers should be reused by a future Batch API pipeline rather than duplicating path logic.

### Current Direct Codex Coding Stage

Current Codex wrapper:

```text
bin/linux/ena_precoding/code_qualitative_transcripts.sh
```

Current prompt folder:

```text
prompts/qualitative_coding/monetary_conversation
```

The direct Codex coding wrapper:

- Runs `codex exec`.
- Feeds in `prompt_wrapper.md`.
- Appends run scope such as talent query, transcript selector, transcript limit, row limit, codebook selector, dry-run, and reprocess mode.
- Directs Codex to edit prepared CSVs in place for small smoke tests.
- Supports safe targeting of one transcript and row-limited test runs.

The successful first live test:

- Targeted Avaritia transcript `k84ImiUcjbE`.
- Coded rows 26-50 in place.
- Updated 18 `code_*` columns.
- Left first pending row at 51.
- Produced a reasonable observed code assignment: `"Hi guys! Thanks for waiting!"` received `code_A1 = 1` and `code_A1b = 1`.

### Files And Folders Involved

Codebook folder:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current
```

Codebook snapshots:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/snapshots
```

Avaritia prepared transcript folder:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Avaritia Hawthorne 【Variance Project】/qualitative coding/monetary conversation codes
```

Prompt folder:

```text
prompts/qualitative_coding/monetary_conversation
```

Wrapper folder:

```text
bin/linux/ena_precoding
```

Planning notes folder:

```text
notes/scripting_notes/qualitative_data_prep_pipeline
```

### What Should Be Preserved

- The current transcript prep stage.
- The prepared CSV folder layout under each talent folder.
- Existing prepared CSV metadata columns.
- Existing `code_*` column naming convention.
- Codebook selector behavior: `current`, `latest_snapshot`, `snapshot:<filename>`, `path:<path>`.
- Parent/child hierarchy behavior.
- Dry-run before execute behavior.
- Path resolution boundaries that avoid searching all of `/mnt/datalake`.
- Current smoke-test Codex wrapper as an audit/debug tool.

## Problem Statement

Direct Codex CSV editing is useful for smoke tests because it can:

- Inspect the selected prepared CSV.
- Inspect the current codebook.
- Code a small row window.
- Validate that the target `code_*` columns were updated.
- Provide human-readable reasoning and diagnostics.

It is not sustainable for full Avaritia production coding.

Current observed Ava state:

```text
Prepared CSV files: 110
Total rows: 408,923
Pending rows: 408,873
Already coded rows: 50
```

The first 25-row direct Codex test used about 37,518 tokens.

Naive scaling:

```text
37,518 tokens / 25 rows = about 1,500 tokens per row
408,873 pending rows / 25 rows per run = about 16,355 runs
16,355 runs * 37,518 tokens = about 614 million tokens
```

This overstates what a tuned batch process would need, but it correctly shows the problem: direct Codex editing spends too much on repo inspection, file reads, command output, and interactive validation. It is excellent for implementation and QA, but inefficient as the production coding engine.

The production workflow should move toward compact model requests, mechanical patch application, and restartable run manifests.

## Proposed Batch API Architecture

The proposed architecture keeps the existing prep artifacts and replaces direct Codex row-by-row editing with a purpose-built Batch API workflow.

### A. Transcript Prep Remains Unchanged

Keep:

```text
bin/linux/ena_precoding/prepare_transcript.sh
r_scripts/run/transcript_analysis/prepare_transcript.r
```

Prep continues to create CSVs with:

- original metadata columns
- `source_file`
- `source_path`
- `code_*` columns initialized as `NA`

No Batch API code should alter raw `text_playback` files.

### B. Compact Codebook Compilation

Create a compiled codebook artifact per run:

```text
compiled_codebook.json
```

It should contain only fields needed for coding:

- `code_id`
- `code_column`
- `primary_code_id`
- `primary_code`
- `secondary_code_id`
- `secondary_code`
- `parent_code_id`
- `definition`
- concise examples or trigger hints

Avoid sending full historical codebook notes with every request. Prefer a compact representation that keeps definitions authoritative.

### C. Pending-Row Selection

For each prepared CSV:

- identify expected `code_*` columns from the selected codebook
- create missing expected columns as `NA` only in dry-run manifests, not in source files until execute mode
- mark rows pending when one or more expected `code_*` values are missing
- skip rows where all expected `code_*` values are already `0/1`, unless `--reprocess` is set

Selection should be deterministic:

- talent query
- coding folder
- transcript selector
- transcript limit
- row limit
- optional date or mtime window later

### D. Candidate Gating

Do not send every row to the model. Split pending rows into:

- model candidates
- likely auto-zero rows
- duplicate-reuse rows
- audit rows

The candidate gate should be configurable and recorded in `manifest.json`.

### E. Duplicate Reuse

Normalize repeated chat lines and reuse previous decisions where safe.

Exact duplicates should not repeatedly consume model tokens. Reused decisions should be written to `duplicate_reuse.csv` and applied mechanically.

### F. Batch JSONL Request Creation

Build a `.jsonl` file where each line is one request.

Recommended strategy:

- one request per compact row batch, not one request per single row
- include prior context rows for each target row
- include only the compact codebook
- include a strict structured output schema
- include a stable `custom_id`

The OpenAI Batch API uses JSONL input where each line includes a unique `custom_id`, `method`, `url`, and request `body`. Batch output order should not be trusted; results must be joined by `custom_id`.

### G. Batch API Submission

Submission should be a separate step from JSONL creation.

The submit step should:

- require `--execute`
- upload `batch_input.jsonl` with purpose `batch`
- create a batch against the chosen endpoint
- record `batch_id`, `input_file_id`, model, endpoint, and `completion_window`
- write all metadata to `manifest.json`

The official Batch API currently supports asynchronous jobs with a `24h` completion window, JSONL input, and status retrieval. Planning should assume asynchronous completion, not immediate results.

### H. Batch Status Check And Retrieval

The check step should:

- read `manifest.json`
- query the batch status
- record status changes
- download `batch_output.jsonl` when available
- download `batch_errors.jsonl` when present
- never apply results automatically

### I. Batch Result Validation

Validation should happen before any CSV write.

Validate:

- every output line has a known `custom_id`
- response JSON parses
- structured output schema parses
- every returned `row_id` belongs to the request mapped by `custom_id`
- all returned codes exist in the compiled codebook
- unknown codes are rejected or quarantined
- rows with `needs_review = true` are not silently trusted
- failed requests are captured in `batch_errors.jsonl`

### J. Mechanical Patch Application To Existing CSVs

Patch application should be deterministic and boring.

Do not ask the model to rewrite CSV files. The model should only return decisions. The local patcher updates `code_*` cells.

### L. Audit Exports And QA Summaries

Every run should export reviewable samples:

- auto-zero sample
- model-coded zero sample
- positive-coded sample
- needs-review sample
- paid-message sample

It should write a readable `qa_summary.md`.

### M. Checkpointing And Run Manifests

Every run should be restartable. The run directory should contain enough metadata to recover, audit, or reapply results.

Use a run manifest as the source of truth:

- run ID
- timestamp
- repo commit if available
- selected talent(s)
- selected transcript(s)
- codebook selector
- resolved codebook path
- candidate-gating config
- model
- batch ID
- file IDs
- row counts
- token/cost estimates
- apply status

## Candidate-Gating Plan

The goal is to reduce model calls by sending only rows likely to need human-like judgment.

### Candidate Triggers

Send a row to the model when any of these are true:

- `paid_amount_value` is present or greater than `0`
- `paid_amount_text` is present
- `paid_currency` is present
- `message_type` suggests:
  - paid chat
  - superchat
  - donation
  - membership
  - gifted membership
  - milestone
  - sticker
  - paid message
- `source` indicates streamer/subtitle/talent speech
- `speaker` indicates streamer/talent account
- row text matches codebook-derived trigger terms
- row is within a configurable context window around paid events
- row is included in a random audit sample of likely auto-zero rows

### Context Window Around Paid Events

For each paid or membership event:

- send the paid row
- send a configurable number of rows before and after, or before-only if preserving the current no-lookahead coding convention
- record why each context row became a candidate

Default proposal:

- current coding input uses up to 4 prior rows as context
- candidate gate may include rows within +/- N rows of paid events for review, but the coding prompt should still specify whether future rows may be used

### Codebook-Derived Trigger Terms

Compile trigger terms from:

- code names
- definitions
- curated examples
- manually maintained trigger list

Examples of possible trigger families:

- thank / thanks / waiting / welcome
- member / membership / gifted / superchat / donation
- raid / raiders
- pin / mods / focus / chat
- subscribe / monetization

Trigger terms should only select candidates. They should not determine final codes.

### Auto-Zero Rows

Rows not selected as candidates can be auto-filled with zeros only if:

- gating config is recorded
- an audit sample is exported
- the pipeline can be rerun with a more permissive gate
- the user explicitly allows auto-zero application

## Duplicate-Reuse Plan

Many chat rows repeat exactly or near-exactly.

### Recommended Duplicate Key

Use an exact normalized key:

```text
source | message_type | speaker_or_speaker_type | normalized_text | paid_flag
```

Where:

- `source` is the row source, such as `chat` or `subtitle`
- `message_type` is the transcript message type
- `speaker_or_speaker_type` is either speaker or a coarser speaker class if available
- `normalized_text` is lowercased, whitespace-squished, Unicode-normalized text with repeated quote artifacts normalized
- `paid_flag` is true when any paid/membership/payment field is present

Suggested normalization:

- trim whitespace
- lowercase
- collapse repeated whitespace
- normalize CSV quote artifacts
- normalize common empty/NA strings
- preserve semantic punctuation unless testing shows it should be removed

### Reuse Rules

- Reuse only exact duplicate keys in the same codebook version.
- Do not reuse across codebook versions unless explicitly allowed.
- Do not reuse if the original decision had `needs_review = true`.
- Keep `duplicate_reuse.csv` with source and destination row mappings.
- Include duplicate-reuse counts in `qa_summary.md`.

## Batch API Request And Response Design

### Request JSONL Structure

One JSONL line per compact coding batch:

```json
{
  "custom_id": "ava__k84ImiUcjbE__rows_000026_000050__run_2026-05-10_001",
  "method": "POST",
  "url": "/v1/responses",
  "body": {
    "model": "MODEL_NAME",
    "input": [
      {
        "role": "system",
        "content": "You are a qualitative coding assistant..."
      },
      {
        "role": "user",
        "content": {
          "codebook": [],
          "rows": [],
          "instructions": "Return structured coding decisions only."
        }
      }
    ]
  }
}
```

The exact endpoint and request body should follow the current OpenAI API standard selected at implementation time. The planning preference is `/v1/responses` unless a repo-level OpenAI helper requires chat completions.

### `custom_id` Design

`custom_id` should encode enough to map results without trusting output order:

```text
<talent_slug>__<video_id>__rows_<start>_<end>__<run_id>__part_<n>
```

Include the full row mapping in `manifest.json`, not only in `custom_id`.

### Output Order

Do not trust Batch API output order. Results must be joined by `custom_id`. This is explicitly part of the official Batch API guidance.

### Structured Output Schema

Recommended response object:

```json
{
  "coded_rows": [
    {
      "row_id": "string or integer",
      "codes": ["A1", "A1b"],
      "confidence": "high",
      "needs_review": true,
      "review_reason": "string or null"
    }
  ],
  "batch_notes": "string or null"
}
```

Interpretation:

- `codes` contains positive code IDs only, not `code_*` column names.
- Absent codes are treated as `0`.
- `confidence` is for audit prioritization, not automatic rejection.
- `needs_review` rows should be exported for review.
- `review_reason` should be concise.

## CSV Patching Design

Patch application should require explicit execution.

Default behavior:

- build preview only
- do not modify CSVs

Execution behavior:

- require `--execute`
- create a backup or snapshot before writing
- set submitted rows to `0` across expected code columns first
- set returned positive codes to `1`
- treat absent codes as `0`
- enforce parent/child hierarchy
- preserve row order
- preserve metadata columns
- preserve original column names except for explicitly added missing code columns
- validate all completed code cells are `0/1`
- write `applied_patch_log.csv`

### Backup Strategy

Before in-place writes, create either:

- a timestamped backup copy beside the CSV, or
- a run-level backup/snapshot folder under the run directory

Do not overwrite backups.

### Validation Before Write

Check:

- row count unchanged
- row order unchanged
- metadata columns unchanged
- all expected code columns exist
- submitted rows have only `0/1`
- no hierarchy violations remain
- no unknown codes were returned
- no duplicate row IDs in patch

## Proposed CLI Scripts

The repo already uses shell wrappers under `bin/linux/ena_precoding`. For safety and cron use, prefer separate scripts for each lifecycle step.

Recommended scripts:

```text
bin/linux/ena_precoding/build_qualitative_batch.sh
bin/linux/ena_precoding/submit_qualitative_batch.sh
bin/linux/ena_precoding/check_qualitative_batch.sh
bin/linux/ena_precoding/apply_qualitative_batch.sh
```

Alternative: one script with subcommands:

```text
bin/linux/ena_precoding/qualitative_batch.sh build
bin/linux/ena_precoding/qualitative_batch.sh submit
bin/linux/ena_precoding/qualitative_batch.sh check
bin/linux/ena_precoding/qualitative_batch.sh apply
```

Given current repo convention, separate shell wrappers are clearer and easier to cron/debug.

### Example Dry-Run Commands

Build a dry-run JSONL preview for one Ava transcript:

```sh
bin/linux/ena_precoding/build_qualitative_batch.sh \
  --talent-query "Avaritia" \
  --transcript "k84ImiUcjbE" \
  --limit 1 \
  --row-limit 25 \
  --codebook current \
  --dry-run
```

Build real batch input files, but do not submit:

```sh
bin/linux/ena_precoding/build_qualitative_batch.sh \
  --talent-query "Avaritia" \
  --transcript "k84ImiUcjbE" \
  --limit 1 \
  --row-limit 25 \
  --codebook current
```

Submit only after reviewing the run directory:

```sh
bin/linux/ena_precoding/submit_qualitative_batch.sh \
  --run-dir /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id> \
  --execute
```

Check and retrieve:

```sh
bin/linux/ena_precoding/check_qualitative_batch.sh \
  --run-dir /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id>
```

Preview patch:

```sh
bin/linux/ena_precoding/apply_qualitative_batch.sh \
  --run-dir /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id> \
  --dry-run
```

Apply patch only after review:

```sh
bin/linux/ena_precoding/apply_qualitative_batch.sh \
  --run-dir /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id> \
  --execute
```

## Run Directory Design

Recommended root:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id>
```

Recommended files:

```text
manifest.json
compiled_codebook.json
candidate_rows.csv
auto_zero_candidates.csv
duplicate_reuse.csv
batch_input.jsonl
batch_output.jsonl
batch_errors.jsonl
patch_preview.csv
applied_patch_log.csv
audit_sample.csv
cost_estimate.csv
cost_estimate.json
qa_summary.md
```

Optional folders:

```text
backups/
logs/
request_chunks/
response_chunks/
quarantine/
```

### Manifest Fields

`manifest.json` should include:

- `run_id`
- `created_at`
- `repo_root`
- `repo_commit`
- `talent_query`
- `resolved_talents`
- `coding_folder`
- `transcript_selectors`
- `codebook_selector`
- `resolved_codebook_path`
- `model`
- `endpoint`
- `candidate_gate_config`
- `context_window_config`
- `dedupe_config`
- `request_count`
- `row_count_total`
- `candidate_row_count`
- `auto_zero_row_count`
- `duplicate_reuse_count`
- `batch_id`
- `input_file_id`
- `output_file_id`
- `error_file_id`
- `apply_status`

## QA And Audit Plan

### Audit Samples

Every run should export:

- auto-zero rows
- model-coded zero rows
- positive-coded rows
- `needs_review` rows
- paid-message rows
- duplicate-reuse rows
- hierarchy-enforced rows

Sampling should include:

- random rows
- rows with money-related metadata
- rows near paid events
- rows with long text
- streamer/subtitle rows
- rows with uncommon positive codes

### Validation Checks

Before applying:

- unknown codes
- invalid `0/1` values
- hierarchy violations
- unmapped `custom_id` values
- missing `custom_id` values
- duplicate `custom_id` values
- duplicate `row_id` problems
- output rows not present in manifest
- manifest rows missing from output
- lost columns
- changed row order
- changed metadata columns
- missing expected code columns
- response parse failures
- expired or failed Batch API requests

After applying:

- row count unchanged
- file checksum or metadata checksum for non-code columns unchanged
- submitted rows complete
- skipped rows remain unchanged
- backup exists
- `applied_patch_log.csv` matches the written CSV

## Migration Plan

### Phase 0: Document Current State

- Preserve this planning note.
- Preserve the Ava cost note.
- Keep direct Codex smoke-test wrapper for QA.
- Confirm current prepared CSV and codebook schemas.

### Phase 1: Build Dry-Run Batch JSONL Generator

- No API submission.
- No CSV writes.
- Generate run directory.
- Generate manifest, compiled codebook, candidate rows, duplicate reuse, auto-zero candidates, cost estimate, and JSONL preview.

### Phase 2: Test One Transcript With Small Row Limit

- Use Avaritia `k84ImiUcjbE`.
- Use `--row-limit 25`.
- Compare candidate selection against direct Codex smoke-test results.
- Confirm the generated JSONL is compact and valid.

### Phase 3: Submit One Small Batch API Job

- Submit only after reviewing generated JSONL.
- Use `--execute`.
- Record batch ID and file IDs.
- Do not apply results automatically.

### Phase 4: Retrieve And Apply Results In Dry-Run Mode

- Download output and errors.
- Parse and validate responses.
- Generate `patch_preview.csv`.
- Generate `qa_summary.md`.
- Do not edit prepared CSVs yet.

### Phase 5: Execute Patch Only After Review

- Review patch preview and audit samples.
- Create backups.
- Apply with `--execute`.
- Validate written CSV.

### Phase 6: Expand To More Transcripts For Ava

- Increase transcript limit.
- Keep row limits conservative.
- Track cost estimate versus actual usage.
- Audit positive-coded rows and paid-message rows.

### Phase 7: Tune Candidate Gate And Cost Model

- Compare auto-zero audit misses.
- Adjust triggers.
- Refine duplicate reuse.
- Refine context windows.
- Update cost model.

### Phase 8: Schedule Recurring Production Runs

- Add lockfile to prevent overlapping runs.
- Run candidate/build/check/apply steps on cron.
- Keep apply step manual until QA is trusted.
- Produce run summaries for review.

## Immediate Next Steps

Implementation should start with the safest non-mutating milestone.

Checklist:

- [ ] Decide whether to implement as separate shell wrappers or one subcommand wrapper.
- [ ] Define the run directory root and run ID convention.
- [ ] Implement codebook compilation in dry-run mode.
- [ ] Implement pending-row scanner for prepared CSVs.
- [ ] Implement candidate-gating config and export `candidate_rows.csv`.
- [ ] Implement duplicate-key normalization and export `duplicate_reuse.csv`.
- [ ] Implement `batch_input.jsonl` generation without API submission.
- [ ] Implement `cost_estimate.json` and/or `cost_estimate.csv`.
- [ ] Implement `qa_summary.md` for the build step.
- [ ] Test build-only mode on Avaritia `k84ImiUcjbE` with `--row-limit 25`.
- [ ] Review generated JSONL manually before any Batch API submission.

Do not implement submission, retrieval, or CSV patching until the dry-run generator is reviewed.

## OpenAI Batch API Planning Notes

Based on the official OpenAI Batch API guide:

- Batch jobs use `.jsonl` input files.
- Each request line needs a unique `custom_id`.
- Batch output order may not match input order, so result processing must use `custom_id`.
- Batch work is asynchronous and should be treated as a separate submit/check/retrieve lifecycle.
- The documented completion window is `24h`.
- The Batch API is designed for work that does not need immediate responses and can offer lower cost than synchronous requests.

These constraints are a good fit for this qualitative coding workload because prepared transcript rows can be selected, encoded, submitted, retrieved, validated, and mechanically patched later.
