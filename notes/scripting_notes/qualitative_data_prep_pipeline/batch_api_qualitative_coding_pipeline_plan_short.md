# Batch API Qualitative Coding Pipeline Plan

Date: 2026-05-10

Status: planning only. Do not implement from this note without a separate implementation task.

## Safety Boundary

- Do not modify transcript prep yet.
- Do not modify prepared transcript CSVs.
- Do not submit Batch API jobs yet.
- Do not change raw `text_playback` files.
- Keep direct Codex coding only as a smoke-test/audit tool for now.

## Current Pipeline

Prep entrypoints:

```text
r_scripts/run/transcript_analysis/prepare_transcript.r
bin/linux/ena_precoding/prepare_transcript.sh
```

Prep currently resolves a talent, reads `<talent folder>/text_playback`, reads the current qualitative codebook, and writes prepared CSVs to:

```text
<talent folder>/qualitative coding/monetary conversation codes
```

Prepared CSVs include metadata, `source_file`, `source_path`, and all `code_*` columns initialized as `NA`.

Key helper:

```text
r_scripts/lib/clean_data/qualitative_data_prep/qualitative_data_prep.r
```

Preserve:

- prepared CSV structure
- `code_*` naming
- codebook selectors: `current`, `latest_snapshot`, `snapshot:<filename>`, `path:<path>`
- parent/child hierarchy rules
- dry-run before execute behavior
- bounded DataLake path resolution

## Why Batch API

Direct Codex editing works for smoke tests but is too costly for production.

Observed Ava state:

```text
Prepared CSV files: 110
Total rows: 408,923
Pending rows: 408,873
Already coded rows: 50
```

The first 25-row Codex coding test used about 37,518 tokens:

```text
37,518 / 25 = about 1,500 tokens per row
408,873 pending rows = about 614M token-equivalent
```

Conclusion: use Codex for testing and audits; use a purpose-built Batch API workflow for production coding.

## Proposed Architecture

1. **Keep transcript prep unchanged.**
   Prepared CSVs remain the working sheets.

2. **Compile a compact codebook.**
   Write `compiled_codebook.json` with only code IDs, columns, hierarchy, definitions, and concise examples/triggers.

3. **Select pending rows.**
   Rows are pending when one or more expected `code_*` values are missing. Skip complete rows unless `--reprocess`.

4. **Candidate gate before model submission.**
   Split rows into model candidates, auto-zero candidates, duplicate reuse, and audit samples.

5. **Reuse duplicate decisions.**
   Reuse exact normalized duplicates for the same codebook version.

6. **Build Batch API JSONL.**
   One request per compact row batch. Include compact codebook, target rows, prior context, and structured-output instructions.

7. **Submit separately.**
   Submission requires `--execute`; build does not submit by default.

8. **Check/retrieve separately.**
   Download output/errors but do not apply automatically.

9. **Validate results.**
   Reject unknown codes, bad JSON, unmapped `custom_id`, invalid row IDs, invalid code values, and hierarchy violations.

10. **Apply mechanically.**
    Set submitted rows to 0, set returned positive codes to 1, enforce hierarchy, preserve metadata/row order, and write only with `--execute`.

11. **Export QA artifacts.**
    Produce audit samples and `qa_summary.md`.

12. **Checkpoint every run.**
    Store manifest, inputs, outputs, errors, cost estimate, previews, logs, and apply status.

## Candidate Gating

Send rows to the model when any are true:

- `paid_amount_value` present or greater than 0
- `paid_amount_text` present
- `paid_currency` present
- `message_type` suggests paid chat, superchat, donation, membership, gifted membership, milestone, sticker, or paid message
- `source` indicates streamer/subtitle/talent speech
- `speaker` indicates the talent account
- text matches codebook-derived trigger terms
- row is near a paid event, using a configurable context window
- row is part of a random audit sample

Trigger terms select candidates only; they do not decide codes.

Auto-zero rows may be filled mechanically only when the gating config is recorded, audit samples are exported, and the user explicitly allows auto-zero application.

## Duplicate Reuse

Recommended duplicate key:

```text
source | message_type | speaker_or_speaker_type | normalized_text | paid_flag
```

Normalize by trimming, lowercasing, squishing whitespace, normalizing quote artifacts, and standardizing empty/NA values.

Reuse only exact duplicate keys within the same codebook version. Do not reuse `needs_review = true` decisions. Write mappings to `duplicate_reuse.csv`.

## Batch Request Shape

Use JSONL with stable `custom_id`. Do not trust output order; join by `custom_id`.

Example line shape:

```json
{
  "custom_id": "ava__k84ImiUcjbE__rows_000026_000050__run_2026-05-10_001",
  "method": "POST",
  "url": "/v1/responses",
  "body": {
    "model": "MODEL_NAME",
    "input": [
      {"role": "system", "content": "You are a qualitative coding assistant..."},
      {"role": "user", "content": {"codebook": [], "rows": [], "instructions": "Return structured coding decisions only."}}
    ]
  }
}
```

Preferred response object:

```json
{
  "coded_rows": [
    {
      "row_id": "string or integer",
      "codes": ["A1", "A1b"],
      "confidence": "high|medium|low",
      "needs_review": true,
      "review_reason": "string or null"
    }
  ],
  "batch_notes": "string or null"
}
```

Interpretation:

- `codes` contains positive code IDs only
- absent codes become 0
- `needs_review` rows are exported for audit
- hierarchy is enforced locally after parsing

## CSV Patching

Default is preview only. In-place writes require `--execute`.

Patch rules:

- backup before write
- set submitted rows to 0 first
- set returned positive codes to 1
- treat absent codes as 0
- enforce parent/child hierarchy
- validate code values are only 0/1
- preserve row order and metadata columns
- write `applied_patch_log.csv`

Validation should check unknown codes, invalid values, hierarchy violations, unmapped or duplicate `custom_id`, duplicate row IDs, missing output rows, changed row count/order, lost columns, and changed non-code metadata.

## Proposed CLI

Prefer separate wrappers under `bin/linux/ena_precoding`:

```text
build_qualitative_batch.sh
submit_qualitative_batch.sh
check_qualitative_batch.sh
apply_qualitative_batch.sh
```

Example dry-run build for one Ava transcript:

```sh
bin/linux/ena_precoding/build_qualitative_batch.sh \
  --talent-query "Avaritia" \
  --transcript "k84ImiUcjbE" \
  --limit 1 \
  --row-limit 25 \
  --codebook current \
  --dry-run
```

Submit only after reviewing generated files:

```sh
bin/linux/ena_precoding/submit_qualitative_batch.sh \
  --run-dir /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id> \
  --execute
```

Preview apply:

```sh
bin/linux/ena_precoding/apply_qualitative_batch.sh \
  --run-dir /mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id> \
  --dry-run
```

## Run Directory

Recommended root:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/qualitative_batch_runs/<run_id>
```

Required artifacts:

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
cost_estimate.json
qa_summary.md
```

Optional:

```text
backups/
logs/
quarantine/
```

## QA Plan

Audit samples:

- auto-zero rows
- model-coded zero rows
- positive-coded rows
- `needs_review` rows
- paid-message rows
- duplicate-reuse rows
- hierarchy-enforced rows

Run summary should report candidate count, auto-zero count, duplicate reuse count, positive code count by code, needs-review count, failed/invalid response count, and estimated/actual cost.

## Migration Plan

1. **Phase 0:** preserve current state and notes.
2. **Phase 1:** build dry-run JSONL generator only.
3. **Phase 2:** test one Ava transcript with `--row-limit 25`.
4. **Phase 3:** submit one small Batch API job after review.
5. **Phase 4:** retrieve results and generate patch preview.
6. **Phase 5:** apply patch only after review and backup.
7. **Phase 6:** expand to more Ava transcripts.
8. **Phase 7:** tune candidate gate, duplicate reuse, and cost model.
9. **Phase 8:** schedule recurring production runs with lockfiles.

## Immediate Next Milestone

Build the dry-run batch generator only.

Checklist:

- [ ] choose separate wrappers vs one subcommand wrapper
- [ ] define run directory and run ID convention
- [ ] compile codebook to `compiled_codebook.json`
- [ ] scan pending rows in prepared CSVs
- [ ] implement candidate gate and write `candidate_rows.csv`
- [ ] implement duplicate reuse and write `duplicate_reuse.csv`
- [ ] generate `batch_input.jsonl`
- [ ] generate `cost_estimate.json`
- [ ] generate `qa_summary.md`
- [ ] test build-only mode on Avaritia `k84ImiUcjbE` with `--row-limit 25`

Do not implement submission, retrieval, or CSV patching until the dry-run generator is reviewed.

## Batch API Constraints

- input is JSONL
- each request needs a unique `custom_id`
- output order is not guaranteed; join by `custom_id`
- jobs are asynchronous
- documented completion window is `24h`
- Batch is appropriate because this coding work does not need immediate responses
