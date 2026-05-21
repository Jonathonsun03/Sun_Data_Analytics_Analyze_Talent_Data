You are working in the Sun Data Analytics qualitative coding pipeline.

Goal:
Create a planning document for adapting the current qualitative coding pipeline into an OpenAI Batch API workflow.

Important:
Do NOT implement code yet.
Do NOT modify the current pipeline yet.
Do NOT submit any OpenAI API or Batch API jobs.
Do NOT edit transcript CSVs.
This task is planning/documentation only.

Repo:
/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data

Planning notes folder:
/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/notes/scripting_notes/qualitative_data_prep_pipeline

Current known context:
The current direct Codex qualitative coding workflow can successfully code small transcript batches in place, but it is not sustainable for full production coding because it uses too many tokens when scaled to 400k+ rows.

The goal is to preserve the current transcript prep stage and the existing prepared CSV structure, while eventually replacing direct Codex row-by-row CSV editing with a purpose-built Batch API workflow.

Known input paths:

Codebook folder:
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Qualitative Codebook/current

Avaritia transcript folder:
/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Avaritia Hawthorne 【Variance Project】/qualitative coding/monetary conversation codes

Existing smoke-test command:
bin/linux/ena_precoding/code_qualitative_transcripts.sh \
  --talent-query "Avaritia" \
  --transcript "k84ImiUcjbE" \
  --limit 1 \
  --row-limit 25

Current observed Ava state:
- Prepared CSV files: 110
- Total rows: 408,923
- Pending rows: 408,873
- Already coded rows: 50
- First live test coded rows 26-50 in place for transcript k84ImiUcjbE.
- The observed coding looked reasonable, but the token cost is not sustainable.

Task:
Inspect the repository enough to understand the current qualitative coding/prep architecture, then create a planning note in this folder:

/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data/notes/scripting_notes/qualitative_data_prep_pipeline

Suggested filename:
batch_api_qualitative_coding_pipeline_plan.md

The planning document should include:

1. Current pipeline summary
   - What the existing transcript prep stage does.
   - What the current direct Codex coding script appears to do.
   - What files/folders are involved.
   - What should be preserved.

2. Problem statement
   - Why direct Codex CSV editing is useful for smoke tests but not sustainable for full Ava production coding.
   - Explain the token/cost scaling problem clearly.

3. Proposed Batch API architecture
   Include a clear staged design:

   A. Transcript prep remains unchanged.
   B. Compact codebook compilation.
   C. Pending-row selection.
   D. Candidate gating.
   E. Duplicate reuse.
   F. Batch JSONL request creation.
   G. Batch API submission.
   H. Batch status check/retrieval.
   I. Batch result validation.
   J. Mechanical patch application to existing CSVs.
   K. Hierarchy enforcement.
   L. Audit exports and QA summaries.
   M. Checkpointing and run manifests.

4. Candidate-gating plan
   Explain how to avoid sending every row to the model.
   Candidate triggers should include:
   - paid_amount_value present or > 0
   - paid_amount_text present
   - paid_currency present
   - message_type suggesting paid chat, superchat, donation, membership, gifted membership, milestone, sticker, etc.
   - source indicating streamer/subtitle/talent speech
   - codebook-derived trigger terms
   - configurable context window around paid events
   - random audit sample of auto-zero rows

5. Duplicate-reuse plan
   Explain how to normalize text and reuse prior decisions for exact duplicate rows.
   Include a recommended duplicate key using:
   - source
   - message_type
   - speaker or speaker_type if available
   - normalized text
   - paid flag

6. Batch API request/response design
   Include recommended JSONL structure.
   Explain use of custom_id.
   Explain why output order should not be trusted.
   Include a proposed structured output schema like:

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

7. CSV patching design
   Explain:
   - Set submitted rows to 0 first.
   - Set returned positive codes to 1.
   - Treat absent codes as 0.
   - Enforce parent/child hierarchy.
   - Validate only 0/1 values in completed code columns.
   - Preserve row order and metadata columns.
   - Require --execute before writing in place.
   - Create backups or snapshots before modifying CSVs.

8. Proposed CLI scripts
   Recommend a safe command structure, such as:

   bin/linux/ena_precoding/build_qualitative_batch.sh
   bin/linux/ena_precoding/submit_qualitative_batch.sh
   bin/linux/ena_precoding/check_qualitative_batch.sh
   bin/linux/ena_precoding/apply_qualitative_batch.sh

   Or recommend a single script with subcommands if that better matches the repo.

   Include example dry-run commands for one Avaritia transcript only.

9. Run directory design
   Recommend a run directory containing:
   - manifest.json
   - compiled_codebook.json
   - candidate_rows.csv
   - auto_zero_candidates.csv
   - duplicate_reuse.csv
   - batch_input.jsonl
   - batch_output.jsonl
   - batch_errors.jsonl
   - patch_preview.csv
   - applied_patch_log.csv
   - audit_sample.csv
   - cost_estimate.csv or cost_estimate.json
   - qa_summary.md

10. QA and audit plan
   Include audit samples for:
   - auto-zero rows
   - model-coded zero rows
   - positive-coded rows
   - needs_review rows
   - paid-message rows

   Include validation checks for:
   - unknown codes
   - invalid 0/1 values
   - hierarchy violations
   - unmapped custom_id values
   - lost columns
   - changed row order
   - duplicate row_id problems

11. Migration plan
   Provide a phased plan:
   - Phase 0: document current state
   - Phase 1: build dry-run batch JSONL generator
   - Phase 2: test one transcript with small row limit
   - Phase 3: submit one small Batch API job
   - Phase 4: retrieve and apply results in dry-run mode
   - Phase 5: execute patch only after review
   - Phase 6: expand to more transcripts for Ava
   - Phase 7: tune candidate gate and cost model
   - Phase 8: schedule recurring production runs

12. Immediate next steps
   End the note with a concrete checklist of what should be implemented first.

Style:
- Write the note as a practical engineering plan.
- Use headings and checklists.
- Keep naming consistent with the existing Sun Data Analytics / qualitative coding / datalake conventions.
- Do not invent a completely new architecture if the repo already has relevant conventions.
- Be explicit about what should NOT be changed yet.
- Make the document useful enough that it can become the implementation prompt later.

After creating the planning note:
1. Print the path to the created markdown file.
2. Briefly summarize what you found in the repo.
3. Briefly summarize the recommended first implementation milestone.
4. Do not implement the Batch API runner yet.