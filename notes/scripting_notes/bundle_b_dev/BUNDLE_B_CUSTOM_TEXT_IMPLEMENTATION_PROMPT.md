# Bundle B Customized Text Implementation Prompt

Use this prompt when extending Bundle B to support per-section customized interpretation text using the same architecture now used for Bundle A.

## Objective

Implement a full customized-text pipeline for Bundle B so that each report can:

1. export the underlying tables and figures used in the report,
2. generate interpretation text from those exported artifacts with `codex exec`,
3. store the generated text in the talent's datalake Bundle B folder,
4. render that text back into the final `Bundle_B.Rmd` report.

The implementation should mirror the Bundle A pattern as closely as possible unless Bundle B has a structural reason to differ.

## Existing Bundle B files

- Template: `templates/reports/Bundle_B/Bundle_B.Rmd`
- Description: `r_scripts/run/bundle_B/Bundle_B_Desc.md`
- Import/artifact builder: `r_scripts/run/bundle_B/import_data.r`
- Render script: `r_scripts/run/bundle_B/render_bundle_B.R`
- Current wrapper: `bin/linux/render_reports/run_bundle_B_report.sh`

## Bundle A reference implementation

Use Bundle A as the exact reference pattern for architecture, naming, and testing:

- Prompt tree: `prompts/reports/bundle_a/`
- Template: `templates/reports/Bundle_A/Bundle_A.Rmd`
- Import/artifact builder: `r_scripts/run/bundle_A/import_data.r`
- Render script: `r_scripts/run/bundle_A/render_bundle_A.R`
- Shell wrappers:
  - `bin/linux/render_reports/run_bundle_A_report.sh`
  - `bin/linux/render_reports/bundle_A/run_bundle_A_artifacts.sh`
  - `bin/linux/render_reports/bundle_A/run_bundle_A_interpretations.sh`
  - `bin/linux/render_reports/bundle_A/run_bundle_A_render_only.sh`
  - `bin/linux/render_reports/bundle_A/run_bundle_A_full_pipeline.sh`

## Required outcomes

### 1. Prompt structure

Create a Bundle B prompt tree under:

- `prompts/reports/bundle_b/`

Requirements:

- organize the prompt folders by the actual section headings in `Bundle_B.Rmd`,
- give each chart/table interpretation point its own folder with `prompt.md`,
- include section-level synthesis prompts where they are useful,
- make the output style consistent across Bundle B.

Use the same narrative pattern used for Bundle A:

- one short paragraph only,
- interpretation first,
- then why it matters,
- then a natural recommended follow-up or bridge,
- no bullets, no labeled subsections, no repetitive phrasing.

### 2. Artifact export

Update `r_scripts/run/bundle_B/import_data.r` so it exports Bundle B evidence to the talent's datalake report folder, not to a repo-local template directory.

Write outputs under:

- `<datalake_root>/<talent>/reports/bundle_B/artifacts/figures/`
- `<datalake_root>/<talent>/reports/bundle_B/artifacts/tables/`

Also write:

- `bundle_b_ai_inputs.json`
- `bundle_b_artifact_manifest.json`

Requirements:

- use the shared datalake helper so the datalake path is consistent,
- align artifact names to prompt/report keys,
- overwrite old artifact files on rerun,
- support the same report window controls used by the final renderer.

### 3. Interpretation generation

Add Bundle B shell runners under:

- `bin/linux/render_reports/bundle_B/`

Create:

- `run_bundle_B_artifacts.sh`
- `run_bundle_B_interpretations.sh`
- `run_bundle_B_render_only.sh`
- `run_bundle_B_full_pipeline.sh`

Then make the top-level convenience wrapper:

- `bin/linux/render_reports/run_bundle_B_report.sh`

act as the main full-pipeline entrypoint, the same way Bundle A now works.

The interpretation runner should:

- read Bundle B artifacts from datalake,
- build one `input.md` per prompt,
- call `codex exec`,
- write one `output.md` per prompt under:
  - `<datalake_root>/<talent>/reports/bundle_B/interpretations/<section>/<prompt>/output.md`

Keep support for test controls such as:

- `--prompt-filter`
- `--max-prompts`
- `--dry-run`

### 4. Render inclusion

Update `templates/reports/Bundle_B/Bundle_B.Rmd` so it can read generated interpretation markdown from the current talent's Bundle B folder and print it below the relevant charts or section intros.

Requirements:

- use `params$render_output_dir` to locate the current Bundle B report folder,
- read `interpretations/.../output.md`,
- gracefully skip missing files without breaking render,
- keep placement aligned with the section heading structure and existing explanatory text.

### 5. Render routing consistency

Update `r_scripts/run/bundle_B/render_bundle_B.R` and any shared render utilities as needed so Bundle B behaves like Bundle A:

- final HTML,
- artifacts,
- interpretations

should all live together under:

- `<datalake_root>/<talent>/reports/bundle_B/`

### 6. Documentation

Document the Bundle B pipeline updates in:

- `r_scripts/run/bundle_B/Bundle_B_Desc.md`
- `bin/linux/render_reports/README.md`

Explain:

- the artifact stage,
- the interpretation stage,
- the render-only step,
- the main top-level wrapper,
- example commands,
- output paths.

## Testing requirements

Use one talent first for validation.

At minimum:

1. run the artifact stage for one talent,
2. run one prompt with `--prompt-filter` and verify the output style,
3. run the full interpretation set for that talent,
4. verify all expected `output.md` files are created,
5. render the final Bundle B HTML and confirm the text is included,
6. confirm output paths are all inside the same talent `reports/bundle_B` folder.

If there are warnings that do not block output generation, note them clearly but do not stop unless they break the pipeline.

## Constraints

- Do not modify raw datalake source data.
- Write non-code outputs only to datalake.
- Keep scripts in the repository, not the datalake.
- Prefer extending existing scripts over inventing parallel ad hoc ones.
- Use `apply_patch` for manual code edits.
- Preserve Bundle B's current visual/report structure unless changes are needed to insert the new text cleanly.

## Deliverables

By the end, Bundle B should have:

- prompt files,
- datalake artifact export,
- codex-based interpretation generation,
- template inclusion of generated text,
- grouped shell runners under `bin/linux/render_reports/bundle_B/`,
- one top-level wrapper `run_bundle_B_report.sh` that runs the full pipeline,
- updated docs,
- a tested example using one talent.
