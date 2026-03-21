# Bundle B: Content Strategy & Optimization

## Best for clients asking

"What kinds of content should we make more or less of?"

## Included analyses

- Engagement metrics by content category
- Content category strengths and weaknesses
- Tag, topic, collaboration, and scheduling diagnostics

## Key questions answered

- Which formats consistently retain viewer attention?
- Where are the strongest and weakest content opportunities by category?
- Which creative attributes and publishing patterns appear directionally favorable?

## Delivered outputs

- Category-level engagement and revenue comparisons
- Strength/weakness summaries by content area
- Opportunity-matrix views across content types, tags, and title labels
- Topic, tag, collaboration, weekday, and weekend diagnostics

## Strategic use

- Content mix optimization
- Format experimentation planning
- Scheduling strategy

## Pipeline

Bundle B now follows the same customized-text pipeline pattern as Bundle A, including report bookends and a final editorial rewrite before render.

Artifacts stage:

- Script: `bin/linux/render_reports/bundle_B/run_bundle_B_artifacts.sh`
- Calls: `r_scripts/run/bundle_B/import_data.r`
- Writes:
  - `<datalake_root>/<talent>/reports/bundle_B/artifacts/figures/`
  - `<datalake_root>/<talent>/reports/bundle_B/artifacts/tables/`
  - `bundle_b_ai_inputs.json`
  - `bundle_b_artifact_manifest.json`

Interpretation stage:

- Script: `bin/linux/render_reports/bundle_B/run_bundle_B_interpretations.sh`
- Reads Bundle B artifacts from the datalake
- Builds one `input.md` per prompt folder under `prompts/reports/bundle_b/`
- Runs `codex exec`
- Writes one `output.md` per prompt under:
  - `<datalake_root>/<talent>/reports/bundle_B/interpretations/<section>/<prompt>/output.md`
- Includes:
  - section/chart interpretations
  - executive summary
  - conclusion

Editorial rewrite stage:

- Script: `bin/linux/render_reports/bundle_B/run_bundle_B_editorial_rewrite.sh`
- Reads all client-facing Bundle B `output.md` files
- Runs one full-report rewrite prompt with `codex exec`
- Overwrites the client-facing interpretation markdown so the final report reads as one cohesive narrative
- Writes working files under:
  - `<datalake_root>/<talent>/reports/bundle_B/interpretations/07_editorial_review/02_full_report_editorial_rewrite/`

Render-only stage:

- Script: `bin/linux/render_reports/bundle_B/run_bundle_B_render_only.sh`
- Calls: `r_scripts/run/bundle_B/render_bundle_B.R`
- Renders final HTML into the same `<datalake_root>/<talent>/reports/bundle_B/` folder

Main wrapper:

- `bin/linux/render_reports/run_bundle_B_report.sh`
- This is the preferred entrypoint and runs the full pipeline

## Example commands

Full pipeline:

```bash
bin/linux/render_reports/run_bundle_B_report.sh \
  --talent "Avaritia Hawthorne 【Variance Project】" \
  --start-date 2025-12-21 \
  --end-date 2026-03-20 \
  --input-source datalake
```

Artifacts only:

```bash
bin/linux/render_reports/bundle_B/run_bundle_B_artifacts.sh \
  --talent "Avaritia Hawthorne 【Variance Project】" \
  --window-days 90 \
  --input-source datalake
```

Single prompt test:

```bash
bin/linux/render_reports/bundle_B/run_bundle_B_interpretations.sh \
  --talent "Avaritia Hawthorne 【Variance Project】" \
  --prompt-filter "01_engagement_distribution_by_content_type" \
  --max-prompts 1
```

Editorial rewrite only:

```bash
bin/linux/render_reports/bundle_B/run_bundle_B_editorial_rewrite.sh \
  --talent "Avaritia Hawthorne 【Variance Project】" \
  --window-days 90 \
  --input-source datalake
```

Render only:

```bash
bin/linux/render_reports/bundle_B/run_bundle_B_render_only.sh \
  --talent "Avaritia Hawthorne 【Variance Project】" \
  --window-days 90 \
  --input-source datalake
```

## Notes / Limitations

Bundle B is revenue-dependent and assumes monetary data is available for the talent.

For streamers without monetary data, use a non-monetary Bundle B variant organized around views, engagement, retention consistency, timing effects, collaboration effects, and topic/tag distributions without revenue-based scoring.
