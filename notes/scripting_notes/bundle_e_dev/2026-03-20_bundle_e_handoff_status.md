# Bundle E Handoff Status

Date: 2026-03-20

## Where We Stopped

We paused after getting a first working Bundle E artifact pipeline in place and then refactoring it into a more modular structure.

Bundle E is now partially modularized, but it is not yet fully factored to the standard we want.

## What Is Working

### 1. Concept and planning notes exist

These notes now exist in `notes/scripting_notes/bundle_e_dev/`:

- `2026-03-20_bundle_e_architecture_plan.md`
- `2026-03-20_bundle_e_column_engineering_plan.md`

### 2. Sample talent selected and validated

Development sample:

- `Leia Memoria【Variance Project】`

Confirmed:

- repeated longitudinal `video_analytics` snapshots exist
- `Video ID` works as the stable join key
- `Published At` and snapshot file dates support lifecycle calculations
- title-classifier metadata can be joined by `Video ID`

### 3. Bundle E import pipeline currently runs

Current runner:

- `r_scripts/run/bundle_E/import_data.r`

Current output location:

- `<datalake_root>/<talent>/reports/bundle_E/artifacts/`

Validated output currently includes:

- figures
- tables
- `bundle_e_ai_inputs.json`
- `bundle_e_artifact_manifest.json`

### 4. Bundle E has been partially modularized

Current reusable Bundle E clean-data modules:

- `r_scripts/lib/clean_data/bundle_e_panel_prep.R`
- `r_scripts/lib/clean_data/bundle_e_summary_metrics.R`

Current Bundle E plot modules:

- `r_scripts/lib/plots/report/bundle_E/00_Bundle_E_Helpers.R`
- `r_scripts/lib/plots/report/bundle_E/00_Layout_Standards.R`
- `r_scripts/lib/plots/report/bundle_E/Lifecycle_Library_Growth_Plots.R`

Current shared runtime/export helpers added:

- `r_scripts/lib/utils/report_runtime_utils.R`
- `r_scripts/lib/utils/report_artifact_utils.R`

### 5. Theme alignment improved

Bundle E was adjusted so Bundle A is treated as the source of truth for layout conventions where practical.

Also updated:

- `r_scripts/lib/plots/themes/core_nyt_theme.R`

to replace deprecated `size=` usage on `element_line()` with `linewidth=`.

## Important Caveat

Even though the current Bundle E pipeline runs, the current first-pass Bundle E plots are not yet strong enough to answer the real Bundle E questions well.

This was explicitly noted before stopping.

## What Still Needs Work

### 1. More factoring / modularization is still needed

Bundle E is cleaner than before, but still not fully where it should be.

Likely next factoring work:

- reduce more orchestration weight in `r_scripts/run/bundle_E/import_data.r`
- consider splitting Bundle E summary-table builders into additional focused modules
- possibly separate:
  - panel coverage summaries
  - cohort summaries
  - attribute longevity summaries
  - leader / sleeper candidate summaries

### 2. Current Bundle E plots are not yet sufficient

Current plots are useful as a first pass, but they do **not** do justice to the Bundle E questions around:

- which videos are still growing vs plateauing
- evergreen vs front-loaded behavior
- video lifecycle shape over time
- early winners vs long-tail winners
- re-acceleration / sleeper behavior
- library durability over time

### 3. New Bundle E plots still need to be designed

Next session should prioritize stronger Bundle E-native visuals, likely including:

- normalized video lifecycle curves by video age
- early velocity vs long-tail performance matrix
- evergreen vs front-loaded classification view
- top video growth trajectories over time
- cohort aging curves
- re-acceleration / sleeper identification plots
- concentration and library depth plots

These should be designed directly against the Bundle E description, not just adapted from Bundle A/B habits.

## Recommended Next Step

When resuming, start by reviewing:

- `r_scripts/run/bundle_E/Bundle_E_desc.md`
- the notes in `notes/scripting_notes/bundle_e_dev/`
- the current Bundle E clean-data and plot modules

Then do this in order:

1. decide the next set of Bundle E visuals that better answer the Bundle E report questions
2. factor Bundle E modules a bit further where needed
3. update the artifact set to match the stronger Bundle E narrative
4. only after that, move toward the Bundle E Rmd/report layer

## Short Summary

Bundle E now has:

- a real longitudinal prep layer
- title-classifier enrichment
- artifact export
- partial modularization

But it still needs:

- additional factoring
- stronger, more Bundle E-specific plots
- a clearer report-section visual strategy before the Rmd stage
