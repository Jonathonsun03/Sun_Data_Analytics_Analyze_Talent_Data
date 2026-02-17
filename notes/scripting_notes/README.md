# Notes: Title Classification and Qualitative Coding Plan

## Session Reconstruction (2026-02-17 UTC)

Evidence-based summary of today's work from local repo state:

- No commits were created on 2026-02-17 (UTC) in this repository.
- Latest commit remains:
  - `74abc30 | 2026-02-16 21:38:48 +0000 | Jonathon Sun`
  - Message: `Working on the title analysis script. Made it through until I needed to upgrade R.`
- Tracked files currently modified in working tree:
  - `renv.lock` (mtime: `2026-02-17 00:46:46 +0000`)
  - `renv/activate.R` (mtime: `2026-02-16 21:48:15 +0000`)
- Change volume in tracked diffs:
  - `2 files changed, 1296 insertions(+), 132 deletions(-)`
- `renv.lock` shows a major environment refresh (including R version moving from `4.2.2` to `4.5.2` and expanded package entries).
- `scripts/run/Title_analysis/Examine_titles.r` was touched on `2026-02-17 03:00:51 +0000`, but there is no current tracked git diff for that file.

Practical interpretation:

- Today's scripting activity was primarily environment/runtime alignment (renv + R upgrade path), not a committed pipeline or classification logic change.
- Main immediate scripting task remains validating that title-analysis scripts run cleanly under the updated environment before further feature work.

## What We Covered

- Confirmed current title classification outputs are:
  - `topic`
  - `format`
  - `language`
  - `is_music`
  - `tags`
  - `confidence`
- Confirmed these are enforced by the active schema in:
  - `classification/prompts/base/schema_v2.json`
- Confirmed the classifier run script is:
  - `scripts/run/Title_classification/title_classification/02_classify_pending_titles.R`
- Updated profile selection logic so classification now takes talent profiles into account more reliably:
  - exact match on normalized talent name
  - matcher-based fallback
  - inferred overlay fallback at `classification/prompts/talents/<talent>/overlay.txt`

## Current Decision

- Hold off on adding new thematic qualitative code fields to the classification schema for now.
- First develop and validate the qualitative codebook before changing model outputs.

## Next Work: Build Qualitative Codebook

I need to build a qualitative codebook with clear definitions and examples.  
Because of the number of codes, this will be split into phases to maintain consistency and coverage.

## Proposed Phased Plan

### Phase 1: Scope and Structure

- Define the top-level code families/themes.
- Decide inclusion/exclusion boundaries for each family.
- Draft a standard template for each code:
  - code name
  - definition
  - include criteria
  - exclude criteria
  - example titles
  - edge cases

### Phase 2: Initial Code Drafting

- Draft definitions for all priority/high-frequency codes first.
- Add positive and negative examples for each.
- Flag ambiguous or overlapping codes.

### Phase 3: Overlap Resolution

- Resolve code collisions and near-duplicates.
- Add decision rules for tricky cases.
- Create tie-breaker rules when multiple codes could apply.

### Phase 4: Pilot Annotation Pass

- Apply the draft codebook to a sample of titles.
- Track disagreements/uncertainty and revise definitions.
- Add missing codes discovered in pilot.

### Phase 5: Finalization for Integration

- Freeze a versioned codebook (v1).
- Map which codes should become:
  - new schema fields
  - controlled enums
  - optional free-text tags
- Plan schema/prompt updates only after codebook stability.

## Deliverables

- Versioned qualitative codebook document.
- Example library per code (accepted/rejected examples).
- Integration plan from codebook -> schema -> prompt -> validation.

## Immediate Next Step

- Start Phase 1 by listing all intended qualitative codes, then group them into code families before writing full definitions.
