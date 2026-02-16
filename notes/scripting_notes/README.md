# Notes: Title Classification and Qualitative Coding Plan

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
