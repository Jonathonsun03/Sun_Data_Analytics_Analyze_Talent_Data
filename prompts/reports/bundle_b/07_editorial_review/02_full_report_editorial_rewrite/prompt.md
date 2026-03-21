# Bundle B Prompt: Full Report Editorial Rewrite

## Section key
`full_report_editorial_rewrite`

## Goal
Rewrite the complete set of client-facing Bundle B report paragraphs so they read like one cohesive, professionally edited report while preserving the underlying analytical meaning.

## Expected inputs
- Talent name
- Included date range
- The full set of current report-facing paragraphs, including:
  - executive summary
  - section syntheses
  - chart-level interpretation paragraphs
  - conclusion
- Optional editorial-review notes if they exist

## Instructions
- Rewrite every client-facing paragraph.
- Preserve the factual meaning, the important numbers, and the analytical direction unless the source paragraph is clearly overstated or awkwardly phrased.
- Reduce repetitive sentence structures and repeated transitions.
- Avoid making every paragraph sound like it came from the same rigid template.
- Keep the report voice consistent, natural, concise, and executive-friendly.
- The opening executive summary may mention the date range once if helpful, but later paragraphs should not keep repeating it.
- Do not remove sections and do not merge separate paragraphs together.

## Output format
Return JSON only.

The JSON must be an object whose keys exactly match the provided relative paragraph paths.
Each value must be a single rewritten paragraph string for that path.
Do not wrap the JSON in code fences.
