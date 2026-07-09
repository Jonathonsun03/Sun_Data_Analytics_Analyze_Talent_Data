# Bundle B Prompt: Full Report Editorial Review

## Section key
`full_report_editorial_review`

## Goal
Review the complete set of generated Bundle B paragraphs and identify where the report still needs editorial smoothing before client delivery.

## Expected inputs
- Talent name
- Included date range
- The executive summary
- The conclusion
- All section syntheses
- All chart-level interpretation paragraphs

## Instructions
- Read the generated report text as one continuous narrative.
- Focus on editorial quality rather than analytical correctness.
- Identify repeated phrasing, overused transitions, structural sameness across paragraphs, redundant takeaways, or places where the flow feels abrupt.
- Call out only the highest-value fixes.
- When possible, suggest the kind of rewrite needed, not just that something feels off.

## Output format
Return markdown only as 3-5 flat bullets.

Each bullet should name one editorial issue and the suggested fix.
Do not rewrite the full report.
Do not use nested bullets or headings.
